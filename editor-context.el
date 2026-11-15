;;; editor-context.el --- Source context for copying and agent drafts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Assisted-by: Codex:GPT-6
;; Maintainer: Jamie Cui <jamie.cui@outlook.com>
;; Version: 0.1.0
;; URL: https://github.com/Jamie-Cui/agent-chat
;; Keywords: tools, processes
;; Package-Requires: ((emacs "30.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of agent-chat.
;;
;; agent-chat is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; agent-chat is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with agent-chat.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;; Capture editor selections once, then format them for a clipboard or draft.
;; Capture is read-only and uses buffer text, including unsaved edits.  Callers
;; own delivery, session/host checks and file-name policy.  No files are read,
;; saved or resolved through a network by this library.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dired)
(require 'rect)
(require 'tabify)

(declare-function evil-visual-state-p "evil-states" ())
(declare-function evil-visual-range "evil-states" ())
(declare-function evil-apply-on-rectangle "evil-common" (function start end &rest args))

(defun editor-context--rectangle-row (left right)
  "Copy display columns LEFT through RIGHT on this line, after filtering.
Columns are zero-based and RIGHT is exclusive.  Match native rectangle
extraction's tab expansion and virtual spaces without modifying the source."
  (save-excursion
    (move-to-column left)
    (let ((beg (point)) (beg-column (current-column)))
      (move-to-column right)
      (let ((end-column (current-column))
            (text (filter-buffer-substring beg (point)))
            (width tab-width))
        (concat
         (make-string (max 0 (- beg-column left)) ?\s)
         (with-temp-buffer
           (setq tab-width width)
           ;; Keep source tab stops even when the selection starts mid-line.
           (insert (make-string beg-column ?\s) text)
           (untabify (1+ beg-column) (point-max))
           (buffer-substring-no-properties (1+ beg-column) (point-max)))
         (make-string (max 0 (- right (max left end-column))) ?\s))))))

(defun editor-context--text ()
  "Capture the current selection or line as one source descriptor."
  (let* ((visual (and (bound-and-true-p evil-local-mode)
                      (fboundp 'evil-visual-state-p)
                      (evil-visual-state-p)
                      (evil-visual-range)))
         (selected (or visual (use-region-p)))
         (rectangle (and selected
                         (if visual (eq (nth 2 visual) 'block)
                           (bound-and-true-p rectangle-mark-mode))))
         (beg (cond (visual (nth 0 visual))
                    (selected (min (point) (mark)))
                    (t (line-beginning-position))))
         (end (cond (visual (nth 1 visual))
                    (selected (max (point) (mark)))
                    (t (line-end-position))))
         (file (or buffer-file-name
                   (when-let* ((base (buffer-base-buffer)))
                     (buffer-local-value 'buffer-file-name base))))
         columns text first last)
    (if rectangle
        (let (rows)
          (funcall (if visual #'evil-apply-on-rectangle #'apply-on-rectangle)
                   (lambda (left right)
                     (setq columns (cons left right))
                     (push (editor-context--rectangle-row left right) rows))
                   beg end)
          (setq text (string-join (nreverse rows) "\n")))
      (setq text (substring-no-properties (filter-buffer-substring beg end))))
    (save-restriction
      (widen)
      (setq first (line-number-at-pos beg)
            last (line-number-at-pos
                  (if (and (not rectangle) (> end beg)) (1- end) end))))
    (list :file file :buffer (buffer-name) :first first :last last
          :columns columns :text text)))

(defun editor-context-capture ()
  "Return source descriptors for the current buffer, without changing it.
Dired supplies its marked files, or the file at point.  Other buffers
supply their exact active selection, including native/Evil rectangles,
or their current line.  Copy filters restore rendered content to text.
Non-text file viewers supply only a source reference, never raw file bytes.

Each descriptor has :file or :buffer, optional :first/:last (one-based
inclusive source lines), :columns (zero-based half-open display columns
for rectangles), and :text.  Dired descriptors also include :directory
so a caller can check their source host before delivery."
  (save-mark-and-excursion
    (save-restriction
      (cond
       ((derived-mode-p 'dired-mode)
        (mapcar (lambda (file) (list :file file :directory default-directory))
                (dired-get-marked-files)))
       ((derived-mode-p 'image-mode 'doc-view-mode 'pdf-view-mode)
        (list (list :file buffer-file-name :buffer (buffer-name))))
       (t (list (editor-context--text)))))))

(defun editor-context-file-reference (file &optional first last)
  "Format FILE as an @ reference, optionally spanning FIRST through LAST.
Quote the whole location when whitespace, quotes or backslashes require it."
  (let ((location (if first (format "%s:%d-%d" file first (or last first)) file)))
    (concat "@" (if (string-match-p "[[:space:]\"\\\\]" location)
                    (let ((print-escape-newlines t)) (prin1-to-string location))
                  location))))

(defun editor-context-format (contexts &optional file-name-function)
  "Format captured CONTEXTS as references followed by their exact text.
FILE-NAME-FUNCTION, when non-nil, maps a source file to its destination
spelling.  By default retain the absolute file name, including TRAMP.
Buffer references are explicitly labelled and quoted.  Rectangle columns
are zero-based and half-open.  Empty text produces only a reference.
Signal `user-error' for absent context; never return an empty success."
  (unless contexts (user-error "No context to copy"))
  (mapconcat
   (lambda (context)
     (let* ((file (plist-get context :file))
            (buffer (plist-get context :buffer))
            (first (plist-get context :first))
            (last (plist-get context :last))
            (columns (plist-get context :columns))
            (text (plist-get context :text))
            (reference
             (cond
              (file (editor-context-file-reference
                     (if file-name-function (funcall file-name-function file) file)
                     first last))
              (buffer (let ((print-escape-newlines t))
                        (concat (format "buffer:%S" buffer)
                                (when first (format ":%d-%d" first last)))))
              (t (user-error "No source for copied context")))))
       (concat reference
               (when columns
                 (format " [rectangle columns %d:%d)" (car columns) (cdr columns)))
               (when (and text (not (string-empty-p text))) (concat "\n\n" text)))))
   contexts "\n"))

(provide 'editor-context)
;;; editor-context.el ends here
