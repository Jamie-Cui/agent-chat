;;; agent-chat-popwin.el --- Display Agent Chat conversations in Popwin -*- lexical-binding: t; -*-

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
;; Reuse the native window pair and restore the original popup after Consult preview.

;;; Code:

(require 'cl-lib)
(declare-function popwin:current-context "popwin" ())
(declare-function popwin:use-context "popwin" (context))
(declare-function popwin:stop-close-popup-window-timer "popwin" ())
(declare-function popwin:start-close-popup-window-timer "popwin" ())
(declare-function popwin:close-popup-window "popwin" (&optional keep-selected))
(declare-function popwin:pop-to-buffer "popwin" (buffer &optional other-window norecord))
(defvar popwin:context-stack)
(defvar popwin:popup-window)
(defvar popwin:popup-buffer)
(defvar popwin:popup-last-config)
(defvar popwin:focus-window)

(declare-function agent-chat--redraw "agent-chat" ())

(defun agent-chat-popwin--prune-contexts ()
  "Forget stale Popwin contexts without closing their repurposed windows."
  (require 'popwin)
  (let* ((current (popwin:current-context))
         (contexts
          (cl-remove-if-not
           (lambda (context)
             (let ((window (plist-get context 'popwin:popup-window)))
               (or (null window)
                   (and (window-live-p window)
                        (eq (window-buffer window)
                            (plist-get context 'popwin:popup-buffer))))))
           (cons current popwin:context-stack))))
    ;; Keep an empty context even if an earlier restore lost the stack's
    ;; sentinel.  `popwin:use-context' does not reset variables given nil.
    (unless contexts
      (setq contexts
            (list (cl-loop for (name _value) on current by #'cddr
                           append (list name nil)))))
    (popwin:use-context (car contexts))
    (setq popwin:context-stack (cdr contexts))
    (unless popwin:popup-window
      (popwin:stop-close-popup-window-timer))))

(defun agent-chat-popwin-layout ()
  "Complete an Agent Chat popup's native window pair before returning focus."
  (require 'popwin)
  (when (window-live-p popwin:popup-window)
    (save-selected-window
      (with-current-buffer (window-buffer popwin:popup-window)
        (when (derived-mode-p 'agent-chat-mode)
          (agent-chat--redraw))))))

(defun agent-chat-popwin--switch-conversation (window buffer norecord)
  "Show conversation BUFFER in dedicated WINDOW, respecting NORECORD.
Only conversation navigation releases the window's native dedication."
  (unless (with-current-buffer buffer (derived-mode-p 'agent-chat-mode))
    (user-error "Not an Agent Chat conversation buffer"))
  (unwind-protect
      (progn
        (set-window-dedicated-p window nil)
        (with-selected-window window
          (let ((switch-to-buffer-obey-display-actions nil))
            (switch-to-buffer buffer norecord t))))
    (when (window-live-p window) (set-window-dedicated-p window t))))

(defun agent-chat-popwin-reuse-window (buffer _alist)
  "Display Agent Chat BUFFER in its existing Popwin panel, without selecting it.
Return nil when no Agent Chat popup is available, so Popwin opens one normally."
  (agent-chat-popwin--prune-contexts)
  (when (and (window-live-p popwin:popup-window)
             (eq (window-frame popwin:popup-window) (selected-frame))
             (with-current-buffer (window-buffer popwin:popup-window)
               (derived-mode-p 'agent-chat-mode)))
    (let ((window popwin:popup-window))
      (agent-chat-popwin--switch-conversation window buffer t)
      (setq popwin:popup-buffer buffer
            popwin:popup-last-config (cons buffer (cdr popwin:popup-last-config)))
      (agent-chat-popwin-layout)
      window)))

(defun agent-chat-popwin-state ()
  "Preview Agent Chat candidates in popwin, restoring the popup on cancellation."
  (agent-chat-popwin--prune-contexts)
  (let* ((window (and (window-live-p popwin:popup-window)
                      (with-current-buffer (window-buffer popwin:popup-window)
                        (derived-mode-p 'agent-chat-mode))
                      popwin:popup-window))
         (original (and window (window-buffer window)))
         (point (and window (window-point window)))
         (start (and window (window-start window)))
         (last-config popwin:popup-last-config)
         preview)
    (cl-labels
        ((show (buffer norecord)
           (agent-chat-popwin--prune-contexts)
           (if (and (window-live-p window) (eq window popwin:popup-window))
               (progn
                 (agent-chat-popwin--switch-conversation window buffer norecord)
                 ;; Keep popwin's buffer identity in sync when reusing its pane.
                 (setq popwin:popup-buffer buffer
                       popwin:popup-last-config
                       (cons buffer (cdr popwin:popup-last-config))))
             (when (eq window popwin:popup-window)
               (popwin:close-popup-window t))
             (setq window (save-selected-window
                            (popwin:pop-to-buffer buffer nil norecord))))
           (agent-chat-popwin-layout))
         (restore ()
           (when preview
             (when (or (eq window popwin:popup-window)
                       (and (not (window-live-p window)) (not popwin:popup-window)))
               (if (buffer-live-p original)
                   (progn
                     (show original t)
                     (set-window-point window point)
                     (set-window-start window start t))
                 ;; Closing redisplays.  Pause the idle watcher so it cannot
                 ;; reenter close and discard the previous popup's context.
                 (popwin:stop-close-popup-window-timer)
                 (unwind-protect
                     (popwin:close-popup-window t)
                   (when (window-live-p popwin:popup-window)
                     (popwin:start-close-popup-window-timer)))))
             (setq popwin:popup-last-config last-config
                   preview nil))))
      (lambda (action candidate)
        (let ((buffer (and candidate (get-buffer candidate))))
          (pcase action
            ('preview
             (if (buffer-live-p buffer)
                 (progn
                   (setq preview t)
                   (show buffer t))
               (restore)))
            ('exit (restore))
            ('return
             (restore)
             (when candidate
               (unless (buffer-live-p buffer)
                 (user-error "The selected Agent Chat buffer no longer exists"))
               (show buffer nil)
               (setq popwin:focus-window window)
               (select-window window)))))))))

(provide 'agent-chat-popwin)
;;; agent-chat-popwin.el ends here
