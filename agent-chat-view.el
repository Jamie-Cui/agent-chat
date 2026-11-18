;;; agent-chat-view.el --- Own native Agent Chat window lifecycles -*- lexical-binding: t; -*-

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
;; Own the native message/draft pair, dedication and atomic-window metadata.
;; Callers choose display policy, then use `agent-chat-view-show',
;; `agent-chat-view-replace' or `agent-chat-view-hide'.  No operation kills a
;; session or changes its draft, connection or queue.  Show/replace restore
;; the original complete layout on failure and re-signal the primary error.
;; The core requires this library; declarations avoid a circular require.

;;; Code:

(require 'cl-lib)
(declare-function agent-chat-owner "agent-chat" (&optional buffer))
(declare-function agent-chat--message-buffer "agent-chat" ())
(declare-function agent-chat--prompt-window-size "agent-chat" (window total))
(declare-function agent-chat--render-queue "agent-chat" (window))
(declare-function agent-chat--render-history "agent-chat" (window))
(declare-function agent-chat--redraw "agent-chat" ())
(declare-function agent-chat--window-change "agent-chat" (window))
(defvar agent-chat--history-start)

(defvar agent-chat-view-replaced-functions nil
  "Functions called with a retained WINDOW after show or replace.
Adapters may synchronize their buffer identity here.  These functions also
run after rollback.  Native deletion is handled by the display adapter's
own close lifecycle, so it does not call this hook.")

(defvar agent-chat-view--changing nil
  "Non-nil while a view transaction suppresses layout hooks.")

(defun agent-chat-view--panes (window)
  "Return WINDOW's (PROMPT HISTORY) pair, or nil for an unpaired window.
Use the live tree to validate pointers and recover missing or stale ones
following workspace restoration.  Existing pointers may address a forced
replacement that still needs to be released."
  (when (window-live-p window)
    (let* ((owner (agent-chat-owner (window-buffer window)))
           (next (window-next-sibling window))
           (prompt (if (and (window-live-p next)
                            (or (eq next (window-parameter window 'agent-chat-prompt-window))
                                (and owner (eq (window-buffer next) owner)
                                     (with-current-buffer (window-buffer window)
                                       (derived-mode-p 'agent-chat-history-mode)))))
                       next window))
           (history (window-prev-sibling prompt)))
      (when (and (window-live-p history)
                 (null (window-prev-sibling history))
                 (null (window-next-sibling prompt))
                 (= (car (window-edges history)) (car (window-edges prompt)))
                 (or (eq history (window-parameter prompt 'agent-chat-history-window))
                     (eq prompt (window-parameter history 'agent-chat-prompt-window))
                     (and owner (eq (window-buffer prompt) owner)
                          (eq (agent-chat-owner (window-buffer history)) owner)
                          (with-current-buffer (window-buffer history)
                            (derived-mode-p 'agent-chat-history-mode)))))
        (list prompt history)))))

(defun agent-chat-view--remember (windows)
  "Remember draft points and message scroll positions in WINDOWS."
  (dolist (window windows)
    (when (window-live-p window)
      (with-current-buffer (window-buffer window)
        (when (agent-chat-owner)
          (goto-char (window-point window))
          (when (derived-mode-p 'agent-chat-history-mode)
            (setq agent-chat--history-start (window-start window))))))))

(defun agent-chat-view--transaction (window function)
  "Call FUNCTION atomically with respect to WINDOW's frame layout.
Restore native windows, parameters, selection and history on error or quit.
Keep the primary failure visible even if rollback itself fails."
  (unless (window-live-p window) (user-error "The target window is no longer live"))
  (let* ((configuration (current-window-configuration (window-frame window)))
         (parameters nil)
         (agent-chat-view--changing t))
    (walk-window-subtree
     (lambda (node)
       (push (list node (copy-tree (window-parameters node))) parameters))
     (frame-root-window (window-frame window)) t)
    (condition-case failure
        (funcall function)
      ((error quit)
       (condition-case rollback
           (progn
             (set-window-configuration configuration)
             (dolist (entry parameters)
               (let ((node (car entry)) (saved (cadr entry)))
                 (when (window-valid-p node)
                   (dolist (param (window-parameters node))
                     (set-window-parameter node (car param) nil))
                   (dolist (param saved)
                     (set-window-parameter node (car param) (cdr param))))))
             (run-hook-with-args 'agent-chat-view-replaced-functions window))
         ((error quit)
          (display-warning 'agent-chat
                           (format "View rollback failed: %s" (error-message-string rollback))
                           :error)))
       (signal (car failure) (cdr failure))))))

(defun agent-chat-view--collapse (window)
  "Release WINDOW's pair, keeping WINDOW and both buffers' reading positions."
  (when-let* ((panes (agent-chat-view--panes window)))
    (agent-chat-view--remember panes)
    (walk-window-subtree
     (lambda (child) (set-window-parameter child 'window-atom nil))
     (window-parent window) t)
    (dolist (pane panes)
      (dolist (parameter '(agent-chat-history-window agent-chat-prompt-window delete-window))
        (set-window-parameter pane parameter nil))
      (set-window-dedicated-p pane nil))
    (let ((ignore-window-parameters t))
      (delete-window (car (remq window panes))))))

(defun agent-chat-view--ensure (prompt)
  "Pair native PROMPT with this conversation's messages, returning the latter.
The parent is an atomic window: deleting or maximizing either child acts on
both.  Both children are strongly dedicated to their conversation buffers.
The original window remains the prompt, retaining Popwin's identity."
  (let* ((history (cadr (agent-chat-view--panes prompt)))
         (buffer (agent-chat--message-buffer)))
    (unless (and (window-live-p history)
                 (eq (window-parent prompt) (window-parent history)))
      (let* ((ignore-window-parameters t)
             (window-combination-limit t)
             (window-min-height 2)
             (size (agent-chat--prompt-window-size prompt (window-total-height prompt))))
        (setq history (split-window prompt size 'above))))
    (window-make-atom (window-parent prompt))
    (set-window-parameter prompt 'agent-chat-history-window history)
    (set-window-parameter history 'agent-chat-prompt-window prompt)
    (set-window-parameter prompt 'delete-window #'agent-chat-view-hide)
    (set-window-parameter history 'delete-window #'agent-chat-view-hide)
    (unless (eq (window-buffer history) buffer)
      (with-current-buffer (window-buffer history)
        (when (derived-mode-p 'agent-chat-history-mode)
          (goto-char (window-point history))
          (setq agent-chat--history-start (window-start history))))
      (set-window-dedicated-p history nil)
      (set-window-buffer history buffer)
      (with-current-buffer buffer
        (set-window-point history (point))
        (set-window-start history agent-chat--history-start t)))
    ;; Reused or restored windows can retain another buffer's left gutter.
    (dolist (window (list prompt history))
      (set-window-dedicated-p window t)
      (let ((margins (window-margins window)))
        (when (car margins)
          (set-window-margins window 0 (cdr margins)))))
    (let* ((root (window-parent prompt))
           (desired (agent-chat--prompt-window-size prompt (window-total-height root)))
           (delta (- desired (window-total-height prompt))))
      (unless (= delta 0)
        (let ((ignore-window-parameters t))
          (window-resize prompt delta nil t))))
    history))

(defun agent-chat-view-show (owner &optional window norecord)
  "Show conversation OWNER in WINDOW, returning its draft window.
WINDOW defaults to the selected window; either pane of an existing view
addresses the entire view.  Keep focus and Popwin's original draft window.
NORECORD has the meaning used by `switch-to-buffer'.  Failures restore the
complete original layout.  Do not override an unrelated dedicated window."
  (unless (and (buffer-live-p owner) (eq owner (agent-chat-owner owner)))
    (user-error "Not an Agent Chat conversation buffer"))
  (setq window (or window (selected-window)))
  (unless (window-live-p window) (user-error "The target window is no longer live"))
  (let* ((panes (agent-chat-view--panes window))
         (prompt (or (car panes) window)))
    (when (and (window-dedicated-p prompt)
               (not (agent-chat-owner (window-buffer prompt))))
      (user-error "The target window is dedicated to another buffer"))
    (agent-chat-view--transaction
     prompt
     (lambda ()
       (agent-chat-view--remember (or panes (list prompt)))
       (set-window-dedicated-p prompt nil)
       (with-selected-window prompt
         (let ((switch-to-buffer-obey-display-actions nil))
           (switch-to-buffer owner norecord t)))
       (with-current-buffer owner
         (agent-chat-view--ensure prompt)
         (agent-chat--redraw))
       (unless (and (window-live-p prompt)
                    (eq (window-buffer prompt) owner)
                    (agent-chat-view--panes prompt))
         (error "Agent Chat view was removed during display"))
       (run-hook-with-args 'agent-chat-view-replaced-functions prompt)
       prompt))))

(defun agent-chat-view-replace (window buffer)
  "Replace WINDOW's entire conversation view with ordinary BUFFER.
Keep WINDOW, focus and all session data.  Release atomic metadata and
dedication only for an owned view.  Failures restore the complete old view.
Use `agent-chat-view-show' to display another conversation instead."
  (unless (buffer-live-p buffer) (user-error "The replacement buffer is no longer live"))
  (when (agent-chat-owner buffer) (user-error "Use agent-chat-view-show for conversations"))
  (agent-chat-view--transaction
   window
   (lambda ()
     (let ((owned (agent-chat-owner (window-buffer window))))
       (agent-chat-view--collapse window)
       (when owned (set-window-dedicated-p window nil)))
     (with-selected-window window
       (let ((switch-to-buffer-obey-display-actions nil))
         (switch-to-buffer buffer nil t)))
     (run-hook-with-args 'agent-chat-view-replaced-functions window)
     window)))

(defun agent-chat-view-hide (window)
  "Hide the entire conversation containing WINDOW without killing its buffers.
This is also the native `delete-window' handler.  A maximized pair leaves
one ordinary window.  Dead windows are already hidden and require no action."
  (when (window-live-p window)
    (let* ((panes (agent-chat-view--panes window))
           (prompt (car panes)) (history (cadr panes))
           (root (and prompt (or (window-atom-root prompt) (window-parent prompt))))
           (ignore-window-parameters t))
      (unless (window-valid-p root) (user-error "Window has no complete Agent Chat view"))
      (agent-chat-view--remember panes)
      (if (window-parent root)
          (delete-window root)
        ;; Deleting the original prompt lets Popwin observe that it closed.
        (let ((replacement (other-buffer (window-buffer prompt) t (window-frame prompt))))
          (agent-chat-view--collapse history)
          (set-window-buffer history replacement))))))

(defun agent-chat-view-refresh (owner)
  "Refit and render live windows still displaying OWNER.
Recheck window ownership after rendering because callbacks can remove or
repurpose windows.  Never carry a dead window into geometry operations."
  (with-current-buffer owner
    (when-let* ((prompts (get-buffer-window-list owner nil t)))
      (agent-chat--render-queue
       (car (sort prompts (lambda (a b) (< (window-body-width a)
                                          (window-body-width b)))))))
    (let (histories)
      (dolist (prompt (get-buffer-window-list owner nil t))
        (when (and (window-live-p prompt) (eq (window-buffer prompt) owner))
          (push (agent-chat-view--ensure prompt) histories)))
      (setq histories
            (cl-remove-if-not
             (lambda (window)
               (and (window-live-p window)
                    (eq (agent-chat-owner (window-buffer window)) owner))) histories))
      (when histories
        (agent-chat--render-history
         (car (sort histories (lambda (a b) (< (window-body-width a)
                                              (window-body-width b))))))))))

(defun agent-chat-view-maintain-frame (frame)
  "Repair native views after buffer replacement or workspace restore in FRAME."
  (unless agent-chat-view--changing
    ;; Release forced replacements first, independent of the selected pane's
    ;; position in `window-list'.  Otherwise a redraw could overwrite them.
    (dolist (window (window-list frame 'no-minibuffer))
      (when (and (window-live-p window)
                 (not (agent-chat-owner (window-buffer window)))
                 (agent-chat-view--panes window))
        (agent-chat-view--collapse window)
        (run-hook-with-args 'agent-chat-view-replaced-functions window)))
    (dolist (window (window-list frame 'no-minibuffer))
      (when (and (window-live-p window)
                 (eq (window-buffer window) (agent-chat-owner (window-buffer window))))
        (agent-chat--window-change window)))))

(provide 'agent-chat-view)
;;; agent-chat-view.el ends here
