;;; agent-chat-mode-line.el --- Perspective agent activity -*- lexical-binding: t; -*-

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
;; Show the current perspective's conversations as (persp-name: A3 ⠋2 ? !).
;; A counts its Agent Chat composer buffers; the spinner counts busy sessions,
;; ? counts sessions waiting for input, and ! counts failed/disconnected ones.
;; Status counts of one are omitted; empty perspectives show A0.  Hover for
;; titles and full statuses.
;; Use Perspective's membership and Agent Chat's status display.  Cache outside
;; redisplay; the spinner only formats cached counts and never scans buffers.

;;; Code:

(require 'cl-lib)
(require 'agent-chat)
(declare-function persp-curr "perspective" (&optional frame))
(declare-function persp-name "perspective" (perspective))
(declare-function persp-buffers "perspective" (perspective))
(declare-function perspectives-hash "perspective" (&optional frame))
(defvar persp-mode)
(require 'subr-x)

(defgroup agent-chat-mode-line nil
  "Perspective Agent Chat activity in the mode line."
  :group 'mode-line-faces)

(defcustom agent-chat-mode-line-spinner-interval 0.12
  "Seconds between activity spinner frames."
  :type 'number :group 'agent-chat-mode-line)

(defface agent-chat-mode-line-active-face
  '((t (:inherit warning :weight bold)))
  "Face used for running agent work."
  :group 'agent-chat-mode-line)

(defface agent-chat-mode-line-attention-face
  '((t (:inherit error :weight bold)))
  "Face used for failed or disconnected sessions."
  :group 'agent-chat-mode-line)

(defface agent-chat-mode-line-label-face
  '((t (:inherit shadow :weight bold)))
  "Face used for the total number of agent buffers."
  :group 'agent-chat-mode-line)

(defconst agent-chat-mode-line--spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])
(defvar agent-chat-mode-line--spinner-index 0)
(defvar agent-chat-mode-line--spinner-timer nil)
(defvar agent-chat-mode-line--installed nil)
(defvar agent-chat-mode-line--perspectives (make-hash-table :test #'eq)
  "Perspective objects mapped to [total running waiting failed tooltip render].")
(defvar agent-chat-mode-line-mode)

(defvar agent-chat-mode-line--mode-line
  '(:eval (agent-chat-mode-line--render)))
(put 'agent-chat-mode-line--mode-line 'risky-local-variable t)

(defun agent-chat-mode-line--status ()
  "Classify this conversation as running, waiting, failed or ready."
  (let* ((session agent-chat--session)
         (phase (and session (agent-chat--session-phase session)))
         (state (and agent-chat--state (agent-chat--state-status agent-chat--state))))
    (cond
     ((and session (or (agent-chat--session-permissions session)
                       (eq phase 'authentication-required))) 'waiting)
     ((or (memq phase '(failed disconnected))
          (and session (agent-chat--session-stopped session))) 'failed)
     ((or (memq phase '(connecting authenticating))
          (and session (agent-chat--session-cancelling session))
          (eq state 'running)) 'running)
     ((eq state 'failed) 'failed)
     (t 'ready))))

(defun agent-chat-mode-line--count-suffix (count)
  "Return COUNT as a suffix only when greater than one."
  (if (> count 1) (number-to-string count) ""))

(defun agent-chat-mode-line--build-render (entry)
  "Format cached perspective ENTRY with the current spinner frame."
  (when (> (aref entry 0) 0)
    (let ((parts (list (propertize (format "A%d" (aref entry 0))
                                   'face 'agent-chat-mode-line-label-face))))
      (cl-loop for index from 1 to 3
               for symbol in (list (aref agent-chat-mode-line--spinner-frames
                                         agent-chat-mode-line--spinner-index) "?" "!")
               for face in '(agent-chat-mode-line-active-face
                             warning agent-chat-mode-line-attention-face)
               when (> (aref entry index) 0)
               do (push (propertize (concat symbol (agent-chat-mode-line--count-suffix (aref entry index)))
                                    'face face) parts))
      (propertize (string-join (nreverse parts) " ")
                  'help-echo (aref entry 4)))))

(defun agent-chat-mode-line--spinner-tick ()
  "Advance cached perspective indicators without inspecting editor buffers."
  (setq agent-chat-mode-line--spinner-index
        (% (1+ agent-chat-mode-line--spinner-index)
           (length agent-chat-mode-line--spinner-frames)))
  (maphash (lambda (_key entry)
             (when (> (aref entry 1) 0)
               (aset entry 5 (agent-chat-mode-line--build-render entry))))
           agent-chat-mode-line--perspectives)
  (force-mode-line-update t))

(defun agent-chat-mode-line--stop-spinner ()
  "Stop the shared activity timer."
  (when (timerp agent-chat-mode-line--spinner-timer)
    (cancel-timer agent-chat-mode-line--spinner-timer))
  (setq agent-chat-mode-line--spinner-timer nil
        agent-chat-mode-line--spinner-index 0))

(defun agent-chat-mode-line--collect (buffers &optional excluded-buffer)
  "Count composer BUFFERS, omitting EXCLUDED-BUFFER while it is being killed."
  (let ((entry (vector 0 0 0 0 nil nil)))
    (dolist (buffer (delete-dups (copy-sequence buffers)))
      (when (and (buffer-live-p buffer) (not (eq buffer excluded-buffer)))
        (with-current-buffer buffer
          ;; The messages and queue buffers are views, not extra sessions.
          (when (derived-mode-p 'agent-chat-mode)
            (let* ((status (agent-chat-mode-line--status))
                   (index (pcase status ('running 1) ('waiting 2) ('failed 3))))
              (cl-incf (aref entry 0))
              (when index (cl-incf (aref entry index)))
              (push (format "%s — %s — %s"
                            (agent-chat--conversation-status t)
                            (agent-chat--session-title-label
                             (and agent-chat--session (agent-chat--session-title agent-chat--session)))
                            (buffer-name))
                    (aref entry 4)))))))
    (aset entry 4 (when (aref entry 4) (string-join (nreverse (aref entry 4)) "\n")))
    entry))

(defun agent-chat-mode-line--refresh (&optional excluded-buffer)
  "Rebuild Perspective counts, omitting an optional dying EXCLUDED-BUFFER.
Key by perspective object so equal names in different frames remain separate."
  (clrhash agent-chat-mode-line--perspectives)
  (let (running)
    (when persp-mode
      (dolist (frame (frame-list))
        (maphash
         (lambda (_name perspective)
           (let ((entry (agent-chat-mode-line--collect
                         (persp-buffers perspective) excluded-buffer)))
             (puthash perspective entry agent-chat-mode-line--perspectives)
             (when (> (aref entry 1) 0) (setq running t))))
         (perspectives-hash frame))))
    (if running
        (unless (timerp agent-chat-mode-line--spinner-timer)
          (setq agent-chat-mode-line--spinner-timer
                (run-at-time agent-chat-mode-line-spinner-interval
                             agent-chat-mode-line-spinner-interval
                             #'agent-chat-mode-line--spinner-tick)))
      (agent-chat-mode-line--stop-spinner)))
  (maphash (lambda (_perspective entry)
             (aset entry 5 (agent-chat-mode-line--build-render entry)))
           agent-chat-mode-line--perspectives)
  (force-mode-line-update t))

(defconst agent-chat-mode-line--update-functions
  '(agent-chat--publish agent-chat--begin-connection agent-chat--disconnect
    persp-kill)
  "Functions after which session state or perspective membership has changed.")

(defconst agent-chat-mode-line--membership-functions
  '(persp-add-buffer persp-forget-buffer)
  "Perspective operations which change an individual buffer's membership.")

(defconst agent-chat-mode-line--refresh-hooks
  '(persp-activated-hook persp-after-rename-hook persp-state-after-load-hook
    persp-mode-hook agent-chat-mode-hook)
  "Hooks where activity must be ready before the next mode-line redisplay.")

(defun agent-chat-mode-line--refresh-h (&rest _arguments)
  "Refresh after a session, membership or Perspective lifecycle change."
  (when agent-chat-mode-line-mode
    (agent-chat-mode-line--refresh)))

(defun agent-chat-mode-line--membership-a (buffer-or-name &rest _arguments)
  "Refresh after adding or forgetting a composer BUFFER-OR-NAME."
  (when-let* ((agent-chat-mode-line-mode)
              (buffer (get-buffer buffer-or-name)))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-chat-mode)
        (agent-chat-mode-line--refresh)))))

(defun agent-chat-mode-line--kill-buffer-h ()
  "Exclude a composer before kill-buffer finishes removing its buffer object."
  (when (derived-mode-p 'agent-chat-mode)
    (agent-chat-mode-line--refresh (current-buffer))))

(defun agent-chat-mode-line--render ()
  "Label cached activity with the current perspective, without buffer scanning."
  (let* ((perspective (and persp-mode (persp-curr)))
         (entry (gethash perspective agent-chat-mode-line--perspectives))
         (activity (or (and entry (aref entry 5))
                       (propertize "A0" 'face 'agent-chat-mode-line-label-face))))
    ;; Perspective names are frame-local and can change without agent updates.
    ;; Read both the name and counts from the same perspective object.
    (let ((name (and perspective (persp-name perspective))))
      (propertize (concat " (" (when name (concat name ": ")) activity ") ")
                  'help-echo (and entry (aref entry 4))))))

(defun agent-chat-mode-line--enable ()
  "Install Perspective activity tracking and the global mode-line construct."
  (require 'perspective)
  (unless agent-chat-mode-line--installed
    (setq agent-chat-mode-line--installed t)
    (dolist (function agent-chat-mode-line--update-functions)
      (advice-add function :after #'agent-chat-mode-line--refresh-h))
    (dolist (function agent-chat-mode-line--membership-functions)
      (advice-add function :after #'agent-chat-mode-line--membership-a))
    (dolist (hook agent-chat-mode-line--refresh-hooks)
      (add-hook hook #'agent-chat-mode-line--refresh-h))
    (add-hook 'kill-buffer-hook #'agent-chat-mode-line--kill-buffer-h)
    (unless (listp global-mode-string) (setq global-mode-string (list global-mode-string)))
    (unless (stringp (car global-mode-string)) (push "" global-mode-string))
    (add-to-list 'global-mode-string 'agent-chat-mode-line--mode-line t)
    (agent-chat-mode-line--refresh)))

(defun agent-chat-mode-line--disable ()
  "Remove tracking and its indicator without changing any agent sessions."
  (when agent-chat-mode-line--installed
    (setq agent-chat-mode-line--installed nil)
    (dolist (function agent-chat-mode-line--update-functions)
      (advice-remove function #'agent-chat-mode-line--refresh-h))
    (dolist (function agent-chat-mode-line--membership-functions)
      (advice-remove function #'agent-chat-mode-line--membership-a))
    (dolist (hook agent-chat-mode-line--refresh-hooks)
      (remove-hook hook #'agent-chat-mode-line--refresh-h))
    (remove-hook 'kill-buffer-hook #'agent-chat-mode-line--kill-buffer-h)
    (setq global-mode-string (delq 'agent-chat-mode-line--mode-line global-mode-string))
    (clrhash agent-chat-mode-line--perspectives)
    (agent-chat-mode-line--stop-spinner)
    (force-mode-line-update t)))

;;;###autoload
(define-minor-mode agent-chat-mode-line-mode
  "Show the current perspective's Agent Chat buffer count and activity."
  :init-value nil :global t :group 'agent-chat-mode-line
  (if agent-chat-mode-line-mode
      (agent-chat-mode-line--enable)
    (agent-chat-mode-line--disable)))

(provide 'agent-chat-mode-line)
;;; agent-chat-mode-line.el ends here
