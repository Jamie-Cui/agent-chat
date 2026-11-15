;;; consult-agent-chat.el --- Select agent conversations with Consult -*- lexical-binding: t; -*-

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
;; Show the current perspective's Agent Chat buffers with session titles and status.

;;; Code:

(require 'cl-lib)
(require 'agent-chat)
(declare-function consult--buffer-pair "consult" (buffer))
(declare-function consult--buffer-state "consult" ())
(declare-function consult--multi "consult" (sources &rest options))
(declare-function persp-curr "perspective" (&optional frame))
(declare-function persp-buffers "perspective" (perspective))
(declare-function persp-add-buffer "perspective" (buffer-or-name))
(declare-function persp-forget-buffer "perspective" (buffer))
(defvar persp-mode)
(defvar consult--buffer-history)

(defvar marginalia-annotators)

(defun consult-agent-chat-buffer--annotate (candidate)
  "Annotate CANDIDATE with aligned session title and conversation status."
  (when-let* ((buffer (get-buffer candidate))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((title (agent-chat--session-title-label
                    (and agent-chat--session (agent-chat--session-title agent-chat--session)))))
        (concat (propertize (truncate-string-to-width title 48 nil ?\s "…")
                            'face 'font-lock-doc-face 'help-echo title)
                "  " (agent-chat--conversation-status t))))))

(defun consult-agent-chat-buffer--buffers ()
  "Return the current perspective's Agent Chat composers in MRU order.
Without an active perspective, return no existing conversations."
  (let ((members (and persp-mode (persp-buffers (persp-curr)))))
    (cl-remove-if-not
     (lambda (buffer)
       (and (memq buffer members)
            (with-current-buffer buffer (derived-mode-p 'agent-chat-mode))))
     (buffer-list))))

(defvar consult-agent-chat-source
  `(:name "Agent Chat"
    :category buffer
    :face consult-buffer
    :history buffer-name-history
    :state ,#'consult--buffer-state
    :items ,(lambda ()
              (mapcar #'consult--buffer-pair
                      (consult-agent-chat-buffer--buffers))))
  "Agent Chat buffer source for `consult-agent-chat-buffer'.")

(defun consult-agent-chat-buffer--include (perspective)
  "Select an existing conversation to include in PERSPECTIVE.
Preserve its session and membership elsewhere.  Do not preview candidates."
  (unless (and persp-mode (eq perspective (persp-curr)))
    (user-error "The current perspective has changed"))
  (let* ((members (persp-buffers perspective))
         (buffers
          (cl-remove-if-not
           (lambda (buffer)
             (and (not (memq buffer members))
                  (with-current-buffer buffer (derived-mode-p 'agent-chat-mode))))
           (buffer-list))))
    (unless buffers
      (user-error "No Agent Chat buffers outside the current perspective"))
    (when-let* ((selection
                (consult--multi
                 (list (list :name "Sessions" :category 'buffer
                             :face 'consult-buffer :default t
                             :items (mapcar #'consult--buffer-pair buffers)
                             :annotate #'consult-agent-chat-buffer--annotate))
                 :prompt "Include Agent Chat buffer: " :require-match t :sort nil))
               (buffer (car selection)))
      (unless (and persp-mode (eq perspective (persp-curr)))
        (user-error "The current perspective has changed"))
      (unless (and (buffer-live-p buffer) (memq buffer buffers)
                   (eq buffer (agent-chat--conversation-buffer buffer)))
        (user-error "The selected conversation is no longer available"))
      ;; Include the composer and its existing messages/queue views together.
      ;; Adding membership does not move, display or restart the conversation.
      (dolist (view (buffer-list))
        (when (eq buffer (agent-chat--conversation-buffer view))
          (persp-add-buffer view))))))

(defun consult-agent-chat-buffer--ignore (perspective)
  "Select a conversation to disassociate from PERSPECTIVE without killing it.
Do not preview candidates: choosing an action must not display another session."
  (unless (and persp-mode (eq perspective (persp-curr)))
    (user-error "The current perspective has changed"))
  (let ((buffers (consult-agent-chat-buffer--buffers)))
    (unless buffers
      (user-error "No Agent Chat buffers in the current perspective"))
    (when-let* ((selection
                (consult--multi
                 (list (list :name "Sessions" :category 'buffer
                             :face 'consult-buffer :default t
                             :items (mapcar #'consult--buffer-pair buffers)
                             :annotate #'consult-agent-chat-buffer--annotate))
                 :prompt "Ignore Agent Chat buffer: " :require-match t :sort nil))
               (buffer (car selection)))
      (unless (and persp-mode (eq perspective (persp-curr)))
        (user-error "The current perspective has changed"))
      (unless (and (buffer-live-p buffer) (memq buffer buffers)
                   (memq buffer (persp-buffers perspective)))
        (user-error "The selected conversation is no longer in this perspective"))
      ;; Forget owned views as well, so restoring the perspective cannot show
      ;; the ignored conversation through an orphaned messages/queue window.
      ;; Perspective normally protects its last buffer; disassociation is safe
      ;; here even when this conversation is the perspective's only member.
      (dolist (view (append (remq buffer (copy-sequence (persp-buffers perspective)))
                           (list buffer)))
        (when (and (buffer-live-p view)
                   (eq buffer (agent-chat--conversation-buffer view)))
          (let ((kill-buffer-query-functions
                 (remq 'persp-maybe-kill-buffer kill-buffer-query-functions)))
            (persp-forget-buffer view)))))))

;;;###autoload
(defun consult-agent-chat-buffer ()
  "Switch to an Agent Chat buffer in the current perspective.
Show its session title and status, using the same membership as the mode line.
Group existing conversations under Sessions in most recently used order.
Actions always offers New agent-chat buffer, which is the default when
there are no sessions.  Otherwise default to the most recent session.
New creates a separate buffer, never reusing an existing conversation.
Codex then offers a new session or a saved session to load.
Include adds an existing conversation to this perspective without removing
it from others, changing its working directory or restarting its session.
Ignore asks which conversation to remove from this perspective, keeping
the buffer, its background task and its membership in other perspectives."
  (interactive)
  (require 'consult)
  (require 'perspective)
  ;; Use Consult's aligned title/status fields instead of Marginalia's generic
  ;; buffer columns, only for this picker.  Keep the category for buffer actions.
  (let* ((origin (current-buffer))
         (directory default-directory)
         (perspective (and persp-mode (persp-curr)))
         (buffers (consult-agent-chat-buffer--buffers))
         (marginalia-annotators
          (cons '(buffer builtin)
                (and (boundp 'marginalia-annotators) marginalia-annotators))))
    (consult--multi
     (delq nil
           (list (when buffers
                   (append (list :name "Sessions" :default t
                                 :annotate #'consult-agent-chat-buffer--annotate
                                 :items (mapcar #'consult--buffer-pair buffers))
                           consult-agent-chat-source))
                 ;; An action has no preview state and only runs on selection.
                 (list :name "Actions" :category 'agent-chat-action
                       :default (not buffers)
                       :items (append '("New agent-chat buffer")
                                      (and perspective '("Include agent-chat buffer"))
                                      (and buffers '("Ignore agent-chat buffer")))
                       :annotate #'ignore
                       :action
                       (lambda (action)
                         (pcase action
                           ("Include agent-chat buffer"
                            (consult-agent-chat-buffer--include perspective))
                           ("Ignore agent-chat buffer"
                            (consult-agent-chat-buffer--ignore perspective))
                           ("New agent-chat buffer"
                            (unless (buffer-live-p origin)
                              (user-error "The originating buffer no longer exists"))
                            (agent-chat--with-command-buffer origin
                              (let ((default-directory directory)
                                    (current-prefix-arg nil))
                                (funcall-interactively #'agent-chat nil nil 'select)))))))))
     :require-match t
     :prompt "Switch to Agent Chat: "
     :history 'consult--buffer-history
     :sort nil)))

(provide 'consult-agent-chat)
;;; consult-agent-chat.el ends here
