;;; agent-chat-quota.el --- Account quota for Agent Chat -*- lexical-binding: t; -*-

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
;; Show account quota near the draft, using the existing Codex connection.
;; Account snapshots are scoped to the authenticated client, never summed
;; across conversations.  Redisplay only reads cached data.  Unsupported
;; backends have no quota indicator; failed reads preserve the last snapshot.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'button)

(declare-function codex-acp-read-rate-limits "codex-acp" (client success failure))
(declare-function agent-chat--require-conversation "agent-chat" (&optional allow-outside))

(defconst agent-chat-quota--max-age 300
  "Seconds after which a cached quota snapshot needs refreshing.")
(defvar-local agent-chat-quota--client nil)
(defvar-local agent-chat-quota--buckets nil)
(defvar-local agent-chat-quota--current nil)
(defvar-local agent-chat-quota--error nil)
(defvar-local agent-chat-quota--pending nil)
(defvar-local agent-chat-quota--generation nil)
(defvar-local agent-chat-quota--attempted 0)
(defvar-local agent-chat-quota--timer nil)
(defvar-local agent-chat-quota--owner nil)
(defvar agent-chat-quota--details-buffer nil)

(defun agent-chat-quota--window-valid-p (window)
  "Whether WINDOW contains a usable percentage and quota duration."
  (when (or (listp window) (hash-table-p window))
    (let ((used (map-elt window 'usedPercent))
          (minutes (map-elt window 'windowDurationMins)))
      (and (numberp used) (<= 0 used 100)
           (integerp minutes) (> minutes 0)))))

(defun agent-chat-quota--windows (limits)
  "Return the valid quota windows in LIMITS, in server order."
  (when (or (listp limits) (hash-table-p limits))
    (cl-remove-if-not #'agent-chat-quota--window-valid-p
                      (list (map-elt limits 'primary) (map-elt limits 'secondary)))))

(defun agent-chat-quota--duration (window)
  "Format WINDOW's server-provided duration without assuming a plan."
  (let ((minutes (map-elt window 'windowDurationMins)))
    (cond ((zerop (% minutes 10080)) (format "%dw" (/ minutes 10080)))
          ((zerop (% minutes 1440)) (format "%dd" (/ minutes 1440)))
          ((zerop (% minutes 60)) (format "%dh" (/ minutes 60)))
          (t (format "%dm" minutes)))))

(defun agent-chat-quota--remaining (window)
  "Return WINDOW's remaining percentage, rounded down."
  (floor (- 100 (map-elt window 'usedPercent))))

(defun agent-chat-quota--stale-p (entry)
  "Whether cached bucket ENTRY is stale or its reset time has passed."
  (let ((now (float-time)))
    (or (null entry) agent-chat-quota--error (null agent-chat-quota--client)
        (> (- now (plist-get entry :updated)) agent-chat-quota--max-age)
        (cl-some (lambda (window)
                   (let ((reset (map-elt window 'resetsAt)))
                     (and (numberp reset) (<= reset now))))
                 (agent-chat-quota--windows (plist-get entry :limits))))))

(defun agent-chat-quota--selected ()
  "Return the current account bucket, falling back to the Codex bucket."
  (or (cdr (assoc agent-chat-quota--current agent-chat-quota--buckets))
      (cdr (assoc "codex" agent-chat-quota--buckets))
      (cdar agent-chat-quota--buckets)))

(defun agent-chat-quota--tooltip ()
  "Describe all cached account buckets and their freshness in English."
  (let ((lines (list "Account quota (shared across conversations)")))
    (dolist (bucket agent-chat-quota--buckets)
      (let* ((entry (cdr bucket))
             (limits (plist-get entry :limits))
             (name (map-elt limits 'limitName)))
        (push (format "\n%s%s"
                      (if (and (stringp name) (not (string-empty-p name))) name (car bucket))
                      (if (agent-chat-quota--stale-p entry) " [stale]" "")) lines)
        (dolist (window (agent-chat-quota--windows limits))
          (let ((reset (map-elt window 'resetsAt)))
            (push (format "  %s: %d%% remaining; resets %s"
                          (agent-chat-quota--duration window)
                          (agent-chat-quota--remaining window)
                          (if (numberp reset)
                              (format-time-string "%Y-%m-%d %H:%M %Z" (seconds-to-time reset))
                            "unknown")) lines)))
        (push (format "  Updated: %s"
                      (format-time-string "%Y-%m-%d %H:%M:%S %Z"
                                          (seconds-to-time (plist-get entry :updated))))
              lines)))
    (unless agent-chat-quota--buckets (push "Quota is unavailable." lines))
    (when agent-chat-quota--error (push (concat "\n" agent-chat-quota--error) lines))
    (when agent-chat-quota--pending (push "\nRefreshing..." lines))
    (string-join (nreverse lines) "\n")))

(defun agent-chat-quota--changed ()
  "Refresh quota displays without touching conversation history or draft."
  (force-mode-line-update t)
  (when (and (buffer-live-p agent-chat-quota--details-buffer)
             (eq (buffer-local-value 'agent-chat-quota--owner agent-chat-quota--details-buffer)
                 (current-buffer)))
    (agent-chat-quota--render-details)))

(defun agent-chat-quota--accept (data &optional replace)
  "Cache account DATA; REPLACE means a complete read, not a bucket update.
A notification may only contain one bucket; preserve other cached buckets."
  (unless (or (listp data) (hash-table-p data)) (setq data nil))
  (when replace (setq agent-chat-quota--buckets nil agent-chat-quota--current nil))
  (let* ((limits (map-elt data 'rateLimits))
         (all (map-elt data 'rateLimitsByLimitId))
         (now (float-time)))
    (cl-labels ((save-bucket
                 (id value)
                 (when (symbolp id) (setq id (symbol-name id)))
                 (when (and (stringp id) (agent-chat-quota--windows value))
                   (setf (alist-get id agent-chat-quota--buckets nil nil #'equal)
                         (list :limits value :updated now)))))
      (when (or (listp all) (hash-table-p all))
        (map-do (lambda (id value) (save-bucket id value)) all))
      (when (agent-chat-quota--windows limits)
        (let ((id (or (map-elt limits 'limitId) "codex")))
          (save-bucket id limits)
          (when (and (stringp id) (or replace (null agent-chat-quota--current)))
            (setq agent-chat-quota--current id))))))
  (setq agent-chat-quota--error nil
        agent-chat-quota--generation (make-symbol "quota-update"))
  (agent-chat-quota--changed))

(defun agent-chat-quota-refresh (&optional buffer)
  "Refresh account quota asynchronously in BUFFER or the current buffer."
  (interactive (list (agent-chat--require-conversation)))
  (with-current-buffer (or buffer (current-buffer))
    (agent-chat-quota--refresh)))

(defun agent-chat-quota--refresh ()
  "Read quota in its owning buffer and reject stale callbacks."
  (unless agent-chat-quota--client (user-error "Quota is unavailable for this connection"))
  (unless agent-chat-quota--pending
    (let ((owner (current-buffer))
          (client agent-chat-quota--client)
          (generation agent-chat-quota--generation)
          (token (make-symbol "quota-request")))
      (setq agent-chat-quota--pending token agent-chat-quota--attempted (float-time))
      (codex-acp-read-rate-limits
       client
       (lambda (data)
         (when (buffer-live-p owner)
           (with-current-buffer owner
             (when (and (eq client agent-chat-quota--client)
                        (eq token agent-chat-quota--pending))
               (setq agent-chat-quota--pending nil)
               ;; A push received while reading is newer than this response.
               (if (eq generation agent-chat-quota--generation)
                   (agent-chat-quota--accept data t)
                 (agent-chat-quota--changed))))))
       (lambda (err)
         (when (buffer-live-p owner)
           (with-current-buffer owner
             (when (and (eq client agent-chat-quota--client)
                        (eq token agent-chat-quota--pending))
               (setq agent-chat-quota--pending nil)
               (when (eq generation agent-chat-quota--generation)
                 (setq agent-chat-quota--error
                       (format "Quota refresh failed: %s" (or (map-elt err 'message) "Unknown error"))))
               (agent-chat-quota--changed)))))))
    (agent-chat-quota--changed)))

(defun agent-chat-quota--maybe-refresh ()
  "Refresh stale visible quota outside redisplay, throttling failed reads."
  (when (and agent-chat-quota--client (not agent-chat-quota--pending)
             (> (- (float-time) agent-chat-quota--attempted) 60)
             (agent-chat-quota--stale-p (agent-chat-quota--selected)))
    (agent-chat-quota-refresh)))

(defun agent-chat-quota--tick (buffer)
  "Refresh BUFFER's visible account snapshot if it is stale."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (get-buffer-window buffer t) (agent-chat-quota--maybe-refresh))
      (agent-chat-quota--changed))))

(defun agent-chat-quota--stop ()
  "Release the timer and reject callbacks from a detached connection."
  (when (timerp agent-chat-quota--timer) (cancel-timer agent-chat-quota--timer))
  (setq agent-chat-quota--timer nil agent-chat-quota--client nil
        agent-chat-quota--pending nil)
  (agent-chat-quota--changed))

(defun agent-chat-quota--clear ()
  "Forget account data before replacing this conversation's backend."
  (agent-chat-quota--stop)
  (setq agent-chat-quota--buckets nil agent-chat-quota--current nil
        agent-chat-quota--error nil agent-chat-quota--attempted 0
        agent-chat-quota--generation (make-symbol "quota-account"))
  (agent-chat-quota--changed))

(defun agent-chat-quota--start (client)
  "Start account quota tracking on supported CLIENT in the current buffer."
  (agent-chat-quota--clear)
  (setq agent-chat-quota--client client)
  (add-hook 'kill-buffer-hook #'agent-chat-quota--stop nil t)
  (add-hook 'change-major-mode-hook #'agent-chat-quota--stop nil t)
  (setq agent-chat-quota--timer
        (run-at-time 60 60 #'agent-chat-quota--tick (current-buffer)))
  (agent-chat-quota-refresh))

(defun agent-chat-quota--notification (client method params)
  "Handle an account notification with METHOD and PARAMS from CLIENT."
  (when (and agent-chat-quota--client (eq client agent-chat-quota--client))
    (pcase method
      ("_codex/account/rateLimits/updated" (agent-chat-quota--accept params))
      ("_codex/account/updated"
       ;; A changed login must never inherit another account's snapshot.
       (setq agent-chat-quota--buckets nil agent-chat-quota--current nil
             agent-chat-quota--error nil agent-chat-quota--pending nil
             agent-chat-quota--generation (make-symbol "quota-account"))
       (agent-chat-quota-refresh)))))

(defvar agent-chat-quota--map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'agent-chat-quota-show)
    map))

(defun agent-chat-quota--label (&optional compact)
  "Return cached remaining quota, using short text when COMPACT is non-nil."
  (when (or agent-chat-quota--client agent-chat-quota--buckets)
    (let* ((entry (agent-chat-quota--selected))
           (windows (agent-chat-quota--windows (plist-get entry :limits)))
           (stale (and entry (agent-chat-quota--stale-p entry)))
           (least (and windows (apply #'min (mapcar #'agent-chat-quota--remaining windows))))
           (label (concat
                   (unless compact "Remaining ")
                   (if windows
                       (mapconcat (lambda (window)
                                    (format "%s %d%%" (agent-chat-quota--duration window)
                                            (agent-chat-quota--remaining window)))
                                  windows " · ")
                     "—")
                   (when stale " *"))))
      (propertize label 'help-echo (agent-chat-quota--tooltip)
                  'mouse-face 'highlight 'local-map agent-chat-quota--map
                  'face (cond (stale 'shadow) ((and least (<= least 10)) 'error)
                              ((and least (<= least 20)) 'warning) (t 'shadow))))))

(defun agent-chat-quota--render-details ()
  "Render this conversation's cached quota in the shared details buffer."
  (let ((owner (current-buffer))
        (text (agent-chat-quota--tooltip))
        (connected agent-chat-quota--client))
    (with-current-buffer agent-chat-quota--details-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text "\n\n")
        (when connected
          (insert-text-button
           "Refresh" 'action
           (lambda (_button)
             (when (buffer-live-p owner)
               (with-current-buffer owner (agent-chat-quota-refresh)))))))
      (special-mode)
      (setq agent-chat-quota--owner owner)
      (goto-char (point-min)))))

(defun agent-chat-quota-show (&optional event)
  "Show account quota details for the conversation at optional mouse EVENT."
  (interactive (list last-input-event))
  (let ((owner (if (mouse-event-p event)
                   (window-buffer (posn-window (event-start event)))
                 (agent-chat--require-conversation))))
    (with-current-buffer owner
      (unless (or agent-chat-quota--client agent-chat-quota--buckets)
        (user-error "Quota is unavailable for this connection"))
      (setq agent-chat-quota--details-buffer (get-buffer-create "*Agent Chat quota*"))
      (agent-chat-quota--render-details)
      (display-buffer agent-chat-quota--details-buffer))))

(provide 'agent-chat-quota)
;;; agent-chat-quota.el ends here
