;;; agent-chat-steer-test.el --- Active-turn steering regressions -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Offline transport fixtures: never start Codex or send the user's prompts.
;; Initialize installed packages, add the package directory to load-path, load this file,
;; then run ert-run-tests-batch-and-exit with "^agent-chat-steer-test-".

;;; Code:
(require 'ert)
(require 'agent-chat)
(require 'codex-acp)

(defmacro agent-chat-steer-test--backend (&rest body)
  "Run BODY with a live-turn fixture and captured RPC messages in WIRE."
  (declare (indent 0) (debug t))
  `(let* ((process-token (make-symbol "steer-test-process"))
          (original-live (symbol-function 'codex-acp--process-live-p))
          (original-write (symbol-function 'codex-acp--write))
          (original-title (symbol-function 'codex-acp--title-start))
          (backend (codex-acp--state-create :directory default-directory :process process-token
                                         :thread "thread" :turn "turn-1"
                                         :prompt (cons #'ignore #'ignore)))
          (client (list (cons :codex-acp-state backend)))
          wire)
     (setf (codex-acp--state-client backend) client)
     (unwind-protect
         (cl-letf (((symbol-function 'codex-acp--process-live-p)
                    (lambda (process) (if (eq process process-token) t (funcall original-live process))))
                   ((symbol-function 'codex-acp--write)
                    (lambda (state message)
                      (if (eq state backend) (push message wire) (funcall original-write state message))))
                   ((symbol-function 'codex-acp--title-start)
                    (lambda (state) (unless (eq state backend) (funcall original-title state)))))
           ,@body)
       (dolist (entry (codex-acp--state-pending backend)) (cancel-timer (nth 3 entry))))))

(defun agent-chat-steer-test--reply (backend message &optional error-data turn)
  "Reply to captured MESSAGE on BACKEND with ERROR-DATA or accepted TURN."
  (codex-acp--dispatch
   backend `((id . ,(map-elt message 'id))
             ,(if error-data (cons 'error error-data)
                (cons 'result `((turnId . ,(or turn "turn-1"))))))))

(ert-deftest agent-chat-steer-test-rpc-keeps-active-turn-and-tools ()
  (agent-chat-steer-test--backend
    (let ((active (codex-acp--state-prompt backend))
          (permissions '((permission . pending)))
          accepted)
      (setf (codex-acp--state-permissions backend) permissions)
      (codex-acp-steer client "thread" '(((type . "text") (text . "Focus on tests")))
                       (lambda (turn) (setq accepted turn)) #'ert-fail)
      (should (= 1 (length wire)))
      (should (equal (map-elt (car wire) 'method) "turn/steer"))
      (should (equal (map-elt (car wire) 'params)
                     '((threadId . "thread") (expectedTurnId . "turn-1")
                       (input . [((type . "text") (text . "Focus on tests"))]))))
      (agent-chat-steer-test--reply backend (car wire))
      (should (equal accepted "turn-1"))
      (should (eq active (codex-acp--state-prompt backend)))
      (should (eq permissions (codex-acp--state-permissions backend)))
      (should (equal (codex-acp--state-turn backend) "turn-1"))
      (should-not (codex-acp--state-cancelling backend)))))

(ert-deftest agent-chat-steer-test-rejects-inactive-and-wrong-session ()
  (agent-chat-steer-test--backend
    (should-not (codex-acp-steer-ready-p nil))
    (should-error (codex-acp-steer client "other" nil #'ignore #'ignore) :type 'user-error)
    (dolist (state '(no-turn no-prompt cancelling closed))
      (setf (codex-acp--state-turn backend) (unless (eq state 'no-turn) "turn-1")
            (codex-acp--state-prompt backend) (unless (eq state 'no-prompt) (cons #'ignore #'ignore))
            (codex-acp--state-cancelling backend) (eq state 'cancelling)
            (codex-acp--state-closed backend) (eq state 'closed))
      (should-not (codex-acp-steer-ready-p client))
      (should-error (codex-acp-steer client "thread" nil #'ignore #'ignore) :type 'user-error))
    (should-not wire)))

(ert-deftest agent-chat-steer-test-error-timeout-and-invalid-ack-preserve-turn ()
  (dolist (outcome '(error timeout invalid-ack))
    (agent-chat-steer-test--backend
      (let ((active (codex-acp--state-prompt backend)) failure)
        (codex-acp-steer client "thread" '(((type . "text") (text . "Adjust")))
                         #'ert-fail (lambda (err) (setq failure err)))
        (pcase outcome
          ('error (agent-chat-steer-test--reply backend (car wire) '((message . "Turn mismatch"))))
          ('invalid-ack (agent-chat-steer-test--reply backend (car wire) nil "other-turn"))
          ('timeout
           (let ((timer (nth 3 (car (codex-acp--state-pending backend)))))
             (cancel-timer timer)
             (apply (timer--function timer) (timer--args timer)))))
        (should failure)
        (should (eq active (codex-acp--state-prompt backend)))
        (should-not (codex-acp--state-closed backend))
        (should-not (codex-acp--state-cancelling backend))
        (should-not (codex-acp--state-pending backend))))))

(ert-deftest agent-chat-steer-test-expands-remote-skills-without-starting-turn ()
  (agent-chat-steer-test--backend
    (setf (codex-acp--state-skills backend)
          '(((name . "review") (mention . "review") (source . remote) (path . "/skills/review/SKILL.md"))))
    (let (worker accepted)
      (cl-letf (((symbol-function 'codex-acp--submit-job)
                 (lambda (_state _id function _args callback &rest _)
                   (should (eq function 'codex-acp-worker-prompt-input))
                   (setq worker callback))))
        (codex-acp-steer client "thread" '(((type . "text") (text . "/$review check this")))
                         (lambda (turn) (setq accepted turn)) #'ert-fail)
        (should worker)
        (should-not wire)
        (funcall worker '(:status ok :value (((type . "text") (text . "Remote skill contents")))))
        (should (equal (map-elt (car wire) 'method) "turn/steer"))
        (should (equal (map-nested-elt (car wire) '(params input))
                       [((type . "text") (text . "[remote skill review] check this"))
                        ((type . "text") (text . "Remote skill contents"))]))
        (agent-chat-steer-test--reply backend (car wire))
        (should accepted)))))

(ert-deftest agent-chat-steer-test-remote-skill-preparation-cannot-follow-new-turn ()
  (dolist (change '(completed changed-turn replaced-prompt cancelled))
    (agent-chat-steer-test--backend
      (let (worker failure)
        (cl-letf (((symbol-function 'codex-acp--prompt-input)
                   (lambda (&rest _) '(((type . "remoteSkill") (skill . test)))))
                  ((symbol-function 'codex-acp--submit-job)
                   (lambda (_state _id _fn _args callback &rest _) (setq worker callback))))
          (codex-acp-steer client "thread" nil #'ert-fail (lambda (err) (setq failure err)))
          (pcase change
            ('completed (codex-acp--finish backend))
            ('changed-turn (setf (codex-acp--state-turn backend) "turn-2"))
            ('replaced-prompt (setf (codex-acp--state-prompt backend) (cons #'ignore #'ignore)))
            ('cancelled (setf (codex-acp--state-cancelling backend) t)))
          (funcall worker '(:status ok :value (((type . "text") (text . "Skill")))))
          (should failure)
          (should-not wire))))))

(ert-deftest agent-chat-steer-test-normal-prompt-remote-skill-cancellation ()
  (agent-chat-steer-test--backend
    (let (worker finished)
      (cl-letf (((symbol-function 'codex-acp--prompt-input)
                 (lambda (&rest _) '(((type . "remoteSkill") (skill . test)))))
                ((symbol-function 'codex-acp--submit-job)
                 (lambda (_state _id _fn _args callback &rest _) (setq worker callback))))
        (codex-acp--prepare-prompt backend nil (lambda (result) (setq finished result)) #'ert-fail)
        (setf (codex-acp--state-cancelling backend) t)
        (funcall worker '(:status cancelled :message "Cancelled"))
        (should (equal (map-elt finished 'stopReason) "cancelled"))
        (should-not (codex-acp--state-prompt backend))
        (should-not wire)))))

(defmacro agent-chat-steer-test--isolated-client (&rest body)
  "Suppress ACP cleanup for the fixture CLIENT only while running BODY."
  (declare (indent 0) (debug t))
  `(let ((notify (symbol-function 'acp-send-notification))
         (respond (symbol-function 'acp-send-response))
         (shutdown (symbol-function 'acp-shutdown)))
     (cl-letf (((symbol-function 'acp-send-notification)
                (lambda (&rest args)
                  (unless (eq (plist-get args :client) client) (apply notify args))))
               ((symbol-function 'acp-send-response)
                (lambda (&rest args)
                  (unless (eq (plist-get args :client) client) (apply respond args))))
               ((symbol-function 'acp-shutdown)
                (lambda (&rest args)
                  (unless (eq (plist-get args :client) client) (apply shutdown args)))))
       ,@body)))

(defmacro agent-chat-steer-test--conversation (&rest body)
  "Run BODY in an isolated composer connected to the backend fixture."
  (declare (indent 0) (debug t))
  `(agent-chat-steer-test--backend
     (save-window-excursion
       (let ((buffer (generate-new-buffer " *steer-test*"))
             (agent-chat-stream-refresh-interval 0) sent cancelled)
         (unwind-protect
             (progn
               (switch-to-buffer buffer)
               (agent-chat-mode)
               (agent-chat--attach (agent-chat--state-create) nil "Steer test")
               (let ((session (agent-chat--session-create
                               :buffer buffer :agent 'test :cwd default-directory
                               :client client :id "thread" :phase 'ready
                               :capabilities '((promptCapabilities . ((image . t)))))))
                 (setq agent-chat--session session
                       agent-chat--adapter
                       (lambda (request emit)
                         (push (plist-get request :prompt) sent)
                         (setf (agent-chat--session-busy session) t
                               (agent-chat--session-emit session) emit
                               (codex-acp--state-turn backend) (format "turn-%d" (length sent))
                               (codex-acp--state-prompt backend)
                               (cons (lambda (_result) (agent-chat--finish-turn session '(:type done :stop-reason "end_turn")))
                                     (lambda (err) (agent-chat--finish-turn session (list :type 'error :text (map-elt err 'message))))))
                         (lambda () (setq cancelled t))))
                 (agent-chat-steer-test--isolated-client
                   (insert "Initial turn") (agent-chat-submit)
                   ,@body)))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (agent-chat-steer-test--isolated-client
                 (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))))))

(ert-deftest agent-chat-steer-test-accepts-one-item-preserves-images-draft-and-stream ()
  (agent-chat-steer-test--conversation
    (let* ((image '((type . "image") (mimeType . "image/png")
                   (data . "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAAF0lEQVR4nGP4z8BAEiJN9aiGUQ1DSgMAkPn/Afnh+ngAAAAASUVORK5CYII=")))
           (prompt (concat "Inspect " (agent-chat--image-token image) " please"))
           (token (agent-chat--state-token agent-chat--state))
           (cancel (agent-chat--state-cancel agent-chat--state)))
      (agent-chat--enqueue "Keep queued")
      (agent-chat--enqueue prompt)
      (insert "Unsent draft") (goto-char 3) (set-mark 6)
      (let ((entry (cadr agent-chat--queue)) (undo buffer-undo-list))
        (agent-chat--queue-steer entry)
        (should (= 2 (length agent-chat--queue)))
        (should-not (agent-chat--queue-ready-p))
        (should-error (agent-chat--queue-steer entry) :type 'user-error)
        (should-error (agent-chat--queue-withdraw entry) :type 'user-error)
        (should-error (agent-chat--queue-edit entry) :type 'user-error)
        (should-error (agent-chat-queue-clear) :type 'user-error)
        (should (= 1 (length wire)))
        (should (equal (map-nested-elt (car wire) '(params input))
                       `[((type . "text") (text . "Inspect "))
                         ((type . "image") (url . ,(acp-image-to-data-url image)))
                         ((type . "text") (text . " please"))]))
        (agent-chat-steer-test--reply backend (car wire))
        (should (equal (mapcar (lambda (item) (plist-get item :draft)) agent-chat--queue) '("Keep queued")))
        (should (equal (agent-chat--prompt-content (plist-get (car (last (agent-chat--state-history agent-chat--state))) :text))
                       (agent-chat--prompt-content prompt)))
        (should (eq token (agent-chat--state-token agent-chat--state)))
        (should (eq cancel (agent-chat--state-cancel agent-chat--state)))
        (should (agent-chat--session-busy session))
        (should-not cancelled)
        (should (equal (buffer-string) "Unsent draft"))
        (should (= 3 (point))) (should (= 6 (mark)))
        (should (eq undo buffer-undo-list))
        (funcall (agent-chat--session-emit session) '(:type delta :text "Continuing with the image"))
        (should (equal (plist-get (car (last (agent-chat--state-history agent-chat--state))) :text)
                       "Continuing with the image"))
        (should (equal sent '("Initial turn")))))))

(ert-deftest agent-chat-steer-test-finish-before-ack-never-resends-steered-item ()
  (agent-chat-steer-test--conversation
    (agent-chat--enqueue "Steer this")
    (agent-chat--enqueue "Next turn")
    (agent-chat--queue-steer (car agent-chat--queue))
    (codex-acp--finish backend)
    (should-not agent-chat--queue-timer)
    (agent-chat--queue-send-next)
    (should (= 1 (length sent)))
    (agent-chat-steer-test--reply backend (car wire))
    (agent-chat--queue-pause "Test timer control")
    (setq agent-chat--queue-paused nil)
    (agent-chat--queue-send-next)
    (should (equal sent '("Next turn" "Initial turn")))
    (should-not agent-chat--queue)))

(ert-deftest agent-chat-steer-test-failure-retains-queue-and-pauses-after-finish ()
  (dolist (failure '(rpc synchronous))
    (agent-chat-steer-test--conversation
      (agent-chat--enqueue "Keep me")
      (let ((entry (car agent-chat--queue)))
        (if (eq failure 'rpc)
            (progn
              (agent-chat--queue-steer entry)
              (codex-acp--finish backend)
              (agent-chat-steer-test--reply backend (car wire) '((message . "No active turn"))))
          (cl-letf (((symbol-function 'codex-acp-steer) (lambda (&rest _) (error "Transport unavailable"))))
            (agent-chat--queue-steer entry)))
        (should (eq entry (car agent-chat--queue)))
        (should-not (plist-get entry :steering))
        (should (equal agent-chat--queue-paused "Steer failed"))
        (should-not agent-chat--queue-timer)
        (agent-chat--queue-send-next)
        (should (equal sent '("Initial turn")))))))

(ert-deftest agent-chat-steer-test-disconnect-clears-hold-and-ignores-stale-ack ()
  (agent-chat-steer-test--conversation
    (agent-chat--enqueue "Keep me")
    (let ((entry (car agent-chat--queue)))
      (agent-chat--queue-steer entry)
      (agent-chat--disconnect session)
      (should-not (plist-get entry :steering))
      (should (equal agent-chat--queue-paused "Disconnected"))
      (agent-chat-steer-test--reply backend (car wire))
      (should (memq entry agent-chat--queue))
      (should (= 1 (length (agent-chat--state-history agent-chat--state)))))))

(ert-deftest agent-chat-steer-test-controls-and-queue-list ()
  (agent-chat-steer-test--conversation
    (agent-chat--enqueue "Steer this")
    (let* ((entry (car agent-chat--queue))
           (row (cadr (agent-chat--queue-lines 120 20)))
           (at (string-match (regexp-quote "[Steer]") row)))
      (should (equal (get-text-property at 'agent-chat-queue-action row)
                     (list #'agent-chat--queue-steer entry)))
      (agent-chat-queue)
      (should (eq (lookup-key agent-chat-queue-mode-map (kbd "s")) #'agent-chat-queue-steer))
      (when (bound-and-true-p evil-local-mode)
        (should (eq (key-binding (kbd "s")) #'agent-chat-queue-steer)))
      (goto-char (point-min))
      (agent-chat-queue-steer)
      (should (= 1 (length wire)))
      (with-current-buffer buffer
        (let* ((pending (cadr (agent-chat--queue-lines 120 20)))
               (at (string-match (regexp-quote "[Steer]") pending)))
          (should (string-match-p "Steering…" pending))
          (should-not (get-text-property at 'agent-chat-queue-action pending)))))))

(ert-deftest agent-chat-steer-test-unavailable-for-external-acp-and-idle-turn ()
  (agent-chat-steer-test--conversation
    (agent-chat--enqueue "Keep queued")
    (dolist (client-state '(external idle))
      (setf (agent-chat--session-client session) (if (eq client-state 'external) nil client))
      (when (eq client-state 'idle) (codex-acp--finish backend))
      (should-not (agent-chat--queue-steer-ready-p))
      (should-error (agent-chat--queue-steer (car agent-chat--queue)) :type 'user-error)
      (should-not wire))))

(provide 'agent-chat-steer-test)
;;; agent-chat-steer-test.el ends here
