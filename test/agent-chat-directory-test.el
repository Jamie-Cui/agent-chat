;;; agent-chat-directory-test.el --- Directory approval integration -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'ert)
(require 'agent-chat)
(require 'codex-acp)
(require 'codex-acp-worker)

(defmacro agent-chat-directory-test--workspace (&rest body)
  "Run BODY with isolated project, notes and unrelated directory fixtures."
  (declare (indent 0) (debug t))
  `(let* ((base (file-name-as-directory (file-truename (make-temp-file "acp-dirs-" t))))
          (project (expand-file-name "project/" base))
          (notes (expand-file-name "notes/" base))
          (other (expand-file-name "notes-other/" base)))
     (unwind-protect
         (progn
           (mapc (lambda (dir) (make-directory dir t)) (list project notes other))
           (let* ((default-directory project)
                  (state (codex-acp--state-create :directory project :thread "existing-thread"))
          (client (list (cons :codex-acp-state state) (cons :codex-acp-scope 'directory)
                        (cons :request-handlers nil) (cons :notification-handlers nil)
                        (cons :codex-acp-allowed-directories nil))))
             (setf (codex-acp--state-client state) client)
             ,@body))
       (delete-directory base t))))

(defmacro agent-chat-directory-test--transport (&rest body)
  "Run BODY with a current turn, real path checks and captured wire messages."
  (declare (indent 0) (debug t))
  `(agent-chat-directory-test--workspace
     (let ((original-write (symbol-function 'codex-acp--write))
           (original-submit (symbol-function 'codex-acp--submit-job))
           approvals updates wire)
       (setf (codex-acp--state-turn state) "turn"
             (codex-acp--state-prompt state) (cons #'ignore #'ignore)
             (map-elt client :request-handlers) (list (lambda (request) (push request approvals)))
             (map-elt client :notification-handlers) (list (lambda (update) (push update updates))))
       (cl-letf (((symbol-function 'codex-acp--write)
                  (lambda (target message)
                    (if (eq target state) (push message wire) (funcall original-write target message))))
                 ((symbol-function 'codex-acp--submit-job)
                  (lambda (target id function arguments callback timeout mutating &optional lane)
                    (if (not (eq target state))
                        (funcall original-submit target id function arguments callback timeout mutating lane)
                      (let ((result (condition-case err
                                        (list :status 'ok :value (apply function arguments))
                                      (error (list :status 'error :error err)))))
                        (funcall callback result))))))
         (unwind-protect (progn ,@body)
           (codex-acp--close state "Test cleanup"))))))

(defun agent-chat-directory-test--call (tool path &optional id)
  "Build TOOL's pending call for PATH and optional ID."
  `((threadId . "existing-thread") (turnId . "turn") (callId . ,(or id "call"))
    (tool . ,tool) (arguments . ((path . ,path) (text . "Session note")))))

(defun agent-chat-directory-test--answer (client id option)
  "Answer CLIENT's pending ID with OPTION, or cancel when nil."
  (codex-acp--response
   :client client
   :response `(:request-id ,id :result ((outcome . ,(if option
                                                    `((outcome . "selected") (optionId . ,option))
                                                  '((outcome . "cancelled"))))))))

(ert-deftest agent-chat-directory-test-transient-focus-bypass-and-draft ()
  "Bypass grants pending and future directory requests until prompts resume."
  (agent-chat-directory-test--transport
    (let ((agent-chat--permission-auto-queue nil)
          (agent-chat--permission-auto-timer nil)
          (transient-show-menu t)
          (overriding-terminal-local-map nil)
          (overriding-local-map nil)
          (executing-kbd-macro nil)
          (original-response (symbol-function 'acp-send-response))
          (original-publish (symbol-function 'agent-chat--publish)))
      (save-window-excursion
        (with-temp-buffer
          (agent-chat-mode)
          (insert "Draft remains") (goto-char 4)
          (let* ((owner (current-buffer))
                 (session (agent-chat--session-create :buffer owner :client client :phase 'ready
                                                      :id "existing-thread" :cwd project :busy t))
                 (undo buffer-undo-list))
            (setq agent-chat--session session
                  agent-chat--state (agent-chat--state-create)
                  agent-chat--permission-bypass-session nil)
            (setf (map-elt client :request-handlers)
                  (list (lambda (request) (agent-chat--incoming-request session request)))
                  (map-elt client :notification-handlers)
                  (list (lambda (notification)
                          (agent-chat--notification session notification))))
            (cl-letf (((symbol-function 'acp-send-response)
                       (lambda (&rest args)
                         (if (eq (plist-get args :client) client) (apply #'codex-acp--response args)
                           (apply original-response args))))
                      ((symbol-function 'agent-chat--publish)
                       (lambda (&rest args) (unless (eq (current-buffer) owner) (apply original-publish args))))
                      ((symbol-function 'input-pending-p) #'ignore)
                      ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (unwind-protect
                  (progn
                    (codex-acp--tool-call state 1 (agent-chat-directory-test--call "tramp_list_directory" notes))
                    (should (agent-chat--session-permissions session))
                    (should-not (codex-acp-allowed-directories client))
                    (let ((display (agent-chat--transcript agent-chat--state)))
                      (should (string-suffix-p
                               "Permission pending — press C-c C-p to reopen the permission menu"
                               display))
                      (should-not (string-match-p "\\[Allow directory\\|\\[Reject\\|\\[Cancel" display)))
                    (should (string-match-p (regexp-quote notes)
                                            (agent-chat--transcript agent-chat--state t)))
                    (agent-chat--permission-auto-show)
                    (should-not (transient-active-prefix))
                    (switch-to-buffer owner)
                    (agent-chat--permission-auto-show)
                    (should (transient-active-prefix 'agent-chat--approval-menu))
                    (let ((scope (transient-scope 'agent-chat--approval-menu)))
                      (should (string-match-p "Allow directory" (plist-get scope :heading)))
                      (should (assoc "y" (plist-get scope :respond)))
                      (should (assoc "a" (plist-get scope :respond)))
                      (should (assoc "d" (plist-get scope :respond)))
                      (should (assoc "!" (plist-get scope :respond)))
                      (should (equal (map-elt (agent-chat--permission-allow-once (plist-get scope :request)) 'optionId)
                                     "allow-directory-turn"))
                      ;; Switching away closes the menu but leaves the request pending.
                      (with-temp-buffer
                        (switch-to-buffer (current-buffer))
                        (agent-chat--approval-check-window)
                        (should-not (transient-active-prefix)))
                      (switch-to-buffer owner)
                      (should (string-match-p "Permission pending"
                                              (agent-chat--transcript agent-chat--state)))
                      (agent-chat-permissions)
                      ;; Enabling bypass also settles this already pending request.
                      (call-interactively (nth 2 (assoc "!" (plist-get (transient-scope) :respond)))))
                    (should (equal (codex-acp-allowed-directories client) (list notes)))
                    (should-not (map-elt client :codex-acp-allowed-directories))
                    (should-not (agent-chat--session-permissions session))
                    ;; Ordinary tool approvals still use the same bypass path.
                    (let ((check-write (symbol-function 'tramp-async-worker--check-write)))
                      (cl-letf (((symbol-function 'tramp-async-worker--check-write)
                                 (lambda (file)
                                   (unless (equal file (concat notes "draft")) (funcall check-write file)))))
                        (codex-acp--tool-call
                         state 2 (agent-chat-directory-test--call "tramp_write_file" (concat notes "draft")))))
                    (should (file-exists-p (concat notes "draft")))
                    (should (agent-chat--permission-bypass-p session))
                    (should-not (agent-chat--session-permissions session))
                    (should-not (string-match-p "to reopen the permission menu"
                                                (agent-chat--transcript agent-chat--state)))
                    (when (transient-active-prefix 'agent-chat--approval-menu)
                      (transient--emergency-exit :test-next-request))
                    ;; Future directory requests also bypass in a background conversation.
                    (with-temp-buffer
                      (switch-to-buffer (current-buffer))
                      (codex-acp--tool-call
                       state 3 (agent-chat-directory-test--call "tramp_list_directory" other)))
                    (switch-to-buffer owner)
                    (should (equal (codex-acp-allowed-directories client) (list notes other)))
                    (should-not (map-elt (codex-acp--state-client state) :codex-acp-allowed-directories))
                    (should-not (agent-chat--session-permissions session))
                    (should-not (transient-active-prefix 'agent-chat--approval-menu))
                    ;; Disabling bypass restores prompts after the temporary grants expire.
                    (agent-chat--permission-toggle-bypass session)
                    (should-not (agent-chat--permission-bypass-p session))
                    (cl-letf (((symbol-function 'codex-acp--title-start) #'ignore))
                      (codex-acp--finish state))
                    (should-not (codex-acp-allowed-directories client))
                    (setf (codex-acp--state-turn state) "next"
                          (codex-acp--state-prompt state) (cons #'ignore #'ignore))
                    (let ((call (copy-tree (agent-chat-directory-test--call "tramp_list_directory" notes))))
                      (setf (alist-get 'turnId call) "next")
                      (codex-acp--tool-call state 4 call))
                    (should (agent-chat--session-permissions session))
                    (should-not (codex-acp-allowed-directories client))
                    (agent-chat--respond session 4 "reject")
                    (should (equal (buffer-string) "Draft remains"))
                    (should (= (point) 4))
                    (should (eq undo buffer-undo-list)))
                (when (transient-active-prefix 'agent-chat--approval-menu)
                  (transient--emergency-exit :test-cleanup))
                (agent-chat--permission-auto-forget session)
                (setq agent-chat--session nil)))))))))

(provide 'agent-chat-directory-test)
;;; agent-chat-directory-test.el ends here
