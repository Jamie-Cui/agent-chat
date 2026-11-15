;;; consult-agent-chat-status-test.el --- Agent Chat picker status tests -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'consult)
(require 'perspective)

(require 'ert)
(require 'consult)
(require 'marginalia)
(require 'consult-agent-chat)

(ert-deftest consult-agent-chat-status-shares-header-priority ()
  (with-temp-buffer
    (should (string-match-p "Not connected" (agent-chat--conversation-status)))
    (setq agent-chat--state (agent-chat--state-create))
    (dolist (case '((idle . "Ready") (running . "Running")
                    (cancelled . "Interrupted") (failed . "Failed")))
      (setf (agent-chat--state-status agent-chat--state) (car case))
      (should (string-match-p (cdr case) (agent-chat--conversation-status))))
    (setf (agent-chat--state-status agent-chat--state) 'running)
    (setq agent-chat--session (agent-chat--session-create :phase 'connecting))
    (should (string-match-p "connecting" (agent-chat--conversation-status)))
    (setf (agent-chat--session-phase agent-chat--session) 'disconnected)
    (should (string-match-p "disconnected" (agent-chat--conversation-status)))
    (setf (agent-chat--session-phase agent-chat--session) 'ready
          (agent-chat--session-cancelling agent-chat--session) t)
    (should (string-match-p "Cancelling" (agent-chat--conversation-status)))
    (setf (agent-chat--session-permissions agent-chat--session) '((1) (2)))
    (setq agent-chat--queue '((:id 1)) agent-chat--queue-paused t)
    (let ((status (agent-chat--conversation-status)))
      (should (string-match-p "Approval 2.*Queue 1 paused" status))
      (should (equal-including-properties (agent-chat--conversation-status nil t)
                                         (nth 2 (agent-chat--header-line)))))))

(ert-deftest consult-agent-chat-status-shows-title-instead-of-buffer-columns ()
  (with-temp-buffer
    (setq mode-name "Agent/Chat"
          agent-chat--state (agent-chat--state-create :status 'running)
          agent-chat--session (agent-chat--session-create :phase 'ready :title "Project\n summary"))
    (cl-letf (((symbol-function 'marginalia-annotate-buffer)
               (lambda (_) (ert-fail "Generic buffer columns must not be rendered"))))
      (dolist (marginalia-mode '(t nil))
        (let* ((annotation (consult-agent-chat-buffer--annotate (buffer-name)))
               (status "Running"))
          (should (string-prefix-p "Project summary" annotation))
          (should-not (string-match-p "Agent/Chat\\|[\n\r\t]" annotation))
          (should (string-suffix-p status annotation))
          (should (equal (get-text-property (string-search status annotation) 'face annotation)
                         '(font-lock-function-name-face bold)))
          (should (= 50 (string-width (substring annotation 0 (- (length status)))))))))
    (setf (agent-chat--state-status agent-chat--state) 'failed)
    (should (string-match-p "Failed" (consult-agent-chat-buffer--annotate (current-buffer)))))
  (let ((dead (generate-new-buffer " *agent-chat-dead-status*")))
    (kill-buffer dead)
    (should-not (consult-agent-chat-buffer--annotate dead))))

(ert-deftest consult-agent-chat-status-title-bounds-and-notifications ()
  (with-temp-buffer
    (setq agent-chat--state (agent-chat--state-create)
          agent-chat--session (agent-chat--session-create :buffer (current-buffer)
                                                        :phase 'ready :id "title-test"))
    (dolist (title (list nil " \n\t" 42))
      (setf (agent-chat--session-title agent-chat--session) title)
      (should (string-prefix-p "Untitled" (consult-agent-chat-buffer--annotate (current-buffer)))))
    (let ((title (concat (make-string 40 ?\u3042) "long title")))
      (setf (agent-chat--session-title agent-chat--session) title)
      (let ((annotation (consult-agent-chat-buffer--annotate (current-buffer))))
        (should (string-match-p "…" annotation))
        (should (equal title (get-text-property 0 'help-echo annotation)))))
    (agent-chat--notification
     agent-chat--session '((method . "session/update")
                          (params (sessionId . "title-test")
                                  (update (sessionUpdate . "session_info_update")
                                          (title . "Updated session title")))))
    (should (string-prefix-p "Updated session title"
                              (consult-agent-chat-buffer--annotate (current-buffer))))))

(ert-deftest consult-agent-chat-status-scopes-marginalia-to-this-picker ()
  (with-temp-buffer
    (let ((buffer (current-buffer)) (saved marginalia-annotators)
          (original-state (plist-get consult-agent-chat-source :state)))
      (setq agent-chat--state (agent-chat--state-create :status 'running))
      (cl-letf (((symbol-function 'consult-agent-chat-buffer--buffers) (lambda () (list buffer)))
                ((symbol-function 'consult--multi)
                 (lambda (sources &rest _options)
                   (let* ((source (car sources))
                          (candidate (propertize (buffer-name buffer) 'multi-category
                                                 (cons 'buffer (buffer-name buffer)))))
                     (should (eq (plist-get source :category) 'buffer))
                     (should (eq (plist-get source :state) original-state))
                     (let ((marginalia--metadata
                            `(metadata (annotation-function
                                        . ,(lambda (cand)
                                             (funcall (plist-get source :annotate)
                                                      (cdr (get-text-property 0 'multi-category cand))))))))
                       (should (string-match-p "Untitled.*Running"
                                               (marginalia-annotate-multi-category candidate))))
                     (should (string-match-p "Running"
                                             (funcall (plist-get source :annotate) buffer)))))))
        (consult-agent-chat-buffer))
      (should (eq saved marginalia-annotators)))))

(provide 'consult-agent-chat-status-test)
