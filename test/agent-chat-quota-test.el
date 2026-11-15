;;; agent-chat-quota-test.el --- Quota and header regressions -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Synthetic account data only.  No model requests, credentials or history.

;;; Code:
(require 'ert)
(require 'agent-chat)
(require 'codex-acp)

(defun agent-chat-quota-test-data (&optional used minutes)
  "Build a synthetic account snapshot with USED percent over MINUTES."
  `((rateLimits . ((limitId . "codex")
                   (primary . ((usedPercent . ,(or used 28))
                               (windowDurationMins . ,(or minutes 300))
                               (resetsAt . ,(+ (float-time) 3600))))
                   (secondary . ((usedPercent . 59)
                                 (windowDurationMins . 10080)
                                 (resetsAt . ,(+ (float-time) 86400))))))))

(defun agent-chat-quota-test-text (value)
  "Flatten the strings in a header VALUE without evaluating redisplay."
  (if (stringp value) value (mapconcat #'identity value "")))

(ert-deftest agent-chat-quota-account-notifications-between-turns ()
  (let* ((state (codex-acp--state-create))
         (data (agent-chat-quota-test-data)) seen)
    (setf (codex-acp--state-client state)
          (list (cons :notification-handlers (list (lambda (event) (push event seen))))))
    (codex-acp--notification state "account/rateLimits/updated" data)
    (codex-acp--notification state "account/updated" '((authMode . "chatgpt")))
    (should (= 2 (length seen)))
    (should (equal (map-elt (cadr seen) 'method) "_codex/account/rateLimits/updated"))
    (should (equal (map-elt (cadr seen) 'params) data))
    (should-not (codex-acp--state-prompt state))))

(ert-deftest agent-chat-quota-read-is-isolated-and-does-not-start-turn ()
  (let* ((active (cons #'ignore #'ignore))
         (state (codex-acp--state-create :initialized t :prompt active))
         (client (list (cons :codex-acp-state state)))
         seen)
    (cl-letf (((symbol-function 'codex-acp--rpc)
               (lambda (s method _params _success failure &optional isolated)
                 (should (eq s state))
                 (should (equal method "account/rateLimits/read"))
                 (should isolated)
                 (funcall failure '((message . "Unavailable"))))))
      (codex-acp-read-rate-limits client #'ignore (lambda (err) (setq seen err))))
    (should seen)
    (should (eq active (codex-acp--state-prompt state)))
    (should-not (codex-acp--state-closed state))))

(ert-deftest agent-chat-quota-renders-remaining-and-server-duration ()
  (with-temp-buffer
    (setq agent-chat-quota--client '(fixture))
    (agent-chat-quota--accept (agent-chat-quota-test-data))
    (should (equal (agent-chat-quota--label) "Remaining 5h 72% · 1w 41%"))
    (should (string-search "Account quota (shared across conversations)"
                           (agent-chat-quota--tooltip)))
    (should (string-search "resets " (agent-chat-quota--tooltip)))
    (agent-chat-quota--accept (agent-chat-quota-test-data 100 15))
    (should (equal (agent-chat-quota--label) "Remaining 15m 0% · 1w 41%"))
    (should (eq 'error (get-text-property 0 'face (agent-chat-quota--label))))))

(ert-deftest agent-chat-quota-partial-bucket-push-preserves-other-buckets ()
  (with-temp-buffer
    (setq agent-chat-quota--client '(fixture))
    (let ((extra (copy-tree (map-elt (agent-chat-quota-test-data 20 60) 'rateLimits))))
      (setf (alist-get 'limitId extra) "other")
      (agent-chat-quota--accept
       `((rateLimitsByLimitId . ((codex . ,(map-elt (agent-chat-quota-test-data) 'rateLimits))
                                 (other . ,extra)))) t)
      (agent-chat-quota--accept (agent-chat-quota-test-data 35))
      (should (= 2 (length agent-chat-quota--buckets)))
      (should (string-search "5h 65%" (agent-chat-quota--label)))
      (should (string-search "1h: 80% remaining" (agent-chat-quota--tooltip))))))

(ert-deftest agent-chat-quota-does-not-guess-after-reset ()
  (with-temp-buffer
    (setq agent-chat-quota--client '(fixture))
    (let ((data (agent-chat-quota-test-data 100)))
      (setf (alist-get 'resetsAt (map-nested-elt data '(rateLimits primary))) 1)
      (agent-chat-quota--accept data))
    (should (string-search "5h 0%" (agent-chat-quota--label)))
    (should (string-suffix-p " *" (agent-chat-quota--label)))
    (should (string-search "[stale]" (agent-chat-quota--tooltip)))))

(ert-deftest agent-chat-quota-failure-retains-last-snapshot ()
  (with-temp-buffer
    (setq agent-chat-quota--client '(fixture))
    (agent-chat-quota--accept (agent-chat-quota-test-data))
    (cl-letf (((symbol-function 'codex-acp-read-rate-limits)
               (lambda (_ _ok fail) (funcall fail '((message . "Timed out"))))))
      (agent-chat-quota-refresh))
    (should (string-search "72%" (agent-chat-quota--label)))
    (should (string-suffix-p " *" (agent-chat-quota--label)))
    (should (string-search "Timed out" (agent-chat-quota--tooltip)))
    (should-not agent-chat-quota--pending)))

(ert-deftest agent-chat-quota-new-push-wins-over-pending-read ()
  (with-temp-buffer
    (setq agent-chat-quota--client '(fixture))
    (let (reply)
      (cl-letf (((symbol-function 'codex-acp-read-rate-limits)
                 (lambda (_ ok _fail) (setq reply ok))))
        (agent-chat-quota-refresh))
      (agent-chat-quota--accept (agent-chat-quota-test-data 60))
      (funcall reply (agent-chat-quota-test-data 20))
      (should (string-search "5h 40%" (agent-chat-quota--label)))
      (should-not agent-chat-quota--pending))))

(ert-deftest agent-chat-quota-account-change-rejects-old-read ()
  (with-temp-buffer
    (setq agent-chat-quota--client '(fixture))
    (let (replies)
      (cl-letf (((symbol-function 'codex-acp-read-rate-limits)
                 (lambda (_ ok _fail) (push ok replies))))
        (agent-chat-quota-refresh)
        (agent-chat-quota--accept (agent-chat-quota-test-data))
        (agent-chat-quota--notification agent-chat-quota--client
                                        "_codex/account/updated" '((authMode . nil))))
      (should-not agent-chat-quota--buckets)
      (funcall (cadr replies) (agent-chat-quota-test-data 20))
      (should-not agent-chat-quota--buckets)
      (funcall (car replies) (agent-chat-quota-test-data 80))
      (should (string-search "5h 20%" (agent-chat-quota--label))))))

(ert-deftest agent-chat-quota-stopping-rejects-late-results-and-cancels-timer ()
  (with-temp-buffer
    (let (reply timer)
      (cl-letf (((symbol-function 'codex-acp-read-rate-limits)
                 (lambda (_ ok _fail) (setq reply ok))))
        (agent-chat-quota--start '(fixture))
        (setq timer agent-chat-quota--timer)
        (agent-chat-quota--stop)
        (funcall reply (agent-chat-quota-test-data)))
      (should-not agent-chat-quota--client)
      (should-not agent-chat-quota--pending)
      (should-not agent-chat-quota--buckets)
      (should-not (memq timer timer-list)))))

(ert-deftest agent-chat-quota-rendering-never-requests-data ()
  (with-temp-buffer
    (should-not (agent-chat-quota--label))
    (setq agent-chat-quota--client '(fixture))
    (should (equal (agent-chat-quota--label) "Remaining —"))
    (cl-letf (((symbol-function 'codex-acp-read-rate-limits)
               (lambda (&rest _) (ert-fail "Redisplay issued a quota request"))))
      (agent-chat-quota--label)
      (agent-chat--prompt-header))))

(ert-deftest agent-chat-quota-header-separates-queue-from-execution ()
  (with-temp-buffer
    (setq agent-chat--session (agent-chat--session-create
                              :title "Quota design 100%" :phase 'ready
                              :model '((currentModelId . "GPT-6-Astra"))
                              :config-options '(((category . "thought_level") (currentValue . "xhigh"))))
          agent-chat--state (agent-chat--state-create :status 'running)
          agent-chat--label "Codex"
          agent-chat--queue '((:draft "one") (:draft "two"))
          agent-chat--queue-paused "Waiting for approval"
          agent-chat-quota--client '(fixture)
          default-directory "/work/dotfiles/")
    (agent-chat-quota--accept (agent-chat-quota-test-data))
    (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 150)))
      (let ((header (agent-chat-quota-test-text (agent-chat--header-line)))
            (prompt (agent-chat-quota-test-text (agent-chat--prompt-header))))
        (should (string-search "GPT-6-Astra / xhigh" header))
        (should (string-search "dotfiles" header))
        (should-not (string-search "/work/" header))
        (should-not (string-search "Queue" header))
        (should (string-search "Quota design 100%%" prompt))
        (should (string-search "Queue 2 paused" prompt))
        (should (string-search "Remaining 5h 72%%" prompt))
        (should-not (string-search "interrupt" prompt))
        (should-not (string-search "C-c" prompt))
        (should (string-search "Queue 2 paused" (agent-chat--conversation-status t)))))))

(ert-deftest agent-chat-quota-narrow-header-keeps-percentages-visible ()
  (with-temp-buffer
    (setq agent-chat--session (agent-chat--session-create :title (make-string 80 ?x))
          agent-chat--queue '((:draft "one"))
          agent-chat-quota--client '(fixture))
    (agent-chat-quota--accept (agent-chat-quota-test-data))
    (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 40)))
      (let ((text (agent-chat-quota-test-text (agent-chat--prompt-header))))
        (should (string-search "5h 72%% · 1w 41%%" text))
        (should (string-search "Queue 1" text))
        (should-not (string-search "Remaining" text))
        (should (<= (string-width (replace-regexp-in-string "%%" "%" text t t)) 40))))))

(ert-deftest agent-chat-quota-directory-label-retains-remote-host ()
  (with-temp-buffer
    (let ((default-directory "/ssh:fixture:/srv/dotfiles/"))
      (should (equal "fixture:dotfiles" (agent-chat--directory-label))))
    (let ((default-directory "/") (agent-chat--scope 'host))
      (should (equal "General · /" (agent-chat--directory-label))))))


(ert-deftest agent-chat-quota-invalid-windows-stay-unknown ()
  (with-temp-buffer
    (setq agent-chat-quota--client '(fixture))
    (dolist (window '(nil 7 "bad" ((usedPercent . "20") (windowDurationMins . 300))
                         ((usedPercent . 20) (windowDurationMins . nil))))
      (agent-chat-quota--accept `((rateLimits . ((primary . ,window)))) t)
      (should (equal "Remaining —" (agent-chat-quota--label))))))

(ert-deftest agent-chat-quota-unrelated-bucket-does-not-replace-primary ()
  (with-temp-buffer
    (setq agent-chat-quota--client '(fixture))
    (agent-chat-quota--accept (agent-chat-quota-test-data) t)
    (let ((other (copy-tree (agent-chat-quota-test-data 90 60))))
      (setf (alist-get 'limitId (map-elt other 'rateLimits)) "other")
      (agent-chat-quota--accept other))
    (should (string-search "5h 72%" (agent-chat-quota--label)))
    (should (string-search "1h: 10% remaining" (agent-chat-quota--tooltip)))))

(ert-deftest agent-chat-quota-refresh-throttles-and-never-polls-from-display ()
  (with-temp-buffer
    (setq agent-chat-quota--client '(fixture))
    (let ((calls 0))
      (cl-letf (((symbol-function 'codex-acp-read-rate-limits)
                 (lambda (_ _ok fail)
                   (cl-incf calls)
                   (funcall fail '((message . "Offline"))))))
        (agent-chat-quota--maybe-refresh)
        (agent-chat-quota--maybe-refresh)
        (should (= calls 1))
        (setq agent-chat-quota--attempted (- (float-time) 61))
        (agent-chat-quota--maybe-refresh)
        (should (= calls 2))))))

(ert-deftest agent-chat-quota-account-updates-reach-owner-without-session-id ()
  (with-temp-buffer
    (let* ((owner (current-buffer))
           (client '(fixture))
           (session (agent-chat--session-create :buffer owner :client client :phase 'ready)))
      (setq agent-chat--session session agent-chat-quota--client client)
      (with-temp-buffer
        (agent-chat--notification session
                                  `((method . "_codex/account/rateLimits/updated")
                                    (params . ,(agent-chat-quota-test-data)))))
      (should (string-search "5h 72%" (agent-chat-quota--label)))
      (setf (agent-chat--session-phase session) 'disconnected)
      (cl-letf (((symbol-function 'agent-chat--publish) #'ignore))
        (agent-chat--refresh-session session))
      (should-not agent-chat-quota--client)
      (should (string-suffix-p " *" (agent-chat-quota--label))))))

(ert-deftest agent-chat-quota-details-refreshes-without-editing-draft ()
  (let ((agent-chat-quota--details-buffer (generate-new-buffer " *quota-details-test*")))
    (unwind-protect
        (with-temp-buffer
          (let ((owner (current-buffer)))
            (insert "Unsent draft")
            (setq agent-chat-quota--client '(fixture))
            (agent-chat-quota--accept (agent-chat-quota-test-data))
            (agent-chat-quota--render-details)
            (with-current-buffer agent-chat-quota--details-buffer
              (should (eq owner agent-chat-quota--owner))
              (should (derived-mode-p 'special-mode))
              (should (string-search "72% remaining" (buffer-string))))
            (agent-chat-quota--accept (agent-chat-quota-test-data 50))
            (with-current-buffer agent-chat-quota--details-buffer
              (should (string-search "50% remaining" (buffer-string)))
              (should-not (string-search "72% remaining" (buffer-string))))
            (should (equal "Unsent draft" (buffer-string)))))
      (kill-buffer agent-chat-quota--details-buffer))))

(ert-deftest agent-chat-quota-gui-header-percentages-and-title ()
  (skip-unless (display-graphic-p))
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (setq agent-chat--session (agent-chat--session-create :title "Task 100%")
            agent-chat-quota--client '(fixture))
      (agent-chat-quota--accept (agent-chat-quota-test-data))
      (let ((text (format-mode-line (agent-chat--prompt-header) nil
                                    (selected-window) (current-buffer))))
        (should (string-search "72%" text))
        (should-not (string-search "72%%" text))
        (should (string-search "Task 100%" text))
        (should-not (string-search "Task 100%%" text))))))


(ert-deftest agent-chat-quota-backend-replacement-forgets-old-account ()
  (with-temp-buffer
    (let ((old-client '(old-account))
          (agent-chat-agents '((fixture :client-maker (lambda (_) '(new-backend)))))
          reply)
      (cl-letf (((symbol-function 'codex-acp-read-rate-limits)
                 (lambda (_ ok _fail) (setq reply ok)))
                ((symbol-function 'agent-chat--connect) #'ignore))
        (agent-chat-quota--start old-client)
        (agent-chat-quota--accept (agent-chat-quota-test-data))
        (let ((timer agent-chat-quota--timer))
          (agent-chat--begin-connection 'fixture)
          (should-not (memq timer timer-list))))
      (should-not (agent-chat-quota--label))
      (should-not agent-chat-quota--buckets)
      (should-not agent-chat-quota--timer)
      (funcall reply (agent-chat-quota-test-data 90))
      (agent-chat-quota--notification old-client "_codex/account/rateLimits/updated"
                                      (agent-chat-quota-test-data 70))
      (should-not (agent-chat-quota--label))
      (should (equal '(new-backend) (agent-chat--session-client agent-chat--session))))))

(provide 'agent-chat-quota-test)
;;; agent-chat-quota-test.el ends here
