;;; agent-chat-view-test.el --- Window lifecycle contracts -*- lexical-binding: t; -*-

(require 'agent-chat-native-window-test)

(defmacro agent-chat-view-test--with-second (&rest body)
  "Run BODY with a second conversation named SECOND."
  (declare (indent 0) (debug t))
  `(let ((second (generate-new-buffer " *agent-chat-view-second*")))
     (unwind-protect
         (progn
           (with-current-buffer second
             (agent-chat-mode)
             (agent-chat--attach (agent-chat--state-create) nil "Second")
             (insert "Second draft"))
           ,@body)
       (when (buffer-live-p second)
         (with-current-buffer second
           (let ((kill-buffer-query-functions nil)) (kill-buffer second)))))))

(ert-deftest agent-chat-view-owner-does-not-depend-on-display ()
  (agent-chat-native-with-view
    (should (eq (agent-chat-owner owner) owner))
    (should (eq (agent-chat-owner messages) owner))
    (should-not (agent-chat-owner source))
    (agent-chat-view-hide history)
    (should (eq (agent-chat-owner messages) owner))
    (with-temp-buffer
      (agent-chat-history-mode)
      (setq agent-chat--owner (generate-new-buffer " *dead-owner*"))
      (kill-buffer agent-chat--owner)
      (should-not (agent-chat-owner))
      (should-not (agent-chat-owner agent-chat--owner)))))

(ert-deftest agent-chat-view-show-from-either-pane-preserves-session-and-draft ()
  (dolist (side '(prompt history))
    (agent-chat-native-with-view
      (insert "Original draft")
      (let ((state agent-chat--state) (queue (list (list :draft "Queued"))))
        (setq agent-chat--queue queue)
        (agent-chat-view-test--with-second
          (select-window (if (eq side 'prompt) prompt history))
          (let ((selected (selected-window)))
            (should (eq prompt (agent-chat-view-show second selected)))
            (should (eq selected (selected-window)))
            (should (eq (window-buffer prompt) second))
            (should (eq (agent-chat-owner (window-buffer history)) second))
            (should (eq (window-dedicated-p prompt) t))
            (should (eq (window-dedicated-p history) t))
            (agent-chat-view-show owner selected)
            (with-current-buffer owner
              (should (equal (buffer-string) "Original draft"))
              (should (eq agent-chat--state state))
              (should (eq agent-chat--queue queue)))))))))

(ert-deftest agent-chat-view-failed-show-restores-complete-view ()
  (dolist (side '(prompt history))
    (agent-chat-native-with-view
      (agent-chat-native-fill owner)
      (insert "Original draft")
      (set-window-point prompt 4)
      (set-window-point history 15)
      (set-window-start history 8 t)
      (select-window (if (eq side 'prompt) prompt history))
      (agent-chat-view-test--with-second
        (let ((before (window-state-get nil t))
              (selected (selected-window))
              (previous (copy-tree (window-prev-buffers prompt)))
              (next (copy-sequence (window-next-buffers prompt))))
          (cl-letf (((symbol-function 'agent-chat--render-history)
                     (lambda (_window) (error "Injected render failure"))))
            (should (equal '(error "Injected render failure")
                           (should-error (agent-chat-view-show second selected)))))
          (should (equal before (window-state-get nil t)))
          (should (eq selected (selected-window)))
          (should (eq (window-buffer prompt) owner))
          (should (eq (window-buffer history) messages))
          (should (equal previous (window-prev-buffers prompt)))
          (should (equal next (window-next-buffers prompt)))
          (should (eq (window-parameter prompt 'agent-chat-history-window) history))
          (should (eq (window-parameter history 'agent-chat-prompt-window) prompt))
          (should (eq (window-atom-root history) (window-parent prompt))))))))

(ert-deftest agent-chat-view-failed-replace-restores-deleted-pane ()
  (dolist (side '(prompt history))
    (agent-chat-native-with-view
      (delete-other-windows prompt)
      (select-window (if (eq side 'prompt) prompt history))
      (let ((before (window-state-get nil t))
            (selected (selected-window))
            (switch (symbol-function 'switch-to-buffer)))
        (cl-letf (((symbol-function 'switch-to-buffer)
                   (lambda (&rest args)
                     (apply switch args)
                     (error "Injected replacement failure"))))
          (should-error (agent-chat-view-replace selected source)))
        (should (equal before (window-state-get nil t)))
        (should (eq selected (selected-window)))
        (should (window-live-p prompt))
        (should (window-live-p history))
        (should (eq (window-parameter prompt 'agent-chat-history-window) history))
        (should (eq (window-parameter history 'agent-chat-prompt-window) prompt))
        (should (eq (window-atom-root history) (window-parent prompt)))))))

(ert-deftest agent-chat-view-redraw-survives-window-removed-during-queue-render ()
  (agent-chat-native-with-view
    (cl-letf (((symbol-function 'agent-chat--render-queue)
               (lambda (_window) (agent-chat-view-hide prompt))))
      (with-current-buffer owner (agent-chat--redraw)))
    (should-not (window-live-p prompt))
    (should-not (window-live-p history))
    (should (buffer-live-p owner))
    (agent-chat-view-hide prompt)))

(ert-deftest agent-chat-view-replace-preserves-unrelated-dedication ()
  (agent-chat-native-with-view
    (let ((editing (get-buffer-window source)))
      (set-window-dedicated-p editing t)
      (should-error (agent-chat-view-show owner editing) :type 'user-error)
      (should (eq (window-dedicated-p editing) t)))))

(ert-deftest agent-chat-view-forced-replacement-releases-either-pane ()
  (dolist (side '(prompt history))
    (agent-chat-native-with-view
      (let ((target (if (eq side 'prompt) prompt history)))
        (set-window-dedicated-p target nil)
        (set-window-buffer target source)
        (agent-chat-view-maintain-frame (selected-frame))
        (should (window-live-p target))
        (should (eq (window-buffer target) source))
        (should-not (window-atom-root target))
        (should-not (window-dedicated-p target))
        (should-not (get-buffer-window owner))
        (should-not (get-buffer-window messages))
        (should (buffer-live-p owner))))))

(ert-deftest agent-chat-view-can-replace-restored-pair-before-redisplay ()
  (dolist (side '(prompt history))
    (agent-chat-native-with-view
      (set-window-parameter prompt 'agent-chat-history-window nil)
      (set-window-parameter history 'agent-chat-prompt-window nil)
      (walk-window-subtree
       (lambda (window) (set-window-parameter window 'window-atom nil))
       (window-parent prompt) t)
      (let ((target (if (eq side 'prompt) prompt history)))
        (agent-chat-view-replace target source)
        (should (eq (window-buffer target) source))
        (should-not (get-buffer-window owner))
        (should-not (get-buffer-window messages))
        (should-not (window-dedicated-p target))))))

(ert-deftest agent-chat-view-rollback-reports-secondary-failure ()
  (agent-chat-native-with-view
    (agent-chat-view-test--with-second
      (let ((restore (symbol-function 'set-window-configuration)) warnings)
        (cl-letf (((symbol-function 'agent-chat--render-history)
                   (lambda (_window) (error "Primary failure")))
                  ((symbol-function 'set-window-configuration)
                   (lambda (&rest args)
                     (apply restore args)
                     (error "Secondary failure")))
                  ((symbol-function 'display-warning)
                   (lambda (_type message &rest _args) (push message warnings))))
          (should (equal '(error "Primary failure")
                         (should-error (agent-chat-view-show second prompt)))))
        (should (string-match-p "Secondary failure" (car warnings)))
        (should (eq (window-buffer prompt) owner))
        (should (eq (window-buffer history) messages))))))

(ert-deftest agent-chat-view-ignores-live-but-stale-window-pointers ()
  (agent-chat-native-with-view
    (let ((editing (get-buffer-window source)))
      (set-window-parameter prompt 'agent-chat-history-window editing)
      (set-window-parameter history 'agent-chat-prompt-window editing)
      (agent-chat-view-show owner history)
      (should (= 3 (length (window-list))))
      (should (eq (window-buffer editing) source))
      (should (eq (window-parameter prompt 'agent-chat-history-window) history))
      (should (eq (window-parameter history 'agent-chat-prompt-window) prompt)))))

(ert-deftest agent-chat-view-show-rolls-back-if-render-removes-target ()
  (agent-chat-native-with-view
    (agent-chat-view-test--with-second
      (let ((before (window-state-get nil t)))
        (cl-letf (((symbol-function 'agent-chat--render-queue)
                   (lambda (_window) (agent-chat-view-hide prompt))))
          (should-error (agent-chat-view-show second prompt)))
        (should (equal before (window-state-get nil t)))
        (should (eq (window-buffer prompt) owner))
        (should (eq (window-buffer history) messages))))))

(provide 'agent-chat-view-test)
;;; agent-chat-view-test.el ends here
