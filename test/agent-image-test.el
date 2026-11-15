;;; agent-image-test.el --- Clipboard and ACP image regressions -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Offline tests using a generated red PNG, never the user's clipboard.
;; Load with installed packages and the package directory on `load-path'.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'agent-chat)
(require 'codex-acp)

(defconst agent-image-test-png
  "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAAF0lEQVR4nGP4z8BAEiJN9aiGUQ1DSgMAkPn/Afnh+ngAAAAASUVORK5CYII=")

(defun agent-image-test-content ()
  "Return a fresh ACP block for the synthetic red image."
  `((type . "image") (mimeType . "image/png") (data . ,agent-image-test-png)))

(defun agent-image-test-draft ()
  "Copy the draft's text and attachments, excluding transient fontification."
  (agent-chat--image-copy (buffer-string)))

(defmacro agent-image-test-with-session (&rest body)
  "Run BODY in an isolated Agent Chat composer with a fake ACP transport."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (let ((buffer (generate-new-buffer " *image-test*"))
           (agent-chat-stream-refresh-interval 0) sent finish failure)
       (unwind-protect
           (progn
             (switch-to-buffer buffer)
             (agent-chat-mode)
             (agent-chat--attach (agent-chat--state-create) nil "Image test")
             (let ((session (agent-chat--session-create
                             :buffer buffer :agent 'test :cwd default-directory
                             :client (acp-make-client :command "cat") :id "test" :phase 'ready
                             :capabilities '((promptCapabilities . ((image . t)))))))
               (setq agent-chat--session session
                     agent-chat--adapter
                     (lambda (request emit) (agent-chat--send session request emit)))
               (cl-letf (((symbol-function 'acp-send-request)
                          (lambda (&rest args)
                            (push (map-nested-elt (plist-get args :request) '(:params prompt)) sent)
                            (setq finish (plist-get args :on-success)
                                  failure (plist-get args :on-failure))))
                         ((symbol-function 'acp-send-notification) #'ignore)
                         ((symbol-function 'acp-send-response) #'ignore))
                 ,@body)))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))))

(ert-deftest agent-image-test-codec-roundtrip-and-rejections ()
  (let* ((content (agent-image-test-content))
         (data (base64-decode-string agent-image-test-png)))
    (should (equal (acp-image-from-data "image/png" data) content))
    (should (equal (acp-image-data content) data))
    (should (equal (acp-image-from-data-url (acp-image-to-data-url content)) content))
    (should-not (acp-image-from-data-url "https://example.invalid/image.png"))
    (should-not (acp-image-from-data-url "file:///private/image.png"))
    (should-error (acp-image-from-data "image/jpeg" data) :type 'user-error)
    (should-error (acp-image-from-data "image/svg+xml" "<svg/>") :type 'user-error)
    (dolist (encoded '("" "!!!!" "abcd"))
      (should-error (acp-image-data `((type . "image") (mimeType . "image/png")
                                    (data . ,encoded))) :type 'user-error))
    (let ((acp-image-max-bytes 8))
      (should-error (acp-image-data content) :type 'user-error)
      (should-error (acp-image-from-data "image/png" data) :type 'user-error))))

(ert-deftest agent-image-test-native-yank-replaces-region-and-keeps-text-local ()
  (agent-image-test-with-session
    (insert "Keep replace")
    (goto-char 6) (set-mark (point-max)) (setq mark-active t transient-mark-mode t)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (_) t))
              ((symbol-function 'gui-get-selection)
               (lambda (_selection target)
                 (pcase target
                   ('TARGETS [UTF8_STRING image/png])
                   ('image/png (base64-decode-string agent-image-test-png))
                   (_ (ert-fail "Text clipboard must not be read"))))))
      (agent-chat-paste-image))
    (should-not sent)
    (should (equal (buffer-substring-no-properties (point-min) (point-max)) "Keep \ufffc"))
    (insert " after")
    (should-not (get-text-property (1- (point-max)) 'agent-chat-image))
    (should (equal (mapcar (lambda (part) (map-elt part 'type))
                          (agent-chat--prompt-content (buffer-string)))
                   '("text" "image" "text")))))

(ert-deftest agent-image-test-undo-deletion-restores-bytes ()
  (agent-image-test-with-session
    (insert "before ")
    (setq buffer-undo-list nil)
    (agent-chat--insert-image 'image/png (base64-decode-string agent-image-test-png))
    (undo-boundary)
    (let ((snapshot (agent-image-test-draft)))
      (delete-char -1)
      (undo-boundary)
      (undo-only 1)
      (should (equal-including-properties snapshot (agent-image-test-draft)))
      (should (equal (map-elt (car (last (agent-chat--prompt-content (buffer-string)))) 'data)
                     agent-image-test-png)))))

(ert-deftest agent-image-test-macos-tiff-clipboard-converts-locally ()
  (agent-image-test-with-session
    (insert "Keep replace")
    (goto-char 6) (set-mark (point-max)) (setq mark-active t transient-mark-mode t)
    (let ((system-type 'darwin) (default-directory "/ssh:image-test:/tmp/") called)
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'gui-get-selection)
                 (lambda (_selection target)
                   (should (eq target 'TARGETS))
                   [TARGETS image/tiff]))
                ((symbol-function 'executable-find)
                 (lambda (program &rest _) (should (equal program "pngpaste")) "/test/pngpaste"))
                ((symbol-function 'call-process)
                 (lambda (&rest args)
                   (should (equal args '("/test/pngpaste" nil (t nil) nil "-")))
                   (should-not (file-remote-p default-directory))
                   (should-not enable-multibyte-characters)
                   (setq called t)
                   (insert (base64-decode-string agent-image-test-png))
                   0)))
        (agent-chat-paste-image))
      (should called))
    (should-not sent)
    (should (equal (buffer-substring-no-properties (point-min) (point-max)) "Keep \ufffc"))
    (should (equal (cadr (agent-chat--prompt-content (buffer-string)))
                   (agent-image-test-content)))))

(ert-deftest agent-image-test-macos-prefers-native-png-over-tiff ()
  (agent-image-test-with-session
    (let ((system-type 'darwin))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'image-type-available-p) (lambda (_) t))
                ((symbol-function 'gui-get-selection)
                 (lambda (_selection target)
                   (pcase target
                     ('TARGETS [TARGETS image/tiff image/png])
                     ('image/png (base64-decode-string agent-image-test-png))
                     (_ (ert-fail "Only the PNG representation should be read")))))
                ((symbol-function 'call-process)
                 (lambda (&rest _) (ert-fail "Native PNG must not invoke a converter"))))
        (agent-chat-paste-image)))
    (should (equal (agent-chat--prompt-content (buffer-string))
                   (list (agent-image-test-content))))))

(ert-deftest agent-image-test-macos-tiff-failures-preserve-draft ()
  (agent-image-test-with-session
    (insert "Keep selected text")
    (set-mark (point-min)) (setq mark-active t transient-mark-mode t)
    (let ((system-type 'darwin) (draft (agent-image-test-draft)))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'gui-get-selection) (lambda (&rest _) [TARGETS image/tiff])))
        (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
          (should-error (agent-chat-paste-image) :type 'user-error))
        (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/test/pngpaste")))
          (dolist (status '(1 "Killed" 0))
            (cl-letf (((symbol-function 'call-process)
                       (lambda (&rest _) (insert "Not a PNG") status)))
              (should-error (agent-chat-paste-image) :type 'user-error)))))
      (should (equal-including-properties draft (agent-image-test-draft)))
      (should mark-active)
      (should-not sent))))

(ert-deftest agent-image-test-pure-image-and-multiple-images-on-wire ()
  (agent-image-test-with-session
    (dotimes (_ 2) (agent-chat--insert-image 'image/png (base64-decode-string agent-image-test-png)))
    (agent-chat-submit)
    (should (equal (mapcar (lambda (part) (map-elt part 'type)) (car sent)) '("image" "image")))
    (should (string-empty-p (buffer-string)))
    (let ((text (plist-get (car (agent-chat--state-history agent-chat--state)) :text)))
      (should (equal (map-elt (get-text-property 0 'agent-chat-image text) 'data) agent-image-test-png))
      (should (equal (map-elt (get-text-property 1 'agent-chat-image text) 'data) agent-image-test-png)))))

(ert-deftest agent-image-test-capability-rejection-preserves-draft ()
  (agent-image-test-with-session
    (agent-chat--insert-image 'image/png (base64-decode-string agent-image-test-png))
    (let ((draft (agent-image-test-draft)))
      (setf (agent-chat--session-capabilities session) nil)
      (should-error (agent-chat-submit) :type 'user-error)
      (should (equal-including-properties draft (agent-image-test-draft)))
      (should-not sent)
      (should-not (agent-chat--state-history agent-chat--state))
      (should-error (agent-chat--insert-image 'image/png (base64-decode-string agent-image-test-png))
                    :type 'user-error)
      (should (equal-including-properties draft (agent-image-test-draft))))))

(ert-deftest agent-image-test-synchronous-failure-preserves-image ()
  (agent-image-test-with-session
    (agent-chat--insert-image 'image/png (base64-decode-string agent-image-test-png))
    (let ((draft (agent-image-test-draft)))
      (cl-letf (((symbol-function 'acp-send-request) (lambda (&rest _) (error "Offline"))))
        (agent-chat-submit))
      (should (equal-including-properties draft (agent-image-test-draft)))
      (should (eq (agent-chat--state-status agent-chat--state) 'failed)))))

(ert-deftest agent-image-test-asynchronous-failure-keeps-history-image ()
  (agent-image-test-with-session
    (agent-chat--insert-image 'image/png (base64-decode-string agent-image-test-png))
    (agent-chat-submit)
    (insert "next draft")
    (funcall failure '((message . "Disconnected")))
    (should (equal (buffer-string) "next draft"))
    (let ((text (plist-get (car (agent-chat--state-history agent-chat--state)) :text)))
      (should (equal (map-elt (get-text-property 0 'agent-chat-image text) 'data) agent-image-test-png)))))

(ert-deftest agent-image-test-queue-rechecks-capability-and-keeps-image ()
  (agent-image-test-with-session
    (insert "first") (agent-chat-submit)
    (agent-chat--insert-image 'image/png (base64-decode-string agent-image-test-png))
    (agent-chat-submit)
    (setf (agent-chat--session-capabilities session) nil)
    (funcall finish '((stopReason . "end_turn")))
    (agent-chat--queue-send-next)
    (should (= (length sent) 1))
    (should agent-chat--queue-paused)
    (should (equal (map-elt (get-text-property 0 'agent-chat-image
                                              (plist-get (car agent-chat--queue) :draft)) 'data)
                   agent-image-test-png))))

(ert-deftest agent-image-test-rendered-history-keeps-preview-data ()
  (agent-image-test-with-session
    (agent-chat--insert-image 'image/png (base64-decode-string agent-image-test-png))
    (agent-chat-submit)
    (agent-chat--redraw)
    (agent-chat-browse-history)
    (goto-char (point-min))
    (search-forward "\ufffc")
    (should (equal (map-elt (get-text-property (1- (point)) 'agent-chat-image) 'data)
                   agent-image-test-png))
    (should (equal (get-text-property (1- (point)) 'display) "[Image · PNG]"))
    (should-not (string-search agent-image-test-png (buffer-string)))
    (backward-char)
    (let ((position (point))
          (history (current-buffer))
          (window (selected-window))
          previews)
      (should (eq (get-text-property position 'mouse-face) 'highlight))
      (cl-letf (((symbol-function 'agent-chat-preview-image)
                 (lambda ()
                   (push (list (current-buffer) (point)
                               (acp-image-data (get-text-property (point) 'agent-chat-image)))
                         previews))))
        (call-interactively (key-binding (kbd "RET")))
        (let ((click (lookup-key (get-text-property position 'keymap) [mouse-1])))
          (should (commandp click))
          (goto-char (point-min))
          (funcall click `(mouse-1 (,window ,position (0 . 0) 0))))
        (should (equal previews
                       (make-list 2 (list history position
                                          (base64-decode-string agent-image-test-png)))))
        ;; Clicking ordinary text must not open the preceding attachment.
        (agent-chat--reading-click `(mouse-1 (,window 1 (0 . 0) 0)))
        (should (= (length previews) 2)))
      (should (equal (with-current-buffer buffer (buffer-string)) ""))
      (should (= (length sent) 1)))))

(ert-deftest agent-image-test-capture-drops-foreign-text-properties ()
  (agent-image-test-with-session
    (insert (propertize "plain" 'display "unrelated display" 'help-echo "foreign"))
    (let ((draft (agent-image-test-draft)))
      (should (equal draft "plain"))
      (should-not (text-properties-at 0 draft)))
    (should-error (agent-chat--prompt-content "\ufffc") :type 'user-error)))

(ert-deftest agent-image-test-initialize-advertises-image-input ()
  (let* ((state (codex-acp--state-create))
         (client (list (cons :codex-acp-state state))) result)
    (cl-letf (((symbol-function 'codex-acp--rpc)
               (lambda (_state _method _params success &rest _) (funcall success nil)))
              ((symbol-function 'codex-acp--write) #'ignore))
      (codex-acp--request :client client :request (acp-make-initialize-request :protocol-version 1)
                         :on-success (lambda (value) (setq result value))))
    (should (eq (map-nested-elt result '(agentCapabilities promptCapabilities image)) t))
    (should-not (codex-acp-agent-capabilities (acp-make-client :command "cat")))))

(ert-deftest agent-image-test-queue-edit-takes-back-attachments-and-delivers ()
  (agent-image-test-with-session
    (insert "first") (agent-chat-submit)
    (agent-chat--insert-image 'image/png (base64-decode-string agent-image-test-png))
    (agent-chat-submit)
    (let ((owner (current-buffer)) (entry (car agent-chat--queue)))
      (agent-chat--queue-edit entry)
      (should (eq owner (current-buffer)))
      (should-not agent-chat--queue)
      (should (equal (get-text-property (point-min) 'agent-chat-image)
                     (agent-image-test-content)))
      (goto-char (point-max)) (insert " explain")
      (agent-chat-submit)
      (should (zerop (buffer-size)))
      (should-not (eq entry (car agent-chat--queue)))
      (insert "independent draft")
      (funcall finish '((stopReason . "end_turn")))
      (agent-chat--queue-send-next)
      (should (equal (mapcar (lambda (part) (map-elt part 'type)) (car sent)) '("image" "text")))
      (should (equal (map-elt (seq-elt (car sent) 0) 'data) agent-image-test-png))
      (should (equal (buffer-string) "independent draft"))
      (should-not agent-chat--queue))))

(ert-deftest agent-image-test-queue-edit-rejects-an-image-only-draft ()
  (agent-image-test-with-session
    (insert "first") (agent-chat-submit)
    (insert "second") (agent-chat-submit)
    (agent-chat--insert-image 'image/png (base64-decode-string agent-image-test-png))
    (let ((draft (agent-image-test-draft)) (entry (car agent-chat--queue)))
      (should-error (agent-chat--queue-edit entry) :type 'user-error)
      (should (equal-including-properties draft (agent-image-test-draft)))
      (should (equal agent-chat--queue (list entry))))))

(ert-deftest agent-image-test-preview-preserves-draft ()
  (skip-unless (display-images-p))
  (agent-image-test-with-session
    (agent-chat--insert-image 'image/png (base64-decode-string agent-image-test-png))
    (let ((draft (agent-image-test-draft)) preview)
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (setq preview buffer))))
        (agent-chat-preview-image))
      (unwind-protect
          (progn
            (should (buffer-live-p preview))
            (with-current-buffer preview
              (should (derived-mode-p 'image-mode))
              (should (equal (buffer-string) (base64-decode-string agent-image-test-png)))))
        (when (buffer-live-p preview) (kill-buffer preview)))
      (should (equal-including-properties draft (agent-image-test-draft))))))

(ert-deftest agent-image-test-reading-preserves-draft ()
  (agent-image-test-with-session
    (agent-chat--insert-image 'image/png (base64-decode-string agent-image-test-png))
    (let ((draft (agent-image-test-draft)))
      (agent-chat--append agent-chat--state 'assistant "History")
      (agent-chat-browse-history)
      (agent-chat-focus-prompt)
      (should (equal-including-properties draft (agent-image-test-draft))))))

(ert-deftest agent-image-test-skill-and-mixed-codex-input ()
  (let* ((image (agent-image-test-content))
         (state (codex-acp--state-create
                 :skills '(((name . "elisp") (mention . "elisp") (source . local) (path . "/skills/elisp")))))
         (input (codex-acp--prompt-input
                 state (list '((type . "text") (text . "/$elisp inspect ")) image
                             '((type . "text") (text . " this image"))))))
    (should (equal (mapcar (lambda (part) (map-elt part 'type)) input) '("text" "image" "text" "skill")))
    (should (equal (map-elt (cadr input) 'url) (concat "data:image/png;base64," agent-image-test-png)))
    (should (equal (map-elt (car input) 'text) "$elisp inspect "))
    (should (stringp (json-serialize (vconcat input))))))

(ert-deftest agent-image-test-remote-skill-worker-never-receives-image ()
  (let* ((state (codex-acp--state-create
                 :skills '(((name . "elisp") (mention . "elisp") (source . remote) (path . "/skills/elisp")))))
         worker-input prepared)
    (cl-letf (((symbol-function 'codex-acp--submit-job)
               (lambda (_state _id _fn args success &rest _)
                 (setq worker-input (car args))
                 (funcall success '(:status ok :value (((type . "text") (text . "Skill instructions")))))))
              ((symbol-function 'codex-acp--start-turn) (lambda (_state input) (setq prepared input))))
      (codex-acp--prepare-prompt state
                               (list '((type . "text") (text . "$elisp inspect")) (agent-image-test-content))
                               #'ignore #'ignore))
    (should (equal (mapcar (lambda (part) (map-elt part 'type)) worker-input) '("remoteSkill")))
    (should (equal (mapcar (lambda (part) (map-elt part 'type)) prepared) '("text" "image" "text")))
    (should (equal (map-elt (cadr prepared) 'url) (concat "data:image/png;base64," agent-image-test-png)))
    (should-not (string-match-p agent-image-test-png
                                (codex-acp--title-input (codex-acp--title-state state))))))

(ert-deftest agent-image-test-codex-history-restores-inline-image ()
  (agent-image-test-with-session
    (let ((state (codex-acp--state-create)) updates)
      (cl-letf (((symbol-function 'codex-acp--update) (lambda (_state update) (push update updates))))
        (codex-acp--replay-item
         state `((type . "userMessage") (id . "saved")
                 (content . [((type . "text") (text . "Look "))
                             ((type . "image") (url . ,(acp-image-to-data-url (agent-image-test-content))))]))))
      (dolist (update (nreverse updates)) (agent-chat--replay-update session update))
      (let* ((history (agent-chat--state-history agent-chat--state))
             (content (agent-chat--prompt-content (plist-get (car history) :text))))
        (should (= (length history) 1))
        (should (equal (mapcar (lambda (part) (map-elt part 'type)) content) '("text" "image")))
        (should (equal (map-elt (cadr content) 'data) agent-image-test-png))))))

(ert-deftest agent-image-test-linux-shift-insert-dispatch ()
  (skip-unless (featurep 'init-keys))
  (agent-image-test-with-session
    (let ((system-type 'gnu/linux))
      (dolist (state '(normal insert))
        (when (bound-and-true-p evil-local-mode) (evil-change-state state))
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'gui-get-selection) (lambda (&rest _) [TARGETS image/png])))
          (should (eq (key-binding (kbd "S-<insert>")) #'agent-chat-paste-image))
          (should (eq (key-binding (kbd "C-c C-y")) #'agent-chat-paste-image)))
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'gui-get-selection) (lambda (&rest _) [TARGETS UTF8_STRING])))
          (should (eq (key-binding (kbd "S-<insert>")) #'yank))))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
                ((symbol-function 'gui-get-selection)
                 (lambda (&rest _) (ert-fail "TTY lookup must not read a graphical clipboard"))))
        (should-not (eq (key-binding (kbd "S-<insert>")) #'agent-chat-paste-image))))))

(ert-deftest agent-image-test-linux-native-paste-and-send ()
  (agent-image-test-with-session
    (let ((system-type 'gnu/linux))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'image-type-available-p) (lambda (_) t))
                ((symbol-function 'gui-get-selection)
                 (lambda (selection target)
                   (should (eq selection 'CLIPBOARD))
                   (pcase target
                     ('TARGETS [TARGETS image/png UTF8_STRING])
                     ('image/png (base64-decode-string agent-image-test-png))
                     (_ (ert-fail "Only the clipboard PNG should be read")))))
                ((symbol-function 'call-process)
                 (lambda (&rest _) (ert-fail "Linux native paste needs no external converter"))))
        (call-interactively (key-binding (kbd "C-c C-y")))))
    (should-not sent)
    (agent-chat-submit)
    (should (equal (append (car sent) nil) (list (agent-image-test-content))))))

(ert-deftest agent-image-test-native-command-v-dispatch ()
  (skip-unless (featurep 'init-keys))
  (agent-image-test-with-session
    (let ((system-type 'darwin))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'gui-get-selection) (lambda (&rest _) [image/png])))
        (should (eq (key-binding (kbd "M-v")) #'agent-chat-paste-image)))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'gui-get-selection) (lambda (&rest _) [UTF8_STRING])))
        (when (bound-and-true-p evil-local-mode)
          (evil-normal-state)
          (should (eq (key-binding (kbd "M-v")) #'evil-paste-after))
          (evil-insert-state)
          (should (eq (key-binding (kbd "M-v")) #'yank)))))))

(provide 'agent-image-test)
;;; agent-image-test.el ends here
