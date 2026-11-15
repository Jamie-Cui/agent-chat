;;; agent-chat-package-test.el --- Minimal installation regression -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;; Compile in a fresh Emacs with runtime dependencies and no optional UI packages.
;;; Code:
(require 'ert)
(require 'ert-x)
(require 'cl-lib)

(defconst agent-chat-package-test--root
  (expand-file-name "../" (file-name-directory load-file-name)))

(ert-deftest agent-chat-package-loads-and-compiles-without-optional-integrations ()
  (ert-with-temp-directory output
    (let* ((script (expand-file-name "check.el" output))
           (directories
            (cons agent-chat-package-test--root
                  (mapcar (lambda (library)
                            (file-name-directory
                             (or (locate-library library)
                                 (ert-fail (format "Missing dependency: %s" library)))))
                          '("codex-acp" "acp" "async" "ebox" "ecss")))))
      (with-temp-file script
        (insert ";;; -*- lexical-binding: t; -*-\n")
        (prin1
         `(progn
            (require 'bytecomp)
            (require 'agent-chat)
            (let ((byte-compile-error-on-warn t)
                  (byte-compile-dest-file-function
                   (lambda (file)
                     (expand-file-name (concat (file-name-nondirectory file) "c")
                                       ,output))))
              (dolist (library '("agent-chat" "agent-chat-mode-line"
                                 "agent-chat-popwin" "agent-chat-quota"
                                 "consult-agent-chat" "editor-context"))
                (unless (byte-compile-file (expand-file-name (concat library ".el")
                                                             ,agent-chat-package-test--root))
                  (error "Compilation failed: %s" library))
                (require (intern library))))
            (dolist (feature '(consult perspective popwin org-latex-preview))
              (when (featurep feature) (error "Optional dependency loaded: %s" feature))))
         (current-buffer)))
      (with-temp-buffer
        (let ((status
               (apply #'call-process
                      (expand-file-name invocation-name invocation-directory) nil t nil
                      "-Q" "--batch"
                      (append (cl-mapcan (lambda (directory) (list "-L" directory)) directories)
                              (list "-l" script)))))
          (ert-info ((buffer-string)) (should (zerop status))))))))

(provide 'agent-chat-package-test)
;;; agent-chat-package-test.el ends here
