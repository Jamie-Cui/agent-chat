;;; agent-chat-math-commands-test.el --- Common math previews -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Exercise common LaTeX/AMS constructs through the existing Org preview
;; adapter, including configured macros and environments formerly rejected.

;;; Code:
(require 'ert)
(require 'agent-chat)

(defconst agent-chat-math-commands-test-cases
  '((fractions . "\\genfrac{(}{)}{0pt}{}{n}{k} + \\sideset{_a^b}{_c^d}\\sum_i x_i")
    (accents . "\\pmb{\\alpha} + \\mathring{A} + \\dddot{x} + \\ddddot{x}")
    (text . "\\textup{up} \\quad \\textmd{medium}")
    (legacy-fonts . "{\\rm real} + {\\bf v} + {\\it x} + {\\sf A} + {\\tt T} + {\\cal L}")
    (simulation-view . "\\operatorname{View}^{\\rm real}_b\n\\approx_c\n\\operatorname{Sim}_b(I_b,O_b,\\mathcal L),")
    (greek . "\\varkappa + \\digamma")
    (large-operators . "\\bigotimes_i A_i \\quad \\bigodot_i B_i \\quad \\biguplus_i C_i \\quad \\bigsqcup_i D_i")
    (operators . "\\coth x + \\deg f + \\injlim_n A_n + \\projlim_n B_n + \\varliminf_n x_n + \\varlimsup_n x_n")
    (labelled-arrows . "A \\xrightarrow[f]{g} B \\xleftarrow{h} C \\longleftrightarrow D")
    (arrow-tails . "A \\twoheadleftarrow B \\twoheadrightarrow C \\leftarrowtail D \\rightarrowtail E")
    (harpoons . "a \\leftharpoonup b \\leftharpoondown c \\rightharpoonup d \\rightharpoondown e \\rightleftharpoons f \\leftrightharpoons g")
    (other-arrows . "A \\leadsto B \\rightsquigarrow C \\leftrightsquigarrow D \\multimap E \\quad f \\restriction A")
    (negated-arrows . "A \\nleftarrow B \\nrightarrow C \\nleftrightarrow D \\nLeftarrow E \\nRightarrow F \\nLeftrightarrow G")
    (relations . "a \\doteq b \\triangleq c \\asymp d \\lesssim e \\gtrsim f \\leqslant g \\geqslant h")
    (negated-relations . "a \\nleq b \\ngeq c \\nless d \\ngtr e \\ncong f \\nsim g")
    (subsets . "A \\sqsubset B \\sqsupset C \\nsubseteq D \\nsupseteq E")
    (logic . "A \\vDash B \\Vvdash C \\nvdash D \\nvDash E \\nVdash F \\nVDash G")
    (binary-operators . "a \\ominus b \\uplus c \\sqcap d \\sqcup e \\bigcirc f \\diamond g \\amalg h \\wr i")
    (triangles . "A \\triangleleft B \\triangleright C \\trianglelefteq D \\trianglerighteq E")
    (symbols . "x^{\\prime} + \\imath + \\jmath + \\wp + \\hslash \\quad \\therefore \\because \\square \\blacksquare \\angle \\measuredangle \\checkmark")
    (delimiters . "f\\colon \\lbrace x \\bigm| y \\Bigm| z \\biggm| u \\Biggm| v \\rbrace \\to \\lbrack a,b \\rbrack \\backslash C")
    (spacing . "a\\enskip b\\negthinspace c\\negmedspace d\\negthickspace e")
    (stretch-accents . "\\overleftrightarrow{AB} + \\underleftrightarrow{CD}")
    (atom-classes . "\\mathopen{\\langle} x \\mathclose{\\rangle} + \\mathinner{y}"))
  "Representative LaTeX and AMS formulas compiled through Org preview.")

(defun agent-chat-math-commands-test--wait ()
  "Wait for this test's queued Org previews to finish."
  (let ((deadline (+ (float-time) 30)))
    (while (and (< (float-time) deadline)
                (or agent-chat--math-running agent-chat--math-queue))
      (accept-process-output nil 0.05)))
  (should-not agent-chat--math-running)
  (should-not agent-chat--math-queue))

(ert-deftest agent-chat-math-commands-native-org-baseline-and-fitting ()
  (skip-unless (and (display-graphic-p) (image-type-available-p 'svg)))
  (save-window-excursion
    (let ((buffer (generate-new-buffer " *Agent Chat native math test*")))
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (dolist (height '(16 80))
              (dolist (ascent '(70 98))
                (let* ((source "\\[\nJ_1=\\pi_1^{-1}(J)\n\\]")
                       (image (create-image
                               "<svg xmlns='http://www.w3.org/2000/svg' width='80' height='80'><path d='M0 0h80v80H0Z'/></svg>"
                               'svg t :height height :ascent ascent))
                       (preview (agent-chat--math-image-text source image t (selected-window)))
                       native-metrics)
                  ;; Compare native Org-style overlay display with Agent Chat's
                  ;; display property using the same image and surrounding text.
                  (erase-buffer)
                  (insert "Before\nX\nAfter\n")
                  (let ((overlay (make-overlay 8 9)))
                    (overlay-put overlay 'display image)
                    (goto-char 8) (redisplay t)
                    (setq native-metrics (list (line-pixel-height)
                                               (window-text-pixel-size nil (point-min) (point-max))))
                    (delete-overlay overlay))
                  (erase-buffer)
                  (insert "Before\n" preview "\nAfter\n")
                  (goto-char 8) (redisplay t)
                  (should (equal native-metrics
                                 (list (line-pixel-height)
                                       (window-text-pixel-size nil (point-min) (point-max)))))
                  (should (equal (agent-chat--math-copy-source preview) source)))))
            (let* ((image (create-image
                           "<svg xmlns='http://www.w3.org/2000/svg' width='8000' height='80'><path d='M0 0h8000v80H0Z'/></svg>"
                           'svg t :height 80 :ascent 87))
                   (preview (agent-chat--math-image-text "\\[x\\]" image t (selected-window))))
              (should (<= (string-pixel-width preview (current-buffer))
                          (- (window-body-width nil t) (* 4 (window-font-width)))))
            (should-not (image-property image :max-width))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest agent-chat-math-commands-real-org-compilation ()
  (skip-unless (and (fboundp 'org-latex-preview-place)
                    (executable-find "xelatex") (executable-find "dvisvgm")))
  (let ((agent-chat--math-cache (make-hash-table :test #'equal))
        (agent-chat--math-queue nil)
        (agent-chat--math-running nil)
        ;; A deterministic Org configuration, including custom definitions.
        (org-latex-compiler "xelatex")
        (org-latex-packages-alist '(("" "amsmath" t) ("" "amssymb" t)))
        (org-latex-preview-preamble
         (concat org-latex-preview-preamble
                 "\n\\newcommand{\\AgentChatFixture}[1]{\\mathbb{#1}}"
                 "\n\\newenvironment{agentchatmatrix}{\\begin{matrix}}{\\end{matrix}}\n"))
        (org-latex-preview-process-precompile nil)
        (org-latex-preview-process-default 'dvisvgm)
        (org-latex-preview-cache (make-temp-file "agent-chat-commands-cache-" t))
        (org-latex-preview--table nil)
        (default-directory temporary-file-directory)
        entries timers)
    (cl-letf* ((schedule (symbol-function 'run-at-time))
               ((symbol-function 'run-at-time)
                (lambda (&rest args)
                  (let ((timer (apply schedule args)))
                    (push timer timers) timer))))
      (unwind-protect
          (progn
            (dolist (example
                     (append agent-chat-math-commands-test-cases
                             '((unlisted-command . "\\mathchoice{D}{T}{S}{SS}")
                               (org-macro . "\\AgentChatFixture{F}")
                               (org-environment . "\\begin{agentchatmatrix}a&b\\\\c&d\\end{agentchatmatrix}"))))
              (push (cons (car example) (agent-chat--math-request (cdr example) t)) entries))
            (agent-chat--math-next)
            (agent-chat-math-commands-test--wait)
            (dolist (example entries)
              (let ((entry (cdr example)))
                (ert-info ((format "%s: %s" (car example) (plist-get entry :error)))
                  (should (eq (plist-get entry :status) 'ready))
                  (should (file-exists-p (image-property (plist-get entry :image) :file)))
                  (when (display-graphic-p)
                    (let ((size (image-size (plist-get entry :image) t)))
                      (should (> (car size) 0))
                      (should (> (cdr size) 0)))))))
            ;; Unknown commands now reach Org, whose compilation error must
            ;; leave the source visible even after the one automatic retry.
            (let* ((body "\\AgentChatUndefinedCommand{x}")
                   (source (concat "\\[" body "\\]"))
                   (agent-chat--math-window (selected-window)))
              (dotimes (attempt 2)
                (let ((entry (agent-chat--math-request body t)))
                  (should (= (plist-get entry :attempt) (1+ attempt)))
                  (agent-chat--math-next)
                  (agent-chat-math-commands-test--wait)
                  (should (eq (plist-get entry :status) 'failed))
                  (should (plist-get entry :error))
                  (should-not (plist-get entry :image))))
              (should (equal (agent-chat--math-render source body t) source))
              (should-not agent-chat--math-queue)))
        (mapc #'cancel-timer timers)
        (when (buffer-live-p agent-chat--math-running)
          (kill-buffer agent-chat--math-running))))))

(provide 'agent-chat-math-commands-test)
;;; agent-chat-math-commands-test.el ends here
