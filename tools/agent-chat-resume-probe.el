;;; agent-chat-resume-probe.el --- Synthetic resume profiling -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Run `agent-chat-resume-probe-run' in a GUI Emacs.  It creates a hidden frame
;; and compares identical synthetic history with previews disabled, cold, and
;; warm.  No Codex connection or real conversation is loaded.  Results contain
;; timings and counts only.  Org's real compiler/converter supplies the images.
;; With EXTENDED non-nil, include commands, macros and an environment formerly
;; rejected by Agent Chat's allowlists, defined using Org's preview preamble.

;;; Code:
(require 'agent-chat)
(require 'cl-lib)

(defvar agent-chat-resume-probe-result nil)
(defvar agent-chat-mode-line-mode)

(defun agent-chat-resume-probe--message (index extended)
  "Build message INDEX, using formerly rejected formulas when EXTENDED."
  (if extended
      (format (concat "Message %d: inline $\\mathchoice{\\AgentChatFixture{x_{%d}}}{T}{S}{SS}$ and display:\n"
                      "\\[\n\\begin{agentchatmatrix}\\boxed{\\operatorname{View}^{\\rm real}_{%d}} & \\approx_c\\\\"
                      "\\AgentChatFixture{Sim}_{%d} & \\mathcal L\\end{agentchatmatrix}\n\\]\nEnd.")
              index index index index)
    (format "Message %d: inline $x_{%d}^{2}$ and display:\n\\[\n\\frac{x_{%d}+1}{1+x_{%d}^{2}}\n\\]\nEnd."
            index index index index)))

(defun agent-chat-resume-probe--case (kind count extended)
  "Replay COUNT synthetic messages under preview cache condition KIND.
EXTENDED selects formulas formerly rejected by Agent Chat's allowlists."
  (let* ((owner (generate-new-buffer " *Agent Chat resume probe*"))
         (agent-chat-render-math (not (eq kind 'disabled)))
         (agent-chat-render-mermaid nil)
         (agent-chat-mode-line-mode nil)
         (agent-chat--math-queue nil)
         (agent-chat--math-running nil)
         (request (symbol-function 'agent-chat--request))
         (redraw (symbol-function 'agent-chat--redraw))
         (publish (symbol-function 'agent-chat--publish))
         (refresh (symbol-function 'agent-chat--math-refresh))
         (complete (symbol-function 'agent-chat--math-complete))
         (preview (symbol-function 'org-latex-preview-place))
         (schedule (symbol-function 'run-at-time))
         (redraw-count 0) (redraw-seconds 0.0) (refresh-count 0) (publications 0)
         (complete-seconds 0.0) (batches 0) (regions 0) (jobs 0)
         timers entries result)
    (unwind-protect
        (progn
          (switch-to-buffer owner)
          (agent-chat-mode)
          (setq default-directory temporary-file-directory)
          (agent-chat--attach (agent-chat--state-create) nil "Resume probe")
          (setq agent-chat--session
                (agent-chat--session-create :buffer owner :cwd default-directory
                                            :capabilities '((loadSession . t))))
          (agent-chat--redraw)
          (insert "Keep this unsent draft")
          (goto-char 6)
          (cl-letf
              (((symbol-function 'agent-chat--request)
                (lambda (session request-form success &rest rest)
                  (if (not (eq owner (agent-chat--session-buffer session)))
                      (apply request session request-form success rest)
                    (dotimes (i count)
                      (agent-chat--notification
                       session `((method . "session/update")
                                 (params . ((sessionId . "probe")
                                            (update . ((sessionUpdate . "agent_message_chunk")
                                                       (messageId . ,(number-to-string i))
                                                       (content . ((type . "text")
                                                                   (text . ,(agent-chat-resume-probe--message i extended)))))))))))
                    (funcall success '((sessionId . "probe"))))))
               ((symbol-function 'agent-chat--publish)
                (lambda ()
                  (when (eq (current-buffer) owner) (cl-incf publications))
                  (funcall publish)))
               ((symbol-function 'agent-chat--redraw)
                (lambda ()
                  (if (not (eq (current-buffer) owner)) (funcall redraw)
                    (let ((start (float-time)))
                      (cl-incf redraw-count)
                      (prog1 (funcall redraw)
                        (cl-incf redraw-seconds (- (float-time) start)))))))
               ((symbol-function 'agent-chat--math-refresh)
                (lambda ()
                  (when (eq (current-buffer) owner) (cl-incf refresh-count))
                  (funcall refresh)))
               ((symbol-function 'agent-chat--math-complete)
                (lambda (&rest args)
                  (let ((start (float-time)))
                    (prog1 (apply complete args)
                      (cl-incf complete-seconds (- (float-time) start))))))
               ((symbol-function 'org-latex-preview-place)
                (lambda (converter fragments &rest args)
                  (cl-incf batches)
                  (cl-incf regions (length fragments))
                  (let ((job (apply preview converter fragments args)))
                    (when job (cl-incf jobs))
                    job)))
               ((symbol-function 'run-at-time)
                (lambda (&rest args)
                  (let ((timer (apply schedule args)))
                    (when (or (memq (nth 2 args) '(agent-chat--math-next agent-chat--math-complete))
                              (and (memq (nth 2 args) '(agent-chat--flush-render agent-chat--spinner-tick))
                                   (eq (nth 3 args) owner)))
                      (push timer timers))
                    timer))))
            (let ((start (float-time)) replay-seconds replay-redraws)
              (agent-chat--load-session agent-chat--session "probe")
              (setq replay-seconds (- (float-time) start)
                    replay-redraws redraw-count)
              (let ((deadline (+ (float-time) 45)))
                (while (and (< (float-time) deadline)
                            (or agent-chat--math-queue agent-chat--math-running))
                  (accept-process-output nil 0.05)))
              (maphash (lambda (_ entry)
                         (when (memq owner (plist-get entry :buffers)) (push entry entries)))
                       agent-chat--math-cache)
              (setq result
                    (list :case kind :messages count :extended extended
                          :total-seconds (- (float-time) start)
                          :replay-seconds replay-seconds :redraws redraw-count
                          :replay-redraws replay-redraws :publications publications
                          :redraw-seconds redraw-seconds :formula-refreshes refresh-count
                          :preview-batches batches :preview-regions regions :preview-jobs jobs
                          :completion-seconds complete-seconds
                          :ready-images (cl-count 'ready entries :key (lambda (entry) (plist-get entry :status)))
                          :failed-images (cl-count 'failed entries :key (lambda (entry) (plist-get entry :status)))
                          :displayed-images
                          (with-current-buffer agent-chat--history-buffer
                            (let ((at (point-min)) (images 0))
                              (while (< at (point-max))
                                (when (and (get-text-property at 'agent-chat-math-source)
                                           (eq (car-safe (get-text-property at 'display)) 'image))
                                  (cl-incf images))
                                (setq at (next-single-property-change
                                          at 'agent-chat-math-source nil (point-max))))
                              images))
                          :unfinished (and (or agent-chat--math-running agent-chat--math-queue) t)
                          :draft-preserved (and (equal (buffer-string) "Keep this unsent draft")
                                                (= (point) 6))
                          :history-count (length (agent-chat--state-history agent-chat--state)))))))
      (mapc #'cancel-timer timers)
      (when (buffer-live-p agent-chat--math-running)
        (when-let* ((process (get-buffer-process agent-chat--math-running)))
          (delete-process process))
        (kill-buffer agent-chat--math-running))
      (when (buffer-live-p owner) (kill-buffer owner)))
    result))

(defun agent-chat-resume-probe-run (&optional count extended)
  "Measure three preview conditions for COUNT messages in a hidden GUI frame.
With EXTENDED non-nil, use formerly rejected formulas and Org definitions."
  (interactive)
  (unless (and (display-graphic-p) (fboundp 'org-latex-preview-place))
    (user-error "Run this probe in GUI Emacs with Org preview"))
  (when (or agent-chat--math-running agent-chat--math-queue)
    (user-error "Wait for existing conversation previews before profiling"))
  (let ((frame (let ((after-make-frame-functions nil))
                 (make-frame '((name . "Agent Chat resume probe") (visibility . nil)
                               (width . 100) (height . 36)))))
        (agent-chat--math-cache (make-hash-table :test #'equal))
        (org-latex-preview-cache (make-temp-file "agent-chat-resume-preview-" t))
        ;; Org's non-persist in-memory index is shared across cache directories.
        ;; Isolate it too, so consecutive runs both compile their cold case.
        (org-latex-preview--table nil)
        (org-latex-preview-process-precompile nil)
        (org-latex-preview-preamble
         (if extended
             (concat org-latex-preview-preamble
                     "\n\\newcommand{\\AgentChatFixture}[1]{\\mathsf{#1}}"
                     "\n\\newenvironment{agentchatmatrix}{\\begin{matrix}}{\\end{matrix}}\n")
           org-latex-preview-preamble))
        (agent-chat-process-groups-expand-by-default nil))
    (setq agent-chat-resume-probe-result nil)
    (unwind-protect
        (save-selected-window
          (with-selected-frame frame
            (dolist (kind '(disabled cold warm))
              (let ((result (agent-chat-resume-probe--case kind (or count 12) extended)))
                (setq agent-chat-resume-probe-result
                      (append agent-chat-resume-probe-result (list result)))))))
      (when (frame-live-p frame) (delete-frame frame t)))
    agent-chat-resume-probe-result))

(provide 'agent-chat-resume-probe)
