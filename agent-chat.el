;;; agent-chat.el --- Standalone ACP agent workspace with Ebox -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Assisted-by: Codex:GPT-6
;; Maintainer: Jamie Cui <jamie.cui@outlook.com>
;; Version: 0.1.0
;; URL: https://github.com/Jamie-Cui/agent-chat
;; Keywords: tools, processes
;; Package-Requires: ((emacs "30.1") (acp "0.17.1") (ebox "3.0.0") (transient "0.11.0") (codex-acp "0.1.0"))
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
;; M-x agent-chat starts a standalone conversation, with Codex or Magent.
;; One editable buffer owns its connection, session model and native draft.
;; Its private read-only message buffer lives in the upper native window.
;; An atomic window group keeps both panes together on close and maximize.
;; Native window navigation, movement, scrolling, selection and undo apply.
;; In messages, C-n/C-p move between message and process-group block starts.
;; TAB toggles headings and collapses the innermost detail from its body.
;; Messages follow output only at the bottom; active selections defer refresh.
;; Normal RET sends; insert RET adds a newline.
;; C-c C-c interrupts, and ? (normal) / C-c ? opens the native transient.
;; C-g returns Evil editing states to normal; another C-g can dismiss popwin.
;; C-c RET selects an advertised setting, including legacy models and modes.
;; Choosing a model then offers its advertised reasoning effort, when available.
;; /permissions selects a permission mode in a Transient popup; C-c C-p or
;; /approvals opens pending approvals.  New approvals open automatically only
;; while their Agent Chat window is selected, the editor is idle, and no other menu or
;; minibuffer is active.  Requests appear in arrival order within each session;
;; q or switching away defers without deciding.  C-c C-p reopens deferred ones.
;; Approvals show directory/command details above Respond, Queue and Inspect
;; columns: y allows once, a allows longer when offered, d rejects, p/N move
;; between requests, v opens full details, and x explicitly cancels a request.
;; The header shows the advertised permission mode and pending approval count.
;; Pending approvals, including directory grants, share one C-c C-p reminder
;; below the messages.  Responses and details live in the approval popup.
;; Granted approvals join the collapsible process summary; rejected and
;; cancelled requests remain visible outside it.  Full history is retained.
;; ! in an approval or ? menu toggles session bypass, after confirmation when
;; enabling.  Bypass uses only advertised allow_once options; ? ! restores
;; prompts.  A new connection starts with bypass off.
;; Emacs Codex file tools ask for extra directory access in the same approval
;; popup: y allows for this turn, a until reconnect, d rejects.  Bypass selects
;; the turn-scoped option.  Approval retries the paused operation in the same turn;
;; the conversation and draft stay intact.  Each grant expires at its chosen scope.
;; While a turn runs, submitted prompts queue in FIFO order.  Use ? Queue
;; to edit or withdraw pending prompts.  Steer adds a queued prompt to the
;; current Emacs Codex turn without interrupting it.  Interruptions and failures
;; pause the queue; reconnecting never automatically replays pending prompts.
;; Use the queue panel's Continue button or ? R to continue automatic delivery.
;; In the queue list, d withdraws, e edits, s steers, r resumes and c clears.
;; Edit takes a pending prompt back into the empty main draft, including images.
;; Send or clear an existing draft first.  Other queued prompts keep flowing;
;; resubmitting the edited draft appends it to the queue as a new prompt.
;; Pending prompts live only in memory; closing or exiting asks to discard.
;; C-c C-y pastes a clipboard image into the draft using native `yank-media'.
;; On macOS, TIFF-only clipboard images are converted with `pngpaste'.
;; Images appear as single editable attachment items; C-c C-v previews one.
;; Delete/undo works normally.  Drafts and queued prompts own their image data
;; in memory, including for TRAMP sessions; pasting never sends a message.
;; Type / for local and agent commands, or $ for advertised skills; TAB
;; completes with descriptions.  /skill inserts a skill using a picker.
;; A leading $skill is sent as /$skill, the ACP skill-command spelling;
;; inline mentions and existing /$skill commands are sent unchanged.
;; Agent commands take precedence over local commands with the same name.
;; Markdown links show their labels; click or RET visits a file/line or URL.
;; Embark, when installed, recognizes their destinations rather than labels.
;; Invoking from a project file reuses its conversation and appends the current
;; line/selection as context.  C-u or `agent-chat-new' starts a fresh session.
;; C-c C-n starts a fresh session with the current agent in the same workspace;
;; C-u C-c C-n lets you choose another agent.
;; C-c C-r renames the session title in either pane, preserving the draft.
;; Outside a project, reuse a general conversation on the same host.  Its
;; initial directory remains the command cwd; file references are absolute.
;; Remote Codex projects use codex-acp.el: Codex runs locally and Emacs runs
;; approved workspace tools through TRAMP.  No remote codex-acp is needed.
;; M-x revert-buffer rebuilds the view and retries failed formula previews,
;; retaining the connection, conversation, draft and undo history.
;; Offline ERT suites live in the source checkout's test/ directory.
;; (agent-chat-setup) installs bindings without starting an agent.
;; Starting Codex interactively lists saved sessions alongside New session.
;; The source window stays visible until a session is chosen; cancelling a
;; new picker releases its hidden buffer and connection.  Local history also
;; includes native Codex App/CLI sessions from the same project.  Those use
;; the configured external codex-acp command to retain their native tools.
;; Use `agent-chat-open-history' to reopen the picker when the agent
;; advertises session/list and session/load.  Client-side filesystem,
;; terminal requests remain outside this frontend.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'font-lock)
(require 'browse-url)
(require 'url-util)
(require 'treesit)
(require 'transient)
(require 'tabulated-list)
(require 'acp)
(require 'agent-chat-quota)
(require 'project)
(require 'dired)
(require 'editor-context)
(require 'tramp)
(require 'image)
(require 'yank-media)
(require 'acp-image)
(require 'ebox)
(require 'org-latex-preview nil t)

(declare-function org-latex-preview-place "org-latex-preview"
                  (processing-type entries &optional numbering-offsets latex-preamble))
(declare-function org-latex-preview-clear-cache "org-latex-preview"
                  (&optional beg end clear-entire-cache))
(defvar org-inhibit-startup)
(defvar org-latex-preview-process-default)
(defvar org-latex-preview-process-finish-functions)
(defvar org-latex-preview-preamble)
(defvar org-latex-preview-compiler-command-map)

(declare-function evil-local-mode "evil" (&optional arg))
(declare-function evil-normal-state "evil" (&optional arg))
(declare-function evil-force-normal-state "evil-commands" ())
(declare-function evil-normalize-keymaps "evil-core" (&optional state))
(declare-function evil-change-state "evil" (state &optional message))
(declare-function evil-define-key* "evil" (state keymap key def &rest bindings))
(declare-function ert-select-tests "ert" (selector universe))
(declare-function ert-delete-test "ert" (test-name))
(declare-function ert-test-name "ert" (test))
(declare-function ert-stats-completed-expected "ert" (stats))
(declare-function ert-stats-completed-unexpected "ert" (stats))
(declare-function ert-stats-skipped "ert" (stats))
(declare-function ert-test-most-recent-result "ert" (test))
(declare-function ert-test-failed-p "ert" (result))
(declare-function ert-test-result-with-condition-condition "ert" (result))
(declare-function ns-insert-working-text "ns-win" ())
(declare-function ns-delete-working-text "ns-win" ())
(declare-function codex-acp-agent-capabilities "codex-acp" (client))
(defvar ns-working-overlay)
(defvar ns-working-text)

;;;; Request state and formatting

(defgroup agent-chat nil
  "Standalone ACP sessions with an Agent Chat frontend."
  :group 'applications)

(defcustom agent-chat-render-math t
  "Preview complete LaTeX formulas using Org's asynchronous preview engine.
Supports \\(…\\), \\[…\\], $…$ and $$…$$ outside code.  Compilation is
asynchronous and cached by `org-latex-preview' (Org 10 or newer), using its
compiler, preamble, packages and appearance settings.  Missing support,
incomplete or unsupported formulas leave the original source visible.
Commands, environments and macros follow Org's configuration without a
separate allowlist.  Formula source is trusted as in an Org document;
compilation disables shell escape.  No packages are installed automatically."
  :type 'boolean
  :group 'agent-chat)

(defcustom agent-chat-render-mermaid t
  "Preview complete Mermaid fences using the locally installed mmdc.
Render asynchronously and cache images.  Missing tools or rendering failures
leave code visible; `revert-buffer' retries failures.  Copying a preview
restores its fenced source.  No packages are installed automatically."
  :type 'boolean
  :group 'agent-chat)

(defcustom agent-chat-process-groups-expand-by-default nil
  "Whether consecutive tool and thought entries start expanded as a group.
When nil, show one summary with counts for the whole group.  When non-nil,
show each entry's summary; its detailed content still expands separately.
Clicking a group overrides this default for that group in the current buffer.
Changes apply to untouched groups in existing panes on the next redraw.
Expand a group or entry before selecting its detailed text for copying."
  :type 'boolean
  :group 'agent-chat)

(defcustom agent-chat-stream-refresh-interval 0.1
  "Minimum pause in seconds between streamed history renders.
Long renders allow twice their own duration for input before the next one.
Zero disables batching.  Completion, errors and permissions refresh at once."
  :type 'number
  :group 'agent-chat)

(defface agent-chat-header '((t :inherit shadow :weight normal :slant normal))
  "Quiet session metadata in the fixed header." :group 'agent-chat)
(defconst agent-chat--user-face-spec
  '((((class color) (background dark))
     :inherit font-lock-keyword-face :background "#1b2433" :weight bold :extend t)
    (((class color) (background light))
     :inherit font-lock-keyword-face :background "#eaf0f6" :weight bold :extend t)
    (t :inherit font-lock-keyword-face :weight bold :extend t)))
(defface agent-chat-user agent-chat--user-face-spec
  "Subdued blue message blocks, distinct from the native region selection.
The background covers references and blank lines; Visual uses `region'."
  :group 'agent-chat)
(defface agent-chat-muted '((t :inherit shadow))
  "Process summaries and secondary information." :group 'agent-chat)
(defface agent-chat-error '((t :inherit error))
  "Failed requests and tools." :group 'agent-chat)
(defface agent-chat-queue-panel
  '((((class color) (background dark)) :inherit default :background "#172b35")
    (((class color) (background light)) :inherit default :background "#eaf4fa")
    (t :inherit default))
  "Background of the pending prompt panel." :group 'agent-chat)
(defface agent-chat-queue-title '((t :inherit font-lock-keyword-face :weight bold))
  "Queue heading and row numbers." :group 'agent-chat)
(defface agent-chat-queue-control '((t :inherit link :weight bold))
  "Clickable actions on pending prompts." :group 'agent-chat)
(defconst agent-chat--code-face-spec
  '((((class color) (background dark)) :inherit default :background "#181818" :weight normal)
    (((class color) (background light)) :inherit default :background "#f1f1f1" :weight normal)
    (t :inherit default :weight normal)))
(defface agent-chat-code agent-chat--code-face-spec
  "Code background using the current buffer's font and native syntax faces."
  :group 'agent-chat)

(defcustom agent-chat-code-language-modes
  '(("go" go-ts-mode go-mode) ("golang" go-ts-mode go-mode)
    ("sh" sh-mode) ("shell" sh-mode) ("bash" bash-ts-mode sh-mode) ("zsh" sh-mode)
    ("python" python-ts-mode python-mode) ("py" python-ts-mode python-mode)
    ("elisp" emacs-lisp-mode) ("emacs-lisp" emacs-lisp-mode) ("lisp" lisp-mode)
    ("c" c-ts-mode c-mode) ("cpp" c++-ts-mode c++-mode) ("c++" c++-ts-mode c++-mode)
    ("rust" rust-ts-mode rust-mode) ("ruby" ruby-ts-mode ruby-mode)
    ("js" js-ts-mode js-mode) ("javascript" js-ts-mode js-mode)
    ("ts" typescript-ts-mode typescript-mode) ("typescript" typescript-ts-mode typescript-mode)
    ("tsx" tsx-ts-mode) ("json" json-ts-mode js-json-mode)
    ("yaml" yaml-ts-mode yaml-mode) ("yml" yaml-ts-mode yaml-mode)
    ("toml" toml-ts-mode) ("html" mhtml-mode) ("css" css-ts-mode css-mode)
    ("sql" sql-mode) ("diff" diff-mode) ("makefile" makefile-mode))
  "Installed major modes to try for each fenced code language.
Only explicitly mapped languages invoke a mode.  Tree-sitter candidates
are used only when their grammar is already available; nothing is installed."
  :type '(alist :key-type string :value-type (repeat function))
  :group 'agent-chat)

(defvar-local agent-chat--code-cache nil
  "Bounded cache of native syntax faces for unchanged code blocks.")

(defun agent-chat--code-mode (language)
  "Resolve LANGUAGE to an installed native mode, without prompting."
  (let ((name (downcase (or (car (split-string (or language "") "[ \t]+" t)) ""))))
    (seq-find
     (lambda (mode)
       (and (fboundp mode)
            (or (not (string-suffix-p "-ts-mode" (symbol-name mode)))
                (and (fboundp 'treesit-language-available-p)
                     (treesit-language-available-p
                      (or (cdr (assq mode '((c++-ts-mode . cpp) (js-ts-mode . javascript))))
                          (intern (string-remove-suffix "-ts-mode" (symbol-name mode)))))))))
     (cdr (assoc name agent-chat-code-language-modes)))))

(defun agent-chat--go-fragment-start (code)
  "Give native Go parsing enough context for CODE; return its buffer start.
Try declarations, statements, composite fields and type fields.  The parser
selects the first valid context; the synthetic text is never displayed."
  (let ((contexts '(("package agentchat\n" . "")
                    ("package agentchat\nfunc snippet() {\n" . "\n}\n")
                    ("package agentchat\nvar _ = T{\n" . "\n}\n")
                    ("package agentchat\ntype T struct {\n" . "\n}\n")))
        (start (point-min)))
    (while (and contexts (treesit-node-check (treesit-buffer-root-node 'go) 'has-error))
      (let ((context (pop contexts)))
        (erase-buffer)
        (insert (car context) code (cdr context))
        (setq start (1+ (length (car context))))))
    (when (treesit-node-check (treesit-buffer-root-node 'go) 'has-error)
      (erase-buffer) (insert code) (setq start (point-min)))
    ;; An unfinished streaming fragment is not a diagnostic for the user.
    (treesit-font-lock-recompute-features nil '(error))
    start))

(defun agent-chat--highlight-code (code language)
  "Return CODE with only native syntax faces for LANGUAGE.
Fontify whole blocks, including unfinished streaming blocks, so multi-line
strings and comments retain their context.  Major-mode hooks are suppressed:
this is a display buffer, not a file visit or an editable language session."
  (let* ((mode (agent-chat--code-mode language))
         (key (list mode code)))
    (unless agent-chat--code-cache
      (setq agent-chat--code-cache (make-hash-table :test #'equal)))
    (or (gethash key agent-chat--code-cache)
        (let ((result (substring-no-properties code)))
          ;; Bound synchronous fontification work for unusually large output.
          (when (and mode (<= (length code) 100000))
            (condition-case nil
                (with-temp-buffer
                  (let ((default-directory temporary-file-directory)
                        (inhibit-message t))
                    (insert code)
                    (delay-mode-hooks
                      (funcall mode)
                      (let* ((start (if (eq mode 'go-ts-mode)
                                        (agent-chat--go-fragment-start code) (point-min)))
                             (end (+ start (length code)))
                             (position start))
                        (font-lock-ensure (point-min) (point-max))
                        (while (< position end)
                          (let ((next (next-property-change position nil end))
                                (face (or (get-text-property position 'face)
                                          (get-text-property position 'font-lock-face))))
                            (when face
                              (put-text-property (- position start) (- next start) 'face face result))
                            (setq position next)))))))
              (error nil)))
          ;; Copy only faces: never copy mode keymaps, fields or display hooks.
          (add-face-text-property 0 (length result) 'agent-chat-code t result)
          (when (>= (hash-table-count agent-chat--code-cache) 64)
            (clrhash agent-chat--code-cache))
          (puthash key result agent-chat--code-cache)))))

(defun agent-chat--code-lines (lines language)
  "Render code LINES with LANGUAGE's native faces and an even background."
  (when lines
    (let* ((highlighted (split-string (agent-chat--highlight-code
                                       (string-join lines "\n") language) "\n" nil))
           (width (min (max 1 (- (window-body-width) 5))
                       (apply #'max (mapcar #'string-width lines)))))
      (mapcar (lambda (line)
                (concat (propertize "│ " 'face 'shadow
                                    'agent-chat-code-chrome t) line
                        (propertize (make-string (max 0 (- width (string-width line))) ?\s)
                                    'face 'agent-chat-code
                                    'agent-chat-code-chrome t)))
              highlighted))))

(defvar-local agent-chat--expanded nil
  "Indices of process entries expanded in this frontend.")

(defvar-local agent-chat--process-group-overrides nil
  "Alist of group starting indices and their explicit expansion states.
An absent index follows `agent-chat-process-groups-expand-by-default'.")

(cl-defstruct (agent-chat--state
               (:constructor agent-chat--state-create))
  history (status 'idle) token cancel changed)

(defvar-local agent-chat--state nil)
(defvar-local agent-chat--adapter nil)
(defvar-local agent-chat--label nil)

(defconst agent-chat--spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Dot spinner frames, matching the global activity indicator.")
(defvar-local agent-chat--spinner-index 0)
(defvar-local agent-chat--spinner-timer nil)

(defvar-local agent-chat--queue nil
  "FIFO of pending prompts, separate from the sent conversation history.")
(defvar-local agent-chat--queue-paused nil
  "Pause reason, or nil when pending prompts may be sent automatically.")
(defvar-local agent-chat--queue-timer nil)
(defvar-local agent-chat--queue-view nil)
(defvar-local agent-chat--queue-owner nil)
(defvar agent-chat--queue-confirming nil
  "Non-nil while confirming whether pending prompts should be discarded.")

(defun agent-chat--state-publish (state)
  "Notify the presentation owned by STATE."
  (when-let* ((changed (agent-chat--state-changed state)))
    (funcall changed)))

(defun agent-chat--append (state role text)
  "Append a ROLE entry containing TEXT to STATE."
  (setf (agent-chat--state-history state)
        (append (agent-chat--state-history state)
                (list (list :role role :text (substring-no-properties text))))))

(defun agent-chat--event (state token event)
  "Apply EVENT only while TOKEN identifies STATE's active request."
  (when (and (eq token (agent-chat--state-token state))
             (eq (agent-chat--state-status state) 'running))
    (let ((type (plist-get event :type))
          (text (plist-get event :text)))
      (pcase type
        ((or 'delta 'reasoning 'tool 'error)
         (unless (stringp text)
           (error "Frontend event %s requires string :text" type)))
        ('done nil)
        (_ (error "Unknown frontend event: %S" type)))
      (pcase type
        ((or 'delta 'reasoning)
         (let ((last (car (last (agent-chat--state-history state))))
               (role (if (eq type 'reasoning) 'reasoning 'assistant)))
           (if (and (eq (plist-get last :role) role)
                    (equal (plist-get last :message-id) (plist-get event :message-id)))
               (setf (plist-get last :text)
                     (concat (plist-get last :text)
                             (substring-no-properties text)))
             (agent-chat--append state role text)
             (when (plist-get event :message-id)
               (setf (plist-get (car (last (agent-chat--state-history state))) :message-id)
                     (plist-get event :message-id))))))
        ('tool
         (if-let* ((id (plist-get event :id)))
             (let ((entry (seq-find
                           (lambda (item) (and (equal (plist-get item :tool-id) id)
                                              (eq (plist-get item :turn) token)))
                           (agent-chat--state-history state))))
               (unless entry
                 (setq entry (list :role 'tool :text "" :tool-id id :turn token
                                   :tool-status nil :detail nil))
                 (setf (agent-chat--state-history state)
                       (append (agent-chat--state-history state) (list entry))))
               (setf (plist-get entry :text) (substring-no-properties text)
                     (plist-get entry :tool-id) id
                     (plist-get entry :turn) token
                     (plist-get entry :tool-status) (plist-get event :status)
                     (plist-get entry :detail) (plist-get event :detail)))
           (agent-chat--append state 'tool text)))
        ('error (agent-chat--append state 'error text)))
      (when (memq type '(done error))
        (setf (agent-chat--state-status state)
              (if (eq type 'done) 'idle 'failed)
              (agent-chat--state-token state) nil
              (agent-chat--state-cancel state) nil))
      (agent-chat--state-publish state))))

(defun agent-chat--submit (state adapter prompt directory)
  "Submit PROMPT in DIRECTORY through ADAPTER, updating STATE."
  (unless (functionp adapter)
    (user-error "No ACP session connected"))
  (when (eq (agent-chat--state-status state) 'running)
    (user-error "Cancel the active request before sending another"))
  (when (string-empty-p (string-trim prompt))
    (user-error "Prompt is empty"))
  (let ((token (make-symbol "request"))
        (display (agent-chat--image-copy prompt))
        (history (copy-tree (agent-chat--state-history state))))
    (agent-chat--append state 'user prompt)
    (setf (plist-get (car (last (agent-chat--state-history state))) :text)
          display)
    (setf (agent-chat--state-token state) token
          (agent-chat--state-status state) 'running)
    (agent-chat--state-publish state)
    (condition-case err
        (let ((cancel
               (funcall adapter
                        (list :prompt prompt :history history
                              :directory directory)
                        (lambda (event)
                          (agent-chat--event state token event)))))
          ;; A synchronous adapter may have finished before returning.
          (when (eq token (agent-chat--state-token state))
            (unless (functionp cancel)
              (error "Adapter must return a cancellation function"))
            (setf (agent-chat--state-cancel state) cancel)))
      (error
       (agent-chat--event
        state token (list :type 'error :text (error-message-string err)))))))

(defun agent-chat--cancel (state)
  "Invalidate STATE's active request before invoking its cancellation hook."
  (when (eq (agent-chat--state-status state) 'running)
    (let ((cancel (agent-chat--state-cancel state)))
      (setf (agent-chat--state-token state) nil
            (agent-chat--state-cancel state) nil
            (agent-chat--state-status state) 'cancelled)
      (unwind-protect
          (when cancel (funcall cancel))
        (agent-chat--state-publish state)))))

;;;; Org LaTeX preview adapter

(defvar agent-chat--math-cache (make-hash-table :test #'equal))
(defvar agent-chat--math-queue nil)
(defvar agent-chat--math-running nil)
(defvar agent-chat--math-window nil)
(defvar-local agent-chat--math-generation 0)

(defun agent-chat--math-escaped-p (text at)
  "Whether the delimiter at AT in TEXT is preceded by an odd escape run."
  (let ((slashes 0))
    (while (and (> at 0) (= (aref text (1- at)) ?\\))
      (cl-incf slashes) (cl-decf at))
    (= (% slashes 2) 1)))

(defun agent-chat--math-fragment (text at)
  "Parse a complete formula at AT in TEXT as (END BODY DISPLAY-P)."
  (save-match-data
    (let* ((tail (substring text at))
           (open (cond ((string-prefix-p "\\(" tail) "\\(")
                       ((string-prefix-p "\\[" tail) "\\[")
                       ((string-prefix-p "$$" tail) "$$")
                       ((string-prefix-p "$" tail) "$")))
           (close (pcase open ("\\(" "\\)") ("\\[" "\\]") (_ open)))
           (start (+ at (length open))) (scan start) end)
      (when (and open (not (agent-chat--math-escaped-p text at))
                 (or (not (equal open "$"))
                     (and (< start (length text))
                          (not (memq (aref text start) '(?\s ?\t ?\n))))))
        (while (and (not end) (string-match (regexp-quote close) text scan))
          (let ((pos (match-beginning 0)) (after (match-end 0)))
            (setq scan after)
            (when (and (> pos start)
                       (not (agent-chat--math-escaped-p text pos))
                       (or (not (equal open "$"))
                           (and (not (memq (aref text (1- pos)) '(?\s ?\t ?\n ?$)))
                                (or (= after (length text))
                                    (not (string-match-p "[0-9$]"
                                                         (substring text after (1+ after))))))))
              (setq end (list after (substring text start pos)
                              (member open '("\\[" "$$")))))))
        end))))

(defconst agent-chat--math-settings-variables
  '(org-latex-compiler org-latex-default-packages-alist org-latex-packages-alist
    org-latex-preview-preamble org-latex-preview-appearance-options
    org-latex-preview-process-default org-latex-preview-process-alist
    org-latex-preview-compiler-command-map org-latex-preview-process-precompile
    org-latex-preview-numbered)
  "Org settings shared by conversation previews and their cache keys.")

(defvar-local agent-chat--math-entries nil
  "Entries owned by a private Org preview buffer.")

(defun agent-chat--math-settings ()
  "Snapshot the current Org preview configuration, when supported."
  (when (fboundp 'org-latex-preview-place)
    (mapcar (lambda (symbol) (cons symbol (copy-tree (symbol-value symbol))))
            agent-chat--math-settings-variables)))

(defun agent-chat--math-key (body block)
  "Identify BODY in BLOCK style with the current Org preview settings."
  (list body (and block t) (agent-chat--math-settings)))

(defun agent-chat--math-finish (entry image &optional error)
  "Finish ENTRY with Org's IMAGE spec, or retain source and ERROR on failure.
Return its subscribers when changed; the batch owner refreshes them together."
  (unless (memq (plist-get entry :status) '(ready failed))
    (setf (plist-get entry :status) (if image 'ready 'failed)
          (plist-get entry :image) image
          (plist-get entry :error) error)
    (plist-get entry :buffers)))

(defun agent-chat--math-complete (buffer &optional error)
  "Collect Org previews from private BUFFER and release this batch.
Run after Org's callbacks so cached images and failed fragments are final.
Refresh each subscriber once, after all images in this batch are available."
  (let (subscribers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (dolist (entry agent-chat--math-entries)
          (let* ((overlay (and (plist-get entry :begin)
                               (seq-find
                                (lambda (ov)
                                  (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay))
                                (overlays-at (plist-get entry :begin)))))
                 (image (and overlay (overlay-get overlay 'org-preview-image)))
                 (failure (or error (and overlay (overlay-get overlay 'help-echo)))))
            (dolist (subscriber
                     (agent-chat--math-finish
                      entry (and (not failure) (copy-tree image))
                      (or failure (unless image "Org did not produce a preview"))))
              (cl-pushnew subscriber subscribers))
            (when (and (not error) (eq (plist-get entry :status) 'failed))
              ;; Org can cache an error on the fragment before a malformed one.
              ;; Evict only this failed region so the isolated retry recompiles.
              (let ((inhibit-message t) (message-log-max nil))
                (org-latex-preview-clear-cache
                 (plist-get entry :begin) (plist-get entry :end))))))
        (kill-buffer buffer)))
    (when (eq buffer agent-chat--math-running)
      (setq agent-chat--math-running nil)
      (run-at-time 0 nil #'agent-chat--math-next))
    (dolist (subscriber subscribers)
      (when (buffer-live-p subscriber)
        (with-current-buffer subscriber (agent-chat--math-refresh))))))

(defun agent-chat--math-org-finished (_exit-code _process-buffer info)
  "Collect only conversation previews after Org finishes processing INFO."
  (let ((buffer (plist-get info :org-buffer)))
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'agent-chat--math-entries buffer))
      ;; Org invokes this hook before its own final validation and cleanup.
      (run-at-time 0 nil #'agent-chat--math-complete buffer))))

(with-eval-after-load 'org-latex-preview
  (add-hook 'org-latex-preview-process-finish-functions
            #'agent-chat--math-org-finished))

(defun agent-chat--math-next ()
  "Batch queued formulas through Org's public asynchronous preview interface."
  (unless agent-chat--math-running
    (when-let* ((first (car agent-chat--math-queue)))
      (let* ((settings (plist-get first :settings))
             ;; Retry failures individually: one malformed formula must not
             ;; prevent its neighbours from recovering on the next redraw.
             (entries (if (> (plist-get first :attempt) 1) (list first)
                        (seq-filter
                         (lambda (entry)
                           (and (= (plist-get entry :attempt) 1)
                                (equal settings (plist-get entry :settings))))
                         agent-chat--math-queue)))
             (buffer (generate-new-buffer " *Agent Chat Org previews*")))
        (setq agent-chat--math-running buffer
              agent-chat--math-queue (seq-difference agent-chat--math-queue entries #'eq))
        (with-current-buffer buffer
          (let ((org-inhibit-startup t)) (delay-mode-hooks (org-mode)))
          (setq default-directory (file-name-as-directory temporary-file-directory)
                agent-chat--math-entries entries)
          (dolist (setting settings)
            (set (make-local-variable (car setting)) (copy-tree (cdr setting))))
          ;; Keep Org's preamble and engine while retaining the automatic
          ;; preview boundary: no shell escape, missing glyphs are failures.
          (setq-local org-latex-preview-preamble
                      (concat org-latex-preview-preamble "\n\\tracinglostchars=3\n"))
          (setq-local org-latex-preview-compiler-command-map
                      (mapcar (lambda (pair) (cons (car pair)
                                                   (concat (cdr pair) " -no-shell-escape")))
                              org-latex-preview-compiler-command-map))
          (condition-case err
              (let (regions)
                (dolist (entry entries)
                  (setf (plist-get entry :status) 'running
                        (plist-get entry :begin) (point))
                  ;; Block delimiters already contribute boundary newlines.
                  ;; Keep one before closing to terminate trailing comments,
                  ;; without introducing a paragraph inside TeX math mode.
                  (insert (if (plist-get entry :block) "\\[" "\\(")
                          (string-trim (plist-get entry :body))
                          (if (plist-get entry :block) "\n\\]" "\n\\)"))
                  (setf (plist-get entry :end) (point))
                  (push (list (plist-get entry :begin) (point)) regions)
                  (insert "\n\n"))
                ;; A nil result means every image was already in Org's cache.
                (unless (org-latex-preview-place
                         org-latex-preview-process-default (nreverse regions))
                  (run-at-time 0 nil #'agent-chat--math-complete buffer)))
            (error
             (agent-chat--math-complete buffer (error-message-string err)))))))))

(defun agent-chat--math-request (body block)
  "Find or enqueue BODY in BLOCK style using the current Org settings.
Pass formulas shorter than 12000 characters directly to Org preview.
Retry failed previews once; `revert-buffer' permits another attempt."
  (let* ((key (agent-chat--math-key body block))
         (settings (nth 2 key))
         (entry (gethash key agent-chat--math-cache)))
    (when (or (not entry)
              (and (eq (plist-get entry :status) 'failed)
                   (< (plist-get entry :attempt) 2)))
      (when (and settings (< (length body) 12000))
        (setq entry (list :body body :block block :settings settings :status 'queued
                          :attempt (if entry (1+ (plist-get entry :attempt)) 1)
                          :buffers (copy-sequence (plist-get entry :buffers))
                          :begin nil :end nil :image nil :error nil))
        (puthash key entry agent-chat--math-cache)
        (setq agent-chat--math-queue (nconc agent-chat--math-queue (list entry)))
        (run-at-time 0 nil #'agent-chat--math-next)))
    (when entry (cl-pushnew (current-buffer) (plist-get entry :buffers)))
    entry))

(defun agent-chat--math-image-text (source spec block window)
  "Display SOURCE with Org's original image SPEC in WINDOW.
Keep Org's baseline and font-relative height for native redisplay.  A BLOCK
occupies one logical line, regardless of its source or image height."
  (let ((image (copy-tree spec)))
    ;; Native image fitting preserves Org's dimensions unless the formula is
    ;; wider than this view.  Never recenter or slice a LaTeX preview.
    (setf (image-property image :max-width)
          (max 1 (- (window-body-width window t) (* 4 (window-font-width window)))))
    (propertize (if block (replace-regexp-in-string "\n" " " source t t) source)
                'display image 'help-echo source
                'agent-chat-math-source (list source))))

(defun agent-chat--math-render (source body block)
  "Preview SOURCE containing BODY in BLOCK style in the bound GUI window."
  (or (and agent-chat-render-math
           (window-live-p agent-chat--math-window)
           (display-graphic-p (window-frame agent-chat--math-window))
           (fboundp 'org-latex-preview-place)
           (let ((entry (agent-chat--math-request body block)))
             (when (eq (plist-get entry :status) 'ready)
               (condition-case err
                   (agent-chat--math-image-text
                    source (plist-get entry :image) block agent-chat--math-window)
                 (error
                  (setf (plist-get entry :status) 'failed
                        (plist-get entry :error) (error-message-string err))
                  nil)))))
      source))

;;;; Source copying for rendered content

(defun agent-chat--math-copy-source (text)
  "Restore formula and diagram source in copied TEXT once per preview.
Also remove renderer-owned code borders and padding.
Native Emacs and Evil copying share this filter.  Other text is unchanged."
  (let ((pos 0) parts)
    (while (< pos (length text))
      (let ((next (next-single-property-change
                   pos 'agent-chat-code-chrome text (length text))))
        (unless (get-text-property pos 'agent-chat-code-chrome text)
          (push (substring text pos next) parts))
        (setq pos next)))
    (setq text (apply #'concat (nreverse parts))))
  (let ((pos 0) (start 0) previous parts)
    (while (< pos (length text))
      (let* ((reference (get-text-property pos 'agent-chat-math-source text))
             (next (next-single-property-change
                    pos 'agent-chat-math-source text (length text))))
        (when reference
          (let ((gap (substring text start pos)))
            (unless (and (eq reference previous) (string-blank-p gap))
              (push gap parts))
            (unless (eq reference previous)
              (push (substring-no-properties (car reference)) parts)))
          (setq previous reference start next))
        (setq pos next)))
    (if (not parts) text
      (push (substring text start) parts)
      (apply #'concat (nreverse parts)))))

(defun agent-chat--install-copy-filter ()
  "Preserve existing copy filters while restoring rendered source on copying."
  (add-function :filter-return (local 'filter-buffer-substring-function)
                #'agent-chat--math-copy-source))

;;;; Local, asynchronous Mermaid previews

(defvar agent-chat--mermaid-cache (make-hash-table :test #'equal))
(defvar agent-chat--mermaid-queue nil)
(defvar agent-chat--mermaid-running nil)

(defun agent-chat--mermaid-safe-p (body)
  "Accept bounded diagram BODY without embedded configuration or resources."
  (let ((case-fold-search t))
    (and (< (length body) 12000)
         (not (string-match-p
               (concat "\\`[ \t\n]*---\\|%%[ \t]*{\\|<[!a-z/]"
                       "\\|[a-z][a-z0-9+.-]*:[/\\\\]\\|//"
                       "\\|\\_<\\(?:url\\|img\\|image\\|href\\|src\\)[ \t]*[:(=]"
                       "\\|[\0-\10\13-\37]") body)))))

(defun agent-chat--mermaid-finish (entry png &optional failure)
  "Finish ENTRY with PNG or FAILURE and release its local resources."
  (unless (memq (plist-get entry :status) '(ready failed))
    (setf (plist-get entry :status) (if png 'ready 'failed)
          (plist-get entry :png) png
          (plist-get entry :error) failure)
    (when-let* ((timer (plist-get entry :timer))) (cancel-timer timer))
    (when-let* ((process (plist-get entry :process)))
      (when (process-live-p process) (delete-process process)))
    (when-let* ((directory (plist-get entry :directory)))
      (condition-case err
          (delete-directory directory t)
        (file-error (message "Mermaid cleanup: %s" (error-message-string err)))))
    (when (eq agent-chat--mermaid-running entry)
      (setq agent-chat--mermaid-running nil))
    (dolist (buffer (plist-get entry :buffers))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (agent-chat--math-refresh))))
    (run-at-time 0 nil #'agent-chat--mermaid-next)))

(defun agent-chat--mermaid-next ()
  "Start one queued Mermaid job outside redisplay, without invoking a shell."
  (unless agent-chat--mermaid-running
    (when-let* ((entry (pop agent-chat--mermaid-queue)))
      (setq agent-chat--mermaid-running entry)
      (setf (plist-get entry :status) 'running)
      (condition-case err
          (let* ((default-directory temporary-file-directory)
                 (directory (make-temp-file "agent-chat-mermaid-" t))
                 (default-directory (file-name-as-directory directory))
                 (coding-system-for-write 'utf-8-unix))
            (setf (plist-get entry :directory) directory)
            (with-temp-file (expand-file-name "diagram.mmd" directory)
              (insert (plist-get entry :body)))
            (with-temp-file (expand-file-name "config.json" directory)
              (insert "{\"securityLevel\":\"strict\",\"htmlLabels\":false,"
                      "\"flowchart\":{\"htmlLabels\":false}}"))
            (with-temp-file (expand-file-name "browser.json" directory)
              (insert "{\"args\":[\"--proxy-server=http://127.0.0.1:9\","
                      "\"--proxy-bypass-list=<-loopback>\"]}"))
            (setf (plist-get entry :timer)
                  (run-at-time 30 nil #'agent-chat--mermaid-finish
                               entry nil "Rendering timed out"))
            (setf (plist-get entry :process)
                  (make-process
                   :name "agent-chat-mermaid" :buffer nil :noquery t :connection-type 'pipe
                   :command (list (plist-get entry :program) "-i" "diagram.mmd"
                                  "-o" "diagram.png" "-c" "config.json"
                                  "-p" "browser.json" "-b" "white" "-q")
                   :sentinel
                   (lambda (process _event)
                     (when (and (memq (process-status process) '(exit signal))
                                (eq (plist-get entry :status) 'running))
                       ;; The process boundary retains a reason and literal code
                       ;; on failure rather than interrupting the conversation.
                       (condition-case err
                           (let ((file (expand-file-name "diagram.png" directory)))
                             (unless (and (= (process-exit-status process) 0)
                                          (file-exists-p file)
                                          (< (file-attribute-size (file-attributes file)) 4000000))
                               (error "mmdc failed or produced no bounded PNG"))
                             (agent-chat--mermaid-finish
                              entry (with-temp-buffer
                                      (set-buffer-multibyte nil)
                                      (insert-file-contents-literally file)
                                      (buffer-string))))
                         (error (agent-chat--mermaid-finish
                                 entry nil (error-message-string err)))))))))
        (error (agent-chat--mermaid-finish entry nil (error-message-string err)))))))

(defun agent-chat--mermaid-request (body)
  "Return BODY's cached preview, scheduling at most one local compilation."
  (let ((entry (gethash body agent-chat--mermaid-cache)))
    (unless entry
      (when-let* (((agent-chat--mermaid-safe-p body))
                  (program (let ((default-directory temporary-file-directory))
                             (executable-find "mmdc"))))
        (setq entry (list :body body :program program :status 'queued
                          :buffers nil :directory nil :process nil :timer nil
                          :png nil :error nil))
        (puthash body entry agent-chat--mermaid-cache)
        (setq agent-chat--mermaid-queue
              (nconc agent-chat--mermaid-queue (list entry)))
        (run-at-time 0 nil #'agent-chat--mermaid-next)))
    (when entry (cl-pushnew (current-buffer) (plist-get entry :buffers)))
    entry))

(defun agent-chat--mermaid-image-text (source png window)
  "Display SOURCE as a PNG diagram fitted to WINDOW and split into native rows."
  (let* ((image (create-image png 'png t :scale 1 :ascent 'center :margin 0))
         (font-height (window-font-height window))
         (size (image-size image t (window-frame window)))
         (scale (min (/ (float font-height) 24)
                     (/ (float (max 1 (- (window-body-width window t)
                                         (* 4 (window-font-width window)))))
                        (car size))))
         (width (max 1 (ceiling (* scale (car size)))))
         (height (max 1 (ceiling (* scale (cdr size)))))
         (rows (max 1 (/ height font-height)))
         (reference (list source)))
    (setf (image-property image :width) width
          (image-property image :height) height)
    ;; Slice tall diagrams for scrolling.  Each slice spans at least a font
    ;; line (except a short single-row image), avoiding gaps from text padding.
    (with-temp-buffer
      (dotimes (row rows)
        (let ((start (* row font-height))
              (end (if (= row (1- rows)) height (* (1+ row) font-height))))
          (insert-image image
                        (if (= row 0)
                            (replace-regexp-in-string "\n" " " source t t)
                          " ")
                        nil (list 0 start width (- end start)))
          (insert "\n")))
      ;; Preserve native newline metrics, as in `insert-sliced-image'.
      (add-text-properties
       (point-min) (point-max)
       (list 'line-height t 'help-echo source 'agent-chat-math-source reference))
      (buffer-substring (point-min) (1- (point-max))))))

(defun agent-chat--mermaid-render (body)
  "Return a preview of complete BODY, or nil to retain the code block."
  (when (and agent-chat-render-mermaid
             (window-live-p agent-chat--math-window)
             (display-graphic-p (window-frame agent-chat--math-window))
             (image-type-available-p 'png))
    (let ((entry (agent-chat--mermaid-request body)))
      (when (eq (plist-get entry :status) 'ready)
        (condition-case err
            (agent-chat--mermaid-image-text
             (concat "```mermaid\n" body "\n```")
             (plist-get entry :png) agent-chat--math-window)
          (error
           (setf (plist-get entry :status) 'failed
                 (plist-get entry :error) (error-message-string err))
           nil))))))

(defun agent-chat--markdown-pair-end (text start open close)
  "Find the closing delimiter for OPEN at START in TEXT, respecting escapes."
  (let ((at (1+ start)) (depth 1))
    (while (and (< at (length text)) (> depth 0))
      (let ((char (aref text at)))
        (cond ((= char ?\\) (cl-incf at))
              ((= char open) (cl-incf depth))
              ((= char close) (cl-decf depth))))
      (cl-incf at))
    (when (= depth 0) (1- at))))

(defun agent-chat--markdown-link (text start)
  "Parse an inline link at START in TEXT as (END LABEL TARGET), or nil.
Accept balanced brackets/parentheses, angle destinations and quoted titles.
Incomplete streamed links remain literal until the closing delimiter arrives."
  (when-let* ((label-end (agent-chat--markdown-pair-end text start ?\[ ?\]))
              (open (1+ label-end))
              ((and (< open (length text)) (= (aref text open) ?\()))
              (close (agent-chat--markdown-pair-end text open ?\( ?\))))
    (let* ((inside (string-trim (substring text (1+ open) close)))
           (at 0) target rest)
      (if (string-prefix-p "<" inside)
          (when (string-match ">" inside 1)
            (setq target (substring inside 1 (match-beginning 0))
                  rest (substring inside (match-end 0))))
        (while (and (< at (length inside)) (not (memq (aref inside at) '(?\s ?\t ?\n))))
          (when (= (aref inside at) ?\\) (cl-incf at))
          (setq at (min (length inside) (1+ at))))
        (setq target (substring inside 0 at) rest (substring inside at)))
      (when (and target (not (string-empty-p target))
                 (or (string-empty-p rest)
                     (string-match-p "\\`[ \t]+\\(?:\"[^\"\n]*\"\\|'[^'\n]*'\\)[ \t]*\\'" rest)))
        (list (1+ close) (substring text (1+ start) label-end)
              (replace-regexp-in-string "\\\\\\([][()<>\\\\]\\)" "\\1" target))))))

(defun agent-chat--inline-markdown (text &optional no-links)
  "Render inline Markdown in TEXT, retaining link targets as inert properties.
NO-LINKS prevents nested links inside labels.  Code spans remain literal."
  (let ((start 0) parts)
    (while (string-match (rx (or "`" "**" "[" "$$" "$" (seq "\\" anychar))) text start)
      (let* ((at (match-beginning 0)) (end (match-end 0))
             (token (match-string 0 text)) rendered)
        (push (substring text start at) parts)
        (cond
         ((member token '("\\(" "\\[" "$" "$$"))
          (if-let* ((formula (agent-chat--math-fragment text at)))
              (setq end (car formula)
                    rendered (agent-chat--math-render
                              (substring text at end) (cadr formula) (nth 2 formula)))
            ;; Keep unfinished math intact while the agent is still streaming.
            (when (member token '("\\(" "\\[" "$$"))
              (setq end (length text) rendered (substring text at)))))
         ((string-prefix-p "\\" token)
          (setq rendered (if (string-match-p "[[:punct:]]" (substring token 1))
                             (substring token 1) token)))
         ((equal token "`")
          (while (and (< end (length text)) (= (aref text end) ?`)) (cl-incf end))
          (let ((scan end) close)
            (while (and (not close) (string-match "`+" text scan))
              (setq scan (match-end 0))
              (when (= (- scan (match-beginning 0)) (- end at))
                (setq close (match-beginning 0))))
            (when close
              (setq rendered (propertize (substring text end close) 'face 'agent-chat-code)
                    end scan))))
         ((equal token "**")
          (when (string-match (regexp-quote token) text end)
            (let ((close (match-beginning 0)) (after (match-end 0)))
              (setq rendered (agent-chat--inline-markdown (substring text end close) no-links)
                    end after)
              (add-face-text-property 0 (length rendered) 'bold t rendered))))
         ((and (equal token "[") (not no-links)
               (not (and (> at 0) (= (aref text (1- at)) ?!))))
          (when-let* ((link (agent-chat--markdown-link text at)))
            (setq end (car link)
                  rendered (agent-chat--inline-markdown (cadr link) t))
            (add-face-text-property 0 (length rendered) 'link nil rendered)
            (add-text-properties 0 (length rendered)
                                 (list 'agent-chat-link (nth 2 link)
                                       'help-echo (nth 2 link) 'mouse-face 'highlight) rendered))))
        (push (or rendered (substring text at end)) parts)
        (setq start end)))
    (push (substring text start) parts)
    (apply #'concat (nreverse parts))))

(defun agent-chat--link-target (target)
  "Resolve TARGET as a web URL or workspace-host file with line and column.
This only parses text; it never visits files or fetches URLs while rendering."
  (when (string-match-p "[[:cntrl:]]" target) (user-error "Invalid link target"))
  (if (string-match-p "\\`https?://[^ /]+" target)
      (list :url target)
    (let ((path target) line column)
      (when (string-prefix-p "file:" path)
        (unless (string-match "\\`file://\\(?:localhost\\)?\\(/.*\\)\\'" path)
          (user-error "Unsupported file URL"))
        (setq path (decode-coding-string (url-unhex-string (match-string 1 path)) 'utf-8)))
      (when (string-match "\\(?::\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\|#L\\([0-9]+\\)\\(?:C\\([0-9]+\\)\\)?\\)\\'" path)
        (setq line (string-to-number (or (match-string 1 path) (match-string 3 path)))
              column (when-let* ((value (or (match-string 2 path) (match-string 4 path))))
                       (string-to-number value))
              path (substring path 0 (match-beginning 0))))
      (when (or (string-empty-p path) (string-prefix-p "#" path)
                (string-match-p "[[:cntrl:]]" path)
                (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" path))
        (user-error "Unsupported link target: %s" target))
      (list :file (agent-chat--workspace-file path default-directory) :line line :column column))))

(defun agent-chat--open-link (target)
  "Visit TARGET only after an explicit click or RET on a rendered link."
  (let ((destination (agent-chat--link-target target)))
    (if-let* ((url (plist-get destination :url)))
        (browse-url url)
      (let ((file (plist-get destination :file))
            (enable-local-eval nil) (enable-local-variables :safe))
        (unless (file-exists-p file) (user-error "File does not exist: %s" file))
        (find-file-other-window file)
        (when-let* ((line (plist-get destination :line)))
          (goto-char (point-min))
          (forward-line (max 0 (1- line)))
          (when-let* ((column (plist-get destination :column)))
            (move-to-column (max 0 (1- column)))))))))

;;;; Optional Embark targets for rendered links

(defvar embark-target-finders)
(defvar embark-keymap-alist)

(defun agent-chat--embark-target-link ()
  "Find the rendered link at point, letting active regions take precedence.
URL targets use Embark's standard actions.  File targets contain the resolved
path, without a line suffix that file actions could mistake for a filename."
  (when (and (derived-mode-p 'agent-chat-history-mode) (not (use-region-p)))
    (when-let* ((link (get-text-property (point) 'agent-chat-link))
                (destination (condition-case nil (agent-chat--link-target link)
                               (user-error nil))))
      (let ((start (previous-single-property-change
                    (1+ (point)) 'agent-chat-link nil (point-min)))
            (end (next-single-property-change
                  (point) 'agent-chat-link nil (point-max))))
        (if-let* ((url (plist-get destination :url)))
            `(url ,url ,start . ,end)
          `(agent-chat-file-link ,(plist-get destination :file) ,start . ,end))))))

(defun agent-chat-visit-file-reference (file)
  "Visit FILE, retaining the current rendered reference's line and column.
As an Embark action, other file operations still receive just the file path.
Outside the source link, such as in an Embark collection, visit FILE normally."
  (interactive "fFile: ")
  (let* ((link (get-text-property (point) 'agent-chat-link))
         (destination (and link (condition-case nil (agent-chat--link-target link)
                                 (user-error nil)))))
    (agent-chat--open-link
     (if (equal (expand-file-name file) (plist-get destination :file)) link file))))

(defvar agent-chat--embark-file-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-chat-visit-file-reference)
    map)
  "Default action for file references; other actions come from Embark.")

(defun agent-chat--enable-embark ()
  "Recognize links locally when optional Embark is already loaded."
  (when (featurep 'embark)
    (add-hook 'embark-target-finders #'agent-chat--embark-target-link nil t)))

(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist
               '(agent-chat-file-link agent-chat--embark-file-link-map embark-file-map))
  ;; Also support loading Embark for the first time from an existing session.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-chat-history-mode)
        (agent-chat--enable-embark)))))

(defvar agent-chat--markdown-width nil
  "Available text columns, excluding the message gutter, during rendering.")

(defun agent-chat--table-cells (line)
  "Split a Markdown table LINE at unescaped pipes outside code spans.
Return nil when LINE has no delimiters.  Optional outer pipes are removed."
  (let ((line (string-trim line)) (at 0) (start 0) code cells found trailing)
    (while (< at (length line))
      (pcase (aref line at)
        (?\\ (setq at (min (length line) (+ at 2)) trailing nil))
        (?` (let ((end at))
              (while (and (< end (length line)) (= (aref line end) ?`))
                (cl-incf end))
              (cond ((not code) (setq code (- end at)))
                    ((= code (- end at)) (setq code nil)))
              (setq at end trailing nil)))
        (?| (if code (setq at (1+ at) trailing nil)
              (push (substring line start at) cells)
              (setq found t trailing t start (1+ at) at (1+ at))))
        (_ (setq at (1+ at) trailing nil))))
    (when found
      (push (substring line start) cells)
      (setq cells (nreverse cells))
      (when (string-prefix-p "|" line) (setq cells (cdr cells)))
      (when trailing (setq cells (butlast cells)))
      (mapcar (lambda (cell)
                (replace-regexp-in-string "\\\\|" "|" (string-trim cell) t t)) cells))))

(defun agent-chat--table-alignments (header delimiter)
  "Return column alignments when HEADER and DELIMITER start a table."
  (when (and delimiter (string-match-p "\\`[-:| \t]+\\'" delimiter))
    (let ((head (agent-chat--table-cells header))
          (cells (agent-chat--table-cells delimiter)))
      (when (and head cells (= (length head) (length cells))
                 (cl-every (lambda (cell) (string-match-p "\\`:?-\\{3,\\}:?\\'" cell)) cells))
        (mapcar (lambda (cell)
                  (cond ((and (string-prefix-p ":" cell) (string-suffix-p ":" cell)) 'center)
                        ((string-suffix-p ":" cell) 'right)
                        (t 'left))) cells)))))

(defun agent-chat--table-cell-width (text)
  "Measure TEXT in columns, accounting for previews in the reading window."
  (if (and (window-live-p agent-chat--math-window)
           (display-graphic-p (window-frame agent-chat--math-window)))
      (/ (float (ebox-string-pixel-width text)) (window-font-width agent-chat--math-window))
    (string-width text)))

(defun agent-chat--wrap-cell (text width)
  "Wrap propertized TEXT within WIDTH columns, preserving every nonspace char.
Use native pixel layout for previews, keeping each formula image whole."
  (if (and (window-live-p agent-chat--math-window)
           (display-graphic-p (window-frame agent-chat--math-window))
           (text-property-not-all 0 (length text) 'agent-chat-math-source nil text))
      (let* ((ebox-viewport-width (* (max 2 width) (window-font-width agent-chat--math-window)))
             (ebox-viewport-height 1)
             (text (copy-sequence text))
             (pos 0))
        ;; A formula wider than its column must fit before Ebox wraps it.
        ;; Copy the display spec so the shared Org preview stays full-sized.
        (while (< pos (length text))
          (let* ((end (next-single-property-change pos 'display text (length text)))
                 (display (get-text-property pos 'display text)))
            (when (eq (car-safe display) 'image)
              (let ((image (copy-tree display)))
                ;; Keep Org's font-relative dimensions.  Measuring with
                ;; `image-size' here would use the frame font, underestimating
                ;; scaled buffer images and letting the table clip them.
                (setf (image-property image :max-width) ebox-viewport-width)
                (put-text-property pos end 'display image text)))
            (setq pos end)))
        (agent-chat--native-history-lines
         (ebox-render
          (ebox-node-create 'box '(:wrap-mode char)
                            (list (ebox-text-create text))))))
    (let ((width (max 2 width)) result)
      (dolist (part (split-string (replace-regexp-in-string "\t" " " text t t) "\n" nil))
        (while (> (string-width part) width)
          (let* ((prefix (truncate-string-to-width part width))
                 ;; Undisplayable characters may be wider than a tiny viewport.
                 ;; Always consume one so arbitrary agent text cannot stall Emacs.
                 (end (max 1 (length prefix)))
                 (space (string-match "[ ]+[^ ]*\\'" prefix)))
            (when (and space (> space 0)) (setq end space))
            (push (substring part 0 end) result)
            (setq part (string-trim-left (substring part end)))))
        (push part result))
      (nreverse result))))

(defun agent-chat--table (rows alignments width)
  "Render ROWS with ALIGNMENTS in WIDTH columns, without changing source.
Narrow windows use labeled fields instead of squeezing columns to nothing."
  (let* ((ebox--measure-buffer (if (window-live-p agent-chat--math-window)
                                  (window-buffer agent-chat--math-window)
                                ebox--measure-buffer))
         (unit (if (and (window-live-p agent-chat--math-window)
                        (display-graphic-p (window-frame agent-chat--math-window)))
                   (window-font-width agent-chat--math-window) 1))
         (count (length alignments))
         (rows (mapcar (lambda (row)
                         (cl-loop for column below count
                                  collect (agent-chat--inline-markdown
                                           (replace-regexp-in-string
                                            "<br[ \t]*/?>" "\n" (or (nth column row) "") t t))))
                       rows))
         (natural (apply #'cl-mapcar
                         (lambda (&rest cells)
                           (max 1 (apply #'max
                                         (mapcar (lambda (cell)
                                                   (ceiling (apply #'max (mapcar #'agent-chat--table-cell-width
                                                                                (split-string cell "\n" nil)))))
                                                 cells)))) rows))
         (budget (- width (* 3 count) 1))
         (widths (mapcar (lambda (size) (min 6 size)) natural))
         output)
    (if (> (apply #'+ widths) budget)
        (progn
          (dolist (row (or (cdr rows) (list (make-list count ""))))
            (cl-loop for heading in (car rows) for cell in row do
                     (let ((label (copy-sequence heading)))
                       (add-face-text-property 0 (length label) 'bold t label)
                       (setq output (append output
                                            (agent-chat--wrap-cell (concat label ": " cell) width)))))
            (setq output (append output (list ""))))
          (string-join (butlast output) "\n"))
      ;; Share spare columns fairly; short columns stop growing at their content.
      (let ((remaining (- budget (apply #'+ widths))))
        (while (and (> remaining 0) (cl-some #'< widths natural))
          (cl-loop for tail on widths for wanted in natural
                   when (and (> remaining 0) (< (car tail) wanted))
                   do (setcar tail (1+ (car tail))) (cl-decf remaining))))
      (cl-labels
          ((space (size)
             (if (> unit 1) (ebox-pixel-space size) (make-string size ?\s)))
           (rule (left middle right)
             (propertize (concat left (mapconcat (lambda (size) (make-string (+ size 2) ?─))
                                                widths middle) right) 'face 'shadow))
           (row-lines (row header)
             (let* ((wrapped (cl-mapcar
                              (lambda (cell size)
                                (let ((cell (copy-sequence cell)))
                                  (when header (add-face-text-property 0 (length cell) 'bold t cell))
                                  (agent-chat--wrap-cell cell size)))
                              row widths))
                    (height (apply #'max (mapcar #'length wrapped))))
               (cl-loop for line below height collect
                        (concat
                         (propertize "│ " 'face 'shadow)
                         (mapconcat
                          #'identity
                          (cl-mapcar
                           (lambda (cell size align)
                             (let* ((text (copy-sequence (or (nth line cell) "")))
                                    (padding (max 0 (round (* unit (- size (agent-chat--table-cell-width text))))))
                                    (left (pcase align ('right padding) ('center (/ padding 2)) (_ 0))))
                               (if (string-empty-p text) (space padding)
                                 (concat (space left) text (space (- padding left))))))
                           wrapped widths alignments)
                          (propertize " │ " 'face 'shadow))
                         (propertize " │" 'face 'shadow))))))
        (push (rule "┌" "┬" "┐") output)
        (cl-loop for row in rows for index from 0 do
                 (when (> index 0) (push (rule "├" "┼" "┤") output))
                 (dolist (line (row-lines row (= index 0))) (push line output)))
        (push (rule "└" "┴" "┘") output)
        (string-join (nreverse output) "\n")))))

(defun agent-chat--markdown (text &optional width)
  "Render code fences, tables, headings and inline markup without executing TEXT.
WIDTH is the available text columns, excluding the message gutter.
The history retains the unmodified source.  Incomplete streaming code fences
are accepted and their contents remain literal.  This is not full CommonMark."
  (let ((input (split-string text "\n" nil))
        (width (or width agent-chat--markdown-width
                   (max 2 (- (agent-chat--window-columns (selected-window)) 2))))
        fence language code lines)
    (while input
      (let ((line (pop input)) alignments)
	(cond
	 ((and (not fence) (string-match "\\`[ \t]*\\(```+\\|~~~+\\)\\(.*\\)\\'" line))
          (setq fence (match-string 1 line)
		language (string-trim (match-string 2 line)))
          (push (propertize (concat "┌ " language)
                            'face 'shadow) lines))
	 ((and fence
               (string-match-p
		(concat "\\`[ \t]*" (regexp-quote fence)
			(regexp-quote (substring fence 0 1)) "*[ \t]*\\'") line))
          (let* ((body (nreverse code))
                 (preview (and (equal (downcase language) "mermaid")
                               (agent-chat--mermaid-render (string-join body "\n")))))
            (if preview
                (push preview lines)
              (dolist (rendered (agent-chat--code-lines body language))
                (push rendered lines))))
          (setq fence nil code nil)
          (push (propertize "└" 'face 'shadow) lines))
	 (fence (push line code))
         ((string-match "\\`[ \t]*\\(\\\\\\[\\|\\$\\$\\)" line)
          (let* ((at (match-beginning 1))
                 (remaining (string-join (cons line input) "\n"))
                 (formula (agent-chat--math-fragment remaining at)))
            (if (not formula)
                (progn (push remaining lines) (setq input nil))
              (let* ((end (car formula))
                     (tail (substring remaining end))
                     (tail-lines (split-string tail "\n" nil)))
                (push (concat (substring line 0 at)
                              (agent-chat--math-render
                               (substring remaining at end) (cadr formula) t)
                              (agent-chat--inline-markdown (car tail-lines))) lines)
                (setq input (cdr tail-lines))))))
	 ((setq alignments (agent-chat--table-alignments line (car input)))
          (pop input)
          (let ((rows (list (agent-chat--table-cells line))) cells)
            (while (and input (setq cells (agent-chat--table-cells (car input)))
			(<= (length cells) (length alignments)))
              (push cells rows)
              (pop input))
            (push (agent-chat--table (nreverse rows) alignments width) lines)))
	 ((string-match "\\`#+[ \t]+\\(.*\\)\\'" line)
          (let ((heading (agent-chat--inline-markdown (match-string 1 line))))
            (add-face-text-property 0 (length heading) 'bold t heading)
            (push heading lines)))
	 ((string-match "\\`[ \t]*>[ \t]?\\(.*\\)\\'" line)
          (let ((quote (concat "│ " (agent-chat--inline-markdown (match-string 1 line)))))
            (add-face-text-property 0 (length quote) 'agent-chat-muted t quote)
            (push quote lines)))
	 ((string-match "\\`\\([ \t]*\\)[-*+][ \t]+\\(.*\\)\\'" line)
          (push (concat (match-string 1 line) "• "
			(agent-chat--inline-markdown (match-string 2 line))) lines))
	 (t (push (agent-chat--inline-markdown line) lines)))))
    (when fence
      (dolist (rendered (agent-chat--code-lines (nreverse code) language))
        (push rendered lines)))
    (string-join (nreverse lines) "\n")))

(defun agent-chat--format-entry (entry index full)
  "Format ENTRY at INDEX, showing all details when FULL is non-nil."
  (if (eq (plist-get entry :role) 'permission)
      (agent-chat--format-permission entry)
    (let* ((role (plist-get entry :role))
         (prefix (pcase role ('user "› ") ('tool "  └ ")
                        ('reasoning "◇ Reasoning\n  ")
                        ('error "! Error\n  ") ('notice "— ") (_ "• ")))
         (text (if (memq role '(assistant reasoning))
                   (agent-chat--markdown (plist-get entry :text))
                 (plist-get entry :text)))
         (continuation (make-string (if (memq role '(reasoning error))
                                        2 (string-width prefix)) ?\s))
         (process (memq role '(reasoning tool)))
         (expanded (or full (memq index agent-chat--expanded)))
         (detail (if (eq role 'tool) (or (plist-get entry :detail) text) text))
         (body
          (if (and process (not full))
              (concat
               (propertize
                (concat (if expanded "▾ " "▸ ")
                        (if (eq role 'reasoning) "Reasoning · " "Tool · ")
                        (truncate-string-to-width
                         (replace-regexp-in-string "[\n\r\t ]+" " "
                                                   (string-trim (plist-get entry :text)))
                         (max 8 (min 72 (- (window-body-width) 10))) nil nil "…"))
                'agent-chat-entry index)
               (when expanded (concat "\n  " (replace-regexp-in-string
                                              "\n" "\n  " detail t t))))
            (concat prefix (replace-regexp-in-string
                            "\n" (concat "\n" continuation)
                            (if (and full (eq role 'tool) (plist-get entry :detail))
                                (concat text "\n" (plist-get entry :detail)) text) t t)))))
    (when-let* ((face (pcase role
                        ('user 'agent-chat-user)
                        ('error 'agent-chat-error)
                        ('notice 'agent-chat-muted)
                        ('tool (if (equal (plist-get entry :tool-status) "failed")
                                   'agent-chat-error 'agent-chat-muted))
                        ('reasoning 'agent-chat-muted))))
      (add-face-text-property 0 (length body) face t body))
    body)))

(defun agent-chat--format-permission (entry)
  "Format permission ENTRY for history without response controls."
  (let ((resolved (plist-get entry :resolved)))
    (concat (propertize (format "%s %s%s"
                               (if resolved "✓" "? Approval required ·")
                               (plist-get entry :text)
                               (if resolved (format " · %s" resolved) ""))
                        'face (if resolved 'shadow 'warning))
            (unless resolved
              (when-let* ((detail (plist-get entry :detail))
                          ((not (string-empty-p detail))))
                (concat "\n  " (replace-regexp-in-string "\n" "\n  " detail t t)))))))

(defun agent-chat--permission-entry-pending-p (entry)
  "Whether permission ENTRY still awaits an explicit response."
  (and (eq (plist-get entry :role) 'permission)
       (not (plist-get entry :resolved))))

(defun agent-chat--permission-reminder (count)
  "Format the shared in-buffer reminder for COUNT pending permissions."
  (concat (propertize (if (= count 1) "Permission pending"
                       (format "%d permissions pending" count))
                     'face 'warning)
          " — press " (propertize "C-c C-p" 'face 'help-key-binding)
          " to reopen the permission menu"))

(defun agent-chat--permission-approved-p (entry)
  "Whether ENTRY records an explicitly advertised allow decision.
Use the recorded option kinds, not the current bypass setting, so existing
history folds correctly without guessing whether a decision was automatic."
  (and (eq (plist-get entry :role) 'permission)
       (plist-get entry :resolved)
       (not (equal (plist-get entry :resolved) "cancelled"))
       (seq-some (lambda (option)
                   (and (equal (map-elt option 'optionId) (plist-get entry :resolved))
                        (member (map-elt option 'kind) '("allow_once" "allow_always"))))
                 (plist-get entry :options))))

(defun agent-chat--process-entry-p (entry)
  "Whether ENTRY belongs to a collapsible process group."
  (or (memq (plist-get entry :role) '(reasoning tool))
      (agent-chat--permission-approved-p entry)))

(defun agent-chat--process-group-expanded-p (start)
  "Return the effective expansion state of the group at START."
  (if-let* ((override (assq start agent-chat--process-group-overrides)))
      (cdr override)
    agent-chat-process-groups-expand-by-default))

(defun agent-chat--format-process-group (entries start)
  "Summarize consecutive process ENTRIES starting at START."
  (let* ((tools (seq-count (lambda (entry) (eq (plist-get entry :role) 'tool)) entries))
         (thoughts (seq-count (lambda (entry) (eq (plist-get entry :role) 'reasoning)) entries))
         (approvals (seq-count #'agent-chat--permission-approved-p entries))
         (failures (seq-count (lambda (entry) (equal (plist-get entry :tool-status) "failed")) entries))
         (expanded (agent-chat--process-group-expanded-p start))
         (summary (propertize
                   (concat (if expanded "▾ " "▸ ") "Activity · "
                           (string-join
                            (delq nil (list (when (> tools 0) (format "%d tool%s" tools (if (= tools 1) "" "s")))
                                            (when (> thoughts 0) (format "%d thought%s" thoughts (if (= thoughts 1) "" "s")))
                                            (when (> approvals 0) (format "%d approved" approvals))
                                            (when (> failures 0) (format "%d failed" failures))))
                            " · "))
                   'agent-chat-group start
                   'face (if (> failures 0) 'agent-chat-error 'agent-chat-muted))))
    (if (not expanded) summary
      (let ((index (1- start)))
        (concat summary "\n"
                (mapconcat
                 (lambda (entry)
                   (let ((text (concat "  " (replace-regexp-in-string
                                            "\n" "\n  "
                                            (agent-chat--format-entry entry (cl-incf index) nil) t t))))
                     ;; Keep folding scope separate from clickable headings:
                     ;; selecting body text or following links must still work.
                     (when (memq (plist-get entry :role) '(reasoning tool))
                       (put-text-property 0 (length text) 'agent-chat-fold-entry index text))
                     text))
                 entries "\n"))))))

(defun agent-chat--transcript (state &optional full)
  "Format STATE, grouping consecutive tools, thoughts and granted approvals.
Pending approvals share one reminder at the end.
FULL includes every detail without grouping for copying and searching."
  (let ((entries (agent-chat--state-history state)) (index 0) (pending 0) parts)
    (while entries
      (cond
       ((and (not full) (agent-chat--permission-entry-pending-p (car entries)))
        (pop entries)
        (cl-incf index)
        (cl-incf pending))
       ((and (not full) (agent-chat--process-entry-p (car entries)))
        (let ((start index) group)
          (while (and entries (agent-chat--process-entry-p (car entries)))
            (push (pop entries) group)
            (cl-incf index))
          (push (agent-chat--format-process-group (nreverse group) start) parts)))
       (t
        (push (agent-chat--format-entry (pop entries) index full) parts)
        (cl-incf index))))
    (when (> pending 0)
      (push (agent-chat--permission-reminder pending) parts))
    ;; Message spacing belongs to this join.  An agent's final newline must
    ;; not add a second blank row before the next message or process group.
    ;; Trim only trailing blank lines in the presentation, retaining source,
    ;; internal paragraphs, code spacing and spaces on the last content line.
    (mapconcat (lambda (part) (string-trim-right part "\n[ \t\r\n]*"))
               (nreverse parts) "\n\n")))

;;;; Standalone ACP core

(defcustom agent-chat-default-agent 'codex
  "Agent selected by default when opening a conversation."
  :type 'symbol :group 'agent-chat)

(defcustom agent-chat-codex-local-backend 'external
  "Backend for new Codex conversations and history in local projects.
`external' starts the configured `codex-acp' command; `emacs' uses
`codex-acp.el' and its Emacs workspace tools.  TRAMP projects and general
conversations always use `codex-acp.el'.  Existing conversations keep the
backend with which they were started.  Native local sessions selected from
history use the external command to retain their original Codex tools."
  :type '(choice (const :tag "External codex-acp" external)
                 (const :tag "Emacs codex-acp.el" emacs))
  :group 'agent-chat)

(defcustom agent-chat-agents
  '((codex :name "Codex" :command ("codex-acp")
           :remote-client-maker agent-chat--codex-remote-client
           :general-client-maker agent-chat--codex-general-client)
    (magent :name "Magent" :client-maker agent-chat--magent-client
            :remote t))
  "ACP agent configurations, independent of any other frontend.
Each value is a plist with :name and either :command (an argv list) or
:client-maker (a function taking the conversation buffer).
:remote-client-maker supplies a different factory for TRAMP projects;
:remote t declares that the ordinary client factory accepts TRAMP paths.
:general-client-maker overrides the factory for general conversations,
on either the local or a supported remote host.
External commands without a remote factory remain local-only.  Optional
:environment is a list of process environment overrides.  Commands are
passed as argv, never interpolated into a shell.  Credentials are left
to the agent's existing login and process environment."
  :type '(alist :key-type symbol :value-type plist) :group 'agent-chat)

(defcustom agent-chat-connect-timeout 30
  "Seconds allowed for initialization and session creation."
  :type 'number :group 'agent-chat)

(defcustom agent-chat-completion-auto t
  "Offer completion when / or $ is typed at a completion boundary.
Manual completion with TAB or `completion-at-point' is always available."
  :type 'boolean :group 'agent-chat)

(declare-function magent-acp-make-client "magent-acp" (&optional context-buffer))
(declare-function magent-runtime-ensure-initialized "magent-runtime" ())
(declare-function codex-acp-make-client "codex-acp" (&optional buffer scope))
(declare-function codex-acp-steer-ready-p "codex-acp" (client))
(declare-function codex-acp-steer "codex-acp" (client session-id prompt success failure))
(declare-function codex-acp-rename-session "codex-acp" (client session-id name success failure))

(cl-defstruct (agent-chat--session
               (:constructor agent-chat--session-create))
  buffer client agent cwd id (phase 'connecting) capabilities auth-methods
  model mode config-options commands title tools permissions pending-updates
  busy cancelling emit stopped timers sentinel-installed startup replaying)

(defvar-local agent-chat--session nil)
(defvar-local agent-chat--rename-pending nil
  "Session whose title rename is awaiting a response in this buffer.")
(defvar-local agent-chat--startup-pending nil
  "Whether this conversation is hidden until its initial session is chosen.")
(defvar-local agent-chat--client-maker-override nil
  "Per-conversation backend override; `:external' uses the agent command.")
(defvar-local agent-chat--permission-bypass-session nil
  "Exact session whose allow_once requests the user chose to bypass.
Keeping the session identity prevents a replacement connection inheriting it.")
(defvar-local agent-chat--scope 'directory
  "Conversation scope: `directory' for a project, or `host' for general use.")
(defvar-local agent-chat--render-cache nil)
(defvar-local agent-chat--render-block-cache nil
  "Geometry and rendered message blocks retained across streamed updates.
Only blocks in the current history are retained; changed entries are compared
against copied data because ACP updates mutate entry plists in place.")
(defvar-local agent-chat--render-timer nil)
(defvar-local agent-chat--render-finished-at 0.0)
(defvar-local agent-chat--render-duration 0.0)

(defun agent-chat--magent-client (buffer)
  "Create Magent's in-process ACP client for BUFFER, without its shell UI."
  (require 'magent-acp)
  (magent-runtime-ensure-initialized)
  (magent-acp-make-client buffer))

(defun agent-chat--codex-remote-client (buffer)
  "Create local Codex with Emacs project tools for BUFFER."
  (require 'codex-acp)
  (codex-acp-make-client buffer))

(defun agent-chat--codex-general-client (buffer)
  "Create local Codex with Emacs tools across BUFFER's host."
  (require 'codex-acp)
  (codex-acp-make-client buffer 'host))

(defun agent-chat--live-p (session)
  "Whether SESSION still belongs to its live conversation buffer."
  (and (not (agent-chat--session-stopped session))
       (buffer-live-p (agent-chat--session-buffer session))
       (eq session (buffer-local-value 'agent-chat--session
                                       (agent-chat--session-buffer session)))))

(defun agent-chat--refresh-session (session)
  "Refresh SESSION's view without modifying its draft."
  (when (agent-chat--live-p session)
    (with-current-buffer (agent-chat--session-buffer session)
      (when (memq (agent-chat--session-phase session) '(failed disconnected))
        (agent-chat-quota--stop))
      (agent-chat--publish))))

(defun agent-chat--display-session (session &optional force)
  "Display SESSION after accepting its session choice.
FORCE also displays a reused, previously cancelled conversation."
  (when (agent-chat--live-p session)
    (with-current-buffer (agent-chat--session-buffer session)
      (when (or force agent-chat--startup-pending)
        (setq agent-chat--startup-pending nil)
        (let* ((startup (agent-chat--session-startup session))
               (origin (plist-get startup :origin-window)))
          (when (or (not origin)
                    (and (eq origin (selected-window)) (window-live-p origin)
                         (eq (window-buffer origin) (plist-get startup :origin-buffer))))
            (pop-to-buffer (current-buffer))
            (agent-chat--redraw)))))))

(defun agent-chat--notice (session text)
  "Show TEXT as a session diagnostic without terminating an active turn."
  (when (agent-chat--live-p session)
    (with-current-buffer (agent-chat--session-buffer session)
      (agent-chat--append agent-chat--state 'error text)
      (agent-chat--publish))))

(defun agent-chat--error-text (error-data)
  "Extract a readable message from ACP ERROR-DATA."
  (if (stringp error-data) error-data
    (format "%s" (or (map-elt error-data 'message) error-data))))

(defun agent-chat--retry-load-startup (session)
  "Return the saved load action to retry when SESSION failed."
  (when (and session (eq (agent-chat--session-phase session) 'failed))
    (let ((startup (agent-chat--session-startup session)))
      (and (eq (plist-get startup :action) 'load)
           (stringp (plist-get startup :id))
           (not (string-empty-p (plist-get startup :id)))
           startup))))

(defun agent-chat--connection-error (session error-data)
  "Record a connection failure for SESSION using ERROR-DATA."
  (setf (agent-chat--session-phase session) 'failed)
  (setf (agent-chat--session-replaying session) nil
        (agent-chat--session-pending-updates session) nil)
  (agent-chat--display-session session)
  (agent-chat--notice session (agent-chat--error-text error-data)))

(defun agent-chat--require-ready (session)
  "Require a ready SESSION, reporting its failure and applicable recovery."
  (unless (and session (agent-chat--live-p session)
               (eq (agent-chat--session-phase session) 'ready))
    (let* ((phase (and session (agent-chat--live-p session) (agent-chat--session-phase session)))
           (failure
            (when (and (eq phase 'failed)
                       (buffer-live-p (agent-chat--session-buffer session)))
              (with-current-buffer (agent-chat--session-buffer session)
                (cl-loop with reason for entry in (agent-chat--state-history agent-chat--state)
                         when (eq (plist-get entry :role) 'error)
                         do (setq reason (plist-get entry :text))
                         finally return reason)))))
      (user-error
       "%s; %s"
       (or failure (format "Agent is %s" (or phase 'disconnected)))
       (cond
        ((agent-chat--retry-load-startup session)
         (if (agent-chat--fork-supported-p session)
             "use C-c C-f to fork or ? r to retry the same session"
           "use ? r to retry the same session"))
        ((and (eq phase 'authentication-required)
              (length> (agent-chat--session-auth-methods session) 0))
         "open ? a to authenticate")
        ((memq phase '(connecting selecting authenticating))
         "wait for the current connection or session selection to finish")
        (t "open ? s to choose a saved session or ? r to start a new session"))))))

(defun agent-chat--request (session request success &optional failure timeout)
  "Send REQUEST on SESSION; invoke SUCCESS or FAILURE exactly once.
TIMEOUT bounds connection operations, never a human approval or model turn.
Callbacks from closed/replaced sessions cannot modify the current session."
  (let ((done nil) timer)
    (cl-labels
        ((settle (callback value)
           (unless done
             (setq done t)
             (when timer
               (cancel-timer timer)
               (setf (agent-chat--session-timers session)
                     (delq timer (agent-chat--session-timers session))))
             (when (agent-chat--live-p session)
               (with-current-buffer (agent-chat--session-buffer session)
                 (funcall callback value)))))
         (failed (value)
           (settle (or failure
                       (lambda (err) (agent-chat--connection-error session err))) value)))
      (when timeout
        (setq timer (run-at-time timeout nil
                                 (lambda () (failed '((message . "ACP request timed out"))))))
        (push timer (agent-chat--session-timers session)))
      (condition-case err
          (progn
            (let ((default-directory (agent-chat--session-cwd session)))
              (acp-send-request
               :client (agent-chat--session-client session)
               :buffer (agent-chat--session-buffer session) :request request
               :on-success (lambda (value) (settle success value))
               :on-failure (lambda (value &optional _raw) (failed value))))
            ;; acp.el fails in-flight requests on exit.  Observe idle exits too.
            (when-let* ((process (map-elt (agent-chat--session-client session) :process))
                        ((processp process))
                        ((not (agent-chat--session-sentinel-installed session))))
              (setf (agent-chat--session-sentinel-installed session) t)
              (set-process-query-on-exit-flag process nil)
              (add-function :after (process-sentinel process)
                            (lambda (proc _event)
                              (when (and (memq (process-status proc) '(exit signal))
                                         (agent-chat--live-p session))
                                (with-current-buffer (agent-chat--session-buffer session)
                                  (agent-chat--queue-pause "Agent process exited"))
                                (agent-chat--finish-turn
                                 session '(:type error :text "Agent process exited"))
                                (setf (agent-chat--session-phase session) 'disconnected)
                                (agent-chat--refresh-session session))))))
        (error (failed (error-message-string err)))))))

(defun agent-chat--new-session (session)
  "Create a new ACP session after SESSION's handshake/authentication."
  (agent-chat--display-session session)
  (setf (agent-chat--session-phase session) 'connecting)
  (agent-chat--request
   session (acp-make-session-new-request :cwd (agent-chat--session-cwd session))
   (lambda (response)
     (if (not (stringp (map-elt response 'sessionId)))
         (agent-chat--connection-error session "Agent returned no session ID")
       (setf (agent-chat--session-id session) (map-elt response 'sessionId)
             (agent-chat--session-model session) (map-elt response 'models)
             (agent-chat--session-mode session) (map-elt response 'modes)
             (agent-chat--session-config-options session) (map-elt response 'configOptions)
             (agent-chat--session-phase session) 'ready)
       (let ((pending (agent-chat--session-pending-updates session)))
         (setf (agent-chat--session-pending-updates session) nil)
         (dolist (notification (nreverse pending))
           (agent-chat--notification session notification)))
       (agent-chat--refresh-session session)))
   (lambda (err)
     (if (and (agent-chat--session-auth-methods session)
              (or (equal (map-elt err 'code) -32000)
                  (string-match-p "[Aa]uth\\|[Ll]ogin\\|[Ss]ign.in"
                                  (agent-chat--error-text err))))
         (progn
           (setf (agent-chat--session-phase session) 'authentication-required)
           (agent-chat--notice session
                                  "Authentication required: open ? → Authenticate"))
       (agent-chat--connection-error session err)))
   agent-chat-connect-timeout))

(defun agent-chat--replay-update (session update)
  "Import historical ACP UPDATE into SESSION; render when loading finishes."
  (with-current-buffer (agent-chat--session-buffer session)
    (let* ((state agent-chat--state)
           (kind (map-elt update 'sessionUpdate))
           (role (pcase kind
                   ("user_message_chunk" 'user)
                   ("agent_message_chunk" 'assistant)
                   ("agent_thought_chunk" 'reasoning)))
           (id (map-elt update 'messageId)))
      (cond
       (role
        (let* ((text (agent-chat--content-text (map-elt update 'content) (eq role 'user)))
               (last (car (last (agent-chat--state-history state)))))
          (if (and (eq (plist-get last :role) role)
                   (equal (plist-get last :message-id) id))
              (setf (plist-get last :text) (concat (plist-get last :text) text))
            (setf (agent-chat--state-history state)
                  (append (agent-chat--state-history state)
                          (list (list :role role :text text
                                      :message-id id)))))))
       ((member kind '("tool_call" "tool_call_update"))
        (when-let* ((tool-id (map-elt update 'toolCallId)))
          (let* ((tool (copy-tree (alist-get tool-id (agent-chat--session-tools session)
                                              nil nil #'equal)))
                 (entry (seq-find (lambda (item) (equal (plist-get item :tool-id) tool-id))
                                  (agent-chat--state-history state))))
            (dolist (pair update) (setf (alist-get (car pair) tool) (cdr pair)))
            (setf (alist-get tool-id (agent-chat--session-tools session) nil nil #'equal) tool)
            (unless entry
              (setq entry (list :role 'tool :text "" :tool-id tool-id
                                :tool-status nil :detail nil))
              (setf (agent-chat--state-history state)
                    (append (agent-chat--state-history state) (list entry))))
            (setf (plist-get entry :text)
                  (format "%s · %s" (or (map-elt tool 'title) "Tool")
                          (or (map-elt tool 'status) "pending"))
                  (plist-get entry :tool-status) (map-elt tool 'status)
                  (plist-get entry :detail) (agent-chat--tool-detail tool)))))))))

(defun agent-chat--load-session (session id &optional after-load)
  "Load historical ACP session ID into SESSION, including replayed updates."
  ;; Remember the choice before the request.  A rejected resume has a
  ;; recovery target, but is not an initialized, writable session.
  (setf (agent-chat--session-startup session)
        (plist-put (plist-put (copy-sequence (agent-chat--session-startup session))
                              :action 'load) :id id))
  (unless after-load (agent-chat--display-session session))
  (if (not (map-elt (agent-chat--session-capabilities session) 'loadSession))
      (agent-chat--connection-error session "Agent does not support session/load")
    (setf (agent-chat--session-phase session) 'connecting
          (agent-chat--session-replaying session) t)
    (with-current-buffer (agent-chat--session-buffer session)
      (setf (agent-chat--state-history agent-chat--state) nil
            (agent-chat--session-tools session) nil))
    (agent-chat--request
     session (acp-make-session-load-request :session-id id
                                            :cwd (agent-chat--session-cwd session)
                                            :mcp-servers [])
     (lambda (response)
       (if (and (map-elt response 'sessionId)
                (not (equal (map-elt response 'sessionId) id)))
           (agent-chat--connection-error session "Agent loaded a different session")
         (setf (agent-chat--session-id session) id
               (agent-chat--session-model session) (map-elt response 'models)
               (agent-chat--session-mode session) (map-elt response 'modes)
               (agent-chat--session-config-options session) (map-elt response 'configOptions))
         (let ((pending (agent-chat--session-pending-updates session)))
           (setf (agent-chat--session-pending-updates session) nil)
           (dolist (notification (nreverse pending))
             (agent-chat--notification session notification)))
         (setf (agent-chat--session-replaying session) nil)
         (if after-load (funcall after-load)
           (setf (agent-chat--session-phase session) 'ready)
           (agent-chat--refresh-session session))))
     (lambda (err)
       (if (and (not after-load) (eq (agent-chat--session-agent session) 'codex)
                (map-elt (agent-chat--session-client session) :codex-acp-state)
                (listp err) (equal (map-nested-elt err '(_meta codex-acp-backend)) "external"))
           (agent-chat--load-native-codex-session session id)
         (agent-chat--connection-error session err)))
     agent-chat-connect-timeout)))

(defun agent-chat--fork-supported-p (session)
  "Whether SESSION advertises forking and history loading."
  (and session
       (map-elt (agent-chat--session-capabilities session) 'loadSession)
       (map-contains-key (map-elt (agent-chat--session-capabilities session)
                                 'sessionCapabilities) 'fork)))

(defun agent-chat--fork-settings (session settings)
  "Restore captured model/effort SETTINGS, then finish SESSION's fork."
  (if (null settings)
      (progn
        (setf (agent-chat--session-phase session) 'ready)
        (agent-chat--display-session session)
        (agent-chat--refresh-session session))
    (let* ((setting (car settings))
           (option (car (agent-chat--selectable-options session (car setting) 'connecting))))
      (cond
       ((null option)
        (agent-chat--connection-error session (format "Fork setting unavailable: %s" (car setting))))
       ((equal (map-elt option 'currentValue) (cdr setting))
        (agent-chat--fork-settings session (cdr settings)))
       (t
        (condition-case err
            (agent-chat--set-option-value
             session (map-elt option 'id) (cdr setting)
             (lambda () (agent-chat--fork-settings session (cdr settings)))
             (lambda (error-data) (agent-chat--connection-error session error-data))
             'connecting)
          (user-error (agent-chat--connection-error session (error-message-string err)))))))))

(defun agent-chat--fork-session (session)
  "Fork SESSION's startup source using its newly initialized ACP client."
  (if (not (agent-chat--fork-supported-p session))
      (agent-chat--connection-error session "Agent must support session/fork and session/load")
    (let* ((startup (agent-chat--session-startup session))
           (source-id (plist-get startup :id)))
      (agent-chat--request
       session (acp-make-session-fork-request :session-id source-id
                                             :cwd (agent-chat--session-cwd session))
       (lambda (response)
         (let ((id (map-elt response 'sessionId)))
           (if (not (and (stringp id) (not (string-empty-p id)) (not (equal id source-id))))
               (agent-chat--connection-error session "Agent returned an invalid fork session ID")
             ;; Commit the new identity before loading.  A retry must never
             ;; issue another non-idempotent fork after this point.
             (setf (agent-chat--session-id session) id
                   (agent-chat--session-startup session)
                   (plist-put (plist-put (copy-sequence startup) :action 'load) :id id)
                   (agent-chat--session-pending-updates session) nil)
             (agent-chat--load-session
              session id (lambda () (agent-chat--fork-settings session (plist-get startup :settings)))))))
       nil agent-chat-connect-timeout))))

(defun agent-chat--load-native-codex-session (session id)
  "Continue native local Codex session ID using SESSION's external backend.
Native sessions retain their original Codex tools, which differ from the
Emacs tools persisted in bridge-created sessions."
  (when (file-remote-p (agent-chat--session-cwd session))
    (user-error "Native Codex sessions can only be resumed locally"))
  (with-current-buffer (agent-chat--session-buffer session)
    (agent-chat--disconnect session)
    (setq agent-chat--client-maker-override :external)
    (agent-chat--begin-connection 'codex (list :action 'load :id id))))

(defun agent-chat--session-title-label (title)
  "Return a plain single-line TITLE, or Untitled when it is absent."
  (let ((text (if (stringp title)
                  (string-trim
                   (replace-regexp-in-string "[[:space:][:cntrl:]]+" " "
                                             (substring-no-properties title)))
                "")))
    (if (string-empty-p text) "Untitled" text)))

(defun agent-chat--session-label (item)
  "Return a bounded, single-line history label for ITEM, retaining its ID."
  (format "%s · %s"
          (truncate-string-to-width (agent-chat--session-title-label (map-elt item 'title))
                                    60 nil nil "…")
          (map-elt item 'sessionId)))

(defun agent-chat--session-id-input (text)
  "Return a single session ID from TEXT, or signal a user error."
  (let ((id (and (stringp text) (string-trim (substring-no-properties text)))))
    (unless (and id (not (string-empty-p id)) (not (string-match-p "[[:cntrl:]]" id)))
      (user-error "Enter a nonblank, single-line session ID"))
    id))

(defun agent-chat--open-session-choice (session id item)
  "Open the selected ID for SESSION, using the backend metadata from ITEM."
  (with-current-buffer (agent-chat--session-buffer session)
    (if (and (eq (plist-get (agent-chat--session-startup session) :action) 'browse)
             agent-chat--startup-pending (zerop (buffer-size)) (null agent-chat--queue)
             (agent-chat--reuse-saved-session (agent-chat--session-cwd session)
                                             (agent-chat--session-agent session)
                                             agent-chat--scope id))
        (kill-buffer (agent-chat--session-buffer session))
      (agent-chat--display-session session t)
      (if (and (eq (agent-chat--session-agent session) 'codex)
               (equal (map-nested-elt item '(_meta codex-acp-backend)) "external"))
          (agent-chat--load-native-codex-session session id)
        (agent-chat--load-session session id)))))

(defun agent-chat--select-session (session sessions &optional include-new)
  "Offer SESSION's saved SESSIONS outside the ACP process callback.
With INCLUDE-NEW require a listed choice and include New session.
Otherwise accept a selection or a literal session ID."
  (let ((choices (mapcar (lambda (item) (cons (agent-chat--session-label item) item))
                         sessions)))
    (when include-new (push '("New session" . :new) choices))
    (setf (agent-chat--session-phase session) 'selecting)
    (agent-chat--refresh-session session)
    (run-at-time
     0 nil
     (lambda ()
       (when (agent-chat--live-p session)
         (condition-case err
             (let* ((table (lambda (input predicate action)
                             (if (eq action 'metadata)
                                 '(metadata (display-sort-function . identity)
                                            (cycle-sort-function . identity))
                               (complete-with-action action choices input predicate))))
                    (choice (completing-read
                             (if include-new "Codex session: " "Select session or enter ID: ")
                             table nil include-new nil nil (when include-new "New session")))
                    (item (cdr (assoc choice choices))))
               (when (agent-chat--live-p session)
                 (if (eq item :new)
                     (progn (agent-chat--display-session session t)
                            (agent-chat--new-session session))
                   (agent-chat--open-session-choice
                    session (or (map-elt item 'sessionId) (agent-chat--session-id-input choice)) item))))
           ((quit user-error)
            (when (agent-chat--live-p session)
              (with-current-buffer (agent-chat--session-buffer session)
                (if agent-chat--startup-pending
                    (kill-buffer (current-buffer))
                  (agent-chat--refresh-session session))))
            (when (eq (car err) 'user-error) (message "%s" (error-message-string err))))))))))

(defun agent-chat--list-history (session &optional cursor include-new sessions)
  "Collect all of SESSION's project history, then offer it in one picker.
CURSOR continues pagination; SESSIONS holds earlier pages.  INCLUDE-NEW
adds New session and falls back to it if session/list is unsupported."
  (if (not (map-contains-key (map-elt (agent-chat--session-capabilities session)
                                    'sessionCapabilities)
                            'list))
      (cond (include-new (agent-chat--new-session session))
            ((map-elt (agent-chat--session-capabilities session) 'loadSession)
             (agent-chat--select-session session nil))
            (t (agent-chat--connection-error session "Agent does not support session/list")))
    (agent-chat--request
     session (acp-make-session-list-request :cwd (agent-chat--session-cwd session)
                                            :cursor cursor)
     (lambda (response)
       (let ((sessions (append sessions (append (map-elt response 'sessions) nil)))
             (next (map-elt response 'nextCursor)))
         (if next
             (agent-chat--list-history session next include-new sessions)
           (agent-chat--select-session session sessions include-new))))
     (when (or include-new (map-elt (agent-chat--session-capabilities session) 'loadSession))
       (lambda (error-data)
         (agent-chat--notice session (format "Could not list saved sessions: %s"
                                            (agent-chat--error-text error-data)))
         (agent-chat--select-session session sessions include-new)))
     agent-chat-connect-timeout)))

(defun agent-chat--connect (session)
  "Subscribe before initializing SESSION's ACP client."
  (let ((client (agent-chat--session-client session))
        (buffer (agent-chat--session-buffer session)))
    (acp-subscribe-to-notifications
     :client client :buffer buffer
     :on-notification (lambda (event)
                        (when (agent-chat--live-p session)
                          (agent-chat--notification session event))))
    (acp-subscribe-to-requests
     :client client :buffer buffer
     :on-request (lambda (request)
                   (when (agent-chat--live-p session)
                     (agent-chat--incoming-request session request))))
    (acp-subscribe-to-errors
     :client client :buffer buffer
     :on-error (lambda (err)
                 (agent-chat--notice session (agent-chat--error-text err)))))
  (agent-chat--request
   session (acp-make-initialize-request
            :protocol-version 1
            :client-info '((name . "agent-chat") (version . "0.1.0")))
   (lambda (response)
     (if (not (equal (map-elt response 'protocolVersion) 1))
         (agent-chat--connection-error session "Unsupported ACP protocol version")
       (setf (agent-chat--session-capabilities session) (map-elt response 'agentCapabilities)
             (agent-chat--session-auth-methods session) (map-elt response 'authMethods))
       (when (eq (map-nested-elt response '(agentCapabilities _meta codexRateLimits)) t)
         (with-current-buffer (agent-chat--session-buffer session)
           (agent-chat-quota--start (agent-chat--session-client session))))
       (let ((startup (agent-chat--session-startup session)))
         (pcase (plist-get startup :action)
           ('select
            (if (eq (map-elt (agent-chat--session-capabilities session) 'loadSession) t)
                (agent-chat--list-history session nil t)
              (agent-chat--new-session session)))
           ('browse (agent-chat--list-history session))
           ('load (agent-chat--load-session
                   session (plist-get startup :id)
                   (when (plist-get startup :forked-from)
                     (lambda () (agent-chat--fork-settings session (plist-get startup :settings))))))
           ('fork (agent-chat--fork-session session))
           (_ (agent-chat--new-session session))))))
   nil agent-chat-connect-timeout))

(defun agent-chat--content-text (content &optional inline-images)
  "Return literal display text for ACP CONTENT, without fetching resources.
With INLINE-IMAGES, retain valid image data in a previewable attachment."
  (pcase (map-elt content 'type)
    ("text" (substring-no-properties (or (map-elt content 'text) "")))
    ("resource" (or (map-nested-elt content '(resource text))
                    (format "[Resource: %s]" (map-nested-elt content '(resource uri)))))
    ("resource_link" (format "%s (%s)" (or (map-elt content 'name) "Resource")
                              (map-elt content 'uri)))
    ("image" (if inline-images
                 (condition-case nil (agent-chat--image-token content)
                   (user-error "[Invalid image]"))
               "[image content]"))
    ("audio" "[audio content]")
    (_ "")))

(defun agent-chat--tool-detail (tool)
  "Format ACP TOOL details, including text output and proposed diffs."
  (string-join
   (delq nil
         (append
          (when-let* ((input (map-elt tool 'rawInput)))
            (list (format "%S" input)))
          (mapcar
           (lambda (item)
             (pcase (map-elt item 'type)
               ("diff" (format "%s\n--- before\n%s\n+++ after\n%s"
                               (or (map-elt item 'path) "Diff")
                               (or (map-elt item 'oldText) "")
                               (or (map-elt item 'newText) "")))
               (_ (when-let* ((content (map-elt item 'content)))
                    (agent-chat--content-text content)))))
           (append (map-elt tool 'content) nil)))) "\n"))

(defun agent-chat--tool-update (session update)
  "Merge a partial tool UPDATE into SESSION, retaining omitted fields."
  (when-let* ((id (map-elt update 'toolCallId)))
    (let ((tool (copy-tree (alist-get id (agent-chat--session-tools session)
                                    nil nil #'equal))))
      (dolist (pair update) (setf (alist-get (car pair) tool) (cdr pair)))
      (setf (alist-get id (agent-chat--session-tools session) nil nil #'equal) tool)
      (when-let* ((emit (agent-chat--session-emit session)))
        (funcall emit (list :type 'tool :id id
                            :text (format "%s · %s" (or (map-elt tool 'title) "Tool")
                                          (or (map-elt tool 'status) "pending"))
                            :status (map-elt tool 'status)
                            :detail (agent-chat--tool-detail tool)))))))

(defun agent-chat--notification (session notification)
  "Reduce NOTIFICATION for SESSION; ignore other sessions' updates."
  (when (agent-chat--live-p session)
    (with-current-buffer (agent-chat--session-buffer session)
      (agent-chat-quota--notification (agent-chat--session-client session)
                                     (map-elt notification 'method)
                                     (map-elt notification 'params))))
  (when (equal (map-elt notification 'method) "session/update")
    (let* ((params (map-elt notification 'params))
           (id (map-elt params 'sessionId))
           (update (map-elt params 'update))
           (kind (map-elt update 'sessionUpdate))
           (emit (agent-chat--session-emit session)))
      (cond
       ((and (not (agent-chat--session-id session))
             (eq (agent-chat--session-phase session) 'connecting))
        (push notification (agent-chat--session-pending-updates session)))
       ((equal id (agent-chat--session-id session))
        (when (agent-chat--session-replaying session)
          (agent-chat--replay-update session update))
        (pcase kind
          ((or "agent_message_chunk" "agent_thought_chunk")
           (when (and emit (not (agent-chat--session-cancelling session)))
             (funcall emit (list :type (if (equal kind "agent_thought_chunk") 'reasoning 'delta)
                                 :message-id (map-elt update 'messageId)
                                 :text (agent-chat--content-text (map-elt update 'content))))))
          ((or "tool_call" "tool_call_update")
           (when (and emit (not (agent-chat--session-cancelling session)))
             (agent-chat--tool-update session update)))
          ("available_commands_update"
           (setf (agent-chat--session-commands session) (map-elt update 'availableCommands)))
          ("current_mode_update"
           (setf (alist-get 'currentModeId (agent-chat--session-mode session))
                 (map-elt update 'currentModeId)))
          ("config_option_update"
           (setf (agent-chat--session-config-options session) (map-elt update 'configOptions)))
          ("session_info_update"
           (when-let* ((title (map-elt update 'title)))
             (setf (agent-chat--session-title session) title)))
          ("plan"
           (when emit
             (funcall emit
                      (list :type 'tool :id "acp-plan" :text "Plan" :status "in_progress"
                            :detail (mapconcat
                                     (lambda (entry) (format "%s · %s"
                                                             (map-elt entry 'status)
                                                             (map-elt entry 'content)))
                                     (append (map-elt update 'entries) nil) "\n"))))))
        (unless (or (agent-chat--session-replaying session)
                    (member kind '("agent_message_chunk" "agent_thought_chunk"
                                   "tool_call" "tool_call_update" "plan")))
          (agent-chat--refresh-session session)))))))

(defun agent-chat--permission-entry (session request &optional resolved)
  "Update the visible permission REQUEST in SESSION; RESOLVED is its result."
  (with-current-buffer (agent-chat--session-buffer session)
    (let* ((id (map-elt request 'id))
           (tool (map-nested-elt request '(params toolCall)))
           (entry (seq-find (lambda (item) (and (eq (plist-get item :role) 'permission)
                                              (equal (plist-get item :request-id) id)))
                            (agent-chat--state-history agent-chat--state))))
      (unless entry
        (setq entry (list :role 'permission :request-id id :text "" :detail nil
                          :options nil :resolved nil))
        (setf (agent-chat--state-history agent-chat--state)
              (append (agent-chat--state-history agent-chat--state) (list entry))))
      (setf (plist-get entry :text) (or (map-elt tool 'title) "Approval required")
            (plist-get entry :detail) (agent-chat--tool-detail tool)
            (plist-get entry :options) (append (map-nested-elt request '(params options)) nil)
            (plist-get entry :resolved) resolved)
      (agent-chat--publish))))

(defun agent-chat--incoming-request (session request)
  "Handle an incoming REQUEST using SESSION's explicitly supported capabilities."
  (let ((client (agent-chat--session-client session))
        (id (map-elt request 'id)))
    (if (equal (map-elt request 'method) "session/request_permission")
        (if (or (not (equal (map-nested-elt request '(params sessionId))
                           (agent-chat--session-id session)))
                (not (agent-chat--session-busy session))
                (agent-chat--session-cancelling session))
            (acp-send-response
             :client client :response (acp-make-session-request-permission-response
                                       :request-id id :cancelled t))
          (unless (assoc id (agent-chat--session-permissions session))
            (setf (agent-chat--session-permissions session)
                  (append (agent-chat--session-permissions session) (list (cons id request))))
            (agent-chat--tool-update session (map-nested-elt request '(params toolCall)))
            (agent-chat--permission-entry session request)
            (unless (agent-chat--permission-maybe-bypass session request)
              (agent-chat--permission-auto-enqueue session request))))
      ;; Unsupported host capabilities were not advertised at initialization.
      (acp-send-response
       :client client :response `((:request-id . ,id)
                                  (:error . ,(acp-make-error
                                              :code -32601 :message "Client method not supported")))))))

(defun agent-chat--respond (session id &optional option-id)
  "Answer pending permission ID in SESSION with OPTION-ID, or cancel it.
Only a still-pending request and an exact agent-supplied option are accepted."
  (when-let* ((request (cdr (assoc id (agent-chat--session-permissions session)))))
    (when (and option-id
               (not (seq-find (lambda (item) (equal option-id (map-elt item 'optionId)))
                              (map-nested-elt request '(params options)))))
      (user-error "Unknown permission option"))
    ;; Remove before delivery: an in-process agent can finish synchronously
    ;; from its response handler and must not answer this request twice.
    (setf (agent-chat--session-permissions session)
          (assoc-delete-all id (agent-chat--session-permissions session)))
    (condition-case err
        (acp-send-response
         :client (agent-chat--session-client session)
         :response (acp-make-session-request-permission-response
                    :request-id id :option-id option-id :cancelled (not option-id)))
      (error
       (push (cons id request) (agent-chat--session-permissions session))
       (signal (car err) (cdr err))))
    (agent-chat--permission-auto-forget session request)
    (when (agent-chat--live-p session)
      (agent-chat--permission-entry session request (or option-id "cancelled")))))

(defun agent-chat--finish-turn (session event)
  "Finish SESSION's active turn with EVENT; discard any late callbacks."
  (when (and (agent-chat--live-p session) (agent-chat--session-busy session))
    (with-current-buffer (agent-chat--session-buffer session)
      (let ((emit (agent-chat--session-emit session))
            (normal (and (eq (plist-get event :type) 'done)
                         (equal (plist-get event :stop-reason) "end_turn")
                         (not (agent-chat--session-cancelling session)))))
        ;; Close the turn before answering approvals: in-process clients may
        ;; invoke completion again synchronously from their response handler.
        (setf (agent-chat--session-busy session) nil
              (agent-chat--session-cancelling session) nil
              (agent-chat--session-emit session) nil)
        (unless normal
          (agent-chat--queue-pause
           (or (plist-get event :text) (plist-get event :stop-reason)
               "Turn stopped")))
        (dolist (entry (copy-sequence (agent-chat--session-permissions session)))
          (ignore-errors (agent-chat--respond session (car entry))))
        (when emit (funcall emit event))
        (agent-chat--queue-changed)
        (when normal (agent-chat--queue-schedule))))))

(defun agent-chat--cancel-turn (session)
  "Cancel SESSION without allowing another turn until cancellation settles."
  (when (and (agent-chat--live-p session) (agent-chat--session-busy session)
             (not (agent-chat--session-cancelling session)))
    (setf (agent-chat--session-cancelling session) t)
    (dolist (entry (copy-sequence (agent-chat--session-permissions session)))
      (agent-chat--respond session (car entry)))
    (acp-send-notification
     :client (agent-chat--session-client session)
     :notification (acp-make-session-cancel-notification
                    :session-id (agent-chat--session-id session)))
    (agent-chat--refresh-session session)))

(defun agent-chat--send (session request emit)
  "Submit REQUEST through SESSION and deliver semantic events through EMIT."
  (agent-chat--require-ready session)
  (when (agent-chat--session-busy session)
    (user-error "Previous turn is still active; wait or reconnect"))
  (when-let* ((directory (plist-get request :directory)))
    (unless (equal (agent-chat--workspace-key directory)
                   (agent-chat--workspace-key (agent-chat--session-cwd session)))
      (user-error "Queued prompt belongs to a different workspace")))
  (agent-chat--check-images (plist-get request :prompt) session)
  (setf (agent-chat--session-busy session) t
        (agent-chat--session-tools session) nil
        (agent-chat--session-emit session) emit)
  (agent-chat--request
   session (acp-make-session-prompt-request
            :session-id (agent-chat--session-id session)
            :prompt (agent-chat--prompt-content (plist-get request :prompt)))
   (lambda (response)
     (agent-chat--finish-turn
      session (list :type 'done :stop-reason (map-elt response 'stopReason))))
   (lambda (err)
     (agent-chat--finish-turn
      session (list :type 'error :text (agent-chat--error-text err)))))
  (lambda () (agent-chat--cancel-turn session)))

(defun agent-chat--disconnect (session)
  "Release SESSION's callbacks, approvals, timers and owned ACP client."
  (when (and session (not (agent-chat--session-stopped session)))
    (agent-chat--permission-clear-bypass session)
    (when (agent-chat--live-p session)
      (with-current-buffer (agent-chat--session-buffer session)
        (agent-chat-quota--stop)
        (agent-chat--queue-pause "Disconnected")
        (dolist (entry agent-chat--queue)
          (setf (plist-get entry :steering) nil))))
    (setf (agent-chat--session-stopped session) t
          (agent-chat--session-phase session) 'disconnected)
    (agent-chat--permission-auto-forget session)
    (dolist (entry (copy-sequence (agent-chat--session-permissions session)))
      (ignore-errors (agent-chat--respond session (car entry))))
    (when (and (agent-chat--session-busy session) (agent-chat--session-id session)
               (not (agent-chat--session-cancelling session)))
      (ignore-errors
        (acp-send-notification
         :client (agent-chat--session-client session)
         :notification (acp-make-session-cancel-notification
                        :session-id (agent-chat--session-id session)))))
    (mapc #'cancel-timer (agent-chat--session-timers session))
    (setf (agent-chat--session-timers session) nil
          (agent-chat--session-emit session) nil)
    (when (agent-chat--session-client session)
      (acp-shutdown :client (agent-chat--session-client session)))))

(defun agent-chat--config-label (session category)
  "Return SESSION's current value label for config CATEGORY."
  (when session
    (let* ((option (seq-find (lambda (item) (equal (map-elt item 'category) category))
                             (agent-chat--session-config-options session)))
           (value (map-elt option 'currentValue))
           (choices (agent-chat--option-values option))
           (choice (seq-find (lambda (item) (equal (map-elt item 'value) value)) choices)))
      (or (map-elt choice 'name) value))))

(defun agent-chat--model-label (session)
  "Return SESSION's current model label, preferring modern config options."
  (when session
    (or (agent-chat--config-label session "model")
        (map-elt (agent-chat--session-model session) 'currentModelId))))

(defun agent-chat--permission-mode-label (session)
  "Return SESSION's effective permission label, or nil if unavailable."
  (when session
    (or (when (agent-chat--permission-bypass-p session) "Bypass (session)")
        (agent-chat--config-label session "mode")
        (let* ((modes (agent-chat--session-mode session))
               (id (map-elt modes 'currentModeId))
               (mode (seq-find (lambda (item) (equal (map-elt item 'id) id))
                               (map-elt modes 'availableModes))))
          (or (map-elt mode 'name) id)))))

(defun agent-chat--spinner-running-p ()
  "Whether this conversation should animate its running indicator."
  (and (derived-mode-p 'agent-chat-mode)
       agent-chat--state
       (eq (agent-chat--state-status agent-chat--state) 'running)
       (or (null agent-chat--session)
           (and (not (agent-chat--session-stopped agent-chat--session))
                (eq (agent-chat--session-phase agent-chat--session) 'ready)
                (not (agent-chat--session-permissions agent-chat--session))
                (not (agent-chat--session-cancelling agent-chat--session))))))

(defun agent-chat--stop-spinner ()
  "Stop this buffer's header animation and reset its frame."
  (when (timerp agent-chat--spinner-timer)
    (cancel-timer agent-chat--spinner-timer))
  (setq agent-chat--spinner-timer nil agent-chat--spinner-index 0))

(defun agent-chat--spinner-tick (buffer)
  "Advance BUFFER's header only, leaving transcript and draft untouched."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (agent-chat--spinner-running-p)
          (setq agent-chat--spinner-index
                (mod (1+ agent-chat--spinner-index) (length agent-chat--spinner-frames)))
        (agent-chat--stop-spinner))
      (force-mode-line-update t))))

(defun agent-chat--sync-spinner ()
  "Keep exactly one header timer while this conversation is running."
  (if (agent-chat--spinner-running-p)
      (unless (timerp agent-chat--spinner-timer)
        (setq agent-chat--spinner-timer
              (run-at-time 0.12 0.12 #'agent-chat--spinner-tick (current-buffer))))
    (agent-chat--stop-spinner)))

(defun agent-chat--conversation-status (&optional text-only omit-queue)
  "Return this conversation's styled status.
With TEXT-ONLY, omit its icon.  OMIT-QUEUE keeps the top header focused on
execution; the draft header displays the pending queue."
  (cl-flet ((label (icon text &optional face)
              (let ((value (if text-only text (concat icon " " text))))
                (if face (propertize value 'face face) value))))
    (concat
     (let ((session agent-chat--session))
       (cond
        ((and session (agent-chat--session-permissions session))
         (label "?" (format "Approval %d" (length (agent-chat--session-permissions session)))
                '(warning bold)))
        ((and session (agent-chat--session-cancelling session))
         (label "■" "Cancelling" '(warning bold)))
        ((and session (not (eq (agent-chat--session-phase session) 'ready)))
         (label "○" (symbol-name (agent-chat--session-phase session))))
        ((null agent-chat--state) (label "○" "Not connected" 'shadow))
        (t (pcase (agent-chat--state-status agent-chat--state)
             ('running (label (aref agent-chat--spinner-frames agent-chat--spinner-index)
                              "Running" '(font-lock-function-name-face bold)))
             ('cancelled (label "■" "Interrupted" '(warning bold)))
             ('failed (label "!" "Failed" 'agent-chat-error))
             (_ (label "○" "Ready" '(success bold)))))))
     (when (and (not omit-queue) agent-chat--queue)
       (propertize
        (format " · Queue %d%s" (length agent-chat--queue)
                (if agent-chat--queue-paused " paused" ""))
        'face 'agent-chat-queue-title)))))

(defun agent-chat--header-text (text)
  "Make dynamic TEXT safe for header-line percent expansion."
  (replace-regexp-in-string "%" "%%" text t t))

(defun agent-chat--directory-label ()
  "Return a short workspace label while retaining the remote host."
  (let* ((directory (directory-file-name default-directory))
         (name (file-name-nondirectory (file-local-name directory)))
         (host (file-remote-p directory 'host)))
    (concat (when (eq agent-chat--scope 'host) "General · ")
            (when host (concat host ":"))
            (if (string-empty-p name) "/" name))))

(defun agent-chat--header-line ()
  "Show execution context and status using cached session information."
  (let* ((session agent-chat--session)
         (model (agent-chat--model-label session))
         (permission-mode (agent-chat--permission-mode-label session))
         (effort (and session
                      (map-elt
                       (seq-find (lambda (option)
                                   (equal (map-elt option 'category) "thought_level"))
                                 (agent-chat--session-config-options session))
                       'currentValue)))
         (status (agent-chat--conversation-status nil t))
         (width (max 1 (window-body-width)))
         (status (if (< width 28) (truncate-string-to-width status 1) status))
         (left (string-join
                (delq nil
                      (list agent-chat--label
                            (when model
                              (concat model (when (and effort (not (string-empty-p effort)))
                                              (concat " / " effort))))
                            (when permission-mode
                              (propertize permission-mode
                                          'face (if (agent-chat--permission-bypass-p session)
                                                    '(warning bold)
                                                  '(font-lock-keyword-face bold))))
                            (agent-chat--directory-label))) " · "))
         (text (concat " " (truncate-string-to-width
                            left (max 1 (- width (string-width status) 4)) nil nil "…"))))
    (add-face-text-property 0 (length text) 'agent-chat-header t text)
    (list (agent-chat--header-text
           (propertize text 'help-echo (concat left "\n" default-directory)))
          (propertize " " 'display `(space :align-to (- right ,(+ 1 (string-width status)))))
          status " ")))

;;;; Native draft and paired message window

(defcustom agent-chat-prompt-height 6
  "Maximum native screen lines reserved for a growing prompt."
  :type 'integer :group 'agent-chat)

(defcustom agent-chat-prompt-min-height 2
  "Minimum native screen lines reserved for the prompt."
  :type 'integer :group 'agent-chat)

(defvar-local agent-chat--drawing nil)
(defvar-local agent-chat--history-buffer nil
  "The read-only message buffer owned by this conversation.")
(defvar-local agent-chat--owner nil
  "Conversation buffer owning this message buffer.")
(defvar-local agent-chat--history-start 1)
(defvar-local agent-chat--history-follow t)
(defvar-local agent-chat--history-render-key nil)
(defvar-local agent-chat--queue-preview nil
  "Display-only queue preview before the native draft.")

;;;; Native image attachments

(defun agent-chat--image-token (content)
  "Make one editable attachment character for validated image CONTENT.
Its text property owns the bytes, so native deletion, undo and queue editing
share the ordinary draft lifecycle without a separate attachment registry."
  (acp-image-data content)
  (propertize
   "\ufffc" 'agent-chat-image
   (list (cons 'type "image") (cons 'mimeType (map-elt content 'mimeType))
         (cons 'data (map-elt content 'data)))
   'display (format "[Image · %s]" (upcase (substring (map-elt content 'mimeType) 6)))
   'help-echo "C-c C-v: preview image; delete or undo to remove"
   'rear-nonsticky t))

(defun agent-chat--prompt-content (prompt)
  "Split native PROMPT into ordered ACP text and image content blocks."
  (let ((position 0) content)
    (while (< position (length prompt))
      (if-let* ((image (get-text-property position 'agent-chat-image prompt)))
          (progn
            (unless (= (aref prompt position) #xfffc)
              (user-error "Invalid image attachment; remove it and paste again"))
            (acp-image-data image)
            (push (copy-tree image) content)
            (cl-incf position))
        (let* ((end (next-single-property-change
                     position 'agent-chat-image prompt (length prompt)))
               (text (substring-no-properties prompt position end)))
          (when (string-search "\ufffc" text)
            (user-error "Image attachment lost its data; remove it and paste again"))
          (push `((type . "text") (text . ,text)) content)
          (setq position end))))
    (nreverse content)))

(defun agent-chat--image-copy (prompt)
  "Copy PROMPT, retaining only owned image properties and plain text."
  (mapconcat (lambda (content) (agent-chat--content-text content t))
             (agent-chat--prompt-content prompt) ""))

(defun agent-chat--check-images (prompt session)
  "Validate PROMPT and require SESSION to support any image attachments."
  (let ((content (agent-chat--prompt-content prompt)))
    (when (and session
               (seq-some (lambda (item) (equal (map-elt item 'type) "image")) content)
               (not (eq (map-nested-elt (agent-chat--session-capabilities session)
                                       '(promptCapabilities image)) t)))
      (user-error "This agent has not advertised image input support"))))

(defun agent-chat--insert-image (type data)
  "Handle native `yank-media' image TYPE and binary DATA in this draft."
  (unless (derived-mode-p 'agent-chat-mode)
    (user-error "Not in an Agent Chat draft"))
  (let ((token (agent-chat--image-token
                (acp-image-from-data (symbol-name type) data))))
    (agent-chat--check-images token agent-chat--session)
    (agent-chat-focus-prompt)
    (atomic-change-group
      (when (use-region-p) (delete-region (region-beginning) (region-end)))
      (insert token))))

(defun agent-chat--enable-images ()
  "Register native clipboard image support in a composer."
  ;; Refresh already-created keymaps on source reload as well.
  (local-set-key (kbd "C-c C-y") #'agent-chat-paste-image)
  (local-set-key (kbd "C-c C-v") #'agent-chat-preview-image)
  (yank-media-handler (mapcar (lambda (item) (intern (car item))) acp-image-mime-types)
                      #'agent-chat--insert-image)
  (setq-local yank-media-preferred-types
              (mapcar (lambda (item) (intern (car item))) acp-image-mime-types)))

(defun agent-chat--clipboard-png ()
  "Read the local macOS clipboard as PNG bytes using `pngpaste'.
This is the converter used by Org and agent-shell for TIFF-only clipboards.
Run locally even for a remote workspace, without creating a file."
  (let ((program (executable-find "pngpaste")))
    (unless program
      (user-error "TIFF clipboard images require pngpaste to convert them to PNG"))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let* ((default-directory temporary-file-directory)
             (coding-system-for-read 'binary)
             (coding-system-for-write 'binary)
             (status (call-process program nil '(t nil) nil "-")))
        (unless (eq status 0)
          (user-error "pngpaste could not read the clipboard image (%s)" status)))
      (buffer-string))))

(defmacro agent-chat--with-command-buffer (buffer &rest body)
  "Run BODY in BUFFER, honoring any explicit window selection it makes."
  (declare (indent 1) (debug t))
  (cl-with-gensyms (window shown)
    `(let ((,window (selected-window)) (,shown (window-buffer (selected-window))))
       (unwind-protect
           (with-current-buffer ,buffer ,@body)
         (unless (and (eq ,window (selected-window))
                      (eq ,shown (window-buffer (selected-window))))
           (set-buffer (window-buffer (selected-window))))))))

(defmacro agent-chat--with-conversation (&rest body)
  "Run BODY in the owning conversation, without selecting its window."
  (declare (indent 0) (debug t))
  `(agent-chat--with-command-buffer (agent-chat--require-conversation) ,@body))

(defun agent-chat--require-conversation (&optional allow-outside)
  "Resolve this command's conversation, rejecting stale menu contexts.
ALLOW-OUTSIDE also accepts ordinary buffers for global startup commands."
  (let* ((context (and (eq transient-current-command 'agent-chat-help)
                       (transient-scope 'agent-chat-help)))
         (owner (if context (car context) (agent-chat--conversation-buffer))))
    (when (and allow-outside (not context) (not owner)
               (not (derived-mode-p 'agent-chat-mode 'agent-chat-history-mode
                                    'agent-chat-queue-mode)))
      (setq owner (current-buffer)))
    (unless (and (buffer-live-p owner)
                 (or allow-outside (with-current-buffer owner (derived-mode-p 'agent-chat-mode))))
      (user-error "Not in a live Agent Chat conversation"))
    (when (and context
               (not (eq (cdr context) (buffer-local-value 'agent-chat--session owner))))
      (user-error "This conversation has reconnected; reopen the menu"))
    owner))

(defun agent-chat-paste-image ()
  "Paste a clipboard image into this draft without sending it.
Use native `yank-media', or `pngpaste' for a macOS TIFF-only clipboard.
Neither path creates a temporary file."
  (interactive)
  (agent-chat--require-conversation)
  (agent-chat-focus-prompt)
  (unless (display-graphic-p) (user-error "Clipboard images require a graphical frame"))
  (agent-chat--enable-images)
  (let ((types (gui-get-selection 'CLIPBOARD 'TARGETS)))
    (if (and (eq system-type 'darwin)
             (seq-contains-p types 'image/tiff)
             (not (seq-some (lambda (item)
                              (seq-contains-p types (intern (car item))))
                            acp-image-mime-types)))
        (agent-chat--insert-image 'image/png (agent-chat--clipboard-png))
      (yank-media))))

(defun agent-chat-preview-image ()
  "Open the attachment at point in an image buffer, without writing a file."
  (interactive)
  (unless (display-images-p) (user-error "This display cannot preview images"))
  (let ((image (or (get-text-property (point) 'agent-chat-image)
                   (and (> (point) (point-min))
                        (get-text-property (1- (point)) 'agent-chat-image)))))
    (unless image (user-error "No image attachment at point"))
    (let ((data (acp-image-data image))
          (buffer (generate-new-buffer "*Agent image*")) ready)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (set-buffer-multibyte nil)
              (insert data)
              (image-mode)
              (setq buffer-read-only t)
              (set-buffer-modified-p nil))
            (setq ready t)
            (pop-to-buffer buffer))
        (unless ready (kill-buffer buffer))))))

(defvar agent-chat-mode-map (make-sparse-keymap)
  "Conversation actions; native editing and movement remain inherited.")

(defvar agent-chat-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-chat--history-activate)
    (define-key map (kbd "?") #'agent-chat-help)
    (define-key map (kbd "C-c ?") #'agent-chat-help)
    (define-key map (kbd "C-c C-c") #'agent-chat-interrupt)
    (define-key map (kbd "C-c C-p") #'agent-chat-permissions)
    map))

(defvar agent-chat--reading-control-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'agent-chat--reading-click)
    map)
  "Controls on real message text; ordinary selection stays native.")

(defun agent-chat--install-history-bindings ()
  "Bind block navigation and folding in the reading pane, including Evil."
  (agent-chat--install-conversation-bindings agent-chat-history-mode-map)
  (define-key agent-chat-history-mode-map (kbd "?") #'agent-chat-help)
  (define-key agent-chat-history-mode-map (kbd "C-n") #'agent-chat-history-next-block)
  (define-key agent-chat-history-mode-map (kbd "C-p") #'agent-chat-history-previous-block)
  (define-key agent-chat-history-mode-map (kbd "TAB") #'agent-chat-history-toggle-fold)
  (define-key agent-chat-history-mode-map (kbd "<tab>") #'agent-chat-history-toggle-fold)
  (when (bound-and-true-p evil-local-mode)
    (evil-define-key* '(normal motion visual emacs) agent-chat-history-mode-map
      (kbd "?") #'agent-chat-help
      (kbd "C-n") #'agent-chat-history-next-block
      (kbd "C-p") #'agent-chat-history-previous-block
      (kbd "TAB") #'agent-chat-history-toggle-fold
      (kbd "<tab>") #'agent-chat-history-toggle-fold)
    (evil-normalize-keymaps)))

(defun agent-chat--install-conversation-bindings (map)
  "Install shared conversation commands in MAP."
  (dolist (binding '(("C-c C-c" . agent-chat-interrupt)
                     ("C-c C-y" . agent-chat-paste-image)
                     ("C-c C-v" . agent-chat-preview-image)
                     ("C-c ?" . agent-chat-help)
                     ("C-c RET" . agent-chat-set-option)
                     ("C-c <return>" . agent-chat-set-option)
                     ("C-c C-p" . agent-chat-permissions)
                     ("C-c C-n" . agent-chat-new)
                     ("C-c C-f" . agent-chat-fork)
                     ("C-c C-r" . agent-chat-rename-session)
                     ("C-c C-s" . agent-chat-submit)))
    (define-key map (kbd (car binding)) (cdr binding))))

(defun agent-chat--install-editing-bindings ()
  "Install application actions without replacing native movement commands."
  (agent-chat--install-conversation-bindings agent-chat-mode-map)
  (define-key agent-chat-mode-map (kbd "TAB") #'completion-at-point)
  (when (bound-and-true-p evil-local-mode)
    (evil-define-key* '(insert replace visual emacs) agent-chat-mode-map
      (kbd "C-g") #'evil-force-normal-state)
    (evil-define-key* 'normal agent-chat-mode-map
      (kbd "?") #'agent-chat-help
      (kbd "RET") #'agent-chat-submit
      (kbd "<return>") #'agent-chat-submit)))

(define-derived-mode agent-chat-mode text-mode "Agent/Chat"
  "Edit this conversation's draft in its native lower window."
  (buffer-enable-undo)
  (agent-chat--enable-images)
  (agent-chat--install-copy-filter)
  (setq-local revert-buffer-function #'agent-chat-revert-buffer
              truncate-lines nil
              left-margin-width 0
              scroll-margin 0
              scroll-conservatively 101
              header-line-format '(:eval (agent-chat--prompt-header)))
  (when (bound-and-true-p evil-mode) (evil-local-mode 1))
  (agent-chat--install-editing-bindings)
  (add-hook 'post-command-hook #'agent-chat--redraw nil t)
  (add-hook 'completion-at-point-functions #'agent-chat--complete-command nil t)
  (add-hook 'post-self-insert-hook #'agent-chat--trigger-completion nil t)
  (add-hook 'window-size-change-functions #'agent-chat--window-change nil t)
  (add-hook 'window-buffer-change-functions #'agent-chat--window-change nil t)
  (add-hook 'kill-buffer-query-functions #'agent-chat--queue-confirm-close nil t)
  (add-hook 'change-major-mode-hook #'agent-chat--stop-spinner nil t)
  (add-hook 'kill-buffer-hook #'agent-chat--cleanup nil t))

(define-derived-mode agent-chat-history-mode special-mode "Agent/Messages"
  "Read live messages with native movement, selection and scrolling."
  (setq-local truncate-lines nil
              revert-buffer-function #'agent-chat-revert-buffer
              mode-line-format nil
              left-margin-width 0
              scroll-margin 0
              scroll-conservatively 101
              buffer-undo-list t
              font-lock-defaults
              '(nil nil nil nil nil
                (font-lock-fontify-region-function . agent-chat--history-fontify)
                (font-lock-unfontify-region-function . ignore))
              font-lock-fontify-region-function #'agent-chat--history-fontify
              font-lock-unfontify-region-function #'ignore
              header-line-format
              '(:eval (when (buffer-live-p agent-chat--owner)
                        (with-current-buffer agent-chat--owner
                          (agent-chat--header-line)))))
  (agent-chat--install-copy-filter)
  (agent-chat--enable-embark)
  (add-hook 'evil-local-mode-hook #'agent-chat--install-history-bindings nil t)
  (when (bound-and-true-p evil-mode)
    (evil-local-mode 1)
    (evil-define-key* '(visual emacs) agent-chat-history-mode-map
      (kbd "C-g") #'evil-force-normal-state)
    (evil-define-key* '(normal motion) agent-chat-history-mode-map
      (kbd "RET") #'agent-chat--history-activate
      (kbd "<return>") #'agent-chat--history-activate
      (kbd "?") #'agent-chat-help)
    (evil-normal-state))
  (agent-chat--install-history-bindings)
  (add-hook 'post-command-hook #'agent-chat--history-post-command nil t)
  (add-hook 'window-scroll-functions #'agent-chat--history-scroll nil t)
  (add-hook 'window-size-change-functions #'agent-chat--window-change nil t)
  (add-hook 'kill-buffer-query-functions #'agent-chat--history-close-owner nil t))

(defun agent-chat--history-fontify (beg end _loudly)
  "Keep the renderer's faces between BEG and END through native JIT passes."
  `(jit-lock-bounds ,beg . ,end))

(defun agent-chat--conversation-buffer (&optional buffer)
  "Return the conversation owning BUFFER, or nil outside an Agent Chat view."
  (with-current-buffer (or buffer (current-buffer))
    (let ((owner (cond ((derived-mode-p 'agent-chat-mode) (current-buffer))
                       ((derived-mode-p 'agent-chat-history-mode) agent-chat--owner)
                       ((derived-mode-p 'agent-chat-queue-mode)
                        agent-chat--queue-owner))))
      (and (buffer-live-p owner)
           (with-current-buffer owner (derived-mode-p 'agent-chat-mode)) owner))))

(defun agent-chat--message-buffer ()
  "Return or create this conversation's private message buffer."
  (unless (buffer-live-p agent-chat--history-buffer)
    (let ((owner (current-buffer)) (directory default-directory)
          (faces (copy-tree face-remapping-alist)))
      (setq agent-chat--history-buffer
            (generate-new-buffer (format " *Agent Chat messages: %s*" (buffer-name))))
      (with-current-buffer agent-chat--history-buffer
        (agent-chat-history-mode)
        (setq agent-chat--owner owner default-directory directory
              face-remapping-alist faces))))
  agent-chat--history-buffer)

(defun agent-chat--prompt-header ()
  "Show the draft's destination, queue and cached remaining account quota."
  (let* ((width (max 1 (window-body-width)))
         (title (agent-chat--session-title-label
                 (and agent-chat--session (agent-chat--session-title agent-chat--session))))
         (queue (when agent-chat--queue
                  (format "Queue %d%s" (length agent-chat--queue)
                          (if agent-chat--queue-paused " paused" ""))))
         (quota (agent-chat-quota--label))
         (quota (if (and quota (> (string-width quota) (/ width 2)))
                    (agent-chat-quota--label t) quota))
         (quota (when quota (truncate-string-to-width quota (max 1 (- width 3)) nil nil "…")))
         (available (max 0 (- width (if quota (+ 3 (string-width quota)) 2))))
         (queue (when queue (truncate-string-to-width queue available nil nil "…")))
         (title-width (max 0 (- available (if queue (+ 3 (string-width queue)) 0))))
         (short (truncate-string-to-width title title-width nil nil "…"))
         (left (concat " " (when (> title-width 0)
                             (propertize short 'face 'agent-chat-header 'help-echo title))
                       (when queue
                         (concat (when (> title-width 0) " · ")
                                 (propertize queue 'face 'agent-chat-queue-title
                                             'help-echo agent-chat--queue-paused))))))
    (if (not quota) (agent-chat--header-text left)
      (list (agent-chat--header-text left)
            (propertize " " 'display `(space :align-to (- right ,(1+ (string-width quota)))))
            (agent-chat--header-text quota) " "))))

(defun agent-chat--window-columns (window)
  "Return safe native text columns in WINDOW, including buffer font scaling."
  (max 1 (1- (floor (window-max-chars-per-line window)))))

(defvar agent-chat--measurement-fonts nil
  "Last frame and resolved face fonts used by Ebox's shared width cache.")

(defun agent-chat--measurement-font-key (window)
  "Invalidate stale Ebox metrics for WINDOW and return the current font key.
Ebox keys widths by face names and buffer remapping, but named faces can
change fonts without changing either key.  Clear once on a font or frame
change; streamed updates with unchanged fonts retain their measurement cache."
  (let* ((frame (window-frame window))
         (fonts (cons frame
                      (mapcar (lambda (face) (face-font face frame))
                              '(default shadow bold italic link agent-chat-code
                                agent-chat-user agent-chat-muted agent-chat-error)))))
    (unless (equal fonts agent-chat--measurement-fonts)
      (ebox-clear-cache)
      (setq agent-chat--measurement-fonts fonts))
    fonts))

(defun agent-chat--history-key (window)
  "Return the geometry and styling that determine history layout in WINDOW."
  (list (max 20 (ebox-viewport-window-width window))
        (max 2 (- (agent-chat--window-columns window) 2))
        (copy-tree face-remapping-alist)
        agent-chat-process-groups-expand-by-default
        agent-chat-render-math
        (and agent-chat-render-math (agent-chat--math-settings))
        agent-chat--math-generation
        agent-chat-render-mermaid
        (face-foreground 'default (window-frame window) t)
        (agent-chat--measurement-font-key window)))

(defun agent-chat--native-history-lines (rendered)
  "Remove Ebox's trailing pixel fill from RENDERED before native display.
Each line already contains its text and spacing.  Retaining the renderer's
viewport filler lets Emacs wrap it again after a font or window size change."
  (mapcar
   (lambda (line)
     (let ((end (length line)))
       (while (and (> end 0) (= (aref line (1- end)) ?\s)
                   (eq (car-safe (get-text-property (1- end) 'display line)) 'space)
                   (not (get-text-property (1- end) 'ebox-node line)))
         (setq end (1- end)))
       (setq line (substring line 0 end))
       ;; Retain native faces, links and glyph fitting, but no mounted UI state.
       (remove-text-properties
        0 end '(keymap nil local-map nil field nil read-only nil cursor nil ebox-node nil) line)
       line))
   (split-string rendered "\n" nil)))

(defun agent-chat--join-history-lines (lines &optional final-newline)
  "Join LINES, retaining the user block face and detail folding scope.
FINAL-NEWLINE includes a final line ending.  Native face extension supplies
the background up to the window edge without inserting padding into copies."
  (let (parts previous-entry)
    (while lines
      (let* ((line (pop lines))
             (face (and (> (length line) 0)
                        (get-text-property (1- (length line)) 'face line)))
             (entry (if (> (length line) 0)
                        (get-text-property (1- (length line)) 'agent-chat-fold-entry line)
                      ;; Ebox's empty lines have no characters to carry scope.
                      ;; Only bridge a gap whose surrounding lines agree.
                      (when-let* ((next (seq-find (lambda (text) (> (length text) 0)) lines))
                                  ((eql previous-entry
                                        (get-text-property 0 'agent-chat-fold-entry next))))
                        previous-entry))))
        (push line parts)
        (when (or lines final-newline)
          (let ((newline (if (memq 'agent-chat-user (if (listp face) face (list face)))
                             (propertize "\n" 'face 'agent-chat-user)
                           (copy-sequence "\n"))))
            (when (integerp entry)
              (put-text-property 0 1 'agent-chat-fold-entry entry newline))
            ;; Ebox splits/rejoins lines.  Restore Mermaid's sliced-image
            ;; newline rule even when padding follows the diagram.
            (when (text-property-any 0 (length line) 'line-height t line)
              (put-text-property 0 1 'line-height t newline))
            (push newline parts)))
        (setq previous-entry entry)))
    (apply #'concat (nreverse parts))))

(defun agent-chat--render-text-lines (text)
  "Lay out TEXT through Ebox using the caller's viewport bindings."
  (agent-chat--native-history-lines (ebox-render (ebox-text-create text))))

(defun agent-chat--history-lines (window)
  "Render messages through Ebox, reusing unchanged message block layouts.
An ACP delta changes only its message or process group.  Completed messages
retain their parsed markup, native code faces and measured lines."
  (let* ((key (agent-chat--history-key window))
         (width (car key)) (columns (cadr key)))
    (unless (equal key (car agent-chat--render-cache))
      (let* ((ebox-viewport-width width)
             (ebox-viewport-height 1)
             (ebox--measure-buffer (current-buffer))
             (agent-chat--math-window window)
             (agent-chat--markdown-width columns)
             (previous (and (equal key (car agent-chat--render-block-cache))
                            (cdr agent-chat--render-block-cache)))
             (cache (make-hash-table :test #'eql))
             (entries (agent-chat--state-history agent-chat--state))
             (index 0) (pending 0) parts)
        (while entries
          (if (agent-chat--permission-entry-pending-p (car entries))
              (progn
                (pop entries)
                (cl-incf index)
                (cl-incf pending))
            (let* ((start index)
                   (group (agent-chat--process-entry-p (car entries)))
                   block)
              (push (pop entries) block)
              (cl-incf index)
              (when group
                (while (and entries (agent-chat--process-entry-p (car entries)))
                  (push (pop entries) block)
                  (cl-incf index)))
              (setq block (nreverse block))
              (let* ((input (list block
                                  (and group (agent-chat--process-group-expanded-p start))
                                  (seq-filter (lambda (i) (and (>= i start) (< i index)))
                                              agent-chat--expanded)))
                     (cached (and previous (gethash start previous)))
                     (result
                      (if (equal input (car cached)) cached
                        (let ((lines (agent-chat--render-text-lines
                                      (string-trim-right
                                       (if group (agent-chat--format-process-group block start)
                                         (agent-chat--format-entry (car block) start nil))
                                       "\n[ \t\r\n]*"))))
                          ;; Mark after layout: wrapping and internal paragraphs
                          ;; must not create extra navigation stops.
                          (when-let* ((first (seq-find (lambda (line) (> (length line) 0)) lines)))
                            (put-text-property 0 1 'agent-chat-block-start start first))
                          (cons (copy-tree input) lines)))))
                (puthash start result cache)
                (when parts (push (list "") parts))
                (push (cdr result) parts)))))
        (when (> pending 0)
          (let ((lines (agent-chat--render-text-lines
                        (agent-chat--permission-reminder pending))))
            (when-let* ((first (seq-find (lambda (line) (> (length line) 0)) lines)))
              (put-text-property 0 1 'agent-chat-block-start index first))
            (when parts (push (list "") parts))
            (push lines parts)))
        (setq agent-chat--render-block-cache (cons key cache)
              agent-chat--render-cache
              (cons key (if parts (apply #'append (nreverse parts))
                          (agent-chat--render-text-lines
                           (propertize "Send a prompt below to begin."
                                       'face 'agent-chat-muted)))))))
    (cdr agent-chat--render-cache)))



(defvar agent-chat--queue-control-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [mouse-1] #'agent-chat--queue-click)
    map))

(defun agent-chat--queue-click (event)
  "Activate the queue preview control in EVENT without moving the draft."
  (interactive "e")
  (let* ((position (event-start event))
         (window (posn-window position))
         (string (posn-string position))
         (action (and string (get-text-property (cdr string) 'agent-chat-queue-action
                                                (car string)))))
    (when (and action (window-live-p window))
      (agent-chat--with-command-buffer (window-buffer window)
        (apply (car action) (cdr action))))))

(defun agent-chat--queue-lines (columns height)
  "Build a bounded queue preview for a window of COLUMNS by HEIGHT."
  (when agent-chat--queue
    (let* ((rows (max 1 (min 4 (/ height 3))))
           (steer-ready (agent-chat--queue-steer-ready-p))
           (index 0)
           (heading (concat
                     (propertize (format "≡ Queue · %d pending" (length agent-chat--queue))
                                 'face 'agent-chat-queue-title)
                     (cond (agent-chat--queue-paused
                             (let ((ready (and agent-chat--session
                                               (agent-chat--live-p agent-chat--session)
                                               (eq (agent-chat--session-phase agent-chat--session)
                                                   'ready)
                                               (not (agent-chat--session-cancelling
                                                     agent-chat--session)))))
                               (concat
                                " · "
                                (propertize
                                 "[Resume queue]"
                                 'face (if ready 'agent-chat-queue-control 'shadow)
                                 'mouse-face (and ready 'highlight)
                                 'keymap (and ready agent-chat--queue-control-map)
                                 'help-echo (if ready
                                                (format "Continue queued prompts in order (stopped: %s)"
                                                        agent-chat--queue-paused)
                                              "Wait for cancellation or reconnect before continuing")
                                 'agent-chat-queue-action
                                 (and ready (list #'agent-chat-queue-resume))))))
                            (t (propertize " · Auto-send" 'face 'success)))
                     " · ? Q manage"))
           (lines (list heading)))
      (dolist (entry (seq-take agent-chat--queue (1- rows)))
        (let ((controls
               (mapconcat
                (lambda (pair)
                  (let ((enabled (and (not (plist-get entry :steering))
                                      (or (not (eq (cdr pair) #'agent-chat--queue-steer))
                                          steer-ready))))
                    (propertize (car pair) 'face (if enabled 'agent-chat-queue-control 'shadow)
                                'mouse-face (and enabled 'highlight)
                                'keymap (and enabled agent-chat--queue-control-map)
                                'help-echo (if (eq (cdr pair) #'agent-chat--queue-steer)
                                               "Add to the active Emacs Codex turn"
                                             (car pair))
                                'agent-chat-queue-action
                                (and enabled (list (cdr pair) entry)))))
                '(("[Withdraw]" . agent-chat--queue-withdraw)
                  ("[Edit]" . agent-chat--queue-edit)
                  ("[Steer]" . agent-chat--queue-steer)) " ")))
          (setq lines (append lines
                              (list (concat
                                     (propertize (format "%d. " (cl-incf index))
                                                 'face 'agent-chat-queue-title)
                                     controls
                                     (if (plist-get entry :steering) " Steering…  " "  ")
                                     (agent-chat--queue-summary entry)))))))
      (mapcar
       (lambda (line)
         (let* ((text (truncate-string-to-width (concat " " line) columns nil nil "…"))
                (row (concat text (make-string (max 0 (- columns (string-width text))) ?\s))))
           (add-face-text-property 0 (length row) 'agent-chat-queue-panel t row)
           row))
       lines))))



(defun agent-chat--render-queue (window)
  "Show the queue above WINDOW's draft without adding editable text."
  (if (null agent-chat--queue)
      (when (overlayp agent-chat--queue-preview)
        (delete-overlay agent-chat--queue-preview)
        (setq agent-chat--queue-preview nil))
    (let* ((history (window-parameter window 'agent-chat-history-window))
           (total (window-total-height
                   (if (window-live-p history) (window-parent window) window)))
           (chrome (- (window-total-height window) (window-body-height window)))
           (rows (max 1 (min 4 (- (/ total 3) chrome agent-chat-prompt-min-height))))
           (lines (agent-chat--queue-lines (agent-chat--window-columns window) (* 3 rows)))
           (text (concat (string-join lines "\n") "\n")))
      (unless (overlayp agent-chat--queue-preview)
        (setq agent-chat--queue-preview (make-overlay (point-min) (point-min))))
      (move-overlay agent-chat--queue-preview (point-min) (point-min))
      (unless (equal-including-properties
               text (overlay-get agent-chat--queue-preview 'before-string))
        (overlay-put agent-chat--queue-preview 'before-string text))
      (overlay-put agent-chat--queue-preview 'agent-chat-rows (length lines)))))

(defun agent-chat--reading-text (lines)
  "Make rendered LINES native selectable text with working controls."
  (let* ((text (agent-chat--join-history-lines lines t)) (pos 0) next)
    (add-text-properties 0 (length text) '(rear-nonsticky t fontified t) text)
    (while (< pos (length text))
      (setq next (next-property-change pos text (length text)))
      (when (cl-some (lambda (property) (get-text-property pos property text))
                    '(agent-chat-group agent-chat-entry agent-chat-link agent-chat-image))
        (put-text-property pos next 'keymap agent-chat--reading-control-map text))
      (when (get-text-property pos 'agent-chat-image text)
        (add-text-properties pos next
                             '(mouse-face highlight
                               help-echo "Click, RET or C-c C-v: preview image") text))
      (setq pos next))
    text))

(defun agent-chat--reading-anchor (position)
  "Remember POSITION by nearby text and a fallback fractional location."
  (save-excursion
    (goto-char position)
    (let ((start (max (point-min) (- (point) 24)))
          (end (min (point-max) (+ (point) 24))))
      (list (buffer-substring-no-properties start end) (- position start)
            (/ (float (- position (point-min))) (max 1 (- (point-max) (point-min))))
            (length (replace-regexp-in-string
                     "[ \t\r\n]" "" (buffer-substring-no-properties (point-min) position)))
            (and (char-after) (not (memq (char-after) '(?\s ?\t ?\r ?\n))))))))

(defun agent-chat--reading-anchor-position (anchor)
  "Locate ANCHOR after reflow, preferring its closest matching text."
  (let* ((fallback (+ (point-min) (round (* (nth 2 anchor) (- (point-max) (point-min))))))
         (text (car anchor)) best)
    (save-excursion
      (goto-char (point-min))
      (unless (string-blank-p text)
        (while (search-forward text nil t)
          (let ((candidate (+ (- (point) (length text)) (cadr anchor))))
            (when (or (not best) (< (abs (- candidate fallback)) (abs (- best fallback))))
              (setq best candidate))))))
    (or best
        ;; Reflow inserts/removes visual newlines inside the old context.
        ;; Match without whitespace, using the old logical character index
        ;; to distinguish repeated passages even when new output was appended.
        (let* ((plain (replace-regexp-in-string
                       "[ \t\r\n]" "" (buffer-substring-no-properties (point-min) (point-max))))
               (needle (replace-regexp-in-string "[ \t\r\n]" "" text))
               (within (length (replace-regexp-in-string
                                "[ \t\r\n]" "" (substring text 0 (cadr anchor)))))
               (target (or (nth 3 anchor) (round (* (nth 2 anchor) (length plain)))))
               (scan 0) logical)
          (unless (string-empty-p needle)
            (while (string-match (regexp-quote needle) plain scan)
              (let ((candidate (+ (match-beginning 0) within)))
                (when (or (not logical) (< (abs (- candidate target)) (abs (- logical target))))
                  (setq logical candidate)))
              (setq scan (1+ (match-beginning 0)))))
          (when logical
            (save-excursion
              (goto-char (point-min))
              (while (and (> logical 0) (re-search-forward "[^ \t\r\n]+" nil t))
                (let ((count (- (match-end 0) (match-beginning 0))))
                  (when (< logical count) (goto-char (+ (match-beginning 0) logical)))
                  (setq logical (max 0 (- logical count)))))
              (when (nth 4 anchor) (skip-chars-forward " \t\r\n"))
              (point))))
        fallback)))

(defun agent-chat--prompt-window-size (window total)
  "Return WINDOW's desired total height within a group of TOTAL lines."
  (let* ((chrome (- (window-total-height window) (window-body-height window)))
         (queue (if (overlayp agent-chat--queue-preview)
                    (overlay-get agent-chat--queue-preview 'agent-chat-rows) 0))
         (maximum (max 1 (min agent-chat-prompt-height
                              (- (/ total 3) chrome queue)))))
    (+ chrome queue
       (save-excursion
         (goto-char (point-min))
         (min maximum (max agent-chat-prompt-min-height
                           (1+ (vertical-motion maximum window))))))))

(defun agent-chat--pair-window (prompt)
  "Pair native PROMPT with this conversation's messages, returning the latter.
The parent is an atomic window: deleting or maximizing either child acts on
both.  Both children are strongly dedicated to their conversation buffers.
The original window remains the prompt, retaining Popwin's identity."
  (let* ((history (window-parameter prompt 'agent-chat-history-window))
         (buffer (agent-chat--message-buffer)))
    ;; Window-state restoration can recreate windows without pointer-valued
    ;; parameters.  Recover the native pair from its tree and owned buffers.
    (when-let* ((sibling (window-prev-sibling prompt))
                ((window-live-p sibling))
                ((eq (window-buffer sibling) buffer))
                ((null (window-prev-sibling sibling)))
                ((null (window-next-sibling prompt)))
                ((= (car (window-edges sibling)) (car (window-edges prompt)))))
      (setq history sibling))
    (unless (and (window-live-p history)
                 (eq (window-parent prompt) (window-parent history)))
      (let* ((ignore-window-parameters t)
             (window-combination-limit t)
             (window-min-height 2)
             (size (agent-chat--prompt-window-size prompt (window-total-height prompt))))
        (setq history (split-window prompt size 'above))))
    (window-make-atom (window-parent prompt))
    (set-window-parameter prompt 'agent-chat-history-window history)
    (set-window-parameter history 'agent-chat-prompt-window prompt)
    (set-window-parameter prompt 'delete-window #'agent-chat--delete-pair)
    (set-window-parameter history 'delete-window #'agent-chat--delete-pair)
    (unless (eq (window-buffer history) buffer)
      (with-current-buffer (window-buffer history)
        (when (derived-mode-p 'agent-chat-history-mode)
          (goto-char (window-point history))
          (setq agent-chat--history-start (window-start history))))
      (set-window-dedicated-p history nil)
      (set-window-buffer history buffer)
      (with-current-buffer buffer
        (set-window-point history (point))
        (set-window-start history agent-chat--history-start t)))
    ;; Reused or restored windows can retain another buffer's left gutter.
    (dolist (window (list prompt history))
      (set-window-dedicated-p window t)
      (let ((margins (window-margins window)))
        (when (car margins)
          (set-window-margins window 0 (cdr margins)))))
    (let* ((root (window-parent prompt))
           (desired (agent-chat--prompt-window-size prompt (window-total-height root)))
           (delta (- desired (window-total-height prompt))))
      (unless (= delta 0)
        (let ((ignore-window-parameters t))
          (window-resize prompt delta nil t))))
    history))

(defun agent-chat--delete-pair (window)
  "Hide WINDOW's pair, including when maximized to fill the frame."
  (let* ((prompt (or (window-parameter window 'agent-chat-prompt-window) window))
         (history (window-parameter prompt 'agent-chat-history-window))
         (root (window-atom-root prompt))
         (ignore-window-parameters t))
    (if (window-parent root)
        (delete-window root)
      ;; Keep the reading window as the sole ordinary window.  Deleting the
      ;; original prompt also lets Popwin recognize its popup as closed.
      (let ((replacement (other-buffer (window-buffer prompt) t (window-frame prompt))))
        (with-current-buffer (window-buffer history)
          (goto-char (window-point history))
          (setq agent-chat--history-start (window-start history)))
        (walk-window-subtree
         (lambda (child) (set-window-parameter child 'window-atom nil)) root t)
        (delete-window prompt)
        (set-window-parameter history 'delete-window nil)
        (set-window-parameter history 'agent-chat-prompt-window nil)
        (set-window-dedicated-p history nil)
        (set-window-buffer history replacement)))))

(defun agent-chat--history-at-end-p (window)
  "Whether WINDOW is displaying the end of its message buffer."
  (with-current-buffer (window-buffer window)
    (or (>= (window-point window) (max (point-min) (1- (point-max))))
        (pos-visible-in-window-p (point-max) window))))

(defun agent-chat--history-scroll (window start)
  "Remember the native scroll START of message WINDOW."
  (unless agent-chat--drawing
    (setq agent-chat--history-start start
          agent-chat--history-follow
          (and (not (memq this-command '(agent-chat-history-next-block
                                        agent-chat-history-previous-block
                                        agent-chat-history-toggle-fold)))
               (agent-chat--history-at-end-p window)))))

(defun agent-chat--history-post-command ()
  "Remember reading position and refresh any deferred selection update."
  (agent-chat--history-scroll (selected-window) (window-start))
  (when (buffer-live-p agent-chat--owner)
    (with-current-buffer agent-chat--owner (agent-chat--redraw))))

(defun agent-chat--render-history (window)
  "Update real message text for WINDOW without moving the draft or focus."
  (let* ((face-remapping-alist (buffer-local-value 'face-remapping-alist (window-buffer window)))
         (lines (agent-chat--history-lines window))
         (key agent-chat--render-cache)
         (buffer (window-buffer window)))
    (with-current-buffer buffer
      ;; Keep native selections stable even when streamed text before them
      ;; changes.  The next ordinary command applies the latest model.
      (unless (or (use-region-p)
                  (equal-including-properties key agent-chat--history-render-key))
        (let* ((windows (get-buffer-window-list buffer nil t))
               (views (mapcar (lambda (win)
                                (list win (agent-chat--reading-anchor (window-point win))
                                      (agent-chat--reading-anchor (window-start win))
                                      agent-chat--history-follow)) windows))
               (point-anchor (agent-chat--reading-anchor (point)))
               (text (agent-chat--reading-text lines))
               (inhibit-read-only t)
               (agent-chat--drawing t))
          (with-temp-buffer
            (insert text)
            (let ((source (current-buffer)))
              (with-current-buffer buffer
                (replace-region-contents (point-min) (point-max)
                                         (lambda () source) 0.05))))
          ;; Native diff preserves matching characters and their old
          ;; properties.  Controls can become enabled without changing
          ;; their label, so always install the current render's props.
          (set-text-properties (point-min) (point-max) nil)
          (let ((at 0) next)
            (while (< at (length text))
              (setq next (next-property-change at text (length text)))
              (set-text-properties (1+ at) (1+ next) (text-properties-at at text))
              (setq at next)))
          (goto-char (agent-chat--reading-anchor-position point-anchor))
          (dolist (view views)
            (pcase-let ((`(,win ,at ,start ,follow) view))
              (if follow
                  (progn
                    (set-window-point win (point-max))
                    (with-selected-window win (recenter -1)))
                (set-window-point win (agent-chat--reading-anchor-position at))
                (set-window-start win (agent-chat--reading-anchor-position start) t))))
          (setq agent-chat--history-render-key key
                agent-chat--history-start (window-start window))
          (set-buffer-modified-p nil))))))

(defun agent-chat--redraw ()
  "Maintain the native window pair and render the latest conversation."
  (when (and (derived-mode-p 'agent-chat-mode)
             agent-chat--state (not agent-chat--drawing))
    (let* ((agent-chat--drawing t)
           (prompts (sort (get-buffer-window-list (current-buffer) nil t)
                          (lambda (a b) (< (window-body-width a) (window-body-width b)))))
           histories)
      (when prompts (agent-chat--render-queue (car prompts)))
      (dolist (prompt prompts)
        (push (agent-chat--pair-window prompt) histories))
      (when histories
        (agent-chat--render-history
         (car (sort histories (lambda (a b) (< (window-body-width a)
                                              (window-body-width b))))))))))

(defun agent-chat--window-change (window)
  "Refit the native conversation containing WINDOW after layout changes."
  (when (window-live-p window)
    (when-let* ((owner (agent-chat--conversation-buffer (window-buffer window))))
      (with-current-buffer owner
        (agent-chat--redraw)
        (agent-chat-quota--maybe-refresh)))))

(defun agent-chat--frame-window-change (frame)
  "Maintain owned pairs after buffer replacement or workspace restore in FRAME."
  (dolist (window (window-list frame 'no-minibuffer))
    (when (window-live-p window)
      (let ((history (window-parameter window 'agent-chat-history-window)))
        (if (with-current-buffer (window-buffer window)
              (derived-mode-p 'agent-chat-mode))
            (agent-chat--window-change window)
          (when (and (window-live-p history)
                     (eq (window-parent window) (window-parent history)))
            ;; Replacing the draft with an ordinary buffer dismisses the
            ;; conversation, preserving that replacement and all session data.
            (with-current-buffer (window-buffer history)
              (goto-char (window-point history))
              (setq agent-chat--history-start (window-start history)))
            (walk-window-subtree
             (lambda (child) (set-window-parameter child 'window-atom nil))
             (window-parent window) t)
            (set-window-parameter window 'agent-chat-history-window nil)
            (set-window-parameter window 'delete-window nil)
            (let ((ignore-window-parameters t)) (delete-window history))))))))

(defun agent-chat--math-refresh ()
  "Refresh completed previews, deferring reading reflow during selection."
  (cl-incf agent-chat--math-generation)
  (when (derived-mode-p 'agent-chat-mode)
    (setq agent-chat--render-cache nil)
    (agent-chat--redraw)))

(defun agent-chat-revert-buffer (&optional _ignore-auto _noconfirm)
  "Rebuild this conversation's view and retry failed formula and diagram previews.
Used by `revert-buffer'.  Retain the ACP session, messages, native draft and
undo history.  Successful previews and active compilation jobs stay cached.
An active selection defers message refresh until the next ordinary command."
  (agent-chat--with-conversation
    (unless (derived-mode-p 'agent-chat-mode)
      (user-error "Not an Agent Chat buffer"))
    ;; Also recover from metrics populated before this frontend tracked fonts.
    (ebox-clear-cache)
    ;; Reset only failures used by this conversation.  The next request creates
    ;; a fresh job, retaining subscribers and the normal bounded retry policy.
    (maphash (lambda (_key entry)
               (when (and (eq (plist-get entry :status) 'failed)
                          (memq (current-buffer) (plist-get entry :buffers)))
                 (setf (plist-get entry :attempt) 0)))
             agent-chat--math-cache)
    (let (failed)
      (maphash (lambda (key entry)
                 (when (and (eq (plist-get entry :status) 'failed)
                            (memq (current-buffer) (plist-get entry :buffers)))
                   (push key failed)))
               agent-chat--mermaid-cache)
      (dolist (key failed) (remhash key agent-chat--mermaid-cache)))
    (setq agent-chat--code-cache nil
          agent-chat--render-cache nil
          agent-chat--render-block-cache nil)
    (cl-incf agent-chat--math-generation)
    (agent-chat--flush-render)))

(defun agent-chat--flush-render (&optional buffer)
  "Render the latest history in BUFFER, cancelling any pending refresh."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (when (timerp agent-chat--render-timer)
        (cancel-timer agent-chat--render-timer))
      (setq agent-chat--render-timer nil
            agent-chat--render-cache nil)
      (let ((start (float-time)))
        (agent-chat--redraw)
        (setq agent-chat--render-finished-at (float-time)
              agent-chat--render-duration
              (- agent-chat--render-finished-at start)))
      (force-mode-line-update t))))

(defun agent-chat--publish ()
  "Publish state immediately, batching expensive streamed history redraws."
  (agent-chat--sync-spinner)
  (if (and (> agent-chat-stream-refresh-interval 0)
           (eq (agent-chat--state-status agent-chat--state) 'running)
           (get-buffer-window (current-buffer) t)
           (not (and agent-chat--session
                     (agent-chat--session-permissions agent-chat--session))))
      (let ((delay (- (+ agent-chat--render-finished-at
                         (max agent-chat-stream-refresh-interval
                              (* 2 agent-chat--render-duration)))
                      (float-time))))
        (if (<= delay 0)
            (agent-chat--flush-render)
          ;; Retain the displayed cache until the flush.  Typing and scrolling
          ;; can still lay out the prompt without re-rendering all messages.
          (unless (timerp agent-chat--render-timer)
            (setq agent-chat--render-timer
                  (run-at-time delay nil #'agent-chat--flush-render
                               (current-buffer))))))
    (agent-chat--flush-render)))



(defun agent-chat--reading-click (event)
  "Use native mouse positioning before activating a history control."
  (interactive "e")
  (mouse-set-point event)
  (when (cl-some (lambda (property) (get-text-property (point) property))
                '(agent-chat-group agent-chat-entry agent-chat-link agent-chat-image))
    (agent-chat--history-activate)))

(defun agent-chat--history-activate ()
  "Activate the image, link or folding control at point."
  (interactive)
  (let ((group (get-text-property (point) 'agent-chat-group))
        (index (get-text-property (point) 'agent-chat-entry))
        (link (get-text-property (point) 'agent-chat-link)))
    (cond ((get-text-property (point) 'agent-chat-image)
           (agent-chat-preview-image))
          (link (agent-chat--open-link link))
          (t
           (when-let* ((owner (agent-chat--conversation-buffer)))
             (with-current-buffer owner
               (cond ((integerp group) (agent-chat--toggle-process-group group))
                     ((integerp index) (agent-chat-toggle-details index)))))))))

(defun agent-chat-history-next-block (&optional count)
  "Move to the next COUNT message or process-group block starts.
Negative COUNT moves backward, first to the current block's start when
inside it.  Folded and expanded process groups each form one block."
  (interactive "p")
  (unless (derived-mode-p 'agent-chat-history-mode)
    (user-error "Not in an Agent Chat message buffer"))
  (let ((count (or count 1)))
    (dotimes (_ (abs count))
      (let ((position (point)))
        (if (> count 0)
            (setq position (text-property-not-all
                            (min (1+ position) (point-max)) (point-max)
                            'agent-chat-block-start nil))
          (while (and (> position (point-min))
                      (null (get-text-property (1- position) 'agent-chat-block-start)))
            (setq position (previous-single-property-change
                            position 'agent-chat-block-start nil (point-min))))
          (setq position (and (> position (point-min)) (1- position))))
        (unless position
          (user-error "No %s message block" (if (> count 0) "next" "previous")))
        (goto-char position)
        (setq agent-chat--history-follow nil)))))

(defun agent-chat-history-previous-block (&optional count)
  "Move backward COUNT block starts, first to this block's start if inside."
  (interactive "p")
  (agent-chat-history-next-block (- (or count 1))))

(defun agent-chat-history-toggle-fold ()
  "Toggle the innermost process or detail at point without activating links."
  (interactive)
  (let* ((group (get-text-property (point) 'agent-chat-group))
         (entry (or (get-text-property (point) 'agent-chat-entry)
                    (get-text-property (point) 'agent-chat-fold-entry)))
         (owner (agent-chat--conversation-buffer))
         (property (if (integerp group) 'agent-chat-group 'agent-chat-entry))
         (index (if (integerp group) group entry)))
    (unless (and (derived-mode-p 'agent-chat-history-mode) owner
                 (or (integerp group) (integerp entry)))
      (user-error "No process or detail at point"))
    (let* ((heading (text-property-any (point-min) (point-max) property index))
           (window (and (eq (current-buffer) (window-buffer)) (selected-window)))
           (row (when (and window heading (pos-visible-in-window-p heading window))
                  (count-screen-lines
                   (window-start window)
                   (save-excursion (goto-char heading) (line-beginning-position))
                   nil window))))
      ;; An explicit fold ends a selection; otherwise render-history defers
      ;; the change and later restores an anchor from the disappearing body.
      (when (and (bound-and-true-p evil-local-mode)
                 (eq (bound-and-true-p evil-state) 'visual))
        (evil-normal-state))
      (deactivate-mark)
      (setq agent-chat--history-follow nil)
      (when heading (goto-char heading))
      (with-current-buffer owner
        (if (integerp group) (agent-chat--toggle-process-group group)
          (agent-chat-toggle-details entry)))
      (when-let* ((target (text-property-any (point-min) (point-max) property index)))
        (goto-char target)
        ;; Keep a visible heading on its screen row.  When collapsing from
        ;; deep inside a body, bring its own heading to the top of the pane.
        (when window
          (let ((agent-chat--drawing t)) (recenter (or row 0)))
          (setq agent-chat--history-start (window-start window)))))))

(defun agent-chat--toggle-process-group (start)
  "Toggle the process group at START without changing the draft."
  (setf (alist-get start agent-chat--process-group-overrides)
        (not (agent-chat--process-group-expanded-p start)))
  ;; Folding is a direct UI action, even while stream updates are throttled.
  (agent-chat--flush-render))

(defun agent-chat--process-group-start (index)
  "Find the group containing process entry INDEX in the current state."
  (let ((number 0) start)
    (seq-find (lambda (entry)
                (setq start (and (agent-chat--process-entry-p entry)
                                 (or start number)))
                (prog1 (= number index) (cl-incf number)))
              (agent-chat--state-history agent-chat--state))
    start))

(defun agent-chat-toggle-details (&optional index)
  "Toggle process entry INDEX, or select one interactively.
This changes only presentation; native prompt editing keys remain inherited."
  (interactive)
  (agent-chat--with-conversation
    (unless (integerp index)
      (let ((number -1) choices)
        (dolist (entry (agent-chat--state-history agent-chat--state))
          (cl-incf number)
          (when (memq (plist-get entry :role) '(reasoning tool))
            (push (cons (format "%d · %s" (1+ number)
                                (truncate-string-to-width (plist-get entry :text) 70 nil nil "…"))
                        number) choices)))
        (unless choices (user-error "No process details yet"))
        (setq choices (nreverse choices))
        (setq index (cdr (assoc (completing-read "Details: " choices nil t) choices)))))
    (if (memq index agent-chat--expanded)
        (setq agent-chat--expanded (delq index agent-chat--expanded))
      (push index agent-chat--expanded)
      (when-let* ((start (agent-chat--process-group-start index)))
        (setf (alist-get start agent-chat--process-group-overrides) t)))
    (agent-chat--flush-render)))

(defun agent-chat-browse-history ()
  "Select this conversation's native message window."
  (interactive)
  (when-let* ((owner (agent-chat--require-conversation))
              (history
               (with-current-buffer owner
                 (unless (get-buffer-window owner) (pop-to-buffer owner))
                 (agent-chat--redraw)
                 (get-buffer-window agent-chat--history-buffer))))
    (select-window history)))

(defun agent-chat-focus-prompt ()
  "Select the native draft without changing either window's position."
  (interactive)
  (let ((owner (agent-chat--require-conversation)))
    (unless (eq (window-buffer (selected-window)) owner)
      (if-let* ((window (get-buffer-window owner)))
          (select-window window)
        (pop-to-buffer owner)))))

(defun agent-chat--history-close-owner ()
  "Kill the owning conversation when its private message buffer is killed."
  (if (buffer-live-p agent-chat--owner)
      (kill-buffer agent-chat--owner)
    t))

;;;; Conversation-owned input queue

(defun agent-chat--queue-steering-p ()
  "Whether a pending prompt is waiting for a Steer acknowledgement."
  (seq-some (lambda (entry) (plist-get entry :steering)) agent-chat--queue))

(defun agent-chat--queue-steer-ready-p ()
  "Whether this conversation can steer its current Emacs Codex turn."
  (and agent-chat--session (agent-chat--live-p agent-chat--session)
       (eq (agent-chat--session-phase agent-chat--session) 'ready)
       (agent-chat--session-busy agent-chat--session)
       (not (agent-chat--session-cancelling agent-chat--session))
       (not (agent-chat--queue-steering-p))
       (fboundp 'codex-acp-steer-ready-p)
       (codex-acp-steer-ready-p (agent-chat--session-client agent-chat--session))))

(defun agent-chat--queue-steer (entry)
  "Add pending ENTRY to the current Codex turn, retaining it until accepted."
  (unless (memq entry agent-chat--queue) (user-error "This prompt is no longer queued"))
  (unless (agent-chat--queue-steer-ready-p)
    (user-error "Steer needs an active Emacs Codex turn and no pending Steer"))
  (let* ((session agent-chat--session)
         (prompt (plist-get entry :prompt))
         (token (make-symbol "steer"))
         (display (agent-chat--image-copy prompt)))
    (unless (equal (agent-chat--workspace-key (plist-get entry :directory))
                   (agent-chat--workspace-key (agent-chat--session-cwd session)))
      (user-error "Queued prompt belongs to a different workspace"))
    (agent-chat--check-images prompt session)
    ;; Keep the identity shared by the queue, its list view and mouse controls.
    (unless (plist-member entry :steering) (nconc entry (list :steering nil)))
    (setf (plist-get entry :steering) token)
    (agent-chat--queue-changed)
    (cl-labels
        ((settle (accepted error-data)
           (when (and (agent-chat--live-p session)
                      (eq token (plist-get entry :steering)))
             (with-current-buffer (agent-chat--session-buffer session)
               (setf (plist-get entry :steering) nil)
               (when (memq entry agent-chat--queue)
                 (if accepted
                     (progn
                       (setq agent-chat--queue (delq entry agent-chat--queue))
                       (agent-chat--append agent-chat--state 'user prompt)
                       (setf (plist-get (car (last (agent-chat--state-history agent-chat--state)))
                                        :text) display))
                   ;; A timeout may leave delivery uncertain.  Never resend
                   ;; automatically as a new turn after a failed Steer.
                   (agent-chat--queue-pause "Steer failed")
                   (let ((text (concat "Steer failed; prompt remains queued: "
                                       (agent-chat--error-text error-data))))
                     (agent-chat--notice session text)
                     (message "%s" text)))
                 (agent-chat--queue-changed)
                 (when accepted (agent-chat--queue-schedule)))))))
      (condition-case err
          (codex-acp-steer
           (agent-chat--session-client session) (agent-chat--session-id session)
           (agent-chat--prompt-content prompt)
           (lambda (_turn) (settle t nil))
           (lambda (error-data) (settle nil error-data)))
        (error (settle nil `((message . ,(error-message-string err)))))))))

(defun agent-chat--queue-pause (reason)
  "Pause pending prompts for REASON and cancel scheduled delivery."
  (when (timerp agent-chat--queue-timer)
    (cancel-timer agent-chat--queue-timer))
  (setq agent-chat--queue-timer nil)
  (when agent-chat--queue
    (setq agent-chat--queue-paused reason)))

(defun agent-chat--queue-ready-p ()
  "Whether this conversation can send a pending prompt."
  (and agent-chat--queue (not agent-chat--queue-paused)
       (not (agent-chat--queue-steering-p))
       (not agent-chat--queue-confirming)
       agent-chat--session (agent-chat--live-p agent-chat--session)
       (eq (agent-chat--session-phase agent-chat--session) 'ready)
       (not (agent-chat--session-busy agent-chat--session))
       (not (eq (agent-chat--state-status agent-chat--state) 'running))))

(defun agent-chat--queue-schedule ()
  "Schedule at most one delivery after the current callback unwinds."
  (when (and (agent-chat--queue-ready-p) (not agent-chat--queue-timer))
    (let ((buffer (current-buffer)) (session agent-chat--session))
      (setq agent-chat--queue-timer
            (run-at-time
             0 nil
             (lambda ()
               (when (and (buffer-live-p buffer) (agent-chat--live-p session))
                 (with-current-buffer buffer
                   (setq agent-chat--queue-timer nil)
                   (agent-chat--queue-send-next)))))))))

(defun agent-chat--queue-send-next ()
  "Send one pending prompt without accessing the native draft."
  (when (agent-chat--queue-ready-p)
    (let ((entry (pop agent-chat--queue)))
      (agent-chat--submit agent-chat--state agent-chat--adapter
                          (plist-get entry :prompt) (plist-get entry :directory))
      ;; A synchronous transport failure must leave the input recoverable.
      ;; Asynchronous failures are already recorded as a sent turn in history.
      (when (eq (agent-chat--state-status agent-chat--state) 'failed)
        (push entry agent-chat--queue)
        (agent-chat--queue-pause "Send failed"))
      (agent-chat--queue-changed))))

(defun agent-chat--queue-changed ()
  "Refresh pending-prompt controls and any open queue list."
  (unless agent-chat--queue
    (setq agent-chat--queue-paused nil))
  (when (buffer-live-p agent-chat--queue-view)
    (with-current-buffer agent-chat--queue-view
      (agent-chat--queue-refresh)))
  (agent-chat--publish))

(defun agent-chat--enqueue (draft)
  "Capture DRAFT as a pending prompt belonging to this conversation."
  (setq agent-chat--queue
        (nconc agent-chat--queue
               (list (list :id (make-symbol "queued-prompt") :draft draft
                           :prompt (agent-chat--skill-prompt draft)
                           :directory default-directory))))
  (when (agent-chat--session-cancelling agent-chat--session)
    (agent-chat--queue-pause "Interrupted"))
  (agent-chat--queue-changed)
        (agent-chat--queue-schedule))

(defun agent-chat-queue-resume ()
  "Continue automatic delivery of pending prompts in FIFO order."
  (interactive)
  (agent-chat--with-conversation
    (unless (and agent-chat--session (agent-chat--live-p agent-chat--session)
                 (eq (agent-chat--session-phase agent-chat--session) 'ready)
                 (not (agent-chat--session-cancelling agent-chat--session)))
      (user-error "Wait for cancellation or reconnect before resuming"))
    (setq agent-chat--queue-paused nil)
    (agent-chat--queue-changed)
    (agent-chat--queue-schedule)))

(defun agent-chat--queue-withdraw (entry)
  "Withdraw still-pending ENTRY without changing the current draft."
  (unless (memq entry agent-chat--queue)
    (user-error "This prompt is no longer queued; interrupt if it is running"))
  (when (plist-get entry :steering) (user-error "Wait for the Steer acknowledgement"))
  (setq agent-chat--queue (delq entry agent-chat--queue))
  (agent-chat--queue-changed))

(defun agent-chat-queue-clear ()
  "Withdraw all pending prompts without interrupting the active turn."
  (interactive)
  (agent-chat--with-conversation
    (when (agent-chat--queue-steering-p) (user-error "Wait for the Steer acknowledgement"))
    (agent-chat--queue-pause "Cleared")
    (setq agent-chat--queue nil agent-chat--queue-paused nil)
    (agent-chat--queue-changed)))

(defvar agent-chat-queue-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "d") #'agent-chat-queue-withdraw)
    (define-key map (kbd "e") #'agent-chat-queue-edit)
    (define-key map (kbd "s") #'agent-chat-queue-steer)
    (define-key map (kbd "c") #'agent-chat-queue-clear)
    (define-key map (kbd "r") #'agent-chat-queue-resume)
    map))

(define-derived-mode agent-chat-queue-mode tabulated-list-mode "Agent Queue"
  "Manage pending prompts: d withdraw, e edit, s steer, c clear, r resume, q quit."
  (setq tabulated-list-format [("#" 4 nil) ("Prompt" 70 nil)]
        tabulated-list-padding 1
        tabulated-list-sort-key nil)
  (setq-local revert-buffer-function (lambda (&rest _) (agent-chat--queue-refresh)))
  (when (bound-and-true-p evil-mode)
    (evil-local-mode 1)
    (evil-normal-state))
  (when (bound-and-true-p evil-local-mode)
    (evil-define-key* '(normal motion) agent-chat-queue-mode-map
      (kbd "d") #'agent-chat-queue-withdraw
      (kbd "e") #'agent-chat-queue-edit
      (kbd "s") #'agent-chat-queue-steer
      (kbd "c") #'agent-chat-queue-clear
      (kbd "r") #'agent-chat-queue-resume
      (kbd "q") #'quit-window)
    (evil-normalize-keymaps))
  (tabulated-list-init-header))

(defun agent-chat--queue-summary (entry)
  "Return a single-line preview of pending ENTRY."
  (replace-regexp-in-string "[\n\r\t]+" " " (plist-get entry :draft)))

(defun agent-chat--queue-refresh ()
  "Refresh this queue list from its owning conversation."
  (let ((entries (and (buffer-live-p agent-chat--queue-owner)
                      (buffer-local-value 'agent-chat--queue agent-chat--queue-owner)))
        (index 0))
    (setq tabulated-list-entries
          (mapcar (lambda (entry)
                    (list entry (vector (number-to-string (cl-incf index))
                                        (agent-chat--queue-summary entry)))) entries))
    (setq header-line-format
          (format " %d queued · d withdraw · e edit · s steer · c clear · r resume · q quit%s"
                  (length entries)
                  (if (and (buffer-live-p agent-chat--queue-owner)
                           (buffer-local-value 'agent-chat--queue-paused agent-chat--queue-owner))
                      " · Paused" "")))
    (tabulated-list-print t)))

(defun agent-chat-queue ()
  "Show the current conversation's pending prompts and queue actions."
  (interactive)
  (agent-chat--with-conversation
    (let ((owner (current-buffer)))
      (unless (buffer-live-p agent-chat--queue-view)
        (setq agent-chat--queue-view
              (generate-new-buffer (format "*Queue: %s*" (buffer-name))))
        (with-current-buffer agent-chat--queue-view
          (agent-chat-queue-mode)
          (setq agent-chat--queue-owner owner)))
      (pop-to-buffer agent-chat--queue-view)
      (agent-chat--queue-refresh))))

(defun agent-chat--queue-list-action (action &optional needs-entry)
  "Apply ACTION to the queue owner, optionally requiring NEEDS-ENTRY."
  (let ((entry (tabulated-list-get-id)) (owner agent-chat--queue-owner))
    (unless (buffer-live-p owner) (user-error "Conversation is closed"))
    (when (and needs-entry (not entry)) (user-error "No queued prompt on this row"))
    (agent-chat--with-command-buffer owner
      (if needs-entry (funcall action entry) (funcall action)))))

(defun agent-chat-queue-withdraw ()
  "Withdraw the pending prompt on the current queue row."
  (interactive)
  (agent-chat--queue-list-action #'agent-chat--queue-withdraw t))

(defun agent-chat-queue-edit ()
  "Take this queue row back into the empty prompt for editing."
  (interactive)
  (agent-chat--queue-list-action #'agent-chat--queue-edit t))

(defun agent-chat-queue-steer ()
  "Add this queue row to the conversation's active Codex turn."
  (interactive)
  (agent-chat--queue-list-action #'agent-chat--queue-steer t))

(defun agent-chat--queue-edit (entry)
  "Take pending ENTRY back into the empty draft without holding the queue.
Resubmission uses ordinary send/queue behavior and does not retain its slot."
  (unless (memq entry agent-chat--queue) (user-error "This prompt is no longer queued"))
  (when (plist-get entry :steering) (user-error "Wait for the Steer acknowledgement"))
  (unless (zerop (buffer-size))
    (user-error "Prompt already has content; send or clear it before editing a queued message"))
  (let ((draft (agent-chat--image-copy (plist-get entry :draft))))
    (agent-chat-focus-prompt)
    ;; Remove the queued copy only after insertion succeeds.
    (atomic-change-group (insert draft))
    (setq agent-chat--queue (delq entry agent-chat--queue))
    (agent-chat--queue-changed)
    (agent-chat--queue-schedule)))

(defun agent-chat--queue-confirm-close ()
  "Ask before discarding pending prompts when closing this conversation."
  (or (null agent-chat--queue)
      (agent-chat--queue-confirm-discard
       (format "Discard %d queued prompts and close this conversation? "
               (length agent-chat--queue)))))

(defun agent-chat--queue-confirm-discard (prompt)
  "Ask PROMPT while holding automatic delivery until the dialog finishes."
  (unwind-protect
      (let ((agent-chat--queue-confirming t)) (yes-or-no-p prompt))
    ;; A turn may finish while the minibuffer is waiting.  Reconsider those
    ;; queues after leaving the dialog; a confirmed close cancels its timer.
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'agent-chat-mode) (agent-chat--queue-schedule))))))

(defun agent-chat--queue-confirm-exit ()
  "Ask before exiting Emacs with pending in-memory prompts."
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'agent-chat-mode)
          (cl-incf count (length agent-chat--queue)))))
    (or (= count 0)
        (agent-chat--queue-confirm-discard
         (format "Discard %d queued prompts and exit Emacs? " count)))))

(defun agent-chat-submit ()
  "Run a local command, send the draft or queue it while a turn is active."
  (interactive)
  (agent-chat--require-conversation)
  (agent-chat-focus-prompt)
  (let ((draft (agent-chat--image-copy (buffer-substring (point-min) (point-max)))))
    (agent-chat--check-images draft agent-chat--session)
    (unless (agent-chat--dispatch-command draft)
      (when agent-chat--session
        (agent-chat--require-ready agent-chat--session)
        (when (string-blank-p draft) (user-error "Prompt is empty")))
      (if (and agent-chat--session
               (or (agent-chat--session-busy agent-chat--session)
                   agent-chat--queue agent-chat--queue-paused))
          (progn
            (agent-chat--enqueue draft)
            (atomic-change-group (delete-region (point-min) (point-max))))
        (agent-chat--submit agent-chat--state agent-chat--adapter
                            (agent-chat--skill-prompt draft) default-directory)
        (unless (eq (agent-chat--state-status agent-chat--state) 'failed)
          (atomic-change-group (delete-region (point-min) (point-max)))))
      (agent-chat--redraw))))

(defun agent-chat-interrupt ()
  "Interrupt this conversation's active turn and stop queued delivery.
Pending prompts stay queued until explicitly continued."
  (interactive)
  (agent-chat--with-conversation
    (agent-chat--queue-pause "Interrupted")
    (agent-chat--cancel agent-chat--state)
    (agent-chat--queue-changed)))

(defun agent-chat--cleanup ()
  "Release only resources owned by this conversation buffer."
  (agent-chat--queue-pause "Closed")
  (setq agent-chat--queue nil)
  (when (buffer-live-p agent-chat--queue-view)
    (with-current-buffer agent-chat--queue-view
      (setq agent-chat--queue-owner nil)
      (set-buffer-modified-p nil))
    (kill-buffer agent-chat--queue-view))
  (when (timerp agent-chat--render-timer)
    (cancel-timer agent-chat--render-timer))
  (setq agent-chat--render-timer nil)
  (when agent-chat--state
    (setf (agent-chat--state-changed agent-chat--state) nil)
    (ignore-errors (agent-chat--cancel agent-chat--state)))
  (agent-chat--disconnect agent-chat--session)
  (agent-chat--stop-spinner)
  (when (buffer-live-p agent-chat--history-buffer)
    (with-current-buffer agent-chat--history-buffer
      (setq agent-chat--owner nil)
      (kill-buffer (current-buffer)))))

(defun agent-chat--attach (state adapter label)
  "Attach semantic STATE and ADAPTER to this composer with LABEL."
  (setq agent-chat--state state agent-chat--adapter adapter
        agent-chat--label label)
  (let ((buffer (current-buffer)))
    (setf (agent-chat--state-changed state)
          (lambda ()
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (agent-chat--publish)))))))

(defun agent-chat--begin-connection (agent &optional startup)
  "Create AGENT's client using the explicit STARTUP action plist.
Actions are new, select, browse, load and fork; load/fork also carry :id."
  (let* ((config (alist-get agent agent-chat-agents))
         (remote (file-remote-p default-directory))
         (maker (unless (eq agent-chat--client-maker-override :external)
                  (or agent-chat--client-maker-override
                      (and (eq agent-chat--scope 'host)
                           (plist-get config :general-client-maker))
                      (and remote (plist-get config :remote-client-maker))
                      (and (eq agent 'codex) (not remote)
                           (eq agent-chat--scope 'directory)
                           (eq agent-chat-codex-local-backend 'emacs)
                           #'agent-chat--codex-remote-client)
                      (plist-get config :client-maker))))
         (session (agent-chat--session-create
                   :buffer (current-buffer) :agent agent :cwd default-directory
                   :startup (or startup '(:action new)))))
    (agent-chat-quota--clear)
    (setq agent-chat--client-maker-override (or maker :external)
          agent-chat--session session
          agent-chat--adapter
          (lambda (request emit) (agent-chat--send session request emit)))
    (condition-case err
        (progn
          (when (and remote (eq agent-chat--client-maker-override :external))
            (user-error "Native Codex sessions can only be resumed locally"))
          (when (and remote (not (or (plist-get config :remote-client-maker)
                                    (and maker (plist-get config :remote)))))
            (user-error "Agent %s does not support TRAMP projects" agent))
          (setf (agent-chat--session-client session)
                (if maker
                    (funcall maker (current-buffer))
                  (let ((argv (plist-get config :command)))
                    (unless (and (consp argv) (cl-every #'stringp argv))
                      (error "Agent :command must be a nonempty argv list"))
                    (acp-make-client :context-buffer (current-buffer)
                                     :command (car argv) :command-params (cdr argv)
                                     :environment-variables (plist-get config :environment)))))
          (agent-chat--connect session))
      (error (agent-chat--connection-error session (error-message-string err))))))

(defun agent-chat--read-agent ()
  "Choose a configured agent for a new conversation."
  (intern (completing-read "Agent: " agent-chat-agents nil t nil nil
                           (symbol-name agent-chat-default-agent))))

(defun agent-chat--remote-prefix (path)
  "Return PATH's full TRAMP prefix, including explicitly supplied hops."
  (when (file-remote-p path)
    ;; `file-local-name' expands ~ on connected hosts, so its length cannot
    ;; locate the prefix in the original path.  Parse the unexpanded suffix.
    (let ((local (tramp-file-name-localname (tramp-dissect-file-name path t))))
      (substring path 0 (- (length path) (length local))))))

(defun agent-chat--normalize-directory (directory)
  "Normalize DIRECTORY while preserving an explicit TRAMP connection prefix."
  (let ((remote (agent-chat--remote-prefix directory)))
    (if remote
        (concat remote (file-name-as-directory (file-local-name (expand-file-name directory))))
      (file-name-as-directory (expand-file-name directory)))))

(defun agent-chat--workspace-file (file directory)
  "Resolve FILE in DIRECTORY without crossing its local or remote host."
  (let ((remote (agent-chat--remote-prefix directory))
        (file-remote (agent-chat--remote-prefix file)))
    (when (and file-remote (not (equal remote file-remote)))
      (user-error "File belongs to a different host"))
    (if remote
        (progn
          (when (string-prefix-p "~" (file-local-name file))
            (user-error "Use a project-relative or absolute remote path"))
          (concat remote (expand-file-name (file-local-name file) (file-local-name directory))))
      (expand-file-name file directory))))

(defun agent-chat--workspace-key (directory &optional scope)
  "Return DIRECTORY's identity for SCOPE without remote filesystem queries.
SCOPE `host' ignores the directory but retains the complete connection prefix."
  (let ((remote (agent-chat--remote-prefix directory)))
    (cond
     ((eq scope 'host) (list 'host (or remote 'local)))
     (remote (list remote (file-name-as-directory (expand-file-name (file-local-name directory) "/"))))
     (t (file-name-as-directory (file-truename directory))))))

(defun agent-chat--workspace-buffer (directory &optional agent scope session-id)
  "Find the most recently used conversation for DIRECTORY, AGENT and SCOPE.
SCOPE defaults to `directory'; general conversations use `host'.
Project conversations and general conversations are never mixed.
When SESSION-ID is supplied, match only its open session or load attempt."
  (let* ((scope (or scope 'directory))
         (root (agent-chat--workspace-key directory scope)))
    (seq-find
     (lambda (buffer)
       (with-current-buffer buffer
         (when-let* (((derived-mode-p 'agent-chat-mode))
                     (session agent-chat--session)
                     (cwd (agent-chat--session-cwd session)))
           (and (eq scope agent-chat--scope)
                (or (not agent) (eq agent (agent-chat--session-agent session)))
                (or (null session-id)
                    (and (agent-chat--live-p session)
                         (equal session-id
                                (pcase (agent-chat--session-phase session)
                                  ('ready (agent-chat--session-id session))
                                  ((or 'connecting 'failed)
                                   (let ((startup (agent-chat--session-startup session)))
                                     (and (eq (plist-get startup :action) 'load)
                                          (plist-get startup :id))))))))
                (equal root (agent-chat--workspace-key cwd scope))))))
     (buffer-list))))

(defun agent-chat--context-file-name (file directory &optional absolute)
  "Spell FILE for a conversation in DIRECTORY.
Non-nil ABSOLUTE keeps the host-absolute path for a general conversation."
  (let ((file (agent-chat--workspace-file file directory)))
    (cond
     (absolute (file-local-name file))
     ((file-remote-p directory)
      (let* ((root (file-name-as-directory (file-local-name directory)))
             (local (file-local-name file)))
        (if (string-prefix-p root local) (file-relative-name local root) local)))
     ((file-in-directory-p file directory)
      (file-relative-name (file-truename file) (file-truename directory)))
     (t (expand-file-name file)))))

(defun agent-chat--file-reference (file directory &optional first last absolute)
  "Format FILE in DIRECTORY, with optional FIRST and LAST line numbers.
Non-nil ABSOLUTE keeps the host-absolute path for a general conversation."
  (propertize
   (editor-context-file-reference
    (agent-chat--context-file-name file directory absolute) first last)
   'face 'link 'help-echo file))

(defun agent-chat--source-context (directory &optional scope)
  "Capture source context before switching to the conversation in DIRECTORY.
Share exact selection, rectangle, line and copy-filter semantics with
`editor-context-capture'.  SCOPE `host' uses host-absolute references.
Never save a file or submit a prompt; neither conversation pane attaches
itself automatically."
  (unless (derived-mode-p 'agent-chat-mode 'agent-chat-history-mode)
    (let ((contexts (editor-context-capture)))
      (dolist (context contexts)
        (when-let* ((source (or (plist-get context :directory)
                               (plist-get context :file))))
          (unless (equal (agent-chat--remote-prefix source)
                         (agent-chat--remote-prefix directory))
            (user-error "Source belongs to a different host"))))
      (editor-context-format
       contexts (lambda (file)
                  (agent-chat--context-file-name file directory (eq scope 'host)))))))

(defun agent-chat--append-context (context)
  "Append CONTEXT as one undoable edit to the native draft, without sending."
  (when (and context (not (string-empty-p context)))
    (goto-char (point-max))
    (undo-boundary)
    (cl-labels ((separate ()
                 (unless (bobp)
                   (unless (eq (char-before) ?\n) (insert "\n"))
                   (unless (and (> (point) (1+ (point-min)))
                                (eq (char-before (1- (point))) ?\n))
                     (insert "\n")))))
      (atomic-change-group (separate) (insert context) (separate)))
    (undo-boundary)))

;;;###autoload
(defun agent-chat (&optional agent directory new)
  "Reuse a project or general conversation, or start one with AGENT.
Within a project, use its root.  Otherwise reuse a general conversation on
the same host; DIRECTORY or `default-directory' is only its initial command
cwd, not a file access boundary.  An explicit DIRECTORY is used as given.
Existing conversations are chosen in most recently used order; AGENT
restricts reuse to that agent.  Only a new interactive session asks for one.
For Codex, then list saved sessions alongside New session when NEW is nil
or `select'.
Keep the source window until a session is selected.
From a source file append its current line or selected region as a reference
and preview; from Dired append selected files.  Existing drafts are preserved.
Non-nil NEW prevents buffer reuse.  The value `select' keeps the interactive
Codex session chooser; other non-nil values force a fresh session.
A prefix argument also asks for its directory.
Display with `pop-to-buffer', respecting `display-buffer-alist'."
  (interactive (list nil (when current-prefix-arg (read-directory-name "Directory: "))
                     current-prefix-arg))
  (agent-chat--with-command-buffer (agent-chat--require-conversation t)
    (let* ((current (and (derived-mode-p 'agent-chat-mode) agent-chat--session))
           (project (unless (and current (not directory))
                      (project-current nil (or directory default-directory))))
           (scope (if (and current (not directory)) agent-chat--scope
                    (if project 'directory 'host)))
           (directory (agent-chat--normalize-directory
                       (or directory
                           (and current (agent-chat--session-cwd current))
                           (and project (project-root project))
                           default-directory))))
      (unless (file-directory-p directory) (user-error "Directory does not exist"))
      (let* ((existing (unless new (agent-chat--workspace-buffer directory agent scope)))
             (context (agent-chat--source-context directory scope)))
        (if existing
            (progn
              (unless (or (buffer-local-value 'agent-chat--startup-pending existing)
                          (let ((session (buffer-local-value 'agent-chat--session existing)))
                            (and session (not (agent-chat--session-id session))
                                 (memq (agent-chat--session-phase session) '(selecting failed))
                                 (memq (plist-get (agent-chat--session-startup session) :action) '(select browse)))))
                (if-let* ((window (get-buffer-window existing)))
                    (select-window window)
                  (pop-to-buffer existing)))
              (with-current-buffer existing
                (agent-chat--append-context context)
                (let ((session agent-chat--session))
                  (when (and session (not agent-chat--startup-pending)
                             (not (agent-chat--session-stopped session))
                             (not (agent-chat--session-id session))
                             (memq (agent-chat--session-phase session) '(selecting failed)))
                    (let* ((startup (agent-chat--session-startup session))
                         (action (plist-get startup :action)))
                      (cond
                       ((memq action '(select browse))
                        (setf (agent-chat--session-phase session) 'connecting)
                        (agent-chat--list-history session nil (eq action 'select)))
                       ((and (eq action 'load)
                             (eq agent-chat--client-maker-override :external))
                        (agent-chat--load-native-codex-session session (plist-get startup :id)))))))
                (agent-chat--redraw))
              existing)
          (let* ((agent (or agent (if (called-interactively-p 'interactive)
                                     (agent-chat--read-agent) agent-chat-default-agent)))
                 (startup (when (and (called-interactively-p 'interactive)
                                     (eq agent 'codex)
                                     (or (not new) (eq new 'select)))
                            '(:action select)))
                 (buffer (agent-chat--start agent directory scope startup)))
            (with-current-buffer buffer
              (agent-chat--append-context context)
              (agent-chat--redraw))
            buffer))))))

(defun agent-chat-new (&optional choose-agent)
  "Start a fresh conversation in the current workspace and scope.
Reuse the current agent in an Agent Chat conversation.
With CHOOSE-AGENT (a prefix argument), or outside a conversation,
prompt for an agent instead."
  (interactive "P")
  (agent-chat--with-command-buffer (agent-chat--require-conversation t)
    (let ((current (and (derived-mode-p 'agent-chat-mode) agent-chat--session)))
      (agent-chat (if (and current (not choose-agent))
                      (agent-chat--session-agent current)
                    (agent-chat--read-agent))
                  nil t))))

(defun agent-chat--reuse-saved-session (directory agent scope id)
  "Show AGENT's existing session ID in DIRECTORY and SCOPE, if any."
  (when-let* ((buffer (agent-chat--workspace-buffer directory agent scope id)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (when-let* ((retry (agent-chat--retry-load-startup agent-chat--session)))
        (agent-chat--restart-connection retry)))
    buffer))

;;;###autoload
(defun agent-chat-open-history (&optional session-id)
  "Continue a saved session in this workspace.
Load SESSION-ID directly when supplied, otherwise open the history picker."
  (interactive)
  (agent-chat--with-command-buffer (agent-chat--require-conversation t)
    (let* ((current (and (derived-mode-p 'agent-chat-mode) agent-chat--session))
           (project (unless current (project-current nil default-directory)))
           (directory (or (and current (agent-chat--session-cwd current))
                          (and project (project-root project)) default-directory))
           (scope (if current agent-chat--scope (if project 'directory 'host)))
           (agent (if current (agent-chat--session-agent current) agent-chat-default-agent)))
      (or (and session-id (agent-chat--reuse-saved-session directory agent scope session-id))
          (agent-chat--start agent directory scope
                             (if session-id (list :action 'load :id session-id) '(:action browse)))))))

;;;###autoload
(defun agent-chat-load-session (&optional id)
  "Select a saved session in this workspace, or load ID directly."
  (interactive)
  (agent-chat-open-history (and id (agent-chat--session-id-input id))))

;;;###autoload
(defun agent-chat-rename-session (&optional title)
  "Persist this conversation's TITLE, prompting with its current title.
Use Emacs Codex, advertised ACP title support, or an advertised /rename
command.  Keep the draft and selected pane; failures retain the old title."
  (interactive)
  (agent-chat--with-conversation
    (let* ((session agent-chat--session)
           (client (and session (agent-chat--session-client session)))
           (method
            (cond
             ((and (map-elt client :codex-acp-state)
                   (fboundp 'codex-acp-rename-session)) 'bridge)
             ((and session
                   (map-contains-key (map-elt (agent-chat--session-capabilities session)
                                              'sessionCapabilities) 'title)) 'title)
             ((and session
                   (seq-find (lambda (command) (equal (map-elt command 'name) "rename"))
                             (agent-chat--session-commands session))) 'command))))
      (unless (and session (agent-chat--live-p session)
                   (agent-chat--session-id session)
                   (eq (agent-chat--session-phase session) 'ready))
        (user-error "Agent is not ready to rename this session"))
      (unless method (user-error "Agent does not advertise session renaming"))
      (when (eq agent-chat--rename-pending session)
        (user-error "A session rename is already pending"))
      (setq title (string-trim (substring-no-properties
                               (or title (read-string "Session title: "
                                                      (agent-chat--session-title session))))))
      (unless (and (not (string-empty-p title))
                   (not (string-match-p "[[:cntrl:]]" title)))
        (user-error "Session title must be a nonblank single line"))
      ;; The connection can be replaced while the minibuffer is active.
      (unless (and (agent-chat--live-p session)
                   (eq (agent-chat--session-phase session) 'ready))
        (user-error "Session changed while reading its title"))
      (when (and (eq method 'command) (agent-chat--session-busy session))
        (user-error "Wait for the active turn before renaming this session"))
      (unless (equal title (agent-chat--session-title session))
        (setq agent-chat--rename-pending session)
        (cl-labels
            ((finish (name error-data)
               (when (agent-chat--live-p session)
                 (with-current-buffer (agent-chat--session-buffer session)
                   (when (eq agent-chat--rename-pending session)
                     (setq agent-chat--rename-pending nil)
                     (if error-data
                         (message "Could not rename session: %s" (agent-chat--error-text error-data))
                       (when name (setf (agent-chat--session-title session) name))
                       (agent-chat--refresh-session session)
                       (message "Session renamed")))))))
          (condition-case err
              (pcase method
                ('bridge
                 (codex-acp-rename-session
                  client (agent-chat--session-id session) title
                  (lambda (name) (finish name nil)) (lambda (error-data) (finish nil error-data))))
                ('title
                 (agent-chat--request
                  session `((:method . "session/set_title")
                            (:params . ((sessionId . ,(agent-chat--session-id session)) (title . ,title))))
                  (lambda (response)
                    (let ((name (map-elt response 'title)))
                      (if (and (stringp name) (not (string-blank-p name)))
                          (finish name nil)
                        (finish nil "Agent returned no saved title"))))
                  (lambda (error-data) (finish nil error-data)) agent-chat-connect-timeout))
                ('command
                 (agent-chat--send
                  session (list :prompt (concat "/rename " title))
                  (lambda (event)
                    (pcase (plist-get event :type)
                      ('done (finish nil (unless (equal (plist-get event :stop-reason) "end_turn")
                                           "Rename was interrupted")))
                      ('error (finish nil (plist-get event :text))))))))
            (error (finish nil (error-message-string err)))))))))

;;;###autoload
(defun agent-chat-fork ()
  "Branch this conversation into an independent, persistent session.
An active turn keeps running on its original connection.  The fork loads
the backend's saved snapshot, which may omit unfinished streamed output.
After a failed load, fork the selected saved session.
Keep the original draft, queue and permissions in their owning conversation."
  (interactive)
  (agent-chat--with-conversation
    (let* ((session agent-chat--session)
           (source-id (and session (agent-chat--live-p session)
                           (if (eq (agent-chat--session-phase session) 'ready)
                               (agent-chat--session-id session)
                             (plist-get (agent-chat--retry-load-startup session) :id)))))
      (unless source-id
        (user-error "Agent is not ready to fork"))
      (unless (agent-chat--fork-supported-p session)
        (user-error "Agent must support session/fork and session/load"))
      (let* ((client (agent-chat--session-client session))
             (backend (or agent-chat--client-maker-override
                          (and (map-elt client :codex-acp-state)
                               (if (eq agent-chat--scope 'host)
                                   #'agent-chat--codex-general-client #'agent-chat--codex-remote-client))
                          (plist-get (alist-get (agent-chat--session-agent session) agent-chat-agents)
                                     :client-maker)
                          :external))
             (settings (cl-loop for category in '("model" "thought_level")
                                 for option = (car (agent-chat--session-options session category))
                                 when option collect (cons category (map-elt option 'currentValue)))))
        (agent-chat--start
         (agent-chat--session-agent session) (agent-chat--session-cwd session) agent-chat--scope
         (list :action 'fork :id source-id
               :forked-from source-id :backend backend :settings settings
               :origin-window (selected-window) :origin-buffer (window-buffer (selected-window))))))))

(defun agent-chat--start (agent directory &optional scope startup)
  "Create a conversation with AGENT in DIRECTORY and optional SCOPE.
STARTUP is an action plist.  Pickers and forks display after completion."
  (let* ((config (alist-get agent agent-chat-agents))
         (directory (agent-chat--normalize-directory directory)))
    (unless config (user-error "Unknown agent: %s" agent))
    (when (and (file-remote-p directory)
               (not (or (plist-get config :remote-client-maker)
                        (and (plist-get config :client-maker) (plist-get config :remote)))))
      (user-error "Agent %s does not support TRAMP projects" agent))
    (unless (file-directory-p directory) (user-error "Directory does not exist"))
    (let* ((label (or (plist-get config :name) (symbol-name agent)))
           (buffer (generate-new-buffer
                    (format "*Agent Chat: %s @ %s*" label
                            (if (eq scope 'host)
                                (format "General · %s"
                                        (or (agent-chat--remote-prefix directory) "local"))
                              (file-name-nondirectory (directory-file-name directory)))))))
      ;; Set the mode before display so mode-based display rules can match.
      (with-current-buffer buffer
        (agent-chat-mode)
        (setq-local default-directory directory
                    agent-chat--scope (or scope 'directory)
                    agent-chat--client-maker-override (plist-get startup :backend)
                    agent-chat--startup-pending (memq (plist-get startup :action) '(select browse fork)))
        (agent-chat--attach (agent-chat--state-create) nil label))
      (unless (buffer-local-value 'agent-chat--startup-pending buffer)
        (pop-to-buffer buffer))
      (with-current-buffer buffer
        (when agent-chat--startup-pending (message "Loading %s sessions…" label))
        (agent-chat--begin-connection agent startup)
        (goto-char (point-max))
        (when (bound-and-true-p evil-local-mode) (evil-normal-state))
        (agent-chat--redraw))
      buffer)))

(defun agent-chat--restart-connection (startup)
  "Replace this buffer's connection using STARTUP, or start a new session."
  (let ((agent (agent-chat--session-agent agent-chat--session)))
    (agent-chat--disconnect agent-chat--session)
    (setf (agent-chat--state-status agent-chat--state) 'idle)
    (unless startup
      (agent-chat--append agent-chat--state 'notice
                          "New session · Earlier messages are not included in its context"))
    (agent-chat--begin-connection agent startup)
    (agent-chat--publish)))

(defun agent-chat-reconnect ()
  "Retry a failed load of the same saved session, retaining the draft.
Otherwise start a new session with earlier messages kept for reading."
  (interactive)
  (agent-chat--with-conversation
    (unless agent-chat--session (user-error "No agent configured"))
    (let ((retry (agent-chat--retry-load-startup agent-chat--session)))
      ;; A dead transport must not prevent explicit recovery.
      (ignore-errors (agent-chat-interrupt))
      (agent-chat--restart-connection retry))))

(defun agent-chat--authentication-available-p ()
  "Whether this conversation currently supports interactive authentication."
  (agent-chat--with-conversation
    (and agent-chat--session
         (agent-chat--live-p agent-chat--session)
         (not (agent-chat--session-id agent-chat--session))
         (memq (agent-chat--session-phase agent-chat--session)
               '(authentication-required failed))
         (length> (agent-chat--session-auth-methods agent-chat--session) 0))))

(defun agent-chat-authenticate ()
  "Authenticate using an agent-advertised method, then create a new session."
  (interactive)
  (agent-chat--with-conversation
    (let* ((session agent-chat--session)
           (methods (and session (agent-chat--session-auth-methods session)))
           (choices (mapcar (lambda (method)
                             (cons (format "%s [%s]" (map-elt method 'name) (map-elt method 'id))
                                   (map-elt method 'id))) (append methods nil))))
      (unless choices
        (when (and session (eq (agent-chat--session-phase session) 'failed))
          (agent-chat--require-ready session))
        (user-error "Agent does not support interactive authentication"))
      (when (agent-chat--session-id session)
        (user-error "This session is already initialized; reconnect to authenticate again"))
      (unless (memq (agent-chat--session-phase session) '(authentication-required failed))
        (user-error "Wait for the current connection or authentication to finish"))
      (when (agent-chat--session-busy session) (user-error "Wait for the current turn"))
      (let ((method (cdr (assoc (completing-read "Authentication: " choices nil t) choices))))
        (setf (agent-chat--session-phase session) 'authenticating)
        (agent-chat--refresh-session session)
        ;; Interactive authentication may take time; do not impose the handshake timeout.
        (agent-chat--request
         session (acp-make-authenticate-request :method-id method)
         (lambda (_response) (agent-chat--new-session session)))))))

(defvar agent-chat--permission-auto-queue nil
  "Arrival-ordered (SESSION . REQUEST) pairs not yet shown automatically.")
(defvar agent-chat--permission-auto-timer nil)

(defun agent-chat--permission-auto-forget (session &optional request)
  "Forget SESSION's REQUEST, or all its requests when REQUEST is nil.
Also discard stale requests and release the idle timer when nothing remains."
  (setq agent-chat--permission-auto-queue
        (cl-delete-if
         (lambda (entry)
           (or (not (agent-chat--permission-pending-p (car entry) (cdr entry)))
               (and (eq session (car entry))
                    (or (null request) (eq request (cdr entry))))))
         agent-chat--permission-auto-queue))
  (unless agent-chat--permission-auto-queue
    (when (timerp agent-chat--permission-auto-timer)
      (cancel-timer agent-chat--permission-auto-timer))
    (setq agent-chat--permission-auto-timer nil)))

(defun agent-chat--permission-auto-enqueue (session request)
  "Arrange to show SESSION's pending REQUEST once the editor is idle."
  (when (and (agent-chat--permission-pending-p session request)
             (not (cl-find-if (lambda (entry)
                               (and (eq session (car entry)) (eq request (cdr entry))))
                             agent-chat--permission-auto-queue)))
    (setq agent-chat--permission-auto-queue
          (append agent-chat--permission-auto-queue (list (cons session request))))
    (unless (timerp agent-chat--permission-auto-timer)
      (setq agent-chat--permission-auto-timer
            (run-with-idle-timer 0.15 t #'agent-chat--permission-auto-show)))))

(defun agent-chat--permission-auto-show ()
  "Show a new approval for the selected Agent Chat window once input is safe."
  (agent-chat--permission-auto-forget nil)
  ;; Transient and other modal UIs own an overriding keymap.  Waiting for
  ;; another idle period avoids changing what already-typed keys will do.
  (when (and agent-chat--permission-auto-queue
             (not (or (active-minibuffer-window) overriding-terminal-local-map
                      overriding-local-map executing-kbd-macro (input-pending-p))))
    (when-let* ((entry (cl-find-if
                       (lambda (entry)
                         (agent-chat--approval-context-p (car entry)))
                       agent-chat--permission-auto-queue)))
      (pcase-let ((`(,session . ,request) entry))
        (agent-chat--permission-auto-forget session request)
        ;; Background requests stay queued.  Consume a notification before
        ;; opening its menu: dismissal or a display error must not prompt again.
        ;; C-c C-p can reopen it; display failure never answers the approval.
        (condition-case err
            (with-current-buffer (agent-chat--session-buffer session)
              (agent-chat--permission-request-menu session request))
          (error (message "Approval popup failed: %s; use C-c C-p to reopen"
                          (error-message-string err))))))))

(defun agent-chat--approval-context-p (session &optional window)
  "Whether SESSION is selected, in WINDOW when specified."
  (and (eq (agent-chat--conversation-buffer (window-buffer (selected-window)))
           (agent-chat--session-buffer session))
       (or (null window) (eq (selected-window) window))
       (eq (frame-visible-p (selected-frame)) t)))

(defun agent-chat--approval-require-context (session)
  "Refuse a menu action outside SESSION's selected Agent Chat window."
  (unless (agent-chat--approval-context-p
           session (and (transient-active-prefix 'agent-chat--approval-menu)
                        (plist-get (transient-scope 'agent-chat--approval-menu) :window)))
    (when (transient-active-prefix 'agent-chat--approval-menu)
      (transient--emergency-exit :agent-chat-window-deselected))
    (user-error "Select the requesting Agent Chat window; use C-c C-p to reopen approvals")))

(defun agent-chat--approval-clear-window-hooks ()
  "Stop tracking the window that owns the approval menu."
  (remove-hook 'window-selection-change-functions #'agent-chat--approval-check-window)
  (remove-hook 'window-buffer-change-functions #'agent-chat--approval-check-window)
  (remove-hook 'post-command-hook #'agent-chat--approval-check-window)
  (remove-hook 'transient-exit-hook #'agent-chat--approval-clear-window-hooks))

(defun agent-chat--approval-check-window (&rest _)
  "Defer the active approval menu when its Agent Chat window loses selection."
  (when (transient-active-prefix 'agent-chat--approval-menu)
    (let ((scope (transient-scope 'agent-chat--approval-menu)))
      (unless (agent-chat--approval-context-p
               (plist-get scope :session) (plist-get scope :window))
        ;; Like agent-shell-permission-transient, remove the overriding map
        ;; immediately.  Transient's public quit commands only work as suffixes.
        (transient--emergency-exit :agent-chat-window-deselected)
        (message "Agent Chat approval deferred; use C-c C-p to reopen")))))

(defun agent-chat--permission-menu-children (_children)
  "Build numbered choices from the current permission popup's scope."
  (let* ((scope (transient-scope 'agent-chat--permission-menu))
         (choices (plist-get scope :choices))
         (digits (length (number-to-string (length choices))))
         (index 0))
    (transient-parse-suffixes
     'agent-chat--permission-menu
     (append
      (when-let* ((detail (plist-get scope :detail))) (list detail))
      (mapcar
       (lambda (choice)
         ;; Use equal-width sequences for 10+ choices so "1" cannot shadow
         ;; "1 0".  Commands are closures, never code supplied by the agent.
         (list (string-join
                (mapcar #'char-to-string
                        (string-to-list (format (format "%%0%dd" digits) (cl-incf index)))) " ")
               (car choice) (cdr choice)))
       choices)))))

(transient-define-prefix agent-chat--permission-menu ()
  "Choose a permission action.  Closing the menu makes no decision."
  [:class transient-column
   :description (lambda () (plist-get (transient-scope 'agent-chat--permission-menu) :title))
   :setup-children agent-chat--permission-menu-children]
  [["Menu" ("q" "Close without deciding" transient-quit-one)]])

(defun agent-chat--permission-popup (title choices &optional detail)
  "Show TITLE with labeled command CHOICES and optional literal DETAIL."
  (transient-setup 'agent-chat--permission-menu nil nil
                   :scope (list :title title :choices choices :detail detail)))

(defun agent-chat--permission-pending-p (session request)
  "Whether SESSION still owns this exact pending REQUEST."
  (and (agent-chat--live-p session)
       (eq request (cdr (assoc (map-elt request 'id)
                              (agent-chat--session-permissions session))))))

(defun agent-chat--permission-decide (session request option-id)
  "Answer REQUEST in SESSION with OPTION-ID, rejecting stale popup actions."
  (unless (agent-chat--permission-pending-p session request)
    (user-error "This approval is no longer pending"))
  (with-current-buffer (agent-chat--session-buffer session)
    (agent-chat--respond session (map-elt request 'id) option-id)))

(defun agent-chat--permission-bypass-p (session)
  "Whether the user enabled bypass for this exact live SESSION."
  (and session (agent-chat--live-p session)
       (eq session (buffer-local-value 'agent-chat--permission-bypass-session
                                       (agent-chat--session-buffer session)))))

(defun agent-chat--permission-clear-bypass (session)
  "Clear SESSION's bypass without affecting a replacement connection."
  (when (buffer-live-p (agent-chat--session-buffer session))
    (with-current-buffer (agent-chat--session-buffer session)
      (when (eq session agent-chat--permission-bypass-session)
        (setq agent-chat--permission-bypass-session nil)))))

(defun agent-chat--permission-allow-once (request)
  "Return REQUEST's advertised allow_once option, or nil.
This includes turn-scoped directory grants; session grants are never selected."
  (seq-find (lambda (option)
              (and (equal (map-elt option 'kind) "allow_once")
                   (stringp (map-elt option 'optionId))
                   (not (string-empty-p (map-elt option 'optionId)))))
            (map-nested-elt request '(params options))))

(defun agent-chat--permission-maybe-bypass (session request)
  "Answer REQUEST if SESSION has bypass enabled; return non-nil on success.
Missing allow_once options stay pending.  Delivery failure disables bypass,
records the failure and leaves the restored request for manual approval."
  (when-let* (((agent-chat--permission-bypass-p session))
              ((agent-chat--session-busy session))
              ((not (agent-chat--session-cancelling session)))
              ((agent-chat--permission-pending-p session request))
              (option (agent-chat--permission-allow-once request)))
    (condition-case err
        (progn
          (agent-chat--permission-decide session request (map-elt option 'optionId))
          t)
      (error
       (agent-chat--permission-clear-bypass session)
       (agent-chat--notice session (format "Session bypass stopped: %s; review pending approvals"
                                           (error-message-string err)))
       nil))))

(defun agent-chat--permission-toggle-bypass (session &optional request)
  "Toggle bypass for SESSION, checking an optional menu REQUEST is current."
  (unless (and session (agent-chat--live-p session))
    (user-error "No active agent session"))
  (when (and request (not (agent-chat--permission-pending-p session request)))
    (user-error "This approval is no longer pending"))
  (if (agent-chat--permission-bypass-p session)
      (progn
        (agent-chat--permission-clear-bypass session)
        (agent-chat--refresh-session session)
        (message "Permission prompts restored for this session"))
    (unless (and (eq (agent-chat--session-phase session) 'ready)
                 (not (agent-chat--session-cancelling session)))
      (user-error "Wait for a ready session before enabling bypass"))
    (when (yes-or-no-p (format "Bypass permissions for %s until reconnect (including pending requests)? "
                               (buffer-name (agent-chat--session-buffer session))))
      ;; The confirmation runs a recursive edit; the session/request may have
      ;; finished, disconnected or been replaced while the user was deciding.
      (unless (and (agent-chat--live-p session)
                   (eq (agent-chat--session-phase session) 'ready)
                   (not (agent-chat--session-cancelling session))
                   (or (null request) (agent-chat--permission-pending-p session request)))
        (user-error "Session or approval changed; reopen the permission menu"))
      (with-current-buffer (agent-chat--session-buffer session)
        (setq agent-chat--permission-bypass-session session))
      (dolist (entry (copy-sequence (agent-chat--session-permissions session)))
        (unless (agent-chat--permission-maybe-bypass session (cdr entry))
          (agent-chat--permission-auto-enqueue session (cdr entry))))
      (agent-chat--refresh-session session))))

(defun agent-chat-toggle-permission-bypass ()
  "Toggle this session's permission bypass, confirming before enabling it."
  (interactive)
  (agent-chat--with-conversation
    (agent-chat--permission-toggle-bypass agent-chat--session)))

(defun agent-chat--approval-field (label value &optional full)
  "Format LABEL and VALUE using Transient faces, without truncation if FULL."
  (let* ((prefix (concat label ": "))
         (text (string-clean-whitespace (format "%s" value))))
    (concat (propertize prefix 'face 'transient-heading)
            (propertize
             (if full text
               (truncate-string-to-width
                text (max 1 (- (min 88 (window-body-width)) (length prefix))) nil nil "…"))
             'face 'transient-value))))

(defun agent-chat--approval-heading (session request tool &optional full)
  "Describe SESSION's REQUEST and TOOL in compact fields, or FULL width."
  (let* ((pending (agent-chat--session-permissions session))
         (index (cl-position request pending :key #'cdr :test #'eq))
         (input (map-elt tool 'rawInput))
         (input-map (and (or (listp input) (hash-table-p input)) input))
         (command (map-elt input-map 'command))
         (path (map-elt input-map 'path))
         (directory (or (map-elt input-map 'cwd) (agent-chat--session-cwd session)
                        (buffer-local-value 'default-directory (agent-chat--session-buffer session))))
         (label (buffer-local-value 'agent-chat--label (agent-chat--session-buffer session))))
    (concat
     (propertize (format "Permission %s/%d · %s · %s"
                         (if index (1+ index) "–") (length pending)
                         (or label "Agent") (if (equal (map-elt tool 'kind) "other")
                                                (or (map-elt tool 'title) "tool")
                                              (or (map-elt tool 'kind) "tool")))
                 'face 'transient-heading)
     "\n" (agent-chat--approval-field "Dir" (abbreviate-file-name directory) full)
     "\n" (cond
             ((and (or (listp command) (vectorp command)) command
                   (seq-every-p #'stringp command))
              (agent-chat--approval-field "Cmd" (string-join (append command nil) " ") full))
             ((stringp command) (agent-chat--approval-field "Cmd" command full))
             ((stringp path) (agent-chat--approval-field "Path" path full))
             (t (agent-chat--approval-field "Tool" (or (map-elt tool 'title) "N/A") full))))))

(defun agent-chat--approval-responses (session request)
  "Build suffixes for REQUEST's advertised options in SESSION.
Shortcut keys follow option kinds; responses always carry the exact option ID."
  (let* ((options (append (map-nested-elt request '(params options)) nil))
         (digits (length (number-to-string (length options))))
         (index 0) used)
    (mapcar
     (lambda (option)
       (let* ((id (map-elt option 'optionId))
              (key (cdr (assoc (map-elt option 'kind)
                               '(("allow_once" . "y") ("allow_always" . "a")
                                 ("reject_once" . "d") ("reject_always" . "X"))))))
         (cl-incf index)
         (when (or (null key) (member key used))
           (setq key (concat "o " (string-join
                                  (mapcar #'char-to-string
                                          (string-to-list
                                           (format (format "%%0%dd" digits) index))) " "))))
         (push key used)
         (list key (or (map-elt option 'name) id)
               (lambda () (interactive)
                 (agent-chat--approval-require-context session)
                 (agent-chat--permission-decide session request id)))))
     options)))

(defun agent-chat--approval-cycle (session request offset)
  "Open the pending approval OFFSET positions from REQUEST in SESSION."
  (agent-chat--approval-require-context session)
  (unless (agent-chat--permission-pending-p session request)
    (user-error "This approval is no longer pending"))
  (let* ((pending (agent-chat--session-permissions session))
         (index (cl-position request pending :key #'cdr :test #'eq)))
    (agent-chat--permission-request-menu
     session (cdr (nth (mod (+ index offset) (length pending)) pending)))))

(defun agent-chat--approval-inspect (session request tool)
  "Show full REQUEST and TOOL details without deciding SESSION's approval."
  (agent-chat--approval-require-context session)
  (unless (agent-chat--permission-pending-p session request)
    (user-error "This approval is no longer pending"))
  (let ((buffer (get-buffer-create
                 (format "*Agent Chat Approval: %s · %s*"
                         (buffer-name (agent-chat--session-buffer session))
                         (map-elt request 'id)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (agent-chat--approval-heading session request tool t)
                "\n\n" (or (map-elt tool 'title) "Tool")
                "\n\n" (agent-chat--tool-detail tool)
                "\n\nC-c C-p: Return to this approval    q: Close details\n")
        (goto-char (point-min))
        (special-mode)
        (use-local-map (copy-keymap (current-local-map)))
        (local-set-key
         (kbd "C-c C-p")
         (lambda () (interactive)
           (unless (agent-chat--permission-pending-p session request)
             (user-error "This approval is no longer pending"))
           (pop-to-buffer (agent-chat--session-buffer session))
           (agent-chat--permission-request-menu session request)))))
    (pop-to-buffer buffer)))

(defun agent-chat--approval-display (buffer alist)
  "Display Transient BUFFER below the requesting conversation using ALIST."
  (when-let* ((window (plist-get (transient-scope) :window))
              ((window-live-p window)))
    (display-buffer-in-direction buffer (append `((direction . below) (window . ,window)) alist))))

;; Compact Transient layout with Codex-style approval keys.
(transient-define-prefix agent-chat--approval-menu ()
  "Inspect and respond to an approval.  Closing makes no decision."
  :display-action '(agent-chat--approval-display (dedicated . t) (inhibit-same-window . t))
  [(:info (lambda () (plist-get (transient-scope) :heading)) :format "%d")]
  [["Respond" :class transient-column :setup-children
    (lambda (_) (transient-parse-suffixes
                 'agent-chat--approval-menu
                 (plist-get (transient-scope) :respond)))]
   ["Queue" :class transient-column :setup-children
    (lambda (_) (transient-parse-suffixes
                 'agent-chat--approval-menu
                 (plist-get (transient-scope) :queue)))]
   ["Inspect" :class transient-column :setup-children
    (lambda (_) (transient-parse-suffixes
                 'agent-chat--approval-menu
                 (plist-get (transient-scope) :inspect)))]])

(defun agent-chat--permission-request-menu (session request)
  "Show SESSION's pending REQUEST with its exact advertised options."
  (unless (agent-chat--permission-pending-p session request)
    (user-error "This approval is no longer pending"))
  (agent-chat--approval-require-context session)
  (let* ((tool (map-nested-elt request '(params toolCall)))
         (tool (or (alist-get (map-elt tool 'toolCallId) (agent-chat--session-tools session)
                              nil nil #'equal) tool)))
    (transient-setup
     'agent-chat--approval-menu nil nil
     :scope
     (list :session session :request request :window (selected-window)
           :heading (agent-chat--approval-heading session request tool)
           :respond (append (agent-chat--approval-responses session request)
                            (when (or (agent-chat--permission-bypass-p session)
                                      (agent-chat--permission-allow-once request))
                              (list (list "!" (if (agent-chat--permission-bypass-p session)
                                                  "Restore prompts" "Bypass session")
                                          (lambda () (interactive)
                                            (agent-chat--approval-require-context session)
                                            (agent-chat--permission-toggle-bypass session request)))))
                            (list (list "x" "Cancel request"
                                        (lambda () (interactive)
                                          (agent-chat--approval-require-context session)
                                          (agent-chat--permission-decide session request nil)))))
           :queue (append
                   (when (length> (agent-chat--session-permissions session) 1)
                     (list (list "p" "Previous" (lambda () (interactive)
                                                (agent-chat--approval-cycle session request -1))
                                 :transient t)
                           (list "N" "Next" (lambda () (interactive)
                                            (agent-chat--approval-cycle session request 1))
                                 :transient t)))
                   (list (list "q" "Defer" #'transient-quit-one)))
           :inspect (list (list "v" "View details / diff"
                                (lambda () (interactive)
                                  (agent-chat--approval-inspect session request tool))))))
    (agent-chat--permission-auto-forget session request)
    (when (transient-active-prefix 'agent-chat--approval-menu)
      (add-hook 'window-selection-change-functions #'agent-chat--approval-check-window)
      (add-hook 'window-buffer-change-functions #'agent-chat--approval-check-window)
      (add-hook 'post-command-hook #'agent-chat--approval-check-window)
      (add-hook 'transient-exit-hook #'agent-chat--approval-clear-window-hooks))))

(defun agent-chat-permissions ()
  "Inspect and answer pending approvals in a Transient popup.
Closing the popup leaves requests pending; cancellation is an explicit choice."
  (interactive)
  (agent-chat--with-conversation
    (let* ((session agent-chat--session)
           (pending (and session (agent-chat--session-permissions session))))
      (unless pending (user-error "No pending approvals"))
      (agent-chat--permission-request-menu session (cdar pending)))))

(defun agent-chat-insert-file (file)
  "Insert a reference to FILE into the native prompt."
  (interactive
   (let ((default-directory
          (buffer-local-value 'default-directory (agent-chat--require-conversation))))
     (list (read-file-name "File: " default-directory nil t))))
  (setq file (expand-file-name
              file (buffer-local-value 'default-directory (agent-chat--require-conversation))))
  (agent-chat-focus-prompt)
  (insert (agent-chat--file-reference file default-directory nil nil
                                     (eq agent-chat--scope 'host)) " "))

(defconst agent-chat--local-commands
  '(("help" "Open conversation actions" agent-chat-help)
    ("new" "Start a new conversation" agent-chat-new)
    ("fork" "Fork the current conversation" agent-chat-fork)
    ("load" "Select or load a saved session" agent-chat-load-session "[SESSION-ID]")
    ("sessions" "Open a saved project session" agent-chat-open-history)
    ("model" "Choose the model" agent-chat-set-model)
    ("permissions" "Choose the agent permission mode" agent-chat-set-permission-mode)
    ("settings" "Choose an agent setting" agent-chat-set-option)
    ("quota" "Show account quota" agent-chat-quota-show)
    ("skill" "Insert an available skill" agent-chat-insert-skill)
    ("mention" "Insert a file reference" agent-chat-insert-file)
    ("approvals" "Answer a pending approval" agent-chat-permissions)
    ("interrupt" "Interrupt the active turn" agent-chat-interrupt)
    ("queue" "Manage queued prompts" agent-chat-queue)
    ("resume" "Resume queued prompts" agent-chat-queue-resume)
    ("reconnect" "Reconnect with a new session" agent-chat-reconnect)
    ("login" "Authenticate with the agent" agent-chat-authenticate))
  "Local slash commands as (NAME DESCRIPTION INTERACTIVE-FUNCTION [ARG-HINT]).
Commands with ARG-HINT accept the remaining text as one string argument.
An agent-advertised command with the same name takes precedence.")

(defun agent-chat--commands ()
  "Return the current agent and local command catalog, deduplicated by name."
  (let (commands)
    (seq-doseq (command (and agent-chat--session
                            (agent-chat--session-commands agent-chat--session)))
      (let ((name (map-elt command 'name)))
        (when (and (stringp name)
                   (string-match-p "\\`[^/[:space:]]+\\'" name)
                   (not (assoc name commands)))
          (push (cons name `((description . ,(map-elt command 'description))
                             (input . ,(map-elt command 'input)))) commands))))
    (dolist (entry agent-chat--local-commands)
      (unless (assoc (car entry) commands)
        (push (cons (car entry) `((description . ,(nth 1 entry))
                                 (local . ,(nth 2 entry))
                                 ,@(when-let* ((hint (nth 3 entry)))
                                     `((input . ((hint . ,hint))))))) commands)))
    (nreverse commands)))

(defun agent-chat--completion-bounds ()
  "Return (START END KIND) for a slash command or skill at point.
Only the first draft token can be a slash command.  Dollar skill mentions
may occur after whitespace or an opening delimiter, including on later lines."
  (when (derived-mode-p 'agent-chat-mode)
    (let ((position (point)))
      (save-excursion
        (goto-char (point-min))
        (skip-chars-forward " \t\n")
        (let ((start (point)))
          (if (and (looking-at "/[^ \t\n]*")
                   (> position start) (<= position (match-end 0)))
              (list start (match-end 0) 'command)
            (goto-char position)
            (skip-chars-backward "[:alnum:]_:.+-")
            (when (eq (char-before) ?$)
              (backward-char)
              (when (or (bobp) (memq (char-before) '(?\s ?\t ?\n ?\( ?\[ ?\{ ?\" ?')))
                (setq start (point))
                (forward-char)
                (skip-chars-forward "[:alnum:]_:.+-")
                (list start (point) 'skill)))))))))

(defun agent-chat--command-annotation (command)
  "Describe COMMAND for completion, including any argument hint."
  (let ((description (map-elt command 'description))
        (hint (map-nested-elt command '(input hint))))
    (concat "  " (if (map-elt command 'local) "[local] " "")
            (replace-regexp-in-string "[[:space:]]+" " "
                                      (if (stringp description) description ""))
            (when (and (stringp hint) (not (string-empty-p hint)))
              (concat " · " (replace-regexp-in-string "[[:space:]]+" " " hint))))))

(defun agent-chat--complete-command ()
  "Complete commands after / and advertised skills only after $."
  (when-let* ((bounds (agent-chat--completion-bounds)))
    (let* ((skillp (eq (nth 2 bounds) 'skill))
           (commands (cl-loop for (name . command) in (agent-chat--commands)
                              when (eq skillp (string-prefix-p "$" name))
                              collect (cons (if skillp name (concat "/" name)) command))))
      (when commands
        (list (car bounds) (cadr bounds) (mapcar #'car commands)
              :exclusive 'no :company-prefix-length t
              :annotation-function
              (lambda (name) (agent-chat--command-annotation (cdr (assoc name commands))))
              :exit-function
              (lambda (_name status)
                (when (and (eq status 'finished)
                           (not (memq (char-after) '(?\s ?\t ?\n))))
                  (insert " "))))))))

(defun agent-chat--trigger-completion ()
  "Offer candidates when a slash or dollar sign begins a completion."
  (when (and agent-chat-completion-auto
             (memq (char-before) '(?/ ?$))
             (agent-chat--complete-command))
    (completion-at-point)))

(defun agent-chat--dispatch-command (draft)
  "Run a recognized local command in DRAFT, returning non-nil if handled.
Keep the draft if the action fails or is quit; consume it before running
successful actions so pickers can insert into the composer."
  (when (string-match "\\`[ \t\n]*/\\([^ \t\n]+\\)\\(\\(?:.\\|\n\\)*\\)\\'" draft)
    (let* ((name (match-string 1 draft))
           (arguments (string-trim (match-string 2 draft)))
           (command (cdr (assoc name (agent-chat--commands))))
           (handler (map-elt command 'local)))
      (when handler
        (unless (or (string-empty-p arguments) (map-elt command 'input))
          (user-error "Local command /%s takes no arguments" name))
        (let ((buffer (current-buffer)))
          (atomic-change-group
            (delete-region (point-min) (point-max))
            (if (string-empty-p arguments) (call-interactively handler)
              (funcall handler arguments)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (agent-chat--redraw))))
        t))))

(defun agent-chat--skill-prompt (draft)
  "Translate a leading advertised dollar skill in DRAFT to ACP /$ syntax."
  (let* ((matched (string-match "\\`[ \t\n]*\\(\\$[^ \t\n]+\\)" draft))
         (name (and matched (match-string 1 draft)))
         (start (and matched (match-beginning 1))))
    (if (and name (assoc name (agent-chat--commands)))
        (concat (substring draft 0 start) "/" (substring draft start))
      draft)))

(defun agent-chat-insert-skill ()
  "Pick an agent-advertised skill and insert its dollar mention."
  (interactive)
  (agent-chat--require-conversation)
  (agent-chat-focus-prompt)
  (let* ((commands (cl-remove-if-not (lambda (entry) (string-prefix-p "$" (car entry)))
                                     (agent-chat--commands)))
         (completion-extra-properties
          (list :annotation-function
                (lambda (name) (agent-chat--command-annotation (cdr (assoc name commands)))))))
    (unless commands (user-error "Agent has not advertised any skills"))
    (insert (completing-read "Skill: " commands nil t) " ")))

(defun agent-chat-set-model ()
  "Choose an agent-advertised model."
  (interactive)
  (agent-chat-set-option "model"))

(defun agent-chat-set-permission-mode ()
  "Choose the agent's advertised permission mode in a Transient popup."
  (interactive)
  (agent-chat-set-option "mode"))

(defun agent-chat--legacy-options (session)
  "Adapt SESSION's legacy model/mode catalogs to select options.
Symbol IDs distinguish these options from string config IDs.  A modern
config option takes precedence over the legacy catalog for its category."
  (cl-loop
   for (id name data current available value-key)
   in (list (list 'model "Model" (agent-chat--session-model session)
                  'currentModelId 'availableModels 'modelId)
            (list 'mode "Permission mode" (agent-chat--session-mode session)
                  'currentModeId 'availableModes 'id))
   for category = (symbol-name id)
   for values = (map-elt data available)
   when (and (> (length values) 0)
             (not (seq-find (lambda (option)
                             (equal (map-elt option 'category) category))
                           (agent-chat--session-config-options session))))
   collect `((id . ,id) (name . ,name) (type . "select") (category . ,category)
             (currentValue . ,(map-elt data current))
             (options . ,(mapcar (lambda (item)
                                  `((name . ,(or (map-elt item 'name)
                                                 (map-elt item value-key)))
                                    (value . ,(map-elt item value-key))))
                                (append values nil))))))

(defun agent-chat--session-options (session &optional category)
  "Return SESSION's advertised select options, optionally for CATEGORY.
This read-only catalog is also available while a turn is running."
  (seq-filter (lambda (option)
                (and (equal (map-elt option 'type) "select")
                     (or (null category) (equal (map-elt option 'category) category))))
              (append (agent-chat--session-config-options session)
                      (agent-chat--legacy-options session))))

(defun agent-chat--selectable-options (session &optional category phase)
  "Return SESSION's selectable settings for CATEGORY, checking its PHASE.
PHASE defaults to ready; fork initialization explicitly uses connecting."
  (unless (and session (agent-chat--live-p session))
    (user-error "No active agent session"))
  (unless (eq (agent-chat--session-phase session) (or phase 'ready))
    (user-error "Agent is not ready"))
  (when (agent-chat--session-busy session) (user-error "Wait for the current turn"))
  (agent-chat--session-options session category))

(defun agent-chat--option-values (option)
  "Flatten OPTION's advertised value groups without changing their order."
  (mapcan (lambda (item)
            (if (map-elt item 'options)
                (append (map-elt item 'options) nil) (list item)))
          (append (map-elt option 'options) nil)))

(defun agent-chat--set-option-value (session id value &optional after-set on-failure phase)
  "Set SESSION's option ID to VALUE only if it is still available.
Call AFTER-SET with no arguments after accepting the updated settings.
ON-FAILURE, if provided, receives the protocol error instead of a notice.
PHASE is the required session phase, defaulting to ready."
  (let ((option (seq-find (lambda (item) (equal (map-elt item 'id) id))
                          (agent-chat--selectable-options session nil phase))))
    (unless (and option (seq-find (lambda (item) (equal (map-elt item 'value) value))
                                  (agent-chat--option-values option)))
      (user-error "This setting value is no longer available"))
    (with-current-buffer (agent-chat--session-buffer session)
      (agent-chat--request
       session (pcase id
                 ('model (acp-make-session-set-model-request
                          :session-id (agent-chat--session-id session) :model-id value))
                 ('mode (acp-make-session-set-mode-request
                         :session-id (agent-chat--session-id session) :mode-id value))
                 (_ (acp-make-session-set-config-option-request
                     :session-id (agent-chat--session-id session) :config-id id :value value)))
       (lambda (response)
         (pcase id
           ('model (setf (alist-get 'currentModelId (agent-chat--session-model session)) value))
           ('mode (setf (alist-get 'currentModeId (agent-chat--session-mode session)) value))
           (_ (setf (agent-chat--session-config-options session) (map-elt response 'configOptions))))
         (agent-chat--refresh-session session)
         (when after-set (funcall after-set)))
       (or on-failure (lambda (err) (agent-chat--notice session (agent-chat--error-text err))))
       agent-chat-connect-timeout))))

(defun agent-chat--choose-model-effort (session id value)
  "Offer effort after SESSION successfully selected model ID's VALUE.
Ignore a superseded selection, a closed session, or a newly started turn."
  (when (and (agent-chat--live-p session)
             (eq (agent-chat--session-phase session) 'ready)
             (not (agent-chat--session-busy session)))
    (with-current-buffer (agent-chat--session-buffer session)
      (let* ((options (agent-chat--selectable-options session))
             (model (seq-find (lambda (option) (equal (map-elt option 'id) id)) options))
             (effort (seq-find (lambda (option)
                                 (equal (map-elt option 'category) "thought_level")) options)))
        (when (and (equal (map-elt model 'currentValue) value)
                   effort (agent-chat--option-values effort))
          (if (or (active-minibuffer-window)
                  (not (eq (window-buffer (selected-window)) (current-buffer))))
              (message "Model changed; choose Effort with C-c RET")
            (condition-case nil
                (agent-chat--choose-option-value session effort)
              (quit (message "Model changed; keeping effort %s"
                             (map-elt effort 'currentValue))))))))))

(defun agent-chat--choose-option-value (session option)
  "Choose a value for SESSION's OPTION, using Transient for permission modes."
  (let ((values (agent-chat--option-values option))
        (id (map-elt option 'id)))
    (unless values (user-error "Agent has not advertised values for this setting"))
    (if (equal (map-elt option 'category) "mode")
        (agent-chat--permission-popup
         (format "Permissions · %s" (map-elt option 'name))
         (mapcar
          (lambda (item)
            (let ((value (map-elt item 'value)))
              (cons (concat (map-elt item 'name)
                            (when (equal value (map-elt option 'currentValue)) " [current]"))
                    (lambda () (interactive)
                      (agent-chat--set-option-value session id value)))))
          values))
      (let* ((choices (mapcar (lambda (item) (cons (map-elt item 'name) (map-elt item 'value)))
                             values))
             (effortp (equal (map-elt option 'category) "thought_level"))
             (current (car (rassoc (map-elt option 'currentValue) choices)))
             (value (cdr (assoc (completing-read (if effortp "Effort: " "Value: ")
                                                choices nil t nil nil current) choices))))
        (agent-chat--set-option-value
         session id value
         (when (equal (map-elt option 'category) "model")
           (lambda ()
             ;; Leave the response dispatcher before entering another minibuffer.
             (run-at-time 0 nil #'agent-chat--choose-model-effort session id value))))))))

(defun agent-chat-set-option (&optional category)
  "Set an agent-advertised select option, optionally limited to CATEGORY."
  (interactive)
  (agent-chat--with-conversation
    (let* ((session agent-chat--session)
           (options (agent-chat--selectable-options session category))
           (choices (mapcar (lambda (option) (cons (map-elt option 'name) option))
                            options)))
      (unless choices (user-error "Agent has not advertised selectable settings"))
      (if (and (equal category "mode") (not (length= choices 1)))
          (agent-chat--permission-popup
           "Permission settings"
           (mapcar (lambda (choice)
                     (cons (car choice)
                           (lambda () (interactive)
                             (agent-chat--choose-option-value session (cdr choice)))))
                   choices))
        (agent-chat--choose-option-value
         session (if (and category (length= choices 1)) (cdar choices)
                   (cdr (assoc (completing-read "Setting: " choices nil t) choices))))))))

(transient-define-prefix agent-chat-help ()
  "Conversation actions for the current standalone ACP session."
  [["Conversation"
    ("RET" "Send / queue" agent-chat-submit)
    ("C" "Interrupt" agent-chat-interrupt)
    ("P" "Pending approvals" agent-chat-permissions)
    ("$" "Insert skill" agent-chat-insert-skill)
    ("i" "Paste clipboard image" agent-chat-paste-image)
    ("@" "Insert file reference" agent-chat-insert-file)]
   ["Queue"
    ("Q" "View / edit / withdraw" agent-chat-queue)
    ("R" "Resume" agent-chat-queue-resume)
    ("X" "Clear pending" agent-chat-queue-clear)]
   ["View"
    ("h" "Select/copy messages" agent-chat-browse-history)
    ("e" "Focus prompt" agent-chat-focus-prompt)
    ("z" "Toggle process details" agent-chat-toggle-details)
    ("u" "Account quota" agent-chat-quota-show)]
   ["Agent"
    ("o" "Settings" agent-chat-set-option)
    ("p" "Permission mode" agent-chat-set-permission-mode)
    ("!" (lambda () (agent-chat--with-conversation
                      (if (agent-chat--permission-bypass-p agent-chat--session)
                          "Restore permission prompts" "Bypass session permissions")))
     agent-chat-toggle-permission-bypass)
    ("a" "Authenticate" agent-chat-authenticate :if agent-chat--authentication-available-p)
    ("r" (lambda () (agent-chat--with-conversation
                      (if (agent-chat--retry-load-startup agent-chat--session)
                          "Retry same session" "Reconnect (new session)"))) agent-chat-reconnect)
    ("s" "Open saved session" agent-chat-open-history)
    ("l" "Load session (select or enter ID)" agent-chat-load-session)
    ("f" "Fork conversation" agent-chat-fork)
    ("N" "Rename session" agent-chat-rename-session)
    ("n" "New conversation" agent-chat-new)]]
  (interactive)
  (let ((owner (agent-chat--require-conversation)))
    (transient-setup 'agent-chat-help nil nil
                     :scope (cons owner (buffer-local-value 'agent-chat--session owner)))))

;;;###autoload
(defun agent-chat-setup ()
  "Install bindings and refresh existing views, without opening connections."
  ;; Upgrade an already-loaded default configuration on a source reload.
  ;; An explicit override, including nil, and custom commands remain intact.
  (when-let* ((config (alist-get 'codex agent-chat-agents))
              ((equal (plist-get config :command) '("codex-acp"))))
    (dolist (entry '((:remote-client-maker . agent-chat--codex-remote-client)
                     (:general-client-maker . agent-chat--codex-general-client)))
      (unless (plist-member config (car entry))
        (setq config (plist-put (copy-sequence config) (car entry) (cdr entry)))))
    (setf (alist-get 'codex agent-chat-agents) config))
  (face-spec-set 'agent-chat-user agent-chat--user-face-spec 'face-defface-spec)
  (face-spec-set 'agent-chat-code agent-chat--code-face-spec 'face-defface-spec)
  (add-hook 'kill-emacs-query-functions #'agent-chat--queue-confirm-exit)
  (add-hook 'window-buffer-change-functions #'agent-chat--frame-window-change)
  (define-key agent-chat-queue-mode-map (kbd "s") #'agent-chat-queue-steer)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-chat-history-mode)
        (setq-local mode-line-format nil
                    revert-buffer-function #'agent-chat-revert-buffer)
        (add-hook 'evil-local-mode-hook #'agent-chat--install-history-bindings nil t)
        (agent-chat--install-history-bindings))
      (when (derived-mode-p 'agent-chat-queue-mode)
        (when (bound-and-true-p evil-local-mode)
          (evil-define-key* '(normal motion) agent-chat-queue-mode-map
            (kbd "s") #'agent-chat-queue-steer)
          (evil-normalize-keymaps))
        (agent-chat--queue-refresh))
      (when (derived-mode-p 'agent-chat-mode)
        (when-let* ((session agent-chat--session)
                    ((fboundp 'codex-acp-agent-capabilities))
                    (capabilities (codex-acp-agent-capabilities
                                   (agent-chat--session-client session))))
          (setf (agent-chat--session-capabilities session) capabilities))
        (agent-chat--enable-images)
        (agent-chat--install-copy-filter)
        (agent-chat--install-editing-bindings)
        (setq-local revert-buffer-function #'agent-chat-revert-buffer)
        (add-hook 'completion-at-point-functions #'agent-chat--complete-command nil t)
        (add-hook 'post-self-insert-hook #'agent-chat--trigger-completion nil t)
        (add-hook 'kill-buffer-query-functions #'agent-chat--queue-confirm-close nil t)
        (add-hook 'change-major-mode-hook #'agent-chat--stop-spinner nil t)
        (setq agent-chat--render-cache nil agent-chat--code-cache nil
              agent-chat--render-block-cache nil)
        (agent-chat--sync-spinner)
        (agent-chat--redraw)))))

;;;; Offline regression suite entry point

(defun agent-chat-run-tests (&optional selector)
  "Load public suites and run regressions matching optional SELECTOR.
Existing test registrations are preserved.  Return pass/fail statistics."
  (interactive)
  (require 'ert)
  (let ((directory (expand-file-name
                    "test/" (file-name-directory
                                (symbol-file 'agent-chat-run-tests 'defun)))))
    (unless (file-directory-p directory)
      (user-error "Run tests from an Agent Chat source checkout"))
    (add-to-list 'load-path directory)
    (add-to-list 'load-path (expand-file-name "../tools" directory))
    (dolist (file (directory-files directory t "^agent-chat-.*-test[.]el$"))
      (unless (featurep (intern (file-name-base file)))
        (load file nil t t))))
  (let* ((pattern (or selector "^agent-chat-"))
         (stats (ert-run-tests-batch pattern)))
    (list :passed (ert-stats-completed-expected stats)
          :failed (ert-stats-completed-unexpected stats)
          :skipped (ert-stats-skipped stats)
          :failures
          (cl-loop for test in (ert-select-tests pattern t)
                   for result = (ert-test-most-recent-result test)
                   when (ert-test-failed-p result)
                   collect (list (ert-test-name test)
                                 (ert-test-result-with-condition-condition result))))))

(provide 'agent-chat)
;;; agent-chat.el ends here
