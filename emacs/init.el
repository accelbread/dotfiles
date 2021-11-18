;;; init.el -*- lexical-binding: t; -*-

;;; Install packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-selected-packages
      '( which-key gcmh auto-minor-mode openwith pdf-tools rg page-break-lines
         evil evil-collection flyspell-correct
         company selectrum orderless marginalia
         fish-completion vterm eshell-vterm eshell-syntax-highlighting esh-help
         diminish mode-line-bell rainbow-delimiters hl-todo rainbow-mode
         markdown-mode rust-mode cargo zig-mode
         cmake-mode toml-mode yaml-mode git-modes
         eglot yasnippet tree-sitter tree-sitter-langs evil-textobj-tree-sitter
         magit forge magit-todos))

(setq package-native-compile t
      native-comp-async-report-warnings-errors nil
      package-quickstart t
      vterm-always-compile-module t)

(package-install-selected-packages t)


;;; Config utils

(defmacro after-frame (&rest body)
  "Evaluate BODY when frame is available."
  `(if (daemonp)
       (let ((wrapper (lambda (self)
                        (remove-hook 'server-after-make-frame-hook
                                     (apply-partially self self))
                        ,@body)))
         (add-hook 'server-after-make-frame-hook
                   (apply-partially wrapper wrapper) 90))
     ,@body))

(defun y-or-n-p-always-y-advice (orig-fun &rest args)
  "Run function with always `y' for `y-or-n-p'."
  (cl-letf (((symbol-function #'y-or-n-p) (lambda (_prompt) t)))
    (apply orig-fun args)))


;;; Hide welcome messages

(setq inhibit-startup-screen t
      initial-scratch-message nil
      server-client-instructions nil)


;;; Reduce confirmations

(defalias 'yes-or-no-p #'y-or-n-p)

(setq confirm-kill-processes nil
      kill-buffer-query-functions nil
      auth-source-save-behavior nil)


;;; Make moving past view edge not jump

(setq scroll-conservatively 101
      scroll-margin 0)


;;; Prevent misc file creation

(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/autosave/" t))
      make-backup-files nil
      create-lockfiles nil
      custom-file null-device)


;;; Prevent input method from consuming keys

(setq pgtk-use-im-context nil)


;;; Disable overwriting of system clipboard with selection

(setq select-enable-clipboard nil)


;;; Leave frame size alone

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)


;;; Update files modified on disk

(global-auto-revert-mode 1)
(diminish #'auto-revert-mode)


;;; Auto close pairs

(electric-pair-mode)


;;; Formatting

(setq-default fill-column 80
              tab-width 4
              indent-tabs-mode nil
              tab-always-indent nil)

(setq sentence-end-double-space nil)

(add-hook 'text-mode #'auto-fill-mode)


;;; UI

(blink-cursor-mode -1)
(window-divider-mode)
(fringe-mode 9)
(column-number-mode)
(global-whitespace-mode)
(global-prettify-symbols-mode)
(global-page-break-lines-mode)
(mode-line-bell-mode)
(global-hl-todo-mode)

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'display-fill-column-indicator-mode))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(diminish #'abbrev-mode)
(diminish #'global-whitespace-mode)

(setq mode-line-compact 'long
      page-break-lines-lighter nil
      whitespace-style '(face trailing tab-mark missing-newline-at-eof)
      whitespace-global-modes '(prog-mode text-mode))

(after-frame
 (load-theme 'my-purple t)
 (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 150)
 (set-fontset-font t 'emoji "Twemoji" nil 'prepend))


;;; Performance

(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(global-so-long-mode)


;;; Eldoc

(setq eldoc-echo-area-prefer-doc-buffer t
      eldoc-minor-mode-string " Doc")


;;; Ediff

(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

(advice-add 'ediff-quit :around #'y-or-n-p-always-y-advice)


;;; Tramp

(with-eval-after-load 'tramp
  (setq tramp-default-method-alist `((,tramp-local-host-regexp nil "sudo"))
        tramp-default-method "ssh")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


;;; Evil

(defun my-evil-lookup-func ()
  (defvar woman-use-topic-at-point)
  (let ((woman-use-topic-at-point t))
    (woman)))

(setq evil-want-integration t
      evil-want-keybinding nil
      evil-want-minibuffer t
      evil-undo-system 'undo-redo
      evil-search-module 'evil-search
      evil-cross-lines t
      evil-want-Y-yank-to-eol t
      evil-split-window-below t
      evil-vsplit-window-right t
      evil-intercept-esc t
      evil-want-C-u-scroll t
      evil-ex-complete-emacs-commands t
      evil-lookup-func #'my-evil-lookup-func
      evil-collection-setup-minibuffer t
      evil-collection-want-unimpaired-p nil
      evil-collection-magit-want-horizontal-movement t
      evil-collection-magit-use-z-for-folds t)

(evil-mode)
(evil-collection-init)

(evil-set-type 'evil-backward-word-begin 'inclusive)
(evil-set-type 'evil-backward-WORD-begin 'inclusive)

(evil-ex-define-cmd "bd[elete]" #'kill-current-buffer)
(evil-ex-define-cmd "wbd[elete]" #'save-kill-current-buffer)
(evil-ex-define-cmd "bdq[uit]" #'kill-buffer-and-window)
(evil-ex-define-cmd "wbdq[uit]" #'save-kill-buffer-and-window)
(evil-global-set-key 'normal ["z ="] #'flyspell-correct-at-point)


;;; Spell checking

(setq ispell-dictionary "en_US"
      ispell-program-name "aspell"
      ispell-extra-args '("--camel-case")
      flyspell-issue-message-flag nil
      flyspell-mode-line-string nil)

(defun flyspell-buffer-if-enabled (&rest _)
  "Run `flyspell-buffer' only if `flyspell-mode' is enabled."
  (when flyspell-mode (flyspell-buffer)))

(add-hook 'flyspell-mode-hook #'flyspell-buffer-if-enabled)
(advice-add 'ispell-pdict-save :after #'flyspell-buffer-if-enabled)
(advice-add 'evil-paste-before :after #'flyspell-buffer-if-enabled)
(advice-add 'evil-paste-after :after #'flyspell-buffer-if-enabled)

(setq-default flyspell-prog-text-faces '(tree-sitter-hl-face:comment
                                         tree-sitter-hl-face:doc
                                         tree-sitter-hl-face:string
                                         font-lock-comment-face
                                         font-lock-doc-face
                                         font-lock-string-face))

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)


;;; Completion

(setq completion-styles '(orderless))

(selectrum-mode)
(marginalia-mode)

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0
      company-require-match nil
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil
      company-backends '(company-files
                         company-capf
                         (:separate company-dabbrev company-ispell)))

(global-company-mode)
(diminish #'company-mode)

(define-key company-active-map [return] nil)
(define-key company-active-map ["RET"] nil)
(define-key company-active-map [tab] #'company-complete)
(define-key company-active-map ["TAB"] #'company-complete)


;;; Flymake

(setq flymake-mode-line-format '(" " flymake-mode-line-counters)
      flymake-mode-line-counter-format '("[" flymake-mode-line-error-counter
                                         flymake-mode-line-warning-counter "]"))


;;; Comint

(setq comint-terminfo-terminal "dumb-emacs-ansi")


;;; Eshell

(defun my-eshell-prompt ()
  (concat (unless (eshell-exit-success-p)
            (propertize
             (number-to-string eshell-last-command-status) 'face 'error))
          (if (file-remote-p default-directory) "# " "$ ")))

(defun my-eshell-buffer-name ()
  (rename-buffer (concat "eshell:" (abbreviate-file-name default-directory)) t))

(defun my-eshell-init ()
  (setenv "TERM" "dumb-emacs-ansi")
  (setenv "GIT_PAGER" ""))

(setq eshell-modules-list '( eshell-basic eshell-cmpl eshell-dirs eshell-glob
                             eshell-hist eshell-ls eshell-pred eshell-prompt
                             eshell-term eshell-tramp eshell-unix)
      eshell-error-if-no-glob t
      eshell-glob-include-dot-dot nil
      eshell-ask-to-save-last-dir nil
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-hist-move-to-end nil
      eshell-prompt-function #'my-eshell-prompt
      eshell-prompt-regexp "^[0-9]*[$#] "
      eshell-input-filter #'eshell-input-filter-initial-space
      eshell-destroy-buffer-when-process-dies t)

(add-hook 'eshell-mode-hook #'fish-completion-mode)
(add-hook 'eshell-mode-hook #'abbrev-mode)
(add-hook 'eshell-mode-hook #'my-eshell-init)

(add-hook 'eshell-before-prompt-hook #'eshell-begin-on-new-line)
(add-hook 'eshell-before-prompt-hook #'my-eshell-buffer-name)

(with-eval-after-load 'eshell
  (eshell-syntax-highlighting-global-mode)
  (eshell-vterm-mode)
  (setup-esh-help-eldoc))

(with-eval-after-load 'esh-cmd
  (dolist (v '(eshell-last-commmand-name
               eshell-last-command-status
               eshell-last-command-result))
    (make-variable-buffer-local v)))

(with-eval-after-load 'esh-var
  ;; Have `$/' evaluate to root of current remote.
  (add-to-list
   'eshell-variable-aliases-list
   `("/" ,(lambda (_indices) (concat (file-remote-p default-directory) "/")))))

(with-eval-after-load 'abbrev
  (define-abbrev-table 'eshell-mode-abbrev-table
    '(("gitsub" "git submodule update --init --recursive --checkout")
      ("gitcl" "git clone --filter=blob:none"))))

(defun eshell/e (&rest args)
  "Open files in ARGS."
  (dolist (file (reverse
                 (mapcar #'expand-file-name
                         (flatten-tree
                          (mapcar (lambda (s)
                                    (if (stringp s) (split-string s "\n") s))
                                  args)))))
    (find-file file)))

(put #'eshell/e 'eshell-no-numeric-conversions t)
(put #'eshell/e 'eshell-filename-arguments t)


;;; Vterm

(setq vterm-max-scrollback 5000
      vterm-timer-delay 0.01
      vterm-buffer-name-string "vterm:%s")

(evil-define-key '(normal insert) vterm-mode-map
  ["C-c ESC"] #'vterm-send-escape)


;;; Server

(require 'server)

(unless (daemonp)
  (setq server-name (concat server-name (number-to-string (emacs-pid))))
  (server-start))

(setenv "EMACS_SOCKET_NAME" (expand-file-name server-name server-socket-dir))
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" "emacsclient")


;;; Magit

(setq magit-view-git-manual-method 'woman
      transient-history-file null-device
      magit-save-repository-buffers 'dontask
      magit-delete-by-moving-to-trash nil)

(with-eval-after-load 'magit
  (require 'forge))


;;; Man

(setq woman-fill-column 80
      woman-default-indent 4)

(defun man-woman (man-args)
  "Run `man', opening file using `woman'"
  (pcase-let ((`(,status . ,output)
               (with-temp-buffer
                 (cons (apply #'process-file "man" nil t nil
                              "--path" (split-string man-args))
                       (string-trim (buffer-string))))))
    (if (= 0 status)
        (woman-find-file (concat (file-remote-p default-directory) output))
      (error output)))
  nil)

(advice-add #'Man-getpage-in-background :override #'man-woman)


;;; LSP

(setq eglot-stay-out-of '(company))

(setq-default eglot-workspace-configuration
              '((rust-analyzer (checkOnSave (command . "clippy")))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer"))))

(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'zig-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook #'eglot-ensure)

(yas-global-mode)
(diminish #'yas-minor-mode)


;;; Treesitter

(global-tree-sitter-mode)
(diminish #'tree-sitter-mode)

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(pcase-dolist (`(,type ,key ,class)
               '((a "c" "comment")
                 (a "S" "statement")
                 (a "f" "function")
                 (i "f" "function")
                 (a "k" "block")
                 (i "k" "block")
                 (a "i" "conditional")
                 (i "i" "conditional")
                 (a "P" "parameter")
                 (i "P" "parameter")
                 (a "C" "call")
                 (i "C" "call")
                 (a "l" "loop")
                 (i "l" "loop")
                 (a "L" "class")
                 (i "L" "class")
                 (i "S" "scopename")))
  (let ((map (pcase type
               ('a evil-outer-text-objects-map)
               ('i evil-inner-text-objects-map)))
        (desc (concat "TS " class))
        (query (concat class (pcase type ('a ".outer") ('i ".inner")))))
    (define-key map key
                `(,desc . ,(eval
                            `(evil-textobj-tree-sitter-get-textobj ,query))))))


;;; Which-key

(setq which-key-idle-delay 0.5
      which-key-compute-remaps t
      which-key-sort-order 'which-key-description-order
      which-key-side-window-max-height 0.5
      which-key-unicode-correction 0)

(which-key-mode)
(diminish #'which-key-mode)


;;; Elisp

(setq elisp-flymake-byte-compile-load-path
      (append elisp-flymake-byte-compile-load-path load-path))


;;; Markdown

(setq markdown-asymmetric-header t
      markdown-fontify-code-blocks-natively t
      markdown-ordered-list-enumeration nil
      markdown-unordered-list-item-prefix nil
      markdown-disable-tooltip-prompt t
      markdown-command '("pandoc" "--from=markdown" "--to=html5"))


;;; Rust

(setq rust-format-on-save t)

(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'toml-mode-hook #'cargo-minor-mode)

(with-eval-after-load 'cargo
  (diminish #'cargo-minor-mode))


;;; C

(with-eval-after-load 'cc-mode
  (setf (alist-get 'other c-default-style) "stroustrup")
  (add-hook 'c-mode-hook
            (lambda () (setf (alist-get 'inextern-lang c-offsets-alist) [0]))))


;;; Themes

(setq custom-safe-themes t)

(add-to-list 'auto-minor-mode-alist '("-theme\\.el\\'" . rainbow-mode))


;;; PDF

(pdf-loader-install)

(with-eval-after-load 'pdf-view
  (diminish #'pdf-view-midnight-minor-mode))


;;; Open outside Emacs

(setq openwith-associations
      '(("\\.mkv\\'" "mpv" (file))))

(openwith-mode 1)

(advice-add #'abort-if-file-too-large :around
            (lambda (orig-fun size op-type filename &optional offer-raw)
              "Skip size warning for externally handled file types."
              (unless (string-match-p
                       (mapconcat #'car openwith-associations "\\|")
                       filename)
                (funcall orig-fun size op-type filename offer-raw))))


;;; Local configuration

(let ((local-config (concat user-emacs-directory "local-config.el")))
  (if (file-exists-p local-config)
      (load-file local-config)))


;;; Commands

(defun save-kill-current-buffer ()
  "Save and kill current buffer."
  (interactive)
  (save-buffer)
  (kill-current-buffer))

(defun save-kill-buffer-and-window ()
  "Save and kill current buffer and window."
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(defun esp32c3 ()
  "Serial console for esp32-c3."
  (interactive)
  (serial-term (serial-read-name) 115200))


;;; Garbage collect when idle

(setq gcmh-idle-delay 'auto
      gcmh-auto-idle-delay-factor 10)

(gcmh-mode)
(diminish #'gcmh-mode)
