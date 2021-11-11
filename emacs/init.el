;;; init.el -*- lexical-binding: t; -*-

;;; Install packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-selected-packages
      '( which-key gcmh auto-minor-mode openwith pdf-tools rg
         evil evil-collection flyspell-correct
         company selectrum orderless marginalia
         fish-completion vterm eshell-vterm eshell-syntax-highlighting
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

(defun y-or-n-p-always-y (orig-fun &rest args)
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
      kill-buffer-query-functions
      (delq #'process-kill-buffer-query-function kill-buffer-query-functions)
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

(when (eq window-system 'pgtk) (pgtk-use-im-context nil))

;;; Disable overwriting of system clipboard with selection

(setq select-enable-clipboard nil)

;;; Leave frame size alone

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

;;; Update files modified on disk

(global-auto-revert-mode 1)
(diminish #'auto-revert-mode)

;;; Hide abbrev mode

(diminish #'abbrev-mode)

;;; Formatting

(setq-default fill-column 80
              tab-width 4
              indent-tabs-mode nil
              tab-always-indent nil)

(setq sentence-end-double-space nil
      whitespace-style '(face trailing tab-mark missing-newline-at-eof))

(with-eval-after-load 'whitespace
  (diminish #'whitespace-mode))

;;; UI

(blink-cursor-mode -1)
(window-divider-mode 1)
(fringe-mode 9)
(column-number-mode 1)
(global-prettify-symbols-mode)
(mode-line-bell-mode)
(global-hl-todo-mode)

(setq mode-line-compact 'long)

(after-frame
 (load-theme 'my-purple t)
 (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 100)
 (set-fontset-font t 'emoji "Twemoji" nil 'prepend))

;;; Auto close pairs

(electric-pair-mode)

;;; Prog mode

(dolist (fn '(display-fill-column-indicator-mode
              whitespace-mode
              flyspell-prog-mode
              rainbow-delimiters-mode))
  (add-hook 'prog-mode-hook fn))

;;; Text mode

(dolist (fn '(display-fill-column-indicator-mode
              whitespace-mode
              flyspell-mode
              turn-on-auto-fill))
  (add-hook 'text-mode-hook fn))

;;; Enable rainbow mode for theme files

(add-to-list 'auto-minor-mode-alist '("-theme\\.el\\'" . rainbow-mode))

;;; Eldoc

(setq eldoc-echo-area-prefer-doc-buffer t
      eldoc-minor-mode-string " Doc")

;;; Ediff

(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

(advice-add 'ediff-quit :around #'y-or-n-p-always-y)

;;; Tramp

(with-eval-after-load 'tramp
  (setq tramp-default-method-alist `((,tramp-local-host-regexp nil "sudo"))
        tramp-default-method "ssh")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;; Evil

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

;;; Completion

(setq completion-styles '(orderless))

(selectrum-mode)
(marginalia-mode)

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil
      company-backends '(company-files
                         (:separate company-yasnippet company-capf
                                    company-dabbrev company-ispell)))

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

;;; Shell

(setq eshell-modules-list '( eshell-alias eshell-basic eshell-cmpl eshell-dirs
                             eshell-glob eshell-hist eshell-ls eshell-pred
                             eshell-prompt eshell-term eshell-tramp eshell-unix)
      eshell-error-if-no-glob t
      eshell-glob-include-dot-dot nil
      eshell-glob-include-dot-files t
      eshell-ask-to-save-last-dir nil)

(add-hook 'eshell-mode-hook #'fish-completion-mode)
(add-hook 'eshell-mode-hook (lambda () (company-mode -1)))

(with-eval-after-load 'eshell
  (eshell-vterm-mode)
  (eshell-syntax-highlighting-global-mode))

;;; Term

(setq vterm-max-scrollback 5000
      vterm-timer-delay 0.01
      vterm-buffer-name-string "vterm:%s")

(evil-define-key '(normal insert) vterm-mode-map
  ["C-c ESC"] #'vterm-send-escape)

;;;; Start server and set environment

(require 'server)

(unless (daemonp)
  (setq server-name (concat server-name (number-to-string (emacs-pid))))
  (server-start))

(setenv "EMACS_SOCKET_NAME" (expand-file-name server-name server-socket-dir))
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" "emacsclient")

;;; Magit

(setq magit-view-git-manual-method 'man
      transient-history-file null-device
      magit-save-repository-buffers 'dontask)

(with-eval-after-load 'magit
  (require 'forge))

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
