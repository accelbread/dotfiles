;;; Function defs for config
(defun add-once-hook (hook fn)
  (let ((fn-wrapper (lambda (self hook fn)
                      (remove-hook hook (apply-partially self self hook fn))
                      (funcall fn))))
    (add-hook hook (apply-partially fn-wrapper fn-wrapper hook fn))))

;;; Configure and install packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(eval-when-compile
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

;;;; use-package deps
(use-package diminish)

;;;; Evil
(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-redo)
  (evil-search-module 'evil-search)
  (evil-cross-lines t)
  (evil-want-Y-yank-to-eol t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-intercept-esc t)
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-ex-complete-emacs-commands t)
  :config
  (evil-mode)
  (evil-set-type 'evil-backward-word-begin 'inclusive)
  (evil-set-type 'evil-backward-WORD-begin 'inclusive)
  (defun save-kill-current-buffer ()
    "Save and kill current buffer"
    (interactive)
    (save-buffer)
    (kill-current-buffer))
  (evil-ex-define-cmd "bd[elete]" 'kill-current-buffer)
  (evil-ex-define-cmd "wbd[elete]" 'save-kill-current-buffer)
  (evil-global-set-key 'normal (kbd "z=") 'flyspell-correct-at-point))
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-want-unimpaired-p nil)
  (evil-collection-magit-want-horizontal-movement t)
  (evil-collection-magit-use-z-for-folds t)
  :config
  (evil-collection-init))
(use-package flyspell-correct
  :after flyspell)
(when (version< emacs-version "28")
  (use-package undo-fu
    :custom
    (evil-undo-system 'undo-fu)))

;;;; Completion
(use-package selectrum
  :config
  (selectrum-mode))
(use-package orderless
  :custom
  (completion-styles '(orderless)))
(use-package marginalia
  :init
  (marginalia-mode))
(use-package company
  :diminish
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-backends '(company-files
    (:separate company-yasnippet company-capf company-dabbrev company-ispell)))
  :config
  (global-company-mode))

;;;; Utils
(use-package vterm
  :after evil
  :custom
  (vterm-shell "fish")
  (vterm-always-compile-module t)
  (vterm-max-scrollback 5000)
  (vterm-timer-delay 0.01)
  (vterm-buffer-name-string "vterm:%s")
  :config
  (evil-define-key '(normal insert) vterm-mode-map (kbd "C-c ESC")
    'vterm-send-escape))
(use-package magit
  :custom
  (magit-view-git-manual-method 'man)
  (transient-history-file null-device)
  (magit-save-repository-buffers 'dontask))
(use-package magit-todos)

;;;; Language support
(use-package eglot
  :config
  (add-to-list 'eglot-stay-out-of 'company)
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  :hook ((c-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (zig-mode . eglot-ensure)
         (python-mode . eglot-ensure)))
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode))
(use-package markdown-mode
  :custom
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-unordered-list-item-prefix nil)
  (markdown-disable-tooltip-prompt t)
  (markdown-command '("pandoc" "--from=markdown" "--to=html5")))
(use-package rust-mode
  :custom
  (rust-format-on-save t))
(use-package toml-mode)
(use-package cargo
  :diminish cargo-minor-mode
  :hook ((rust-mode . cargo-minor-mode)
         (toml-mode . cargo-minor-mode)))
(use-package zig-mode)
(use-package cmake-mode)

;;;; Treesitter
(use-package tree-sitter
  :diminish
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (setq-default flyspell-prog-text-faces
    '(tree-sitter-hl-face:comment
      tree-sitter-hl-face:doc
      tree-sitter-hl-face:string
      font-lock-comment-face
      font-lock-doc-face
      font-lock-string-face)))
(use-package tree-sitter-langs)
(use-package evil-textobj-tree-sitter
  :config
  (define-key evil-outer-text-objects-map "k" `("TS block" .
    ,(evil-textobj-tree-sitter-get-textobj "block.outer")))
  (define-key evil-outer-text-objects-map "c" `("TS call" .
    ,(evil-textobj-tree-sitter-get-textobj "call.outer")))
  (define-key evil-outer-text-objects-map "C" `("TS class" .
    ,(evil-textobj-tree-sitter-get-textobj "class.outer")))
  (define-key evil-outer-text-objects-map "/" `("TS comment" .
    ,(evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-outer-text-objects-map "i" `("TS conditional" .
    ,(evil-textobj-tree-sitter-get-textobj "conditional.outer")))
  (define-key evil-outer-text-objects-map "f" `("TS function" .
    ,(evil-textobj-tree-sitter-get-textobj "function.outer")))
  (define-key evil-outer-text-objects-map "l" `("TS loop" .
    ,(evil-textobj-tree-sitter-get-textobj "loop.outer")))
  (define-key evil-outer-text-objects-map "P" `("TS parameter" .
    ,(evil-textobj-tree-sitter-get-textobj "parameter.outer")))
  (define-key evil-outer-text-objects-map "S" `("TS statement" .
    ,(evil-textobj-tree-sitter-get-textobj "statement.outer")))
  (define-key evil-inner-text-objects-map "k" `("TS block" .
    ,(evil-textobj-tree-sitter-get-textobj "block.inner")))
  (define-key evil-inner-text-objects-map "c" `("TS call" .
    ,(evil-textobj-tree-sitter-get-textobj "call.inner")))
  (define-key evil-inner-text-objects-map "C" `("TS class" .
    ,(evil-textobj-tree-sitter-get-textobj "class.inner")))
  (define-key evil-inner-text-objects-map "i" `("TS conditional" .
    ,(evil-textobj-tree-sitter-get-textobj "conditional.inner")))
  (define-key evil-inner-text-objects-map "f" `("TS function" .
    ,(evil-textobj-tree-sitter-get-textobj "function.inner")))
  (define-key evil-inner-text-objects-map "l" `("TS loop" .
    ,(evil-textobj-tree-sitter-get-textobj "loop.inner")))
  (define-key evil-inner-text-objects-map "P" `("TS parameter" .
    ,(evil-textobj-tree-sitter-get-textobj "parameter.inner")))
  (define-key evil-inner-text-objects-map "S" `("TS scopename" .
    ,(evil-textobj-tree-sitter-get-textobj "scopename.inner"))))

;;;; UI
(use-package which-key
  :after evil
  :diminish
  :custom
  (which-key-idle-delay 0.5)
  (which-key-compute-remaps t)
  :config
  (which-key-mode))
(use-package doom-themes
  :custom
  (doom-themes-padded-modeline t))
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package rainbow-mode)
(use-package hl-todo
  :config
  (global-hl-todo-mode))

;;; Settings
;;;; Theme (Needs to be set when frame available)
(let ((theme-settings (lambda ()
        (load-theme 'doom-moonlight t)
        (require 'config-emoji-modifiers)
        (set-face-attribute 'default nil :font "DejaVu Sans Mono-10")
        (set-fontset-font t 'symbol "Twemoji")
        (set-face-attribute 'fringe nil :foreground "deep sky blue"))))
  (if (daemonp)
    (add-once-hook 'server-after-make-frame-hook theme-settings)
    (funcall theme-settings)))

;;;; Hide welcome messages
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;;;; Reduce confirmations
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-processes nil)
(setq kill-buffer-query-functions
  (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;;;; Vim like scrolling
(setq scroll-step 1
      scroll-margin 0)

;;;; Don't touch my files
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/autosave/" t))
      make-backup-files nil
      create-lockfiles nil
      custom-file (make-temp-file "emacs-custom"))

;;;; Don't use input methods (allows S-SPC)
(when (eq window-system 'pgtk)
  (pgtk-use-im-context nil))

;;;; Don't overwrite system clipboard with selections
(setq select-enable-clipboard nil)

;;;; Let tab be used for completion
(setq tab-always-indent 'complete)

;;;; Built-in plugin conf
(setq eldoc-echo-area-prefer-doc-buffer t
      eldoc-minor-mode-string " Doc"
      flymake-mode-line-counter-format
  '("[" flymake-mode-line-error-counter flymake-mode-line-warning-counter "]")
      flymake-mode-line-format '(" " flymake-mode-line-counters)
      tramp-default-method "ssh")

;;;; Formatting
(setq-default fill-column 80
              tab-width 4
              indent-tabs-mode nil)
(setq sentence-end-double-space nil
      whitespace-style '(face trailing tab-mark))

;;;; Spell checking
(setq ispell-dictionary "en_US"
      ispell-program-name "aspell"
      ispell-extra-args '("--camel-case")
      flyspell-issue-message-flag nil
      flyspell-mode-line-string nil)
(defun my-flyspell-buffer (&rest _) (when flyspell-mode (flyspell-buffer)))
(add-hook 'flyspell-mode-hook #'my-flyspell-buffer)
(advice-add 'ispell-pdict-save :after #'my-flyspell-buffer)
(advice-add 'evil-paste-before :after #'my-flyspell-buffer)
(advice-add 'evil-paste-after :after #'my-flyspell-buffer)

;;;; Modes
(window-divider-mode 1)
(column-number-mode 1)
(blink-cursor-mode 0)
(fringe-mode 9)
(global-auto-revert-mode 1)

(add-hook 'prog-mode-hook (lambda ()
  (display-fill-column-indicator-mode)
  (whitespace-mode)
  (flyspell-prog-mode)
  (show-paren-mode)))

(add-hook 'text-mode-hook (lambda ()
  (display-fill-column-indicator-mode)
  (whitespace-mode)
  (flyspell-mode)
  (turn-on-auto-fill)))

(add-hook 'emacs-lisp-mode-hook (apply-partially 'electric-indent-local-mode 0))

(diminish 'abbrev-mode)
(diminish 'whitespace-mode)
(diminish 'auto-revert-mode)

;;; Start server and set environment
(let ((emacsclient "emacsclient"))
  (unless (daemonp)
    (setq server-name (concat server-name (number-to-string (emacs-pid)))
          emacsclient (concat emacsclient " --socket-name="
      (shell-quote-argument (expand-file-name server-name server-socket-dir))))
    (server-start))
  (setenv "EDITOR" emacsclient)
  (setenv "VISUAL" emacsclient))

;;; Commands
(defun esp32c3 ()
  "Serial console for esp32-c3"
  (interactive)
  (serial-term (serial-read-name) 115200))
