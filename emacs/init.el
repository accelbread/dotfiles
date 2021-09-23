;;; Configure and install packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'load-path "~/.config/emacs/lisp")

(eval-when-compile
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package diminish)

;;;; Evil
(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-undo-system
    (if (version<= "28" emacs-version)
      'undo-redo
      'undo-fu))
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
  (evil-set-type 'evil-backward-WORD-begin 'inclusive))
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-want-unimpaired-p nil)
  (evil-collection-magit-want-horizontal-movement t)
  (evil-collection-magit-use-z-for-folds t)
  :config
  (evil-collection-init))
(use-package undo-fu
  :if (version< emacs-version "28"))

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
  :config
  (global-company-mode))

;;;; Utils
(use-package vterm
  :after evil
  :custom
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
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  :hook ((c-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (zig-mode . eglot-ensure)))
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode))
(use-package markdown-mode
  :custom
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-unordered-list-item-prefix nil))
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
(use-package hl-todo
  :config
  (global-hl-todo-mode))

;;; Settings

(require 'config-emoji-modifiers)
(load-theme 'doom-moonlight t)
(set-face-attribute 'default nil :font "DejaVu Sans Mono-10")
(set-fontset-font t 'symbol "Twemoji")
(set-face-attribute 'fringe nil :foreground "deep sky blue")

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-file-name-transforms `((".*" "~/.cache/emacs/autosave/" t)))
(setq custom-file null-device)
(setq select-enable-clipboard nil)
(setq confirm-kill-processes nil)
(setq native-comp-async-report-warnings-errors nil)
(setq whitespace-style '(face trailing tab-mark))
(setq-default fill-column 80)
(setq-default tab-width 4)
(setq eldoc-echo-area-prefer-doc-buffer t)
(setq eldoc-minor-mode-string " Doc")
(setq flymake-mode-line-counter-format
  '("[" flymake-mode-line-error-counter flymake-mode-line-warning-counter "]"))
(setq flymake-mode-line-format '(" " flymake-mode-line-counters))
(setq ispell-dictionary "en_US")
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--camel-case"))
(setq flyspell-issue-message-flag nil)
(setq flyspell-mode-line-string nil)
(add-hook 'flyspell-mode-hook
  (lambda () (if flyspell-mode (run-with-idle-timer 0 nil
    (lambda () (with-local-quit (flyspell-buffer)))))))

(window-divider-mode 1)
(column-number-mode 1)
(blink-cursor-mode 0)
(fringe-mode 9)
(global-whitespace-mode 1)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(diminish 'abbrev-mode)
(diminish 'global-whitespace-mode)
