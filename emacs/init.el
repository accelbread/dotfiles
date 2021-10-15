;;; Install packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-selected-packages
      '(vterm
        evil evil-collection
        selectrum orderless marginalia company
        doom-themes rainbow-delimiters diminish hl-todo rainbow-mode
        magit forge magit-todos
        flyspell-correct
        markdown-mode cmake-mode rust-mode toml-mode cargo zig-mode yaml-mode
        eglot yasnippet
        tree-sitter tree-sitter-langs evil-textobj-tree-sitter
        which-key))

(setq no-byte-compile t
      package-native-compile t
      native-comp-async-report-warnings-errors nil
      package-quickstart t
      vterm-always-compile-module t)

(advice-add 'package-quickstart-refresh :after
            (lambda (&rest _) (native-compile-async package-quickstart-file)))

(package-install-selected-packages t)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;; Macros used for configuration

(eval-when-compile
  (defmacro after-frame (&rest body)
    "Evaluate body when frame is available."
    `(if (= 0 (length (frame-list)))
         (let ((wrapper (lambda (self)
                          (remove-hook 'server-after-make-frame-hook
                                       (apply-partially self self))
                          ,@body)))
           (add-hook 'server-after-make-frame-hook
                     (apply-partially wrapper wrapper) 90))
       ,@body)))

;;; Configuration

;;;; Hide UI elements
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;;;; Hide welcome messages
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;;;; Reduce confirmations
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-processes nil)
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;;;; Make moving past view edge not jump
(setq scroll-step 1
      scroll-margin 0)

;;;; Prevent misc file creation
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/autosave/" t))
      make-backup-files nil
      create-lockfiles nil
      custom-file (make-temp-file "emacs-custom"))

;;;; Prevent input method from consuming keys
(when (eq window-system 'pgtk) (pgtk-use-im-context nil))

;;;; Disable overwriting of system clipboard with selection
(setq select-enable-clipboard nil)

;;;; Disable audible beeps
(setq visual-bell 1)

;;;; Allow setting frame size in pixels
(setq frame-resize-pixelwise t)

;;;; Update files modified on disk
(global-auto-revert-mode 1)

;;;; Formatting
(setq-default fill-column 80
              tab-width 4
              indent-tabs-mode nil)
(setq sentence-end-double-space nil
      whitespace-style '(face trailing tab-mark))

;;;; UI
(blink-cursor-mode 0)
(window-divider-mode 1)
(fringe-mode 9)
(column-number-mode 1)

(global-hl-todo-mode)
(setq doom-themes-padded-modeline t)

(after-frame
 (load-theme 'doom-moonlight t)
 (set-face-attribute 'default nil :font "DejaVu Sans Mono-10")
 (set-fontset-font t 'symbol "Twemoji")
 (set-face-attribute 'fringe nil :foreground "deep sky blue")
 (require 'emoji-zwj-sequences))

;;;; Prog mode
(dolist (fn '(display-fill-column-indicator-mode
              whitespace-mode
              flyspell-prog-mode
              show-paren-mode
              rainbow-delimiters-mode))
  (add-hook 'prog-mode-hook fn))

;;;; Text mode
(dolist (fn '(display-fill-column-indicator-mode
              whitespace-mode
              flyspell-mode
              turn-on-auto-fill))
  (add-hook 'text-mode-hook fn))

;;;; Etc
(setq eldoc-echo-area-prefer-doc-buffer t
      tramp-default-method "ssh"
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;;;; Evil
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
(evil-ex-define-cmd "bd[elete]" 'kill-current-buffer)
(evil-ex-define-cmd "wbd[elete]" 'save-kill-current-buffer)
(evil-ex-define-cmd "bdq[uit]" 'kill-buffer-and-window)
(evil-ex-define-cmd "wbdq[uit]" 'save-kill-buffer-and-window)
(evil-global-set-key 'normal (kbd "z=") 'flyspell-correct-at-point)

;;;; Spell checking
(setq ispell-dictionary "en_US"
      ispell-program-name "aspell"
      ispell-extra-args '("--camel-case")
      flyspell-issue-message-flag nil
      flyspell-mode-line-string nil)

(defun my-flyspell-buffer (&rest _)
  "Run flyspell-buffer only if flyspell-mode is enabled."
  (when flyspell-mode (flyspell-buffer)))

(add-hook 'flyspell-mode-hook #'my-flyspell-buffer)
(advice-add 'ispell-pdict-save :after #'my-flyspell-buffer)
(advice-add 'evil-paste-before :after #'my-flyspell-buffer)
(advice-add 'evil-paste-after :after #'my-flyspell-buffer)

;;;; Completion
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

(define-key company-active-map [return] nil)
(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map [tab] 'company-complete)
(define-key company-active-map (kbd "TAB") 'company-complete)

;;;; Vterm
(setq vterm-shell "fish"
      vterm-max-scrollback 5000
      vterm-timer-delay 0.01
      vterm-buffer-name-string "vterm:%s")

(evil-define-key '(normal insert) vterm-mode-map (kbd "C-c ESC")
  'vterm-send-escape)

;; Start server and set environment
(require 'server)
(let ((emacsclient "emacsclient"))
  (unless (daemonp)
    (setq server-name (concat server-name (number-to-string (emacs-pid)))
          emacsclient (concat emacsclient " --socket-name="
                              (shell-quote-argument
                               (expand-file-name server-name
                                                 server-socket-dir))))
    (server-start))
  (setenv "EDITOR" emacsclient)
  (setenv "VISUAL" emacsclient))

;;;; Magit
(setq magit-view-git-manual-method 'man
      transient-history-file null-device
      magit-save-repository-buffers 'dontask)

(with-eval-after-load 'magit
  (require 'forge))

;;;; LSP
(setq eglot-stay-out-of '(company))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer"))))

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'zig-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

(yas-global-mode)

;;;; Treesitter
(global-tree-sitter-mode)

(add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode)
(setq-default flyspell-prog-text-faces '(tree-sitter-hl-face:comment
                                         tree-sitter-hl-face:doc
                                         tree-sitter-hl-face:string
                                         font-lock-comment-face
                                         font-lock-doc-face
                                         font-lock-string-face))

(eval-when-compile
  (defmacro ts-textobj-map (type key class)
    `(define-key
       ,(if (string= "i" type)
            'evil-inner-text-objects-map
          'evil-outer-text-objects-map)
       ,key
       '(,(concat "TS " class) .
         (evil-textobj-tree-sitter-get-textobj
           ,(concat class (if (string= "i" type) ".inner" ".outer")))))))

(ts-textobj-map "a" "c" "comment")
(ts-textobj-map "a" "S" "statement")
(ts-textobj-map "a" "f" "function")
(ts-textobj-map "i" "f" "function")
(ts-textobj-map "a" "k" "block")
(ts-textobj-map "i" "k" "block")
(ts-textobj-map "a" "i" "conditional")
(ts-textobj-map "i" "i" "conditional")
(ts-textobj-map "a" "P" "parameter")
(ts-textobj-map "i" "P" "parameter")
(ts-textobj-map "a" "C" "call")
(ts-textobj-map "i" "C" "call")
(ts-textobj-map "a" "l" "loop")
(ts-textobj-map "i" "l" "loop")
(ts-textobj-map "a" "L" "class")
(ts-textobj-map "i" "L" "class")
(ts-textobj-map "i" "S" "scopename")

;;;; Which-key
(setq which-key-idle-delay 0.5
      which-key-compute-remaps t)
(which-key-mode)

;;;; Markdown
(setq markdown-asymmetric-header t
      markdown-fontify-code-blocks-natively t
      markdown-unordered-list-item-prefix nil
      markdown-disable-tooltip-prompt t
      markdown-command '("pandoc" "--from=markdown" "--to=html5"))

;;;; Rust
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'toml-mode-hook 'cargo-minor-mode)

;;;; Reduce minor mode indicators
(setq eldoc-minor-mode-string " Doc"
      flymake-mode-line-format '(" " flymake-mode-line-counters)
      flymake-mode-line-counter-format '("[" flymake-mode-line-error-counter
                                         flymake-mode-line-warning-counter "]"))

(diminish 'auto-revert-mode)
(diminish 'company-mode)
(diminish 'abbrev-mode)
(diminish 'yas-minor-mode)
(diminish 'which-key-mode)
(diminish 'tree-sitter-mode)
(with-eval-after-load 'whitespace
  (diminish 'whitespace-mode))
(with-eval-after-load 'cargo
  (diminish 'cargo-minor-mode))

;;;; Local configuration
(require 'local-config)

;;; Commands

(defun native-compile-packages ()
  "Native compile all packages"
  (interactive)
  (dolist (dir load-path)
    (native-compile-async dir 'recursively)))

(defun save-kill-current-buffer ()
  "Save and kill current buffer"
  (interactive)
  (save-buffer)
  (kill-current-buffer))

(defun save-kill-buffer-and-window ()
  "Save and kill current buffer and window"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(defun esp32c3 ()
  "Serial console for esp32-c3"
  (interactive)
  (serial-term (serial-read-name) 115200))
