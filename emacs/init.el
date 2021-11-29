;;; init.el -*- lexical-binding: t; -*-

;;; Fonts

(set-face-attribute 'default nil :height 150 :family "DejaVu Sans Mono")
(set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono")
(set-face-attribute 'variable-pitch nil :family "DejaVu Sans")

(set-fontset-font t 'emoji "Twemoji" nil 'prepend)


;;; Networking

(setq network-security-level 'high
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.2:-VERS-TLS1.1:-VERS-TLS1.0")


;;; Install packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-selected-packages
      '( gcmh page-break-lines rainbow-delimiters hl-todo diminish evil
         evil-collection flyspell-correct company company-posframe selectrum
         orderless marginalia fish-completion vterm eshell-vterm esh-help
         eshell-syntax-highlighting eglot yasnippet tree-sitter
         tree-sitter-langs evil-textobj-tree-sitter magit forge magit-todos
         which-key rg markdown-mode rust-mode cargo zig-mode cmake-mode
         toml-mode yaml-mode git-modes rainbow-mode auto-minor-mode openwith
         pdf-tools))

(setq package-native-compile t
      native-comp-async-report-warnings-errors nil
      package-quickstart t
      vterm-always-compile-module t)

(unless (seq-every-p #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages t))


;;; Config utils

(defun y-or-n-p-always-y-advice (orig-fun &rest args)
  "Call ORIG-FUN with ARGS, automatically using `y' for `y-or-n-p' questions."
  (cl-letf (((symbol-function #'y-or-n-p) (lambda (_prompt) t)))
    (apply orig-fun args)))

(defun inhibit-redisplay-advice (orig-fun &rest args)
  "Call ORIG-FUN with ARGS with display inhibited."
  (let ((inhibit-redisplay t))
    (apply orig-fun args)))

(defmacro push-default (newelt var)
  "Add NEWELT to the list stored in the default value of VAR."
  `(setq-default ,var
                 (cons ,newelt (default-value ,var))))


;;; Hide welcome messages

(setq inhibit-startup-screen t
      initial-scratch-message nil
      server-client-instructions nil)


;;; Reduce confirmations

(defalias 'yes-or-no-p #'y-or-n-p)

(setq confirm-kill-processes nil
      kill-buffer-query-functions nil
      auth-source-save-behavior nil)


;;; Prevent misc file creation

(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/autosave/" t))
      make-backup-files nil
      create-lockfiles nil
      custom-file null-device)


;;; Prevent input method from consuming keys

(setq pgtk-use-im-context nil)


;;; Disable overwriting of system clipboard with selection

(setq select-enable-clipboard nil)


;;; Update files modified on disk

(global-auto-revert-mode)

(diminish #'auto-revert-mode)


;;; Scrolling

(setq scroll-conservatively 101
      scroll-margin 0)

(pixel-scroll-precision-mode)


;;; Formatting

(setq-default fill-column 80
              indent-tabs-mode nil
              tab-always-indent nil)

(setq sentence-end-double-space nil)

(add-hook 'text-mode #'auto-fill-mode)


;;; Misc UI

(setq mode-line-compact 'long
      whitespace-style '(face trailing tab-mark missing-newline-at-eof)
      whitespace-global-modes '(prog-mode text-mode conf-mode))

(blink-cursor-mode -1)
(window-divider-mode)
(fringe-mode 9)
(column-number-mode)
(global-whitespace-mode)
(global-prettify-symbols-mode)
(global-hl-todo-mode)

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'display-fill-column-indicator-mode))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(diminish #'abbrev-mode)
(diminish #'global-whitespace-mode)

(with-eval-after-load 'face-remap
  (diminish #'buffer-face-mode))


;;; Flash active mode line for bell

(defface mode-line-flash nil
  "Face used for flashing mode line.")

(defvar mode-line-flash-state nil
  "If non-nil, mode line flash is active.")

(defun mode-line-flash-end ()
  "End the mode line flash"
  (when mode-line-flash-state
    (setq mode-line-flash-state nil)
    (face-remap-reset-base 'mode-line-active)))

(defun mode-line-flash ()
  "Flash the mode line."
  (unless mode-line-flash-state
    (setq mode-line-flash-state t)
    (face-remap-set-base 'mode-line-active '(:inherit (mode-line-flash)))
    (run-with-timer 0.05 nil #'mode-line-flash-end)))

(setq-default ring-bell-function #'mode-line-flash)


;;; Display page breaks as lines

(setq page-break-lines-max-width 80
      page-break-lines-lighter nil)

;; page-break-lines sets the height of its face to the default face height which
;; breaks text-scale-mode
(defun page-break-lines-no-set-face-advice (orig-fun &rest args)
  "Disable set-face-attribute for function."
  (cl-letf (((symbol-function #'set-face-attribute) (lambda (&rest _))))
    (apply orig-fun args)))

(advice-add #'page-break-lines--update-display-table :around
            #'page-break-lines-no-set-face-advice)

(global-page-break-lines-mode)


;;; Performance

(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(global-so-long-mode)


;;; Auto close pairs

(electric-pair-mode)


;;; Evil

(defun my-evil-lookup-man ()
  "Open man page for term at point."
  (call-interactively #'man-follow))

(defun evil-lookup-use-eldoc ()
  "Set `evil-lookup-func' to use eldoc in current buffer."
  (interactive)
  (setq-local evil-lookup-func #'eldoc-print-current-symbol-info))

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
      evil-lookup-func #'my-evil-lookup-man
      evil-collection-setup-minibuffer t
      evil-collection-want-unimpaired-p nil
      evil-collection-magit-want-horizontal-movement t
      evil-collection-magit-use-z-for-folds t)

(evil-mode)
(evil-collection-init)

(evil-set-type 'evil-backward-word-begin 'inclusive)
(evil-set-type 'evil-backward-WORD-begin 'inclusive)

(global-set-key ["<escape>"] #'keyboard-escape-quit)

(evil-global-set-key 'normal ["z ="] #'flyspell-correct-at-point)

(evil-ex-define-cmd "bd[elete]" #'kill-current-buffer)
(evil-ex-define-cmd "wbd[elete]" #'save-kill-current-buffer)


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
                         (:separate company-dabbrev company-ispell))
      company-posframe-lighter nil
      company-posframe-show-indicator nil
      company-posframe-show-metadata nil
      company-posframe-quickhelp-delay nil)

(global-company-mode)
(company-posframe-mode)

(diminish #'company-mode)

(define-key company-active-map [return] nil)
(define-key company-active-map ["RET"] nil)
(define-key company-active-map [tab] #'company-complete)
(define-key company-active-map ["TAB"] #'company-complete)


;;; Spell checking

(setq ispell-dictionary "en_US"
      ispell-program-name "aspell"
      ispell-extra-args '("--camel-case")
      flyspell-issue-message-flag nil
      flyspell-mode-line-string nil
      flyspell-duplicate-distance 0)

(setq-default flyspell-prog-text-faces '(tree-sitter-hl-face:comment
                                         tree-sitter-hl-face:doc
                                         tree-sitter-hl-face:string
                                         font-lock-comment-face
                                         font-lock-doc-face
                                         font-lock-string-face))

(defun flyspell-configure-jit-lock ()
  "Set `flyspell-region' in jit-lock functions matching `flyspell-mode'."
  (if flyspell-mode
      (jit-lock-register #'flyspell-region)
    (jit-lock-unregister #'flyspell-region)))

(add-hook 'flyspell-mode-hook #'flyspell-configure-jit-lock)

(advice-add #'flyspell-region :around #'inhibit-redisplay-advice)

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)


;;; Tramp

(with-eval-after-load 'tramp
  (setq tramp-default-method-alist `((,tramp-local-host-regexp nil "sudo"))
        tramp-default-method "ssh")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


;;; LSP

(setq eglot-stay-out-of '(company))

(yas-global-mode)

(diminish #'yas-minor-mode)

(add-hook 'eglot-managed-mode-hook #'evil-lookup-use-eldoc)

(add-hook 'zig-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook #'eglot-ensure)


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


;;; Shell

(setq comint-terminfo-terminal "dumb-emacs-ansi")

(defun set-ls-colors ()
  "Set LS_COLORS based off of eshell-ls colors."
  (let (ls-colors)
    (pcase-dolist (`(,entry ,face)
                   '(("di" eshell-ls-directory)   ; Directory
                     ("ex" eshell-ls-executable)  ; Executable
                     ("ln" eshell-ls-symlink)     ; Symlink
                     ("mi" eshell-ls-missing)     ; Missing file
                     ("pi" eshell-ls-special)     ; Named pipe
                     ("bd" eshell-ls-special)     ; Block device
                     ("cd" eshell-ls-special)     ; Char device
                     ("so" eshell-ls-special)))   ; Socket
      ;; Define face if eshell not loaded yet in order to grab color from theme.
      (unless (facep face)
        (eval `(defface ,face nil nil)))
      (let* ((face-color (face-attribute face :foreground nil t))
             (r (substring face-color 1 3))
             (g (substring face-color 3 5))
             (b (substring face-color 5 7)))
        ;; Convert to base 10
        (setq r (number-to-string (string-to-number r 16)))
        (setq g (number-to-string (string-to-number g 16)))
        (setq b (number-to-string (string-to-number b 16)))
        (setq ls-colors (concat ls-colors entry "=38;2;" r ";" g ";" b ":"))))
    (setenv "LS_COLORS" ls-colors)))

(set-ls-colors)


;;; Eshell

(defun my-eshell-prompt ()
  "Eshell prompt with last error code and `#' to indicate remote directory."
  (concat (unless (eshell-exit-success-p)
            (propertize
             (number-to-string eshell-last-command-status) 'face 'error))
          (if (file-remote-p default-directory) "# " "$ ")))

(defun my-eshell-buffer-name ()
  "Rename eshell buffer to unique name based off of current directory."
  (rename-buffer (concat "eshell:" (abbreviate-file-name default-directory)) t))

(defun my-eshell-init ()
  "Function to run in new eshell buffers."
  (face-remap-set-base 'nobreak-space nil)
  (setenv "TERM" "dumb-emacs-ansi")
  (setenv "GIT_PAGER" ""))

(setq eshell-modules-list '( eshell-basic eshell-cmpl eshell-dirs eshell-glob
                             eshell-hist eshell-ls eshell-pred eshell-prompt
                             eshell-term eshell-tramp eshell-unix)
      eshell-error-if-no-glob t
      eshell-glob-include-dot-dot nil
      eshell-ask-to-save-last-dir nil
      eshell-buffer-maximum-lines 5000
      eshell-history-size 512
      eshell-hist-ignoredups t
      eshell-hist-move-to-end nil
      eshell-prompt-function #'my-eshell-prompt
      eshell-prompt-regexp "^[0-9]*[$#] "
      eshell-input-filter #'eshell-input-filter-initial-space
      eshell-destroy-buffer-when-process-dies t
      eshell-ls-archive-regexp ""
      eshell-ls-backup-regexp ""
      eshell-ls-clutter-regexp ""
      eshell-ls-product-regexp "")

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
    '(("gitcl"
       "git clone --filter=blob:none")
      ("gitsub"
       "git submodule update --init --recursive --checkout --depth 1"))))

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


;;; Magit

(setq magit-view-git-manual-method 'man
      transient-history-file null-device
      magit-save-repository-buffers 'dontask
      magit-delete-by-moving-to-trash nil)

(with-eval-after-load 'magit
  (require 'forge))


;;; Ediff

(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

(advice-add #'ediff-quit :around #'y-or-n-p-always-y-advice)


;;; Which-key

(setq which-key-idle-delay 0.5
      which-key-compute-remaps t
      which-key-sort-order 'which-key-description-order
      which-key-side-window-max-height 0.5
      which-key-unicode-correction 0)

(which-key-mode)

(diminish #'which-key-mode)


;;; Proced

(setq proced-auto-update-interval 1)


;;; Eldoc

(setq eldoc-echo-area-prefer-doc-buffer t
      eldoc-minor-mode-string " Doc")


;;; Flymake

(setq flymake-mode-line-format '(" " flymake-mode-line-counters)
      flymake-mode-line-counter-format '("[" flymake-mode-line-error-counter
                                         flymake-mode-line-warning-counter "]"))


;;; Info

(add-hook 'Info-mode-hook #'variable-pitch-mode)


;;; Man

(setq Man-width-max nil)

(add-hook 'Man-mode-hook #'variable-pitch-mode)


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

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (push-default '(rust-analyzer (checkOnSave (command . "clippy")))
                eglot-workspace-configuration))

(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'toml-mode-hook #'cargo-minor-mode)

(with-eval-after-load 'cargo
  (diminish #'cargo-minor-mode))


;;; C

(add-hook 'c-mode-hook #'eglot-ensure)

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

(openwith-mode)

(advice-add #'abort-if-file-too-large :around
            (lambda (orig-fun size op-type filename &optional offer-raw)
              "Skip size warning for externally handled file types."
              (unless (string-match-p
                       (mapconcat #'car openwith-associations "\\|")
                       filename)
                (funcall orig-fun size op-type filename offer-raw))))


;;; Commands

(defun save-kill-current-buffer ()
  "Save and kill current buffer."
  (interactive)
  (save-buffer)
  (kill-current-buffer))

(defun esp32c3 ()
  "Serial console for esp32-c3."
  (interactive)
  (serial-term (serial-read-name) 115200))


;;; Local configuration

(let ((local-init (concat user-emacs-directory "local-init.el")))
  (if (file-exists-p local-init)
      (load-file local-init)))


;;; Server

(require 'server)

(unless (daemonp)
  (setq server-name (concat server-name (number-to-string (emacs-pid))))
  (server-start))

(setenv "EMACS_SOCKET_NAME" (expand-file-name server-name server-socket-dir))
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" "emacsclient")


;;; Garbage collect when idle

(setq gcmh-idle-delay 'auto
      gcmh-auto-idle-delay-factor 10)

(gcmh-mode)

(diminish #'gcmh-mode)
