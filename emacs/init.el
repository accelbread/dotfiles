;;; init.el -*- lexical-binding: t; -*-

;;; Default fonts

(set-face-attribute 'default nil :height 100 :family "DejaVu Sans Mono")
(set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono")
(set-face-attribute 'variable-pitch nil :family "DejaVu Sans")


;;; Networking

(setq network-security-level 'high
      gnutls-verify-error t
      gnutls-min-prime-bits 3072
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.2:-VERS-TLS1.1:-VERS-TLS1.0")

(setq auth-sources '("~/.authinfo.gpg"))


;;; Install packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-selected-packages
      '( meow gcmh svg-lib page-break-lines rainbow-delimiters flyspell-correct
         corfu corfu-doc cape kind-icon vertico orderless marginalia consult
         which-key esh-help vterm fish-completion tree-sitter tree-sitter-langs
         magit magit-todos hl-todo forge code-review virtual-comment rg rmsbolt
         eglot yasnippet markdown-mode clang-format cmake-mode rust-mode cargo
         zig-mode scad-mode toml-mode yaml-mode git-modes pdf-tools rainbow-mode
         auto-minor-mode openwith org-present))

(setq package-native-compile t
      native-comp-async-report-warnings-errors nil
      package-quickstart t
      vterm-always-compile-module t)

(unless (seq-every-p #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages t))


;;; Config utils

(defun y-or-n-p-always-y-wrapper (orig-fun &rest args)
  "Call ORIG-FUN with ARGS, automatically using `y' for `y-or-n-p' questions."
  (cl-letf (((symbol-function #'y-or-n-p) #'always))
    (apply orig-fun args)))

(defun inhibit-redisplay-wrapper (orig-fun &rest args)
  "Call ORIG-FUN with ARGS with display inhibited."
  (let ((inhibit-redisplay t))
    (apply orig-fun args)))

(defmacro push-default (newelt var)
  "Add NEWELT to the list stored in the default value of VAR."
  `(setq-default ,var (cons ,newelt (default-value ,var))))

(defun command-var (var)
  "Return lambda which calls command in VAR."
  (lambda () (interactive) (call-interactively (symbol-value var))))

(defmacro completion-pred (&rest body)
  "Return completion-predicate which runs BODY."
  `(lambda (sym buffer) (with-current-buffer buffer ,@body)))

(defun hide-minor-mode (mode &optional value)
  "Remove display for minor mode MODE from the mode line or set to VALUE."
  (setf (alist-get mode minor-mode-alist) (list value)))

(defun set-header-fixed-pitch ()
  "Set the header-line face to use fixed-pitch in the current buffer."
  (face-remap-add-relative 'header-line '(:inherit (fixed-pitch))))

(defvar program-text-faces '(tree-sitter-hl-face:comment
                             tree-sitter-hl-face:doc
                             tree-sitter-hl-face:string
                             font-lock-comment-face
                             font-lock-doc-face
                             font-lock-string-face)
  "Faces corresponding to text in prog-mode buffers.")

(defun inside-program-text-p (&rest args)
  "Checks if point is in a comment, string, or doc."
  (memq (car (ensure-list (get-text-property (1- (point)) 'face)))
        program-text-faces))

(defvar after-frame-hook nil
  "Hook for execution after first frame in daemon mode.")

(defun run-after-frame-hook ()
  "Run and clean up `after-frame-hook'."
  (remove-hook 'server-after-make-frame-hook #'run-after-frame-hook)
  (run-hooks 'after-frame-hook)
  (setq after-frame-hook nil))

(add-hook 'server-after-make-frame-hook #'run-after-frame-hook)

(defmacro after-frame (&rest args)
  "Run `args' now if not daemon and after first frame if daemon."
  (if (daemonp)
      `(add-hook 'after-frame-hook (lambda () ,@args))
    `(progn ,@args)))

(defun load-face (face)
  "Recursively define face so theme attributes can be queried."
  (unless (facep face)
    (eval `(defface ,face nil nil))
    (if-let* ((inherit (face-attribute face :inherit))
              (listp inherit))
        (mapc #'load-face inherit))))

(defmacro window-font-dim-override (face &rest body)
  "Override default face for `window-font-width' and `window-font-height'."
  (declare (indent 1))
  `(cl-letf* ((orig-window-font-width (symbol-function 'window-font-width))
              (orig-window-font-height (symbol-function 'window-font-height))
              ((symbol-function 'window-font-width)
               (lambda ()
                 (funcall orig-window-font-width nil ,face)))
              ((symbol-function 'window-font-height)
               (lambda ()
                 (funcall orig-window-font-height nil ,face))))
     ,@body))


;;; Hide welcome messages

(setq inhibit-startup-screen t
      initial-scratch-message nil
      server-client-instructions nil)


;;; Reduce confirmations

(setq use-short-answers t
      confirm-kill-processes nil
      kill-buffer-query-functions nil
      auth-source-save-behavior nil
      enable-local-variables :safe
      disabled-command-function nil)

(global-set-key ["C-x k"] 'kill-current-buffer)


;;; Disable use of dialog boxes

(setq use-dialog-box nil
      use-file-dialog nil)


;;; Prevent misc file creation

(setq auto-save-file-name-transforms
      `((".*" ,(file-name-concat user-emacs-directory "auto-save/") t))
      make-backup-files nil
      create-lockfiles nil
      custom-file null-device)


;;; Prevent input method from consuming keys

(setq pgtk-use-im-context nil)


;;; Prevent loop when printing recursive structures

(setq print-circle t)


;;; Disable overwriting of system clipboard with selection

(setq select-enable-clipboard nil)


;;; Save minibuffer history

(savehist-mode)


;;; Increase undo history

(setq undo-limit (* 4 1024 1024)
      undo-strong-limit (* 6 1024 1024)
      kill-ring-max 512
      kill-do-not-save-duplicates t)


;;; Update files modified on disk

(setq global-auto-revert-non-file-buffers t)

(global-auto-revert-mode)


;;; Scrolling

(setq scroll-conservatively 101
      scroll-margin 0
      next-screen-context-lines 3)

(pixel-scroll-precision-mode)


;;; Default to utf-8

(set-default-coding-systems 'utf-8)


;;; Formatting

(setq-default fill-column 80
              indent-tabs-mode nil
              tab-always-indent nil
              require-final-newline t)

(setq sentence-end-double-space nil)

(add-hook 'text-mode-hook #'auto-fill-mode)

(hide-minor-mode 'auto-fill-function " ↩️")


;;; Misc UI

(setq whitespace-style '(face trailing tab-mark tabs missing-newline-at-eof)
      whitespace-global-modes '(prog-mode text-mode conf-mode)
      resize-mini-windows t
      enable-recursive-minibuffers t
      suggest-key-bindings nil
      mouse-drag-and-drop-region t
      mouse-yank-at-point t)

(blink-cursor-mode -1)
(window-divider-mode)
(fringe-mode 9)
(column-number-mode)
(global-whitespace-mode)
(global-prettify-symbols-mode)
(global-hl-todo-mode)
(context-menu-mode)

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'display-fill-column-indicator-mode))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(hide-minor-mode 'abbrev-mode)
(hide-minor-mode 'global-whitespace-mode)

(with-eval-after-load 'face-remap
  (hide-minor-mode 'buffer-face-mode))


;;; Emoji

(after-frame
 (set-fontset-font t 'emoji "Noto Emoji")
 (set-fontset-font t 'emoji "Noto Color Emoji" nil 'append))

(create-fontset-from-fontset-spec
 (font-xlfd-name (font-spec :registry "fontset-coloremoji")))

(set-fontset-font "fontset-coloremoji" 'emoji "Noto Color Emoji")

(defface color-emoji nil
  "Face which uses the coloremoji fontset.")

(after-frame
 (set-face-attribute 'color-emoji nil :fontset "fontset-coloremoji"))

(defvar-local color-emoji-remapping nil
  "Holds cookie for color emoji face remapping entry.")

(define-minor-mode color-emoji-mode
  "Minor mode for color emoji."
  :lighter ""
  (when color-emoji-remapping
    (face-remap-remove-relative color-emoji-remapping))
  (setq color-emoji-remapping
        (and color-emoji-mode
             (face-remap-add-relative 'default 'color-emoji))))


;;; Mode line

(setq mode-line-compact 'long)

(setq-default mode-line-format
              `("%e "
                (:eval (when (window-dedicated-p) "📌"))
                (:eval (cond ((meow-normal-mode-p) "😺")
                             ((meow-insert-mode-p) "😸")
                             ((meow-beacon-mode-p) "😻")
                             ((meow-keypad-mode-p) "😾")
                             ((meow-motion-mode-p) "😿")
                             (t "🙀")))
                (:eval (pcase (list buffer-read-only (buffer-modified-p))
                         ('(nil nil) "✨")
                         ('(nil t) "🖋️")
                         ('(t nil) "🔒")
                         ('(t t) "🔏")))
                (:eval (when (file-remote-p default-directory) "✈️"))
                (:eval (when (buffer-narrowed-p) "🔎"))
                (:eval (propertize
                        " %l " 'display
                        (window-font-dim-override 'mode-line
                          (svg-lib-progress-bar
                           (/ (float (point)) (point-max))
                           nil :width 3 :height 0.48 :stroke 1 :padding 2
                           :radius 1 :margin 1))))
                " " (:propertize "%12b" face mode-line-buffer-id) "  "
                (:propertize
                 (:eval (unless (eq buffer-file-coding-system 'utf-8-unix)
                          (let ((base (coding-system-base
                                       buffer-file-coding-system))
                                (eol (coding-system-eol-type
                                      buffer-file-coding-system)))
                            (if (or (eq base 'utf-8)
                                    (eq base 'undecided))
                                (pcase eol (1 "dos  ") (2 "mac  "))
                              `(,(symbol-name
                                  (if (eq eol 0) base
                                    buffer-file-coding-system))
                                "  ")))))
                 face italic)
                (flymake-mode (flymake-mode-line-error-counter
                               flymake-mode-line-warning-counter "  "))
                mode-name mode-line-process
                (:eval (when (eq major-mode 'term-mode)
                         (term-line-ending-mode-line)))
                minor-mode-alist
                "  " mode-line-misc-info))


;;; Flash active mode line for bell

(defface mode-line-flash nil
  "Face used for flashing mode line.")

(defvar mode-line-flash-state nil
  "If non-nil, contains buffer with active mode line flash.")

(defun mode-line-flash-end ()
  "End the mode line flash"
  (when mode-line-flash-state
    (with-current-buffer mode-line-flash-state
      (face-remap-reset-base 'mode-line-active)
      (setq mode-line-flash-state nil))))

(defun mode-line-flash ()
  "Flash the mode line."
  (unless mode-line-flash-state
    (setq mode-line-flash-state (current-buffer))
    (face-remap-set-base 'mode-line-active '(:inherit (mode-line-flash)))
    (run-with-timer 0.05 nil #'mode-line-flash-end)))

(setq-default ring-bell-function #'mode-line-flash)


;;; Display page breaks as lines

(setq page-break-lines-max-width 80
      page-break-lines-lighter nil)

(advice-add #'page-break-lines--update-display-table :around
            (lambda (orig-fun &rest args)
              "Disable `set-face-attribute'.
`page-break-lines-mode' sets the height of its face to the default face height
which breaks `text-scale-mode'."
              (cl-letf (((symbol-function #'set-face-attribute) #'ignore))
                (apply orig-fun args)))
            '((name . ignore-setting-face)))

(global-page-break-lines-mode)


;;; Inline annotations

(setq virtual-comment-default-file
      (file-name-concat user-emacs-directory "evc"))

(with-eval-after-load 'virtual-comment
  (hide-minor-mode 'virtual-comment-mode " 📝"))


;;; Performance

(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      auto-mode-case-fold nil
      pgtk-wait-for-event-timeout 0.001
      read-process-output-max (* 1024 1024)
      process-adaptive-read-buffering nil
      command-line-ns-option-alist nil
      remote-file-name-inhibit-cache 60)

(global-so-long-mode)


;;; Meow

(require 'meow)

(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
      meow-cursor-type-motion meow-cursor-type-insert
      meow-expand-hint-counts '((word . 10)
                                (line . 0)
                                (block . 0)
                                (find . 10)
                                (till . 10)))

(dolist (m '(meow-normal-mode
             meow-insert-mode
             meow-beacon-mode
             meow-keypad-mode
             meow-motion-mode))
  (hide-minor-mode m))

(defun meow-toggle-normal ()
  "Switch between normal and motion modes."
  (interactive)
  (if (meow-normal-mode-p)
      (meow-motion-mode)
    (meow-normal-mode)))

(defun meow-delete/kill ()
  "Kill if region active, else delete."
  (interactive)
  (if (use-region-p)
      (meow-kill)
    (meow-delete)))

(defun meow-backspace/clipboard-kill ()
  "Kill to clipboard if region active, else backwards delete."
  (interactive)
  (if (use-region-p)
      (meow-clipboard-kill)
    (meow-backspace)))

(defun meow-undo-only ()
  "Cancel current selection then call `undo-only'."
  (interactive)
  (when (region-active-p)
    (meow--cancel-selection))
  (undo-only))

(defvar-keymap window-traverse-map
  "h" #'windmove-left
  "j" #'windmove-down
  "k" #'windmove-up
  "l" #'windmove-right
  "s" #'other-window)

(defun window-traverse ()
  "Activate window movement keymap."
  (interactive)
  (set-transient-map window-traverse-map t))

(defvar-keymap system-command-map
  "c" #'meow-clipboard-save
  "x" #'meow-clipboard-kill
  "v" #'meow-clipboard-yank)

(defvar-local meow-motion-next-function #'meow-next
  "Function to use for next in motion mode.")

(defvar-local meow-motion-prev-function #'meow-prev
  "Function to use for prev in motion mode.")

(meow-motion-overwrite-define-key
 `("j" . ,(command-var 'meow-motion-next-function))
 `("k" . ,(command-var 'meow-motion-prev-function))
 '("<escape>" . ignore))

(meow-leader-define-key
 '("j" . "H-j")
 '("k" . "H-k")
 '("1" . meow-digit-argument)
 '("2" . meow-digit-argument)
 '("3" . meow-digit-argument)
 '("4" . meow-digit-argument)
 '("5" . meow-digit-argument)
 '("6" . meow-digit-argument)
 '("7" . meow-digit-argument)
 '("8" . meow-digit-argument)
 '("9" . meow-digit-argument)
 '("0" . meow-digit-argument)
 '("-" . negative-argument)
 '("/" . meow-keypad-describe-key)
 '("?" . meow-cheatsheet)
 '("r" . rg-menu)
 '("n" . meow-toggle-normal)
 '("w" . window-traverse)
 '("i" . buffer-stats))

(meow-normal-define-key
 '("0" . meow-expand-0)
 '("1" . meow-expand-1)
 '("2" . meow-expand-2)
 '("3" . meow-expand-3)
 '("4" . meow-expand-4)
 '("5" . meow-expand-5)
 '("6" . meow-expand-6)
 '("7" . meow-expand-7)
 '("8" . meow-expand-8)
 '("9" . meow-expand-9)
 '("-" . negative-argument)
 '(";" . meow-reverse)
 '(":" . execute-extended-command)
 '("." . meow-bounds-of-thing)
 '("," . meow-inner-of-thing)
 '("<" . meow-beginning-of-thing)
 '(">" . meow-end-of-thing)
 '("[" . beginning-of-defun)
 '("]" . end-of-defun)
 '("?" . which-key-show-top-level)
 '("a" . meow-append)
 '("A" . meow-open-below)
 '("b" . meow-back-word)
 '("B" . meow-back-symbol)
 '("c" . meow-change)
 '("C" . meow-replace)
 '("d" . meow-delete/kill)
 '("D" . meow-backspace/clipboard-kill)
 '("e" . meow-next-word)
 '("E" . meow-next-symbol)
 '("f" . meow-find)
 '("g" . meow-cancel-selection)
 '("G" . meow-grab)
 '("h" . meow-left)
 '("H" . meow-left-expand)
 '("i" . meow-insert)
 '("I" . meow-open-above)
 '("j" . meow-next)
 '("J" . meow-next-expand)
 '("k" . meow-prev)
 '("K" . meow-prev-expand)
 '("l" . meow-right)
 '("L" . meow-right-expand)
 '("m" . meow-join)
 '("o" . meow-block)
 '("O" . meow-to-block)
 '("p" . meow-yank)
 '("P" . meow-clipboard-yank)
 '("q" . meow-quit)
 '("r" . query-replace-regexp)
 '("R" . replace-regexp)
 '("s" . meow-swap-grab)
 '("S" . meow-sync-grab)
 '("t" . meow-till)
 '("u" . meow-undo-only)
 '("U" . undo-redo)
 '("v" . meow-visit)
 '("V" . meow-search)
 '("w" . meow-mark-word)
 '("W" . meow-mark-symbol)
 '("x" . meow-line)
 '("X" . meow-goto-line)
 '("y" . meow-save)
 '("Y" . meow-clipboard-save)
 '("z" . meow-pop-selection)
 '("<escape>" . ignore))

(setq meow-char-thing-table '((?\( . round) (?\) . round)
                              (?\[ . square) (?\] . square)
                              (?\{ . curly) (?\} . curly)
                              (?x . line)
                              (?f . defun)
                              (?\" . string)
                              (?e . symbol)
                              (?w . window)
                              (?b . buffer)
                              (?p . paragraph)
                              (?. . sentence)))

(setq meow-thing-selection-directions '((inner . backward)
                                        (bounds . forward)
                                        (beginning . backward)
                                        (end . forward)))

(meow-global-mode)


;;; Completion

(require 'kind-icon)

(setq read-extended-command-predicate #'command-completion-default-include-p
      completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-in-region-function #'consult-completion-in-region
      orderless-component-separator #'orderless-escapable-split-on-space
      completion-at-point-functions (list #'cape-file
                                          (cape-super-capf #'cape-dabbrev
                                                           #'cape-ispell))
      cape-dabbrev-min-length 3
      corfu-auto t
      corfu-auto-prefix 1
      corfu-margin-formatters '(kind-icon-margin-formatter)
      kind-icon-default-face 'corfu-default
      kind-icon-blend-background nil
      kind-icon-default-style (plist-put kind-icon-default-style ':height 0.75))

(vertico-mode)
(marginalia-mode)
(global-corfu-mode)

(after-frame
 (corfu-doc-mode))

(define-key corfu-map ["RET"] nil)
(define-key corfu-map ["M-p"] #'corfu-doc-scroll-down)
(define-key corfu-map ["M-n"] #'corfu-doc-scroll-up)

(add-hook 'meow-insert-exit-hook (lambda () (when corfu-mode (corfu-quit))))


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

(advice-add #'flyspell-region :around #'inhibit-redisplay-wrapper)

;; flyspell-prog is broken when text has multiple faces
(advice-add #'flyspell-generic-progmode-verify
            :override #'inside-program-text-p)

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)


;;; Tramp

(with-eval-after-load 'tramp
  (setq tramp-default-method-alist `((,tramp-local-host-regexp nil "sudo"))
        tramp-default-method "ssh")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


;;; Shell

(setq comint-terminfo-terminal "dumb-emacs-ansi"
      comint-prompt-read-only t)

(with-eval-after-load 'comint
  (define-key comint-mode-map
              [remap beginning-of-defun] #'comint-previous-prompt)
  (define-key comint-mode-map
              [remap end-of-defun] #'comint-next-prompt))

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
      (load-face face)
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

(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)


;;; Eshell

(setq eshell-modules-list '( eshell-basic eshell-cmpl eshell-dirs eshell-glob
                             eshell-hist eshell-ls eshell-pred eshell-prompt
                             eshell-term eshell-tramp eshell-unix)
      eshell-error-if-no-glob t
      eshell-glob-include-dot-dot nil
      eshell-glob-chars-list '(?\] ?\[ ?*)
      eshell-ask-to-save-last-dir nil
      eshell-buffer-maximum-lines 5000
      eshell-history-size 512
      eshell-hist-ignoredups t
      eshell-hist-move-to-end nil
      eshell-save-history-on-exit nil
      eshell-input-filter #'eshell-input-filter-initial-space
      eshell-destroy-buffer-when-process-dies t
      eshell-cd-on-directory nil
      eshell-ls-archive-regexp "\\`\\'"
      eshell-ls-backup-regexp "\\`\\'"
      eshell-ls-clutter-regexp "\\`\\'"
      eshell-ls-product-regexp "\\`\\'")

(with-eval-after-load 'eshell
  (setup-esh-help-eldoc))

(with-eval-after-load 'esh-cmd
  (dolist (v '(eshell-last-commmand-name
               eshell-last-command-status
               eshell-last-command-result))
    (make-variable-buffer-local v))
  (push "echo" eshell-complex-commands))

(with-eval-after-load 'esh-proc
  (push "rg" eshell-needs-pipe))

(with-eval-after-load 'em-term
  (push "watch" eshell-visual-commands))

(with-eval-after-load 'em-tramp
  (require 'tramp))

(with-eval-after-load 'esh-mode
  (setq eshell-output-filter-functions
        (cons #'eshell-truncate-buffer
              (remove #'eshell-postoutput-scroll-to-bottom
                      eshell-output-filter-functions)))
  (define-key eshell-mode-map
              [remap beginning-of-defun] #'eshell-previous-prompt)
  (define-key eshell-mode-map
              [remap end-of-defun] #'eshell-next-prompt))

(with-eval-after-load 'esh-var
  ;; Have `$/' evaluate to root of current remote.
  (add-to-list
   'eshell-variable-aliases-list
   `("/" ,(lambda (_indices)
            (concat (file-remote-p default-directory) "/")))))

(add-hook 'eshell-before-prompt-hook #'eshell-begin-on-new-line)

(defun my-eshell-init ()
  "Function to run in new eshell buffers."
  (fish-completion-mode)
  (setq completion-at-point-functions '(cape-file
                                        pcomplete-completions-at-point
                                        cape-dabbrev))
  (abbrev-mode)
  (face-remap-set-base 'nobreak-space nil)
  (setenv "TERM" "dumb-emacs-ansi")
  (setenv "GIT_PAGER" "cat")
  (setenv "PAGER" "cat"))

(add-hook 'eshell-mode-hook #'my-eshell-init)

(defun my-eshell-prompt ()
  "Eshell prompt with last error code and `#' to indicate remote directory."
  (concat (unless (eshell-exit-success-p)
            (propertize
             (number-to-string eshell-last-command-status) 'face 'error))
          (if (file-remote-p default-directory) "# " "$ ")))

(setq eshell-prompt-function #'my-eshell-prompt
      eshell-prompt-regexp "^[0-9]*[$#] ")

(defun my-eshell-buffer-name ()
  "Rename eshell buffer to unique name based off of current directory."
  (rename-buffer
   (concat "*eshell " (abbreviate-file-name default-directory) "*")
   t))

(add-hook 'eshell-before-prompt-hook #'my-eshell-buffer-name)

(defun my-eshell-save-history (input)
  "Write INPUT to eshell history file."
  (let ((inhibit-message t)
        (message-log-max nil))
    (write-region (concat input "\n") nil eshell-history-file-name t)))

(advice-add #'eshell-put-history :after #'my-eshell-save-history)

(defface eshell-input nil
  "Face used for eshell input commands.")

(defun my-eshell-highlight-last-input ()
  "Highlight last eshell command."
  (add-text-properties eshell-last-input-start
                       (1- eshell-last-input-end)
                       '(face eshell-input)))

(add-hook 'eshell-pre-command-hook #'my-eshell-highlight-last-input)

(advice-add #'eshell-exec-visual :around
            (lambda (orig-fun &rest args)
              "Advise `eshell-exec-visual' to use vterm."
              (require 'vterm)
              (cl-letf (((symbol-function #'term-mode) #'ignore)
                        ((symbol-function #'term-exec)
                         (lambda (_ _ program _ args)
                           (let ((vterm-shell (string-join
                                               (cons (file-local-name program)
                                                     args)
                                               " ")))
                             (vterm-mode))))
                        ((symbol-function #'term-char-mode) #'ignore)
                        ((symbol-function #'term-set-escape-char) #'ignore))
                (apply orig-fun args)))
            '((name . eshell-visual-vterm)))

(advice-add #'eshell-term-sentinel :around
            (lambda (orig-fun &rest args)
              "Advise `eshell-term-sentinel' to use vterm."
              (cl-letf ((vterm-kill-buffer-on-exit nil)
                        ((symbol-function #'term-sentinel) #'vterm--sentinel))
                (apply orig-fun args)))
            '((name . eshell-visual-vterm)))

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

(defalias #'eshell/v #'eshell-exec-visual)

(put #'eshell/v 'eshell-no-numeric-conversions t)

(with-eval-after-load 'abbrev
  (define-abbrev-table 'eshell-mode-abbrev-table
    '(("gitcl" "git clone --filter=blob:none")
      ("gitsub" "git submodule update --init --recursive --depth 1"))))


;;; Vterm

(setq vterm-max-scrollback 5000
      vterm-timer-delay 0.01
      vterm-buffer-name-string "*vterm %s*"
      vterm-keymap-exceptions '("C-c"))

(with-eval-after-load 'vterm
  (define-key vterm-mode-map ["C-c ESC"] #'vterm-send-escape))

(defvar-keymap vterm-normal-mode-map
  "RET" #'vterm-send-return)

(define-key vterm-normal-mode-map
            [remap yank] #'vterm-yank)
(define-key vterm-normal-mode-map
            [remap xterm-paste] #'vterm-xterm-paste)
(define-key vterm-normal-mode-map
            [remap yank-pop] #'vterm-yank-pop)
(define-key vterm-normal-mode-map
            [remap mouse-yank-primary] #'vterm-yank-primary)
(define-key vterm-normal-mode-map
            [remap self-insert-command] #'vterm--self-insert)
(define-key vterm-normal-mode-map
            [remap beginning-of-defun] #'vterm-previous-prompt)
(define-key vterm-normal-mode-map
            [remap end-of-defun] #'vterm-next-prompt)

(add-hook 'vterm-mode-hook
          (lambda ()
            (add-hook 'meow-normal-enter-hook
                      (lambda () (use-local-map vterm-normal-mode-map))
                      nil t)
            (add-hook 'meow-insert-enter-hook
                      (lambda ()
                        (use-local-map vterm-mode-map)
                        (vterm-goto-char (point)))
                      nil t)))


;;; Term

(with-eval-after-load 'term
  (set-keymap-parent term-raw-escape-map nil)
  (define-key term-raw-escape-map ["ESC"] #'term-send-raw)
  (define-key term-mode-map ["C-c ESC"] #'term-send-raw))

(defvar-local meow-term-char t)

(advice-add #'term-char-mode :before-while
            (lambda ()
              "Set intended input mode to char and switch only in insert mode."
              (setq meow-term-char t)
              (term-update-mode-line)
              (meow-insert-mode-p))
            '((name . meow-term)))

(advice-add #'term-line-mode :before
            (lambda ()
              "Set intended input mode to line and switch."
              (setq meow-term-char nil)
              (term-update-mode-line))
            '((name . meow-term)))

(add-hook 'term-mode-hook
          (lambda ()
            "Ensure normal mode has line keybindings."
            (add-hook 'meow-normal-enter-hook
                      (lambda ()
                        (let ((meow-term-char-temp meow-term-char))
                          (term-line-mode)
                          (setq meow-term-char meow-term-char-temp)
                          (term-update-mode-line)))
                      nil t)
            (add-hook 'meow-insert-enter-hook
                      (lambda ()
                        (when meow-term-char
                          (term-char-mode)))
                      nil t)))

(advice-add #'term-update-mode-line :around
            (lambda (oldfun)
              "Show intended term input mode in mode line."
              (if meow-term-char
                  (let ((real-map (current-local-map)))
                    (use-local-map term-raw-map)
                    (funcall oldfun)
                    (use-local-map real-map))
                (funcall oldfun)))
            '((name . meow-term)))

(defvar-local term-line-ending "\n"
  "Line ending to use for sending to process in `term-mode'")

(defun term-line-ending-sender (proc string)
  "Function to send PROC input STRING and line ending."
  (term-send-string proc (concat string term-line-ending)))

(setq term-input-sender #'term-line-ending-sender)

(with-eval-after-load 'term
  (define-key term-raw-map ["RET"]
              (lambda ()
                "Send line ending to the buffer's current process."
                (interactive)
                (term-send-raw-string term-line-ending))))

(defun term-line-ending-send-lf ()
  "Send `\\n' as line termination."
  (declare (modes term-mode))
  (interactive)
  (setq term-line-ending "\n"))

(defun term-line-ending-send-cr ()
  "Send `\\r' as line termination."
  (declare (modes term-mode))
  (interactive)
  (setq term-line-ending "\r"))

(defun term-line-ending-send-crlf ()
  "Send `\\r\\n' as line termination."
  (declare (modes term-mode))
  (interactive)
  (setq term-line-ending "\r\n"))

(defun term-line-ending-mode-line ()
  "Get mode line string for term line ending."
  (pcase term-line-ending
    ("\n" " LF")
    ("\r" " CR")
    ("\r\n" " CRLF")))


;;; Compilation

(defun compilation-ansi-color ()
  "Apply ansi-color to compilation buffer output."
  (ansi-color-apply-on-region compilation-filter-start (point)))

(add-hook 'compilation-filter-hook #'compilation-ansi-color)


;;; Eglot

(advice-add #'eglot-completion-at-point
            :before-until #'inside-program-text-p)

(with-eval-after-load 'yasnippet
  (hide-minor-mode 'yas-minor-mode)
  (setq yas-minor-mode-map (make-sparse-keymap)))

(add-hook 'yas-keymap-disable-hook (lambda () completion-in-region-mode))

(defun setup-eglot ()
  (yas-minor-mode)
  (eglot-ensure))


;;; Treesitter

(global-tree-sitter-mode)

(hide-minor-mode 'tree-sitter-mode)

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)


;;; Formatting

(defvar-local format-region-function #'indent-region
  "Function to use for formatting region.")

(defvar-local format-buffer-function nil
  "Function to use for formatting buffer.")

(defun format-region ()
  (interactive)
  (if (region-active-p)
      (if format-region-function
          (funcall format-region-function
                   (region-beginning) (region-end))
        (user-error "Region formatting not supported!"))
    (format-buffer)))

(defun format-buffer ()
  (interactive)
  (cond (format-buffer-function
         (funcall format-buffer-function))
        (format-region-function
         (funcall format-region-function (point-min) (point-max)))))

(dolist (sym '(format-region format-buffer))
  (put sym 'completion-predicate (completion-pred (not buffer-read-only))))

(define-minor-mode format-on-save-mode
  "Minor mode for automatically formatting before saving."
  :lighter " 🧹"
  (if format-on-save-mode
      (add-hook 'before-save-hook #'format-buffer nil t)
    (remove-hook 'before-save-hook #'format-buffer t)))

(defun formatter-hook-fn (format-on-save
                          region-function
                          &optional buffer-function)
  "Create hook function to set formatter."
  (lambda ()
    (setq format-region-function region-function)
    (setq format-buffer-function buffer-function)
    (when format-on-save (format-on-save-mode))))


;;; Transient

(setq transient-default-level 7)


;;; Magit

(setq magit-view-git-manual-method 'man
      transient-history-file null-device
      magit-save-repository-buffers 'dontask
      magit-delete-by-moving-to-trash nil)

(with-eval-after-load 'magit
  (require 'forge)
  (remove-hook 'server-switch-hook #'magit-commit-diff)
  (magit-todos-mode))

(add-hook 'magit-mode-hook
          (lambda ()
            (setq meow-motion-next-function #'magit-section-forward)
            (setq meow-motion-prev-function #'magit-section-backward)))


;;; Ediff

(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

(advice-add #'ediff-quit :around #'y-or-n-p-always-y-wrapper)


;;; Which-key

(setq which-key-idle-delay 0.5
      which-key-show-early-on-C-h t
      which-key-compute-remaps t
      which-key-sort-order 'which-key-local-then-key-order
      which-key-sort-uppercase-first nil
      which-key-unicode-correction 0
      which-key-side-window-max-height 0.5)

(which-key-mode)

(hide-minor-mode 'which-key-mode)


;;; Dired

(setq wdired-allow-to-change-permissions t)


;;; Proced

(setq proced-auto-update-interval 3)

(setq-default proced-auto-update-flag t
              proced-tree-flag t
              proced-format 'custom
              proced-filter 'non-kernel)

(with-eval-after-load 'proced
  (add-to-list 'proced-format-alist
               '(custom pid user nice pcpu pmem tree (args comm)))
  (add-to-list 'proced-filter-alist
               '(non-kernel
                 (args . (lambda (arg)
                           (not (string-match "\\`\\[.*]\\'" arg)))))))

(add-hook 'proced-mode-hook #'set-header-fixed-pitch)


;;; Eldoc

(setq eldoc-echo-area-prefer-doc-buffer t
      eldoc-minor-mode-string " 📜")


;;; Flymake

(setq flymake-mode-line-format nil)


;;; Help

(advice-add 'help-buffer :override
            (lambda ()
              "Return \"*Help*\". Ignores `help-xref-following'."
              (get-buffer-create "*Help*"))
            '((name . help-xref-dont-reuse-buffer)))


;;; Info

(setq Info-additional-directory-list load-path)

(add-hook 'Info-mode-hook #'variable-pitch-mode)

(add-hook 'Info-mode-hook
          (lambda ()
            (setq meow-motion-next-function #'Info-scroll-up)
            (setq meow-motion-prev-function #'Info-scroll-down)))


;;; Man

(setq Man-width-max nil)

(add-hook 'Man-mode-hook #'variable-pitch-mode)


;;; Elisp

(setq elisp-flymake-byte-compile-load-path
      (append elisp-flymake-byte-compile-load-path load-path))

(advice-add 'elisp--company-doc-buffer :around
            (lambda (orig-fun &rest args)
              "Use different help buffer for completion docs."
              (cl-letf (((symbol-function #'help-buffer)
                         (lambda ()
                           (get-buffer-create " *help-company-doc-buffer*"))))
                (apply orig-fun args)))
            '((name . custom-help-buffer)))

(add-hook 'emacs-lisp-mode-hook #'format-on-save-mode)


;;; Org

(setq org-elipsis " ▼")

(org-babel-do-load-languages 'org-babel-load-languages
                             '((shell . t)))

(defun my-org-present-init ()
  "Configure org-present."
  (display-fill-column-indicator-mode -1)
  (org-display-inline-images)
  (org-present-read-only)
  (org-present-big)
  (org-indent-mode))

(defun my-org-present-quit ()
  "Clean up org-present configuration."
  (display-fill-column-indicator-mode)
  (org-indent-mode -1))

(add-hook 'org-present-mode-hook #'my-org-present-init)
(add-hook 'org-present-mode-quit-hook #'my-org-present-quit)


;;; Markdown

(setq markdown-asymmetric-header t
      markdown-fontify-code-blocks-natively t
      markdown-ordered-list-enumeration nil
      markdown-unordered-list-item-prefix nil
      markdown-disable-tooltip-prompt t
      markdown-command '("pandoc" "--from=markdown" "--to=html5"))


;;; Rust

(setq rust-format-on-save nil)

(with-eval-after-load 'eglot
  (setf (alist-get 'rust-mode eglot-server-programs) '("rust-analyzer"))
  (push-default '(rust-analyzer (checkOnSave (command . "clippy")))
                eglot-workspace-configuration))

(add-hook 'rust-mode-hook #'setup-eglot)
(add-hook 'rust-mode-hook (formatter-hook-fn
                           t #'indent-region #'rust-format-buffer))
(add-hook 'rust-mode-hook #'cargo-minor-mode)

(add-hook 'toml-mode-hook #'cargo-minor-mode)

(with-eval-after-load 'cargo
  (hide-minor-mode 'cargo-minor-mode))

(dolist (sym '(rust-enable-format-on-save rust-disable-format-on-save))
  (put sym 'completion-predicate #'ignore))


;;; C/C++

(defvar-local clang-format-enabled nil
  "Whether clang-format commands should be available in buffer.")

(defun c-formatter-configure ()
  (when (locate-dominating-file default-directory ".clang-format")
    (setq clang-format-enabled t
          format-region-function #'clang-format-region
          format-buffer-function #'clang-format-buffer)
    (format-on-save-mode)))

(dolist (sym '(clang-format-region clang-format-buffer))
  (put sym 'completion-predicate (completion-pred clang-format-enabled)))

(put 'clang-format 'completion-predicate #'ignore)

(with-eval-after-load 'cc-mode
  (setf (alist-get 'other c-default-style) "stroustrup"))

(add-hook 'c-mode-hook
          (lambda () (setf (alist-get 'inextern-lang c-offsets-alist) [0])))

(add-hook 'c-mode-hook #'setup-eglot)
(add-hook 'c-mode-hook #'c-formatter-configure)

(add-hook 'c++-mode-hook #'setup-eglot)
(add-hook 'c++-mode-hook #'c-formatter-configure)

(tree-sitter-hl-add-patterns 'c
  [(system_lib_string) @constructor
   (preproc_include (string_literal) @constructor)])


;;; Python

(add-hook 'python-mode-hook #'setup-eglot)

(defun ipython ()
  "Run ipython in vterm."
  (interactive)
  (let ((vterm-shell "ipython"))
    (vterm-other-window)))


;;; Zig

(setq zig-format-on-save nil)

(add-hook 'zig-mode-hook #'setup-eglot)
(add-hook 'zig-mode-hook (formatter-hook-fn
                          t #'indent-region #'zig-format-buffer))

(put 'zig-toggle-format-on-save 'completion-predicate #'ignore)


;;; Themes

(setq custom-safe-themes t)

(add-to-list 'auto-minor-mode-alist '("-theme\\.el\\'" . rainbow-mode))


;;; PDF

(pdf-loader-install)

(with-eval-after-load 'pdf-view
  (hide-minor-mode 'pdf-view-midnight-minor-mode))


;;; Open outside Emacs

(setq openwith-associations
      '(("\\.mkv\\'" "umpv" (file))))

(openwith-mode)

(advice-add #'abort-if-file-too-large :around
            (lambda (orig-fun size op-type filename &optional offer-raw)
              "Skip size warning for externally handled file types."
              (unless (string-match-p
                       (mapconcat #'car openwith-associations "\\|")
                       filename)
                (funcall orig-fun size op-type filename offer-raw)))
            '((name . skip-internal-files)))


;;; Commands

(defun pin-buffer ()
  "Toggle whether current window is dedicated to its buffer."
  (interactive)
  (set-window-dedicated-p (selected-window) (not (window-dedicated-p))))

(defun buffer-stats ()
  "Message info about buffer/point/region size/position/etc."
  (interactive)
  (message
   (concat "Point: %d, %d:%d\n"
           "Buffer: %d, %d lines"
           (when (region-active-p)
             (let* ((region-lines (count-lines (region-beginning) (region-end)))
                    (start-line (count-lines (point-min) (region-beginning)))
                    (end-line (1- (+ region-lines start-line)))
                    (start-col (save-excursion
                                 (set-window-point nil (region-beginning))
                                 (current-column)))
                    (end-col (save-excursion
                               (set-window-point nil (region-end))
                               (current-column))))
               (format "\nRegion: %d, %d:%d [%d:%d - %d:%d]"
                       (- (region-end) (region-beginning))
                       region-lines (- end-col start-col)
                       start-line start-col
                       end-line end-col))))
   (point) (1+ (current-line)) (current-column)
   (buffer-size) (count-lines (point-min) (point-max))))

(defun open-serial (device)
  "Run `serial-term' with reasonable defaults."
  (interactive
   (list (read-file-name
          "Serial port: " "/dev/" "" t nil
          (lambda (file)
            (let* ((attr (file-attributes file 'string))
                   (type (string-to-char (file-attribute-modes attr)))
                   (group (file-attribute-group-id attr)))
              (and (= type ?c)
                   (string= group "dialout")))))))
  (serial-term device nil t))


;;; Local configuration

(let ((local-init (file-name-concat user-emacs-directory "local-init.el")))
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
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 32 1024 1024))

(gcmh-mode)

(hide-minor-mode 'gcmh-mode)
