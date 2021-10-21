;;; early-init.el -*- lexical-binding: t; -*-

;;; Temporarily disable GC
(setq gc-cons-threshold most-positive-fixnum)

;;; Hide UI elements
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Don't use X resources
(advice-add #'x-apply-session-resources :override #'ignore)
