;;; early-init.el --- Early Initialization -*- lexical-binding: t; -*-

;; Defer garbage collection during startup for speed
(setq gc-cons-threshold most-positive-fixnum)

;; Disable GUI elements early to avoid visual flicker
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable startup screen
(setq inhibit-startup-screen t)

(provide 'early-init)
