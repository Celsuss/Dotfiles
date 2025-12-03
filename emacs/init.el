;;; init.el --- Main Entry Point -*- lexical-binding: t; -*-

;; Restore garbage collection after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 50 1000 1000)))) ; 50MB

;; ---------------------------------------------------------
;; 1. Package Manager Setup
;; ---------------------------------------------------------
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this if you want to use stable MELPA instead
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; Initialize package.el and ensure use-package integration
(unless package-archive-contents
  (package-refresh-contents))

;; Emacs 29+ includes use-package.
;; We set 'ensure t' globally so we don't have to type it every time.
(require 'use-package)
(setq use-package-always-ensure t)

;; ---------------------------------------------------------
;; 2. Load Modules (We will create these next)
;; ---------------------------------------------------------
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Example of how we will load files later:
;; (require 'init-evil)
;; (require 'init-ui)

;; ---------------------------------------------------------
;; 3. Basic "Sanity" Configs
;; ---------------------------------------------------------

;; Save backup files to a temp directory instead of cluttering project folders
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; Answers "yes" or "no" prompts with "y" or "n"
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'init)
