;;; init-evil.el --- Evil Mode & Keybindings -*- lexical-binding: t; -*-

;; ---------------------------------------------------------
;; 1. Vim Emulation (Evil)
;; ---------------------------------------------------------

;; This must be set BEFORE evil is loaded to prevent evil from
;; overriding bindings in other modes (evil-collection handles this).
(setq evil-want-keybinding nil)

(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already t by default
  (setq evil-want-C-u-scroll t)  ;; Enable C-u scrolling like Vim
  (setq evil-want-C-i-jump nil)  ;; Fix TAB in Org-mode
  :config
  (evil-mode 1)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Set the initial state for specific modes
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; ---------------------------------------------------------
;; 2. Evil Collection (The "Spacemacs Layers" logic)
;; ---------------------------------------------------------

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; ---------------------------------------------------------
;; 3. Undo System
;; ---------------------------------------------------------

;; Emacs 28+ has a good native undo, but undo-fu is easier to interface with Evil
(use-package undo-fu
  :config
  (setq evil-undo-system 'undo-fu))

;; ---------------------------------------------------------
;; 4. Which-Key (The popup helper)
;; ---------------------------------------------------------

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;; ---------------------------------------------------------
;; 5. General.el (The Leader Key)
;; ---------------------------------------------------------

(use-package general
  :config
  ;; create a definer for the leader key
  (general-create-definer my/leader-keys
                          :keymaps '(normal visual emacs)
                          :prefix "SPC"
                          :global-prefix "C-SPC") ;; Use C-SPC in insert mode if needed

  ;; Define the bindings
  (my/leader-keys
   ;; Top level keys
   "SPC" '(execute-extended-command :which-key "M-x")
   "."   '(find-file :which-key "find file")

   ;; Buffer mappings
   "b"   '(:ignore t :which-key "buffer")
   "bb"  '(switch-to-buffer :which-key "switch buffer")
   "bk"  '(kill-current-buffer :which-key "kill buffer")
   "bn"  '(previous-buffer :which-key "previous buffer")
   "bp"  '(next-buffer :which-key "next buffer")
   "br"  '(revert-buffer :which-key "revert buffer")

   ;; Window mappings
   "w"   '(:ignore t :which-key "window")
   "wm"  '(delete-other-windows :which-key "maximize")
   "wd"  '(delete-window :which-key "close window")
   "w-"  '(split-window-below :which-key "split below")
   "w/"  '(split-window-right :which-key "split right")
   "wl"  '(evil-window-right :which-key "move right")
   "wh"  '(evil-window-left :which-key "move left")
   "wj"  '(evil-window-down :which-key "move down")
   "wk"  '(evil-window-up :which-key "move up")

   ;; File mappings
   "f"   '(:ignore t :which-key "file")
   "ff"  '(find-file :which-key "find file")
   "fs"  '(save-buffer :which-key "save file")

   ;; Evaluation
   "e"   '(:ignore t :which-key "eval")
   "eb"  '(eval-buffer :which-key "eval buffer")
   "er"  '(eval-region :which-key "eval region")))

(provide 'my-evil)
