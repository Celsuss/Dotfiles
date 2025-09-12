;; ~/.emacs.d/private/private.el


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions definitions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/get-secret (machine-name)
  "Retrieve a secret for MACHINE-NAME from ~/.authinfo.gpg."
  (let ((creds (car (auth-source-search :host machine-name))))
    (when creds
      (let ((secret (plist-get creds :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))
;; Example usage:
;; (setq my-api-key (my/get-secret "litellm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable definitions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq my/litellm-api-key (my/get-secret "litellm"))
(setq my/execution-context 'work)


;;;;;;;;;;;;;;;
;; Finish up ;;
;;;;;;;;;;;;;;;
(if (eq my/execution-context 'work)
    (message "Configuring for work environment.")
  (message "Configuring for home environment."))

(message "🤫 Private config loaded.")

;; The 'provide' is good practice for Emacs Lisp files
(provide 'private)
