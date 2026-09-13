(let ((json-encoding-pretty-print nil))
  (json-encode
   (list
    (cons 'gptel
          (if (featurep 'gptel)
              (let ((busy-bufs
                     (delq nil (mapcar (lambda (e)
                                         (ignore-errors
                                           (buffer-name (plist-get (gptel-fsm-info (cadr e)) :buffer))))
                                       gptel--request-alist))))
                (vconcat
                 (delq nil
                       (mapcar (lambda (b)
                                 (when (buffer-local-value 'gptel-mode b)
                                   (list (cons 'buffer (buffer-name b))
                                         (cons 'backend (ignore-errors
                                                          (gptel-backend-name (buffer-local-value 'gptel-backend b))))
                                         (cons 'model (format "%s" (buffer-local-value 'gptel-model b)))
                                         (cons 'busy (and (member (buffer-name b) busy-bufs) t)))))
                               (buffer-list)))))
            []))
    (cons 'gptel_default
          (when (featurep 'gptel)
            (list (cons 'backend (ignore-errors (gptel-backend-name gptel-backend)))
                  (cons 'model (format "%s" gptel-model)))))
    (cons 'gptel_busy (if (boundp 'gptel--request-alist) (length gptel--request-alist) 0))
    (cons 'ellama
          (if (featurep 'ellama)
              (vconcat
               (delq nil
                     (mapcar (lambda (b)
                               (let ((s (buffer-local-value 'ellama--current-session b)))
                                 (when s
                                   (list (cons 'buffer (buffer-name b))
                                         (cons 'model (ignore-errors (llm-name (ellama-session-provider s))))
                                         (cons 'busy (and (buffer-local-value 'ellama--current-request b) t))))))
                             (buffer-list))))
            []))
    (cons 'ellama_default
          (when (and (featurep 'ellama) (boundp 'ellama-provider) ellama-provider)
            (ignore-errors (llm-name ellama-provider))))
    (cons 'agent_shell
          (if (featurep 'agent-shell)
              (vconcat
               (delq nil
                     (mapcar (lambda (b)
                               (with-current-buffer b
                                 (when (derived-mode-p 'agent-shell-mode)
                                   (let ((st agent-shell--state))
                                     (list (cons 'buffer (buffer-name b))
                                           (cons 'agent (map-elt (map-elt st :agent-config) :mode-line-name))
                                           (cons 'model (map-elt st :model-id))
                                           (cons 'title (map-elt st :title))
                                           (cons 'cwd (expand-file-name default-directory))
                                           (cons 'busy (and shell-maker--busy t))
                                           (cons 'pid (ignore-errors
                                                        (process-id (map-elt (map-elt st :client) :process)))))))))
                             (buffer-list))))
            [])))))
