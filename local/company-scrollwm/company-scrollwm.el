

(defun company-scrollwm--candidates ()
  "Candidates handler for the company backend."
  (cons :async
        (lambda (cb)
          (let* ((context (scrollwm--name-at-point))
                 (raw-result (scrollwm-cmd
                              (format "emacs.completion_candidates(\"%s\")" context)))
                 (result (split-string raw-result ",")))

            (funcall cb result)))))

(defun company-scrollwm--prefix ()
  (unless (company-in-string-or-comment)
    ;; Trigger completion at once if the immediate left char is '.' or ':'
    ;; (ignoring company-minimum-prefix-length).
    ;; See 'prefix' documentation in company.el
    (or (company-grab-symbol-cons "[.:]" 1)
        'stop)))

(defun company-scrollwm (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-scrollwm))
    (prefix (company-scrollwm--prefix))
    (candidates (company-scrollwm--candidates))
    (duplicates t)
    (sorted nil)))

(provide 'company-scrollwm)
