(message "Defining apply-company-specific-configuration")

(defun apply-company-specific-configuration (package)
  (progn
    (message (format "Applying company specific configuration for %s" package))

    (when (eq package 'inf-clojure-for-example)
      (progn
        (defun company-specific-repl-refresh ()
          "Refreshes the REPL"
          (interactive)
          (inf-clojure-eval-string "(println \"This would refresh if you configured it!\")"))

        (define-key inf-clojure-mode-map (kbd "<f5>") 'inf-clojure-ladder-repl-refresh)
        (define-key inf-clojure-minor-mode-map (kbd "<f5>") 'inf-clojure-ladder-repl-refresh)))))

(provide 'company-specific)
