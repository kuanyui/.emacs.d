(getenv "GOPATH")
(require 'golint)

(defun my-go-setup ()
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)
  (flycheck-mode)
  (go-eldoc-setup)
  )

(add-hook 'go-mode-hook 'my-go-setup)

(setenv "PATH" (string-join (list (getenv "PATH")
                                  (concat (getenv "GOPATH") "/bin/"))
                            ":"))
(provide 'rc-go)
;;; rc-go.el ends here
