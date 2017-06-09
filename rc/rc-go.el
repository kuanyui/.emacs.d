(getenv "GOPATH")
(require 'golint)
(add-hook 'go-mode-hook 'flycheck-mode)
(setenv "PATH" (string-join (list (getenv "PATH")
                                  (concat (getenv "GOPATH") "/bin/"))
                            ":"))
(provide 'rc-go)
;;; rc-go.el ends here
