;;; rc-scheme.el ---                                 -*- lexical-binding: t; -*-
(with-eval-after-load 'scheme-mode
  (require 'geiser-mode)
  (add-hook 'scheme-mode #'rainbow-delimiters-mode)
  (setq geiser-default-implementation 'guile)
  (define-key geiser-mode-map (kbd "C-x C-e") #'geiser-eval-last-sexp)

  (defun geiser-my-eval-last-sexp ()
    (interactive)
    ()
    )
  )
(provide 'rc-scheme)
;;; rc-scheme.el ends here
