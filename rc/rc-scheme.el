;;; rc-scheme.el ---                                 -*- lexical-binding: t; -*-


(add-hook 'scheme-mode #'rainbow-delimiters-mode)
(setq geiser-default-implementation 'guile)
(provide 'rc-scheme)
;;; rc-scheme.el ends here
