;;; rc-scheme.el ---                                 -*- lexical-binding: t; -*-

(setq process-coding-system-alist
      (cons '("guile" utf-8 . utf-8) process-coding-system-alist))
 
(setq scheme-program-name "guile")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)


(provide 'rc-scheme)
;;; rc-scheme.el ends here
