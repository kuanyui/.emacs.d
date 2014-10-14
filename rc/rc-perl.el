;;; rc-perl.el ---                                   -*- lexical-binding: t; -*-

;;; Code:

(add-hook 'cperl-mode-hook (lambda () (perl-completion-mode t)))
(defalias 'perl-mode 'cperl-mode)

(provide 'rc-perl)
;;; rc-perl.el ends here
