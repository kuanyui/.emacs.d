;;; rc-qt.el ---                                     -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.model\\'" . json-mode))
(add-hook 'json-mode-hook 'auto-revert-mode)
(add-to-list 'auto-mode-alist '("\\.qrc\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("qmldir\\'" . conf-mode))




(provide 'rc-qt)
;;; rc-qt.el ends here
