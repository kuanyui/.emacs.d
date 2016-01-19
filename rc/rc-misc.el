;;; rc-misc.el ---                                   -*- lexical-binding: t; -*-

;; misc 雜項

;;======================================================
;; Wikipedia
;;======================================================

(require 'wikipedia-mode)
(add-to-list 'auto-mode-alist '("\\.wiki\\'" . wikipedia-mode))

;;======================================================
;; Google Translate
;;======================================================

(require 'text-translator)
(require 'text-translator-load)
(global-set-key "\C-x\M-t" 'text-translator)
(global-set-key "\C-x\M-T" 'text-translator-translate-last-string)
(setq text-translator-auto-selection-func 'text-translator-translate-by-auto-selection-entw)
(global-set-key "\C-xt" 'text-translator-translate-by-auto-selection)

(setq google-translate-default-source-language "en"
      google-translate-default-target-language "zh")


(provide 'rc-misc)
;;; rc-misc.el ends here
