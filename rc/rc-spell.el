;;; rc-spell.el --- Fuck you                         -*- lexical-binding: t; -*-

;; ======================================================
;; ispell
;; ======================================================

(if (not (executable-find "aspell"))
    (warn "Please install aspell on your system")
  (progn
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode)
    (setq ispell-program-name "aspell")
    (setq ispell-dictionary "american")
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--dont-run-together"))
    ))

;; flyspell-mode
;; (setq-default flyspell-mode nil)

;; flyspell-prog-mode (spell checking only inside comments & strings)
;; (setq-default flyspell-prog-mode t)

;; ======================================================
;; Keybindings
;; ======================================================

(global-set-key (kbd "C-x <f3>") 'flyspell-mode)
(global-set-key (kbd "C-c <f3>") 'flyspell-buffer)
(global-set-key (kbd "<f3>") 'flyspell-check-previous-highlighted-word)
(global-set-key (kbd "C-x <f4>") 'ispell-buffer)
(global-set-key (kbd "<f4>") 'ispell-word) ;;M-$，有夠難記，很容易跟query-replace的M-%搞混


(provide 'rc-spell)
;;; rc-spell.el ends here
