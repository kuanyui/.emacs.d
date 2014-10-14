;;; rc-haskell.el ---                                -*- lexical-binding: t; -*-

;;======================================================
;; Haskell
;;======================================================
(require 'haskell-mode)

;;Indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation); this conflicts with SHM

;; structured-haskell-mode
;; (add-to-list 'load-path "~/.emacs.d/lisps/structured-haskell-mode/elisp/")
;; (require 'shm)
;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
;; (setq shm-program-name "~/.cabal/bin/structured-haskell-mode")
;;; (define-key haskell-mode-map (kbd "RET") 'shm/newline-indent-proxy)

;; activates keybindings associated with interactive mode
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; This enables some handy and benign features.
(setq haskell-process-suggest-remove-import-lines t)
(setq haskell-process-auto-import-loaded-modules t)
(setq haskell-process-log t)

;; ghci, cabal-repl, cabal-dev, cabal-ghci
(setq haskell-process-type 'ghci)

(setq haskell-interactive-prompt "lambda> ")

(define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-bring)



(provide 'rc-haskell)
;;; rc-haskell.el ends here
