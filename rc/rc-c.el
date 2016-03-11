;;; rc-c.el ---                                      -*- lexical-binding: t; -*-

;;======================================================
;; C
;;======================================================
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(require 'company)
(require 'cc-mode)
(require 'flycheck)

(add-hook 'c-mode-hook #'my-c-config)
(defun my-c-config ()
  (add-to-list 'company-backends 'company-c-headers)
  (c-set-style "linux")
  (define-key c-mode-map (kbd "<f5>") 'c-compile-current-file)
  (define-key c-mode-map (kbd "C-c M-n") 'highlight-symbol-at-point)
  (define-key c-mode-map (kbd "M-n")'highlight-symbol-next)
  (define-key c-mode-map (kbd "M-p")'highlight-symbol-prev)
  (define-key c-mode-map (kbd "C-c M-p") 'highlight-symbol-query-replace)
  )
(defun c-compile-current-file ()
  (interactive)
  (save-buffer)
  (if (file-exists-p (file-name-base)) (delete-file (file-name-base)))
  (shell-command (format "gcc -Wall %s -o %s"
                         (buffer-real-name)
                         (file-name-base))))


(add-hook 'c++-mode-hook #'my-c++-config)

(defun my-c++-config ()
  (add-to-list 'company-backends 'company-c-headers)
  (define-key c++-mode-map (kbd "<f5>") 'c++-compile-current-file)
  (define-key c++-mode-map (kbd "C-c M-n") 'highlight-symbol-at-point)
  (define-key c++-mode-map (kbd "M-n")'highlight-symbol-next)
  (define-key c++-mode-map (kbd "M-p")'highlight-symbol-prev)
  (define-key c++-mode-map (kbd "C-c M-p") 'highlight-symbol-query-replace)
  (setq flycheck-gcc-language-standard "c++11")
  (flycheck-mode 1)
  (rainbow-delimiters-mode-enable))


(defun c++-compile-current-file ()
  (interactive)
  (save-buffer)
  (if (file-exists-p (file-name-base)) (delete-file (file-name-base)))
  (shell-command (format "g++ -Wall -std=c++11 %s -o %s && echo '================================================' && ./%s"
                         (buffer-real-name)
                         (file-name-base)
                         (file-name-base))))


(provide 'rc-c)
;;; rc-c.el ends here
