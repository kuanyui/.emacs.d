;;; rc-c.el ---                                      -*- lexical-binding: t; -*-

;;======================================================
;; C
;;======================================================

(require 'company)
(require 'flycheck)


(add-hook 'c-mode-hook #'my-c-config)
(defun my-c-config ()
  ;; Check if FILENAME.cpp existed in same directory.
  ;; If found, switch to c++-mode.
  (if (file-exists-p (concat (file-name-base (buffer-file-name)) ".cpp"))
      (c++-mode)
    (progn
      (add-to-list 'company-backends 'company-c-headers)
      (c-set-style "linux")
      (define-key c-mode-map (kbd "<f5>") 'c-compile-current-file)
      )))
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
  (setq flycheck-gcc-language-standard "c++11")
  (setq flycheck-clang-language-standard "c++11")
  ;; open header file on #include <...>
  (define-key c++-mode-map (kbd "C-c h") 'ff-find-other-file)
  ;;(semantic-mode t)

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

;; ======================================================
;; Semantic
;; ======================================================
(require 'semantic)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(global-semantic-idle-completions-mode nil) ;; 好像跟 Company 的功能重疊
(global-semantic-decoration-mode t)
(global-semantic-highlight-func-mode t) ;; 其實預設就是開的，就是螢幕最上面那一條
(global-semantic-show-unmatched-syntax-mode t)
(mapc (lambda (x)
        (semantic-add-system-include (format "/usr/include/%s" x) 'c++-mode))
      '("QtTest"
        "QtXml"
        "QtXmlPatterns"
        "QtNetwork"
        "QtOpenGL"
        "QtScript"
        "QtSql"
        "QtGui"
        "QtHelp"
        "QtMultimedia"
        "QtDBus"
        "QtDeclarative"
        "QtDesigner"
        "QtCore"
        "Qt3Support"
        "Qt"
        "QtSvg"
        "QtScriptTools"
        "QtUiTools"))

;; ======================================================
;; eldoc
;; ======================================================
;;(add-hook 'c-mode-common-hook 'my-cc-eldoc-setup)
;;(defun my-cc-eldoc-setup ()
;;  (setq c-eldoc-includes "`pkg-config --cflags --libs` -I./ -I../")
;;  (c-turn-on-eldoc-mode))

;; ======================================================
;; gtags
;; ======================================================
(require 'helm-gtags)
(require 'asm-mode)

(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; Set key bindings
(define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
(define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
(define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
(define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)

;; ======================================================
;; irony
;; ======================================================
;;(require 'irony)
;;(eval-after-load 'company
;;  '(add-to-list 'company-backends 'company-irony))
;;
;;(eval-after-load 'flycheck
;;  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;;
;;(add-hook 'c++-mode-hook 'irony-mode)
;;(add-hook 'c-mode-hook 'irony-mode)
;;(add-hook 'objc-mode-hook 'irony-mode)
;;
;;;; replace the `completion-at-point' and `complete-symbol' bindings in
;;;; irony-mode's buffers by irony-mode's function
;;(defun my-irony-mode-hook ()
;;  (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
;;(define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))
;;(add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)



(provide 'rc-c)
;;; rc-c.el ends here
