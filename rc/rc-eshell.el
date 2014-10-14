;;; rc-eshell.el ---                                 -*- lexical-binding: t; -*-
;;======================================================
;; EShell/Term
;;======================================================
;;Term下不要使用當行高亮，避免使用如MOC(music on console)等程式時出現的無意義當行高亮。
(add-hook 'term-mode-hook
          (lambda ()
            (setq global-hl-line-mode nil)
            (setq global-linum-mode nil)))

;; 如果當前user是root，prompt改成#
(setq eshell-prompt-function
      '(lambda ()
         (concat
          user-login-name "@" system-name " "
          (if (search (directory-file-name (expand-file-name (getenv "HOME"))) (eshell/pwd))
              (replace-regexp-in-string (expand-file-name (getenv "HOME")) "~" (eshell/pwd))
            (eshell/pwd))
          (if (= (user-uid) 0) " # " " $ "))))


;; 高亮 prompt...好像不是很有必要
(defun colorfy-eshell-prompt ()
  "Colorfy eshell prompt according to `user@hostname' regexp."
  (let* ((mpoint)
         (user-string-regexp (concat "^" user-login-name "@" system-name)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat user-string-regexp ".*[$#]") (point-max) t)
        (setq mpoint (point))
        (overlay-put (make-overlay (point-at-bol) mpoint) 'face '(:foreground "#729fcf")))
      (goto-char (point-min))
      (while (re-search-forward user-string-regexp (point-max) t)
        (setq mpoint (point))
        (overlay-put (make-overlay (point-at-bol) mpoint) 'face '(:foreground "#72cf6c"))
        ))))


(provide 'rc-eshell)
;;; rc-eshell.el ends here
