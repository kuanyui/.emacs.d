;;; rc-qml.el ---                                    -*- lexical-binding: t; -*-

(require 'qml-mode)
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))
(add-hook 'qml-mode-hook '(lambda ()
                            (local-set-key (kbd "<f5>") 'qml-call-qmlviewer)
                            (setq rainbow-mode t)
                            (rainbow-delimiters-mode t)
                            (highlight-symbol-mode)))
(defun qml-call-qmlviewer ()
  (interactive)
  (save-buffer)
  (let* ((file (replace-regexp-in-string "\\.qml$" ".py" (buffer-real-name))))
    (if (file-exists-p (concat default-directory file))
        (shell-command (format "python3 %s" file))
      (shell-command (format "qmlviewer %s" (buffer-real-name))))))


(provide 'rc-qml)
;;; rc-qml.el ends here