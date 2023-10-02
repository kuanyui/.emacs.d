;;; rc-edit.el ---                                -*- lexical-binding: t; -*-

;; ======================================================
;; Electric... Auto close quote or parenthesis, But too annoying
;; ======================================================
;; (electric-pair-mode -1)

;; ======================================================
;; Align
;; ======================================================
(defun my-align (start end regexp)
  "Repeat alignment with respect to the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end (concat "\\(\\s-*\\)" regexp) 1 1 t))
;;======================================================
;; Goto-last-change
;;======================================================

(require 'goto-chg)
(global-set-key (kbd "M-(") 'goto-last-change)
(global-set-key (kbd "M-)") 'goto-last-change-reverse)

;; ======================================================
;; smartparens
;; ======================================================

;; Auto close parens
;; (require 'smartparens-config)
;; (setq sp-autoinsert-if-followed-by-word nil)
;; (setq sp-autoinsert-quote-if-followed-by-closing-pair nil)
;; (smartparens-global-mode -1)

;; (add-hook 'prog-mode-hook #'smartparens-mode)

;;======================================================
;; multiple-cursors
;;======================================================
;;
(require 'multiple-cursors)
(global-set-key (kbd "C-x C-@") 'mc/edit-lines)
(move-text-default-bindings)


(if (window-system)
    (progn
      ;; Follow my VSCode configuration in GUI Emacs
      (global-set-key (kbd "M-S-<up>") 'mc/mark-previous-like-this)
      (global-set-key (kbd "M-S-<down>") 'mc/mark-next-like-this)
      (global-set-key (kbd "C-c M-'") 'mc/mark-all-like-this)
      (global-set-key [M-down] 'move-text-down)
      (global-set-key [M-up]   'move-text-up)
      (global-set-key (kbd "M-'") 'mc/mark-next-like-this-symbol)
      (global-set-key (kbd "M-\"") 'mc/mark-previous-like-this-symbol)
      (global-set-key (kbd "C-S-d") 'duplicate-thing)
      (global-set-key (kbd "C-z") 'undo-tree-undo)
      (global-set-key (kbd "C-S-z") 'undo-tree-redo)
      )
  (progn
    (global-set-key [M-down] 'move-text-down)
    (global-set-key [M-up]   'move-text-up)
    (global-set-key (kbd "C-c d l") 'duplicate-thing)
    (global-set-key (kbd "M-'") 'mc/mark-next-like-this-symbol)
    (global-set-key (kbd "M-\"") 'mc/mark-previous-like-this-symbol)
    (global-set-key (kbd "C-c M-'") 'mc/mark-all-like-this)
    ))

(define-key mc/mark-more-like-this-extended-keymap (kbd "DEL") 'backward-delete-char-untabify)


;; set-mark, multiple-cursors & cua-mode
;; (cua-mode t)
;; (setq cua-enable-cua-keys nil) ;;変なキーバインド禁止
;; (global-set-key (kbd "C-c C-@") 'cua-set-rectangle-mark)
;; (global-set-key (kbd "M-RET") 'set-mark-command) ;這他媽的會跟org-mode衝啊！
;; (global-set-key (kbd "C-c RET") 'cua-set-rectangle-mark)
;;(global-set-key (kbd "C-x RET") 'mc/edit-lines)
(with-eval-after-load 'org
  (add-hook 'org-mode-hook
            (lambda ()
              ;;            (define-key org-mode-map (kbd "M-RET") 'set-mark-command) ;;讓org-mode能用M-RET來set-mark-command
              (define-key org-mode-map (kbd "C-c SPC") 'ace-jump-word-mode)
              (define-key org-mode-map (kbd "C-c C-e") 'org-export-dispatch)
              ))
  )
;; ace-jump
(global-set-key (kbd "C-c SPC") 'ace-jump-word-mode)

;; ======================================================
;; Expand region
;; ======================================================
(require 'expand-region)
(global-set-key (kbd "C-'") 'er/expand-region)
(global-set-key (kbd "C-\"") 'er/mark-inside-quotes)

;;======================================================
;; Abbrevs
;;======================================================
(setq abbrev-file-name "~/.emacs.d/abbrev_defs.el")
(quietly-read-abbrev-file)
(setq save-abbrevs 'sliently)
(setq-default abbrev-mode t)
;;(quietly-read-abbrev-file)       ;; reads the abbreviations file
;; (if (file-exists-p abbrev-file-name)
;;     (quietly-read-abbrev-file))

;; 哈哈哈輕鬆新增abbrev
(defun abbrev-add-global (begin end)
  "Select the string you like, then press C-x a g to add an
abbrev for it."
  (interactive "r")
  (if (and begin end mark-active)
      (let* ((expansion (buffer-substring-no-properties begin end))
             (abbreviation (read-from-minibuffer (format "[Global] Abbrev of \"%s\": " expansion))))
        (if (yes-or-no-p (format "%s => %s, Continue? " abbreviation expansion))
            (progn
              (define-abbrev global-abbrev-table abbreviation expansion)
              (write-abbrev-file)
              (message "The abbrev saved."))
          (message "Interrupted by user.")))
    (message "Please select the string you want to have an abbrev first.")))
(global-set-key (kbd "C-x a g") 'abbrev-add-global)

(defun abbrev-add-local (begin end)
  (interactive "r")
  (if (and begin end mark-active)
      (let* ((expansion (buffer-substring-no-properties begin end))
             (abbreviation (read-from-minibuffer (format "[Local] Abbrev of \"%s\": " expansion))))
        (if (yes-or-no-p (format "%s => %s, Continue? " abbreviation expansion))
            (progn
              (define-mode-abbrev abbreviation expansion)
              (write-abbrev-file))
          (message "Interrupted by user.")))
    (message "Please select the string you want to have an abbrev first.")))
(global-set-key (kbd "C-x a l") 'abbrev-add-local)


;;======================================================
;; pangu-spacing： 中英文之間自動插入空白
;;======================================================

(require 'pangu-spacing)

;; 只在 org-mode 和 markdown-mode 中啟用 pangu-spacing
;; (add-hook 'org-mode-hook
;;           '(lambda ()
;;              (set (make-local-variable 'pangu-spacing-real-insert-separtor) nil)))
(add-hook 'markdown-mode-hook
          '(lambda ()
             (pangu-spacing-mode 1)
             (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

(setq pangu-spacing-real-insert-separtor t)

;;(remove-hook 'org-mode-hook
;;	     '(lambda ()
;;		(pangu-spacing-mode 1)
;;                (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)
;;		))
;;
;;======================================================
;; vlf - deal with Very Large File
;;======================================================

(add-hook 'find-file-hook (lambda ()
                            (when (> (buffer-size) (* 1024 1024))
                              (setq buffer-read-only t)
                              (buffer-disable-undo)
                              (fundamental-mode))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;======================================================
;; Swoop
;;======================================================
;; (require 'swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'swoop)
;;(global-set-key (kbd "M-I") 'swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'swoop-multi)
;; (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)



(provide 'rc-edit)
;;; rc-edit.el ends here
