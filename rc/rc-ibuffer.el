;;; rc-ibuffer.el ---                                -*- lexical-binding: t; -*-

(require 'helm-buffers)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;;======================================================
;; IBuffer
;;======================================================

;;啟用ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(add-hook 'ibuffer-mode-hook 'hl-line-mode)
(add-hook 'ibuffer-mode-hook 'ibuffer-do-sort-by-filename/process)
;; Kill ibuffer after quit
(defadvice ibuffer-quit (after kill-ibuffer activate)
  "Kill the ibuffer buffer on exit."
  (kill-buffer "*Ibuffer*"))

;; Let's group buffers with ibuffer!!!

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Dired" (mode . dired-mode))
               ("Markdown" (or
                            (name . "^diary$")
                            (mode . markdown-mode)))
               ("C/C++ header" (name . "\\.h$"))
               ("C/C++ source" (or (mode . c-mode)
                                   (mode . c++-mode)))
               ("Go" (or (name . "\\.go$")
                         (mode . go-mode)))
               ("Vue" (name . "\\.vue$"))
               ("ReStructText" (mode . rst-mode))
               ("CSS" (or
                       (mode . stylus-mode)
                       (mode . less-css-mode)
                       (mode . css-mode)))
               ("JS" (or
                      (mode . javascript-mode)
                      (mode . js2-mode)
                      (mode . json-mode)
                      (name . "\\*js\\*")
                      (mode . nodejs-repl-mode)))
               ("JS" (or
                      (mode . javascript-mode)
                      (mode . js2-mode)
                      (mode . json-mode)
                      (mode . nodejs-repl-mode)))
               ("Coffee" (or
                          (name . "\\*CoffeeREPL\\*")
                          (mode . coffee-mode)))
               ("Jade" (or
                        (name . "\\.pug$")
                        (name . "\\.jade$")))
               ("HTML/Template" (or
                                 (mode . html-mode)
                                 (mode . web-mode)
                                 (name . "\\.yml$")))
               ("QML" (mode . qml-mode))
               ("Haskell" (or (mode . haskell-mode)
                              (mode . interactive-haskell-mode)
                              (mode . inferior-haskell-mode)
                              (name . "HS-Error")
                              (name . "\\*haskell-process-log\\*")))
               ("Python" (or (mode . python-mode)
                             (mode . ipython-mode)
                             (mode . inferior-python-mode)))
               ("Ruby" (or
                        (mode . ruby-mode)
                        (mode . enh-ruby-mode)
                        (mode . inf-ruby-mode)
                        ))
               ("LaTeX" (or (mode . latex-mode)
                            (name . "*.tex$")))
               ("IRC" (or
                       (mode . erc-mode)
                       (mode . rcirc-mode)))
               ("Lisp" (or
                        (mode . emacs-lisp-mode)
                        (mode . slime-mode)
                        (name . "^\\*scratch\\*$")
                        (mode . lisp-mode)))
               ("Scheme" (or
                          (name . "Guile")
                          (name . "geiser")
                          (mode . scheme-mode)
                          (mode . geiser-mode)))
               ("Shell Script" (or (mode . shell-script-mode)
                                   (mode . shell-mode)
                                   (mode . sh-mode)))
               ("Perl"  (or (mode . cperl-mode)
                            (mode . perl-mode)))
               ("Twitter" (mode . twittering-mode))
               ("Magit" (or (name . "*magit*")
                            (mode . magit-mode)))
               ("Emacs" (or
                         (name . "^\\*Messages\\*$")
                         (name . "^\\*Compile-Log\\*$")))
               ("Help" (or (mode . woman-mode)
                           (mode . man-mode)
                           (mode . info-mode)
                           (mode . help-mode)
                           (name . "\\*Help\\*$")
                           (name . "\\*info\\*$")))
               ("Terminal" (or (mode . eshell-mode)
                               (mode . term-mode)
                               (mode . eshell-mode)
                               (mode . comint-mode)
                               (name . "\\*scheme\\*$")))
               ("Agenda Files" (filename . "agenda/.+.org$"))
               ("Org" (or
                       (mode . org-mode)
                       (name . "^\\*Calendar\\*$")))
               ("helm" (name . "*helm"))
               ("Ag" (name . "^\\*ag search"))
               ))))


;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000) (format "%7.1fK" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   (t (format "%8dB" (buffer-size)))))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

;; auto update ibuffer
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "default")))

;; Do not show empty group
(setq ibuffer-show-empty-filter-groups nil)

;; recycle move cursor
(defun ibuffer-previous-line ()
  (interactive) (previous-line)
  (if (<= (line-number-at-pos) 2)
      (goto-line (- (count-lines (point-min) (point-max)) 2))))
(defun ibuffer-next-line ()
  (interactive) (next-line)
  (if (>= (line-number-at-pos) (- (count-lines (point-min) (point-max)) 1))
      (goto-line 3)))
(define-key ibuffer-mode-map (kbd "<up>") 'ibuffer-previous-line)
(define-key ibuffer-mode-map (kbd "<down>") 'ibuffer-next-line)

(provide 'rc-ibuffer)
;;; rc-ibuffer.el ends here
