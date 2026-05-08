;;; rc-hl-occurs.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2026  ono


;;======================================================
;; Enhance `symbol-overlay' and wrap it.
;;======================================================

(require 'symbol-overlay)
(require 'hi-lock)

;; ======================================================
;; face cycling
;; ======================================================
(defvar hloccur-faces '(hi-yellow hi-pink hi-green hi-blue hi-salmon hi-aquamarine))
(defvar hloccur-face-index 0)

(defun hloccur-next-face ()
  "Return next face from `hloccur-faces', cycling."
  (let ((face (nth (mod hloccur-face-index (length hloccur-faces)) hloccur-faces)))
    (setq hloccur-face-index (1+ hloccur-face-index))
    face))

;; ======================================================
;; aliases to upstream commands
;; ======================================================

(defalias 'hloccur-jump-next      'symbol-overlay-jump-next)
(defalias 'hloccur-jump-prev      'symbol-overlay-jump-prev)
(defalias 'hloccur-rename         'symbol-overlay-rename)
(defalias 'hloccur-remove-symbols 'symbol-overlay-remove-all)
(defalias 'hloccur-mode           'symbol-overlay-mode)

;; ======================================================
;; commands
;; ======================================================
(defun hloccur-region-literal (beg end)
  "Highlight literal text in region via hi-lock, ignoring syntax."
  (interactive "r")
  (let ((str (regexp-quote (buffer-substring-no-properties beg end))))
    (deactivate-mark)
    (highlight-regexp str (hloccur-next-face))))

(defun hloccur-put ()
  "Highlight region literally if active, otherwise toggle symbol-overlay at point."
  (interactive)
  (if (use-region-p)
      (call-interactively #'hloccur-region-literal)
    (call-interactively #'symbol-overlay-put)))

(defun hloccur-unhighlight ()
  "Remove symbol-overlay at point if any, else fall back to unhighlight-regexp."
  (interactive)
  (if (symbol-overlay-assoc (symbol-overlay-get-symbol nil t))
      (symbol-overlay-put)
    (call-interactively #'unhighlight-regexp)))

(defun hloccur-remove-all ()
  "Remove all highlights from both symbol-overlay and hi-lock."
  (interactive)
  (symbol-overlay-remove-all)
  (unhighlight-regexp t))

;; ======================================================
;; keybinding helper
;; ======================================================

(defun hloccur-bind-keys (map)
  "Bind hloccur commands in MAP."
  (define-key map (kbd "C-c M-n")     #'hloccur-put)
  (define-key map (kbd "C-M-\"")      #'hloccur-put)
  (define-key map (kbd "C-c C-M-\"")  #'hloccur-remove-symbols)
  (define-key map (kbd "M-n")         #'hloccur-jump-next)
  (define-key map (kbd "M-p")         #'hloccur-jump-prev)
  (define-key map (kbd "C-c M-p")     #'hloccur-rename))

;; ======================================================
;; prog-mode + global bindings
;; ======================================================

(hloccur-bind-keys prog-mode-map)

(global-set-key (kbd "M-s h SPC") #'hloccur-put)
(global-set-key (kbd "M-s h u")   #'hloccur-unhighlight)
(global-set-key (kbd "M-s h U")   #'hloccur-remove-all)

;; ======================================================
;; per-mode bindings
;; ======================================================

(with-eval-after-load 'make-mode
  (define-key makefile-mode-map (kbd "M-n") #'hloccur-jump-next)
  (define-key makefile-mode-map (kbd "M-p") #'hloccur-jump-prev))

(mapc
 (lambda (name)
   (let ((mode-symbol      (intern (concat name "-mode")))
         (mode-hook-symbol (intern (concat name "-mode-hook")))
         (mode-map-symbol  (intern (concat name "-mode-map"))))
     (eval-after-load mode-symbol
       `(progn
          (add-hook ',mode-hook-symbol #'hloccur-mode)
          (hloccur-bind-keys ,mode-map-symbol)))))
 '("css" "stylus" "jade" "yajade"
   "conf" "conf-colon"
   "c++" "c" "java"
   "qml" "makefile"
   "js" "js2" "javascript"
   "prog"))

(with-eval-after-load 'cc-mode
  (hloccur-bind-keys c++-mode-map)
  (hloccur-bind-keys c-mode-map))

(provide 'rc-hloccur)
