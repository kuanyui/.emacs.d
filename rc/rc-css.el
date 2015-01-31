;;; rc-css.el ---                                    -*- lexical-binding: t; -*-

;;======================================================
;; Color code convert (derived from Xah Lee's CSS Mode)
;;======================================================
;; (I rename the functions because they are easier to memorize...)

(require 'color)
(defun color-code-hex-to-hsl ()
  "Convert color spec under cursor from '#rrggbb' to CSS HSL format.
'#ffefd5' => hsl(37,100%,91%)"
  (interactive)
  (let* (
         (bds (bounds-of-thing-at-point 'word))
         (p1 (car bds))
         (p2 (cdr bds))
         (currentWord (buffer-substring-no-properties p1 p2)))

    (if (string-match "[a-fA-F0-9]\\{6\\}" currentWord)
        (progn
          (delete-region p1 p2 )
          (if (looking-back "#") (delete-char -1))
          (insert (color-code--hex-to-hsl currentWord )))
      (progn
        (error "The current word 「%s」 is not of the form #rrggbb." currentWord)
        )
      )))

(defun color-code--hex-to-hsl (hexStr)
  "Convert hexStr color to CSS HSL format.
Return a string.
 (color-code--hex-to-hsl \"#ffefd5\") => \"hsl(37,100%,91%)\""
  (let* (
         (colorVec (color-code--convert-hex-to-vec hexStr))
         (xR (elt colorVec 0))
         (xG (elt colorVec 1))
         (xB (elt colorVec 2))
         (hsl (color-rgb-to-hsl xR xG xB) )
         (xH (elt hsl 0))
         (xS (elt hsl 1))
         (xL (elt hsl 2))
         )
    (format "hsl(%d,%d%%,%d%%)" (* xH 360) (* xS 100) (* xL 100) )
    ))

(defun color-code--convert-hex-to-vec (hexcolor)
  "Convert HEXCOLOR from \"rrggbb\" string to a elisp vector [r g b], where the values are from 0 to 1.
Example:
 (color-code--convert-hex-to-vec \"00ffcc\") => [0.0 1.0 0.8]

Note: The input string must NOT start with \"#\". If so, the return value is nil."
  (vector
   (color-code-normalize-number-scale
    (string-to-number (substring hexcolor 0 2) 16) 255)
   (color-code-normalize-number-scale
    (string-to-number (substring hexcolor 2 4) 16) 255)
   (color-code-normalize-number-scale
    (string-to-number (substring hexcolor 4) 16) 255)
   ))

(defun color-code-normalize-number-scale (myVal rangeMax)
  "Return a number between [0, 1] that's a rescaled myVal.
myVal's original range is [0, rangeMax].

The arguments can be int or float.
Return value is float."
  (/ (float myVal) (float rangeMax)))

(defun color-code-hex-to-rgb ()
  (interactive)
  (let* (
         (bds (bounds-of-thing-at-point 'word))
         (p1 (car bds))
         (p2 (cdr bds))
         (string (buffer-substring-no-properties p1 p2)))

    (if (string-match "[a-fA-F0-9]\\{6\\}" string)
	(progn
	  (delete-region p1 p2 )
	  (if (looking-back "#") (delete-char -1))
	  (insert
	   (format "rgb(%s, %s, %s)"
		   (string-to-number (substring string 0 2) 16)
		   (string-to-number (substring string 2 4) 16)
		   (string-to-number (substring string 4 6) 16)))))))

;; (autoload 'scss-mode "scss-mode")
;; (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(mapc
 (lambda (name)
   (require (intern (concat name "-mode")))
   (add-hook (intern (concat name "mode-hook")) 'highlight-symbol-mode)
   (define-key (symbol-value (intern (concat name "-mode-map"))) (kbd "C-c M-n") 'highlight-symbol-at-point)
   (define-key (symbol-value (intern (concat name "-mode-map"))) (kbd "M-n")'highlight-symbol-next)
   (define-key (symbol-value (intern (concat name "-mode-map"))) (kbd "M-p")'highlight-symbol-prev)
   (define-key (symbol-value (intern (concat name "-mode-map"))) (kbd "C-c M-p") 'highlight-symbol-query-replace)
   )
 '("css" "stylus"))

(provide 'rc-css)
;;; rc-css.el ends here
