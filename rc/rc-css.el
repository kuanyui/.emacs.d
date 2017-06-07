;;; rc-css.el ---                                    -*- lexical-binding: t; -*-

;; font-lock in SCSS-mode is buggy and may freeze the whole Emacs. Example:
;; https://gist.github.com/kuanyui/f07579d957ea2dda426b8034d78269ba
;; So derive a new scss-mode as workaround...
(define-derived-mode omg-scss-mode css-mode "OMG SCSS"
  "Major mode to edit \"Sassy CSS\" files."
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-continue " *")
  (setq-local comment-start-skip "/[*/]+[ \t]*")
  (setq-local comment-end-skip "[ \t]*\\(?:\n\\|\\*+/\\)"))
(add-to-list 'auto-mode-alist '("\\.scss$" . omg-scss-mode))

;;======================================================
;; Color code convert (derived from Xah Lee's CSS Mode)
;;======================================================
;; (I rename the functions because they are easier to memorize...)

(require 'color)
(defun color-code-hex-to-hsl ()
  "Convert color spec under cursor from '#rrggbb' to CSS HSL format.
'#ffefd5' => hsl(37,100%,91%)"
  (interactive)
  (let* ((search-from (- (point) 7))
         (search-to   (+ (point) 7))
         (substr (buffer-substring-no-properties search-from search-to))
         (matched-from (string-match "#[a-fA-F0-9]\\{6\\}" substr))
         (matched-to (match-end 0))
         (color-code (match-string 0 substr)))
    (if matched-from
        (progn
          (message (format "%s %s %s" matched-from matched-to color-code))
          (delete-region (+ search-from matched-from) (+ search-from matched-to))
          (insert (color-code--hex-to-hsl color-code)))
      (progn
        (error "The current word 「%s」 is not of the form #rrggbb." color-code)
        )
      )))

(defun color-code--hex-to-hsl (hexStr)
  "Convert hexStr color to CSS HSL format.
Return a string.
 (color-code--hex-to-hsl \"#ffefd5\") => \"hsl(37,100%,91%)\""
  (let* ((clean-hex (if (string-prefix-p "#" hexStr)
                        (substring hexStr 1)
                      hexStr))
         (colorVec (color-code--convert-hex-to-vec clean-hex))
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


(defun color-code-rgb-to-hex ()
  (interactive)
  ((search-from (- (point) 16))
   (search-to   (+ (point) 16))
   (substr (buffer-substring-no-properties search-from search-to))
   (matched-from (string-match "rgb( *\\([0-9]+\\) *, *\\([0-9]+\\) *, *\\([0-9]+\\) *)" substr))
   (matched-to (match-end 0))
   (r (string-to-number (match-string 1 substr)))
   (g (string-to-number (match-string 2 substr)))
   (b (string-to-number (match-string 3 substr))))
  (if matched-from
      (progn
        (delete-region (+ search-from matched-from) (+ search-from matched-to))
        (insert (format "#%x%x%x" r g b)))
    )
  )

;; (autoload 'scss-mode "scss-mode")
;; (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))



(provide 'rc-css)
;;; rc-css.el ends here
