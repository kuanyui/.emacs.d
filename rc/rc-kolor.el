;;; rc-kolor.el ---                                  -*- lexical-binding: t; -*-
;;
;; kcolorchooser --color "#fafafa" --print
(defun kolor-hex-to-hsl ()
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
          (insert (kolor--hex-to-hsl color-code)))
      (progn
        (error "The current word 「%s」 is not of the form #rrggbb." color-code)
        )
      )))

(defun kolor--hex-to-hsl (hexStr)
  "Convert hexStr color to CSS HSL format.
Return a string.
 (kolor--hex-to-hsl \"#ffefd5\") => \"hsl(37,100%,91%)\""
  (let* ((clean-hex (if (string-prefix-p "#" hexStr)
                        (substring hexStr 1)
                      hexStr))
         (colorVec (kolor--convert-hex-to-vec clean-hex))
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

(defun kolor--convert-hex-to-vec (hexcolor)
  "Convert HEXCOLOR from \"rrggbb\" string to a elisp vector [r g b], where the values are from 0 to 1.
Example:
 (kolor--convert-hex-to-vec \"00ffcc\") => [0.0 1.0 0.8]

Note: The input string must NOT start with \"#\". If so, the return value is nil."
  (vector
   (kolor-normalize-number-scale
    (string-to-number (substring hexcolor 0 2) 16) 255)
   (kolor-normalize-number-scale
    (string-to-number (substring hexcolor 2 4) 16) 255)
   (kolor-normalize-number-scale
    (string-to-number (substring hexcolor 4) 16) 255)
   ))

(defun kolor-normalize-number-scale (myVal rangeMax)
  "Return a number between [0, 1] that's a rescaled myVal.
myVal's original range is [0, rangeMax].

The arguments can be int or float.
Return value is float."
  (/ (float myVal) (float rangeMax)))

(defun kolor-hex-to-rgb ()
  (interactive)
  (let* ((search-from (- (point) 7))
         (search-to   (+ (point) 7))
         (substr (buffer-substring-no-properties search-from search-to))
         (matched-from (string-match "#[a-fA-F0-9]\\{6\\}" substr))
         (matched-to (match-end 0))
         (color-code (match-string 0 substr))
         (string (buffer-substring-no-properties matched-from matched-to)))
    (if (delete-region matched-from matched-to)
        (insert
         (format "rgb(%s, %s, %s)"
                 (string-to-number (substring string 0 2) 16)
                 (string-to-number (substring string 2 4) 16)
                 (string-to-number (substring string 4 6) 16))))))


(defun kolor-rgb-to-hex ()
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


(provide 'rc-kolor)
;;; rc-kolor.el ends here
