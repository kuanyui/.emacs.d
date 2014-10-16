;;; byte-compile.el ---


(require 'find-lisp)

(mapc
 (lambda (x) (byte-compile-file x))
 (find-lisp-find-files
  (expand-file-name (concat (file-name-directory (or load-file-name buffer-file-name)) "elpa/"))
  "\\.el$"))
