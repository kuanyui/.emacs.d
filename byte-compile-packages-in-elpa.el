;;; byte-compile.el ---


(require 'find-lisp)

(defun byte-compile-dir-recursively (dir-root)
  (let* (
	 (all-el-files (find-lisp-find-files
			(expand-file-name (concat (file-name-directory (or load-file-name buffer-file-name)) dir-root))
			"\\.el$"))
	 (el-files-needing-compile (delete-if
				    (lambda (f) (file-exists-p (format "%sc" f)))
				    all-el-files))
	 )

    (mapc
     (lambda (x) (ignore-errors (byte-compile-file x)))
     el-files-needing-compile)))

(mapc
 #'byte-compile-dir-recursively
 '("lisps/" "elpa/"))
