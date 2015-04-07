;;; redshank-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "redshank" "redshank.el" (21789 31922 73315
;;;;;;  295000))
;;; Generated autoloads from redshank.el

(autoload 'redshank-mode "redshank" "\
Minor mode for editing and refactoring (Common) Lisp code.

\\{redshank-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'turn-on-redshank-mode "redshank" "\
Turn on Redshank mode.  Please see function `redshank-mode'.

This function is designed to be added to hooks, for example:
  (add-hook 'lisp-mode-hook 'turn-on-redshank-mode)

\(fn)" t nil)

(autoload 'asdf-mode "redshank" "\
Major mode for ASDF files.  This mode is derived from `lisp-mode'
and activates minor mode `redshank-mode' by default.

\\{asdf-mode-map}

\(fn)" t nil)

(autoload 'turn-on-asdf-mode "redshank" "\
Turn on ASDF mode.  Please see function `asdf-mode'.

This function is designed to be added to hooks, for example:
  (add-hook 'lisp-mode-hook 'turn-on-asdf-mode)

\(fn)" t nil)

;;;***

;;;### (autoloads nil "redshank-loader" "redshank-loader.el" (21789
;;;;;;  31922 219315 300000))
;;; Generated autoloads from redshank-loader.el

(autoload 'redshank-setup "redshank-loader" "\
Installs `redshank-mode' on major mode hooks listed in HOOKS.
If AUTOINSERTP is non-nil and `auto-insert-mode' is available,
activate support for that, too.

\(fn HOOKS &optional AUTOINSERTP)" nil nil)

;;;***

;;;### (autoloads nil nil ("redshank-pkg.el") (21789 31922 312222
;;;;;;  592000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; redshank-autoloads.el ends here
