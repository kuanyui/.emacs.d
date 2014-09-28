;;; discover-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (global-discover-mode discover-mode) "discover"
;;;;;;  "discover.el" (21175 11780 137351 52000))
;;; Generated autoloads from discover.el

(autoload 'discover-mode "discover" "\
Helps you discover Emacs with interactive context menus.

Key bindings:
\\{discover-map}

\(fn &optional ARG)" t nil)

(defvar global-discover-mode nil "\
Non-nil if Global-Discover mode is enabled.
See the command `global-discover-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-discover-mode'.")

(custom-autoload 'global-discover-mode "discover" nil)

(autoload 'global-discover-mode "discover" "\
Toggle Discover mode in all buffers.
With prefix ARG, enable Global-Discover mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Discover mode is enabled in all buffers where
`discover-mode-turn-on' would do it.
See `discover-mode' for more information on Discover mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("discover-pkg.el") (21175 11780 322668
;;;;;;  269000))

;;;***

(provide 'discover-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; discover-autoloads.el ends here
