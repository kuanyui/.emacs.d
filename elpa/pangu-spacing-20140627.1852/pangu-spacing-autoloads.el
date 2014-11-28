;;; pangu-spacing-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "pangu-spacing" "pangu-spacing.el" (21575 37223
;;;;;;  184764 762000))
;;; Generated autoloads from pangu-spacing.el

(autoload 'pangu-spacing-mode "pangu-spacing" "\
Toggle pangu-spacing-mode

\(fn &optional ARG)" t nil)

(defvar global-pangu-spacing-mode nil "\
Non-nil if Global-Pangu-Spacing mode is enabled.
See the command `global-pangu-spacing-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-pangu-spacing-mode'.")

(custom-autoload 'global-pangu-spacing-mode "pangu-spacing" nil)

(autoload 'global-pangu-spacing-mode "pangu-spacing" "\
Toggle Pangu-Spacing mode in all buffers.
With prefix ARG, enable Global-Pangu-Spacing mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Pangu-Spacing mode is enabled in all buffers where
`pangu-spacing-mode' would do it.
See `pangu-spacing-mode' for more information on Pangu-Spacing mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; pangu-spacing-autoloads.el ends here
