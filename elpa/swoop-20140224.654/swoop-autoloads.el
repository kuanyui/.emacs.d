;;; swoop-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "swoop" "swoop.el" (21270 1528 981936 700000))
;;; Generated autoloads from swoop.el

(autoload 'swoop "swoop" "\
Search through words within the current buffer.

\(fn &optional $QUERY)" t nil)

(autoload 'swoop-multi "swoop" "\
Search words across currently opened multiple buffers.
Ignore non file buffer.

\(fn &optional $QUERY)" t nil)

(autoload 'swoop-pcre-regexp "swoop" "\
Use PCRE like regexp to swoop.

\(fn &optional $QUERY)" t nil)

(autoload 'swoop-migemo "swoop" "\
Japanese words matching with the alphabet.

\(fn &optional $QUERY)" t nil)

(autoload 'swoop-line-length-over80 "swoop" "\
Get over 80 colomn number linees.

\(fn)" t nil)

(autoload 'swoop-from-isearch "swoop" "\
During isearch, switch over to swoop.

\(fn)" t nil)

(autoload 'swoop-function "swoop" "\
Show function list in buffer judging from major-mode and regexp.
Currently c-mode only.

\(fn &optional $QUERY)" t nil)

(autoload 'swoop-from-evil-search "swoop" "\
During evil-search, switch over to swoop.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("swoop-edit.el" "swoop-lib.el" "swoop-pkg.el")
;;;;;;  (21270 1529 190409 856000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; swoop-autoloads.el ends here
