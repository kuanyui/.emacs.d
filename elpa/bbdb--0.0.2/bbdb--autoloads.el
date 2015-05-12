;;; bbdb--autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "bbdb-" "bbdb-.el" (21828 38619 100153 768000))
;;; Generated autoloads from bbdb-.el

(autoload 'bbdb-:setup "bbdb-" "\
Do setup for using bbdb-.

\(fn)" nil nil)

(autoload 'bbdb-:mode "bbdb-" "\
More easily To/Cc/Bcc search/choice than BBDB.

\(fn)" t nil)

(autoload 'bbdb-:open "bbdb-" "\
Open BBDB- buffer.

- CLEAN-UP is boolean. If non-nil, clean up the condition that select To/Cc/Bcc.
- TOMATCHES is list of Regexp for marking the matched record as To.
- CCMATCHES is list of Regexp for marking the matched record as Cc.
- BCCMATCHES is list of Regexp for marking the matched record as Bcc.

\(fn &optional CLEAN-UP TOMATCHES CCMATCHES BCCMATCHES)" t nil)

(autoload 'bbdb-:start-completion "bbdb-" "\
Start the selection of To/Cc/Bcc on `bbdb-:mail-modes'.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; bbdb--autoloads.el ends here
