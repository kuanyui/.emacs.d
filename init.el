;;kuanyui's ~/.emacs

;; (setq warning-minimum-level :emergency)

;; Don't let `custom.el' shit in `init.el'.
(setq custom-file (expand-file-name ".custom.el" user-emacs-directory))
(if (not (file-exists-p custom-file))
    (write-region "" nil custom-file))
(load custom-file)

;;掃描~/.emacs.d目錄

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'benchmark-init)
(add-hook 'after-init-hook 'benchmark-init/deactivate)

(add-to-list 'load-path "~/.emacs.d/rc")
;; (profiler-start 'cpu)
;; (profiler-stop)
;; (profiler-report)
(require 'rc-basic)
(require 'rc-spell)
(require 'rc-programming)
(require 'rc-web-development)
(require 'rc-templates)
(require 'rc-edit)
(require 'rc-hloccur)
(require 'rc-dired)
(require 'rc-ibuffer)
(require 'rc-eshell)
(require 'rc-calendar)
(require 'rc-term)

;; (require 'rc-gnus)
;; (require 'rc-twittering)

(require 'rc-magit)
;; (if (member system-type '(darwin gnu/linux))
;; (require 'rc-emms)
;; )
(require 'rc-eww)

(require 'rc-org)
(require 'rc-markdown)

(require 'rc-misc)
(require 'rc-private)

(require 'rc-python)
(require 'rc-haskell)
(require 'rc-ruby)
(require 'rc-perl)
(require 'rc-lisp)
(require 'rc-scheme)
(require 'rc-css)
(require 'rc-kolor)
(require 'rc-js)
(require 'rc-json)
(require 'rc-shell)
(require 'rc-qml)
(require 'rc-c)
(require 'rc-go)
(require 'rc-sql)
(require 'rc-qt)
(require 'rc-makefile)
(require 'rc-polymode)

(require 'rc-my)

(require 'rc-junk)

(require 'rc-site-lisp)

(message (emacs-init-time))

;; Custom settings, loaded last so they win over the rc files above
;; (e.g. `delete-selection-mode' and `line-number-mode' stay off).
;;   custom-shared.el : in git, shared by every machine, hand-edited.
;;   custom-file       : .custom.el, per-machine, the only file Custom writes.
;; The early load of `custom-file' above stays: if init breaks halfway,
;; saving Custom must not wipe it.
(load (expand-file-name "custom-shared.el" user-emacs-directory))
(load custom-file)
