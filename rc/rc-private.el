;;; rc-private.el ---                                -*- lexical-binding: t; -*-

;;======================================================
;; 私人的東西
;;======================================================

(mapcar (lambda (x)
          (when (file-exists-p x)
            (load-file x)))
        '("~/.emacs.d/private/school.el"
          "~/.emacs.d/private/twittering-filter-users.el"
          "~/.emacs.d/private/flickr.el"
          "~/.emacs.d/private/family-birthday.el"))


(provide 'rc-private)
;;; rc-private.el ends here
