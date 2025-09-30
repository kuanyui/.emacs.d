;;; rc-private.el ---                                -*- lexical-binding: t; -*-

(let ((private-dir "~/.emacs.d/private"))
  (if (file-exists-p private-dir)
      (mapcar (lambda (x)
                (load-file x)
                x)
              (directory-files private-dir t "\\.el\\'"))))

(provide 'rc-private)
;;; rc-private.el ends here
