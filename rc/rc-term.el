(defun my-serial-term ()
  (interactive)
  (let ((file (cl-find-if #'file-exists-p
                          '("/dev/cu.usbserial"
                            "/dev/ttyUSB0"))))
    (if (not file)
        (message "No console interface detected. Forgot to plug it?")
      (serial-term file 115200))))

(with-eval-after-load 'term
  (require 'term)
  (term-set-escape-char ?\C-x)


  (defun my-serial-term-set-switch-ip ()
    (interactive)
    (term-send-raw-string "system")
    (term-send-raw-string "netstatus")
    (term-send-raw-string (format "netsettingipv4 %s 255.255.254.0 192.168.16.254 8.8.8.8"
                                  (read-from-minibuffer "IP (130~139) " "192.168.16.")))
    (term-send-raw-string "netstatus")
    (term-send-raw-string "save")
    (term-send-raw-string "..")
    )

  (defun my-serial-term-set-ap-ip ()
    (interactive)
    (term-send-raw-string "/")
    (term-send-raw-string "lan")
    (term-send-raw-string "ip")
    (term-send-raw-string (format "static lan1 %s 255.255.254.0"
                                  (read-from-minibuffer "IP (130~139) " "192.168.16.")))
    (term-send-raw-string "/")
    (term-send-raw-string "system")
    (term-send-raw-string "save")
    (term-send-raw-string "/")
    )

  (defun term-send-up    () (interactive) (term-send-raw-string "\e[A"))
  (defun term-send-down  () (interactive) (term-send-raw-string "\e[B"))
  (defun term-send-right () (interactive) (term-send-raw-string "\e[C"))
  (defun term-send-left  () (interactive) (term-send-raw-string "\e[D"))
  )

(provide 'rc-term)
;;; rc-term.el ends here
