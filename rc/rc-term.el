(require 'term)
(term-set-escape-char ?\C-x)

(defun my-serial-term ()
  (interactive)
  (serial-term "/dev/cu.usbserial" 115200))

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


(provide 'rc-term)
;;; rc-term.el ends here
