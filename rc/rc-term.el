(require 'term)
(term-set-escape-char ?\C-x)
(defun my-serial-term ()
  (interactive)
  (serial-term "/dev/cu.usbserial" 115200))

(defun my-serial-term-set-ip ()
  (interactive)
  (term-send-raw-string "system")
  (term-send-raw-string "netstatus")
  (term-send-raw-string (format "netsettingipv4 %s 255.255.254.0 192.168.16.254 8.8.8.8" (read-from-minibuffer "IP (130~139)" "192.168.16.")))
  (term-send-raw-string "netstatus")
  (term-send-raw-string "..")
  )

(provide 'rc-term)
;;; rc-term.el ends here
