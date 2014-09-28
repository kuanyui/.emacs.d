Then add the following lines to ~/.emacs:

     (unless (display-graphic-p)
       (require 'evil-terminal-cursor-changer))

If have gnome-terminal's custom profile, must set like below

     (setq etcc--gnome-profile "Profile0")

If want change cursor type, add below line. This is evil's setting.

     (setq evil-visual-state-cursor 'box) ; █
     (setq evil-insert-state-cursor 'bar) ; ⎸
     (setq evil-emacs-state-cursor 'hbar) ; _
