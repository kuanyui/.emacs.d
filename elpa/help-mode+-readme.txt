   Extensions to `help-mode.el'


 ***** NOTE: The following functions defined in `help-mode.el'
             have been REDEFINED HERE:

 `help-make-xrefs' - Put symbol clause first, so cross-xref links
                     show doc for both fun and var, if available.
 `help-mode'       - If `one-window-p', then delete Help frame.
 `help-xref-on-pp' - Library names are buttonized.


 Put this in your initialization file (`~/.emacs'):

   (require 'help-mode+)
