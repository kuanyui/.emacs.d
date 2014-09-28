   Extensions to `help.el' for Emacs 22 and later.  For similar
   extensions to `help.el' for Emacs 20, see `help+20.el'.

 Commands defined here:

   `help-on-click/key', `mouse-help-on-click',
   `mouse-help-on-mode-line-click', `pop-to-help-toggle'.

 Non-interactive functions defined here:

   `help-on-click/key-lookup'.

 Internal variables defined here:

   `help-origin-buffer'.


 ***** NOTE: The following functions defined in `help.el' have
             been REDEFINED HERE:

 `describe-key', `where-is'.


 ***** NOTE: The doc string for `help-for-help' has been
             REDEFINED HERE (see `make-help-screen help-for-help')

 The following bindings are made here:

   `C-h u'      `manual-entry'
   `C-h C-a'    `apropos'
   `C-h C-l'    `locate-library'
   `C-h RET'    `help-on-click/key'
   `C-h M-a'    `apropos-documentation'
   `C-h M-o'    `pop-to-help-toggle'
   `C-h C-M-a'  `tags-apropos'
   [mouse-1]    `mouse-help-on-click' (non-mode-line)
   [mouse-1]    `mouse-help-on-mode-line-click' (mode-line)

 Suggested additional binding:

  (global-set-key [f1] 'help-on-click/key)
