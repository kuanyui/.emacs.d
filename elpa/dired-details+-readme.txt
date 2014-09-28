Commentary:

 This enhances the functionality of library `dired-details.el'.

 1. It shrink-wraps Dired's frame whenever you show or hide
    details.  For this enhancement, you will need library
    `autofit-frame.el'.

 2. It updates the listing whenever you create new files or
    directories or rename them.

 3. It adds user option `dired-details-propagate-flag' which, if
    non-nil, propagates the last state you chose to the next Dired
    buffer you open.

 4. It binds both `)' and `(' to `dired-details-toggle'.

 Perhaps #2 corresponds to this TO-DO item in `dired-details.el':

   * add a hook for dired-add-file to hide new entries as necessary


 ***** NOTE: The following function defined in `dired-details.el'
             has been REDEFINED HERE:

 `dired-details-activate' - If `dired-details-propagate-flag' is
                            non-nil, then use the last state.


 I have submitted these enhancements to Rob Giardina, the author of
 `dired-details.el', for inclusion in that library.  If they (or
 similar) are added to that library, then I'll remove this library.
 In any case, this feature has been added to Emacs, starting with
 Emacs 22.2, I think.

 Put this in your initialization file (~/.emacs):

  (require 'dired-details+)

 I also recommend customizing `dired-details-hidden-string' to use
 the value "" instead of the default "[...]" - less wasted space.

 Note: This library also calls `dired-details-install', activating
 show/hide and binding keys `(' and `)'.
