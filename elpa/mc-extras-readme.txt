This package contains extra functions for multiple-cursors mode.

Here is a list of the interactive commands provided by mc-extras:

* mc/compare-chars
* mc/compare-chars-backward
* mc/compare-chars-forward
* mc/cua-rectangle-to-multiple-cursors
* mc/remove-current-cursor
* mc/remove-duplicated-cursors

Suggested key bindings are as follows:

  (define-key mc/keymap (kbd "C-. C-d") 'mc/remove-current-cursor)
  (define-key mc/keymap (kbd "C-. d")   'mc/remove-duplicated-cursors)

  (define-key mc/keymap (kbd "C-. =")   'mc/compare-chars)

  (define-key cua--rectangle-keymap (kbd "C-. C-,") 'mc/cua-rectangle-to-multiple-cursors)

To enable interaction between multiple cursors and CUA rectangle
copy & paste:

  (mc/cua-rectangle-setup)
