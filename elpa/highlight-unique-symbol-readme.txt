Add the following to your Emacs init file:

(require 'highlight-unique-symbol)  ;; Not necessary if using ELPA package
(highlight-unique-symbol t)

You can configure these settings with `M-x customize-group RET highlight-unique-symbol RET`.

`highlight-unique-symbol:interval`
Interval to check symbol at cursor.

`highlight-unique-symbol:face`
Face of unique symbols.
