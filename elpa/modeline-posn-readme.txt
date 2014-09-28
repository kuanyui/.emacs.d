 Change variable `mode-line-position' so that the following changes
 are made to the mode line:

 1. Highlight the column number when the current column is greater
    than limit `modelinepos-column-limit'.  Face
    `modelinepos-column-warning' is used for the highlighting.

 2. Make `size-indication-mode' show the size of the region,
    instead of the buffer size, whenever the region is active.

 More precisely for #2: When the region is active, the mode line
 displays some information that you can customize - see option
 `modelinepos-style'.  Customization choices for this include (a)
 the number of chars, (b) the number of chars and number of lines,
 and (c) anything else you might want.  Choice (b) is the default.

 For (c), you provide a `format' expression as separate components:
 the format string and the sexp arguments to be evaluated and
 plugged into the string.  The number of sexp args depends on the
 format string that you use: one for each `%' construct.

 Choice (c) is provided so that you can choose alternative
 formatting styles.  For example, instead of ` 256 ch, 13 l', you
 could show ` (256 chars, 13 lines)'.  But (c) can really show
 information at all.  It need not have anything to do with the
 region, but it is nevertheless shown when the region is active.

 Option `modelinepos-empty-region-flag' determines whether to show
 the active-region indication when the active region is empty.  By
 default it is nil, meaning do not indicate an empty active region.

 Note: Loading this library changes the default definition of
       `mode-line-position'.

 To use this library, put this in your Emacs init file (~/.emacs):

   (require 'modeline-posn)

 To show the column number highlighting, turn on Column Number
 mode.  You can do that in your Emacs init file this way:

   (column-number-mode 1)

 To show the buffer and region size indication in the mode line,
 turn on Size Indication.  You can do that in your Emacs init file
 this way:

   (size-indication-mode 1) ; Turn on Size Indication mode.

 You can customize `modelinepos-column-limit' or bind it to
 different values for different modes.
