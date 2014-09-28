   Highlight specified sets of characters, including whitespace.

This library provides commands and non-interactive functions for
highlighting the following:

* Tab chars (command `hc-toggle-highlight-tabs').

* Hard space (aka no-break space, aka non-breaking space) chars
  (command `hc-toggle-highlight-hard-spaces').

* Hard hyphen (aka non-breaking hyphen) chars (command
  `hc-toggle-highlight-hard-hyphens').

* Trailing whitespace: tabs, spaces, and hard spaces at the end of
  a line of text (command
  `hc-toggle-highlight-trailing-whitespace')

* Any set of chars you choose (commands `hc-highlight-chars' and
  `hc-toggle-highlight-other-chars').  You can specify characters
  in four ways: (1) individually, (2) using ranges, (3) using
  character classes (e.g. [:digit:]), and (4) using character sets
  (e.g. `iso-8859-1' or `lao').

  For `hc-toggle-highlight-other-chars', you can also specify
  characters (the same four ways) that are to be *excluded* from
  highlighting.

  You can thus, for example, highlight all characters in character
  set `greek-iso8859-7' except `GREEK SMALL LETTER LAMBDA'.  Or all
  characters in class `[:space:]' (whitespace) except `tab'.  Or
  all Unicode characters in the range ?\u2190 through ?\u21ff
  (mathematical arrows) except ?\u21b6, ?\u21b7, ?\u21ba, and
  ?\u21bb (curved arrows).  You get the idea.

  - Command `hc-highlight-chars' prompts you for the characters to
    highlight and the face to use.  With a prefix arg it
    unhighlights.

  - Command `hc-toggle-highlight-other-chars' toggles highlighting,
    using face `hc-other-char', of the characters specified by user
    option `hc-other-chars', but excluding the characters specified
    by option `hc-other-chars-NOT'.  With a prefix arg it prompts
    you for the face to use.

  For these particular commands and functions, option
  `hc-other-chars-font-lock-override' controls whether the current
  highlighting face overrides (`t'), is overridden by (`keep'), or
  merges with (`append' or `prepend') any existing highlighting.

To use this library, add this to your init file (~/.emacs):

     (require 'highlight-chars) ; Load this library.

You can then use the commands and functions defined here to turn
the various kinds of highlighting on and off when in Font-Lock
mode.  For example, you can bind a key to toggle highlighting of
trailing whitespace:

     (global-set-key (kbd "<f11>")
                     'hc-toggle-highlight-trailing-whitespace)

Because variable `font-lock-keywords' is buffer-local, that key
binding lets you use `f11' to toggle highlighting separately in
each buffer.

But if you want to use a particular kind of highlighting by default
globally, then just add the corresponding `hc-highlight-*' function
to the hook `font-lock-mode-hook'.  Then, whenever Font-Lock mode
is turned on (in any buffer), the appropriate highlighting will
also be turned on.

For example, you can turn on tab highlighting everywhere by default
by adding function `hc-highlight-tabs' to `font-lock-mode-hook' in
your init file (`~/.emacs'), as follows:

    (add-hook 'font-lock-mode-hook 'hc-highlight-tabs)

In addition to buffer-specific highlighting and global
highlighting, you can turn on a given kind of highlighting
automatically for all buffers that are in a certain major mode.

For that, do the following, where `THE-MODE' is the appropriate
mode symbol (value of variable `major-mode'), such as `text-mode'.
This example turns on trailing whitespace highlighting - use
different `hc-highlight-*' and `hc-dont-highlight-*' functions for
other kinds of highlighting.

     (add-hook 'change-major-mode-hook
               (lambda ()
                 (add-hook 'font-lock-mode-hook
                           'hc-highlight-trailing-whitespace)))

     (add-hook 'after-change-major-mode-hook
               (lambda ()
                 (when (eq major-mode 'THE-MODE)
                   (remove-hook 'font-lock-mode-hook
                                'hc-highlight-trailing-whitespace)
                   (hc-dont-highlight-trailing-whitespace)))
               'APPEND)

Highlighting Different Sets of Characters in Different Buffers
--------------------------------------------------------------

Especially for highlighting non-whitespace characters (commands
`hc-toggle-highlight-other-chars' and `hc-highlight-chars'), it can
sometimes be useful to highlight different characters in different
buffers.  For example, you might want to highlight all Chinese
characters in a Gnus buffer and all hexadecimal digits in a CSS or
HTML buffer.

You can do this by setting the local value of option
`hc-other-chars' (and perhaps option `hc-other-chars-NOT') in each
of the buffers.  For example:

     (with-current-buffer (current-buffer)
       (set (make-local-variable 'hc-other-chars)
            '(chinese-big5-1)))

You can use Customize to find the Lisp value that corresponds to
the highlighting you want: `M-x customize-option hc-other-chars',
then use button `Value Menu' to choose a value, then button `State
to `Set for Current Session'.

Then use `C-h v hc-other-chars' to see what the Lisp value is (for
example, `(chinese-big5-1)'), and plug that value into the
`make-local-variable' expression.

Vanilla Emacs Highlighting of Hard Spaces and Hyphens
-----------------------------------------------------

Vanilla Emacs can itself highlight hard spaces and hard hyphens,
and it does so whenever `nobreak-char-display' is non-nil, which it
is by default.  By "hard" space and hyphen I mean "no-break" or
non-breaking.  These are the non-ASCII Unicode characters with code
points 160 (#xa0) and 8209 (#x2011), respectively.

This low-level vanilla Emacs highlighting does not use Font Lock
mode, and it cannot highlight only one of these characters and not
the other.

Using `highlight-chars.el' to highlight hard space and hyphen chars
requires turning off their default highlighting provided by vanilla
Emacs, that is, setting `nobreak-char-display' to nil.  This is
done automatically by the functions defined here.  When you turn
off this font-lock highlighting, the vanilla Emacs highlighting is
automatically restored.

That is, the value of variable `nobreak-char-display' is reset to
its original value when `highlight-chars.el' was loaded (`t' is the
default value, so if you didn't change it prior to loading
`highlight-chars.el' then t is restored).

NOTE: If you byte-compile this file in an older version of Emacs
(prior to Emacs 23) then the code for highlighting hard hyphens and
hard spaces will not work, even in Emacs 23+.  If you use Emacs 23+
then you should either byte-compile it using Emacs 23+ or evaluate
the source code that defines functions that highlight such
characters.  (This is because older Emacs versions interpret
[\u2011] as just [u2011], etc.)


See Also:

* Library `highlight.el' for ways to highlight text more generally,
  not just specific characters.  It is available here:
  http://www.emacswiki.org/cgi-bin/wiki/highlight.el     (code)
  http://www.emacswiki.org/cgi-bin/wiki/HighlightLibrary (doc)

* Standard library `whitespace.el' for other ways to highlight
  whitespace characters.

  This does some things similar to what `highlight-chars.el' does,
  plus other, unrelated things.  As its name suggests, its effects
  are limited to whitespace characters.  It is also somewhat
  complicated to use (10 faces, 24 options!), and it seems to have
  more than a few bugs.

  Besides being simpler, I think that `highlight-chars.el' has an
  advantage of letting you easily highlight ONLY particular
  whitespace characters.  `whitespace.el' apparently makes you pick
  whether to highlight spaces and hard spaces together, or not, for
  instance.

  (As a workaround, With `whitespace.el' you can get the effect of
  highlighting only one of these kinds of space by customizing the
  face used to highlight the other one so that it is the same as
  the `default' face.  But that will interfere with other font-lock
  highlighting of that other character.  Maybe I'm missing
  something, this seems to me the only workaround.)


Faces defined here:

   `hc-hard-hyphen' (Emacs 23+), `hc-hard-space', `hc-other-char',
   `hc-tab', `hc-trailing-whitespace'.

User options defined here:

   `hc-other-chars', `hc-other-chars-font-lock-override',
   `hc-other-chars-NOT'.

Commands defined here:

   `toggle-highlight-hard-hyphens' (alias, Emacs 23+),
   `toggle-highlight-hard-spaces' (alias),
   `toggle-highlight-other-chars', `toggle-highlight-tabs' (alias),
   `toggle-highlight-trailing-whitespace' (alias),
   `hc-highlight-chars', `hc-toggle-highlight-hard-hyphens' (Emacs
   23+), `hc-toggle-highlight-hard-spaces',
   `hc-toggle-highlight-other-chars', `hc-toggle-highlight-tabs',
   `hc-toggle-highlight-trailing-whitespace'.

Non-interactive functions defined here:

   `hc-dont-highlight-hard-hyphens' (Emacs 23+),
   `hc-dont-highlight-hard-spaces',
   `hc-dont-highlight-other-chars', `hc-dont-highlight-tabs',
   `hc-dont-highlight-trailing-whitespace',
   `hc-highlight-hard-hyphens' (Emacs 23+),
   `hc-highlight-other-chars', `hc-highlight-hard-spaces',
   `hc-highlight-tabs', `hc-highlight-trailing-whitespace',
   `hc-other-chars-defcustom-spec', `hc-other-chars-description',
   `hc-other-chars-font-lock-spec', `hc-other-chars-matcher'.

Internal variables defined here:

   `hc-highlight-hard-hyphens-p', `hc-highlight-hard-spaces-p',
   `hc-highlight-tabs-p', `hc-highlight-trailing-whitespace-p',
   `hc--other-chars-last-match-data',
   `hc--saved-nobreak-char-display'.


History:

Peter Steiner, <unistein@isbe.ch>, wrote `hilite-trail.el', which
included some whitespace character-highlighting commands.  Since
2000 I have extended those and added other character-highlighting
functions, in `show-wspace.el'.  I eventually (2012) renamed
`show-wspace.el' to `highlight-chars.el'.  Highlighting whitespace
and other easily confused characters remains an important use case,
however.
