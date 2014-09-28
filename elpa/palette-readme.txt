 To use this library:

   Add this to your initialization file (~/.emacs or ~/_emacs):

     (require 'palette) ; Load this library.
     M-x palette

   You will also need my library `hexrgb.el'; it is loaded
   automatically by `palette.el'.  Get it here:
   http://www.emacswiki.org/cgi-bin/wiki/hexrgb.el.

 After loading, use command `palette' to display a color palette in
 Color Palette mode (`palette-mode').  This has three sub-palettes
 (from left to right):

  - a hue x saturation palette - buffer Palette (Hue x Saturation).
    Hue is horizontal; saturation is vertical.  Hue is the tint of
    a color, independent of its brightness and grayness.
    Saturation is the purity of a color (opposite of grayness).

  - a color-swatch palette - buffer Current/Original

  - a value (brightness) palette - buffer Brightness

 The color-swatch palette shows the current color and the original
 color or the last color saved.  Saving is not persistent.

 In the color palette:

  - `.' shows info about the current color
  - `mouse-1' or `?' shows info about a color at pointer or cursor
  - `mouse-2' or `RET' anywhere picks a color as the current color
  - Cursor motion is along the grid of colors, with wrapping.
    Shifted cursor motion updates the current color as you move.
  - `n', `C-s' saves the current color
  - `o', `C-o' restores the old (saved) color
  - `l', `u' swaps the current color and the last color (undo)
  - `c', `M-c' picks a color by name or RGB hex string
  - `M-h' picks a color by HSV components (decimal)
  - `M-r' picks a color by RGB components (decimal)
  - `~' picks the complement of the current color
  - `r', `g', `b', `h', `s', `v' decreases the red, green, blue,
    hue, saturation, value  component of the current color,
    respectively; `R', `G', `B', `H', `S', `V' increases the
    component
  - `q' quits the palette
  - `C-l' refreshes the palette: use if you have a display problem
  - `C-h m' provides info on Color Palette mode

 Some things to keep in mind when using the Color Palette:

 * "Hue" means tint; "saturation" means color purity or intenseness
   (opposite of grayness); and "value" means brightness.
   Saturation=0 is grayscale; saturation=1 is pure color (no gray).
   Value=0 is black (no light); value=1 is as bright as possible
   (100% color).

 * Hue 0.0 and hue 1.0 are the same: pure red.  The hue x
   saturation palette shows this discontinuity.  Move the cursor
   horizontally near the right side of this palette and you will
   see the hue value jump between 0.0 and 1.0 at a certain point.

 * The value (brightness) of the current color is indicated by a
   horizontal bar in the Brightness palette (far right).

 * Whenever you input a color name, you can use completion against
   the list of all recognized colors.  If you also use my library
   Icicles, then you can match any part(s) of the color name.

 * Color names supported by your Emacs release and platform are
   those returned by function `x-color-names'.  This often includes
   names that are essentially the same, as duplicates,
   e.g. "LightBlue" and "light blue".  By default, the Color
   Palette canonicalizes these names by lowercasing them and
   removing whitespace.  Then it removes duplicates.  This behavior
   is governed by option `hexrgb-canonicalize-defined-colors-flag'.
   Customize that option to nil if you need the original, names.

 * You can at any time use an RGB hexadecimal color string in place
   of a recognized color name.  An RGB string has the form
   #XXXXXXXXXXXX, where each X is a hex digit (the # is optional
   for input).  The number of Xs must be 12 or less and a multiple
   of 3, with the same number of Xs for each of red, green, and
   blue.  Examples: #FF0099 (red: FF, green: 00, blue: 99),
   #0C1FA329E (red: 0C1, green: FA3, blue: 29E).

 * For output, that is, messages and some return values, an RGB
   value respects user option `palette-hex-rgb-digits', which
   determines the number of hex digits (1 to 4) per RGB component.

 * Once you find a color you like, you can use its RGB string
   anywhere in Emacs as the color definition of a face or a frame.
   Its RGB string is the value of `palette-current-color'.

 * The palette should appear in its own, small frame - on my
   screen, the frame is about 9cm x 13cm (inside dimensions).  If
   the palette appears much larger than that, or if it looks weird,
   then your font is probably too large.  In that case, customize
   option `palette-font' - see it for more information.  Here is a
   screenshot of how the palette should appear:
   http://www.emacswiki.org/cgi-bin/wiki/ColorPalette.el.

 * By default, information about the color at any location is only
   available upon demand, by clicking `mouse-1' or `mouse-2', or
   hitting `?' or `RET'.  If you prefer additional feedback, set
   option `palette-verbose-flag' to non-nil to display color
   information each time you move the cursor, pick a color, or
   modify a color swatch.  This can slow things down a bit, because
   it means additional computation of color components.

 * The cursor is positioned in each of the windows so that it
   corresponds as well as possible to the other windows.  However,
   this correspondance is by no means exact, because of the nature
   of conversion betwen RGB and HSV color spaces.  In particular,
   the color in the main Palette buffer (`Hue x Saturation') does
   not necessarily reflect the current color accurately.  If you
   want information about the current color, then use `.', not `?',
   or use `?' from the color-swatch window.

 * The commands that increase and decrease individual RGB
   components (r, g, b, R, G, B) are sometimes unintuitive.  If you
   set `palette-verbose-flag' to non-nil and then watch the RGB
   feedback in the echo area, these commands will make more sense.
   Because the palette displays colors as hue x saturation, RGB
   components are converted to the closest HSV components in the
   palette.  Increasing an RGB component does not automatically
   decrease the other RGB components, so, for instance, increasing
   red will not necessarily move directly toward the red area of
   the palette.  Just as for HSV component changes (cursor
   movements), RGB component changes cycle when you reach one end.
   For intance, when you decrease red past 0 it wraps around to 1.

 * Non-nil `palette-update-cursor-color-flag' updates the frame
   foreground and cursor color dynamically, so that the position of
   the current color stands out well against the palette.  For
   example, if the current color is red then the foreground color
   becomes cyan.  The default value is nil.  When nil, you cannot
   see the black cursor against a black background.  When non-nil,
   there are two annoyances: 1) updating the cursor color causes
   redisplay of the frame, which is slow; 2) If you ask for
   information about a color that is very different from the
   current color, then it still might be difficult to see the
   cursor because of its color.  In that case, you can hit `RET' to
   make it the current color so its position stands out better.
   (Hit `l' to undo).

 * You can at any time toggle options `palette-verbose-flag' and
   `palette-update-cursor-color-flag' with keys `f' (for
   "feedback") and `e' (for "enhanced cursor color").

 * By default, feedback about a color includes its RGB hex string,
   RGB decimal components, and HSV decimal components.  If your
   minibuffer is too short for all of that info, or if you are
   interested in only some of it, then you can change the value of
   user option `palette-message-info' accordingly.  In addition,
   you can use commands `palette-hex-info', `palette-hsv-info',
   `palette-rgb-info' at any time to obtain only color information
   of one type.

 * I am interested in suggestions for improving the interactive
   response.  You will find that the color palette is usable, but
   some palette operations can be slow.  This is due to using Emacs
   faces to display the colors: 10000 faces are used just for the
   100x100 color hue x saturation palette.  Emacs faces are, so
   far, not designed to be used this way (many at a time).  An
   alternative design would be to use an image instead of
   characters with faces.  I am not interested in such a design,
   however, at least for now.  I want to push the face envelope.

 * If you call `palette' from Emacs-Lisp code, you can use various
   hook functions to do something with the latest color value.  See
   `palette-change-color-hook', `palette-exit-hook',
   and`palette-save-color-hook'.  This gives you a way to react to
   user palette actions.

 ** Eye Dropper and `eyedropper.el' **

 You can at any time, from any Emacs window, pick up the foreground
 or background color at the current cursor position (point),
 setting `palette-picked-background' or`palette-picked-foreground',
 as well as `palette-current-color', to it.  Use commands
 `eyedropper-foreground' and `eyedropper-background' to do this.
 You can then set any Emacs face or frame color using the value of
 `palette-current-color'.  With a prefix argument (`C-u'), these
 commands also display the color palette.

 Library `palette.el' is a superset of the functionality provided
 by library `eyedropper.el'.  If you use Emacs 22 or later, then
 you can use `palette.el' instead of `eyedropper.el'; `palette.el'
 will satisfy all of the requirements by any other libraries that
 require `eyedropper.el'.  It does this via (provide 'eyedropper)
 and by providing aliases for all of the `eyedropper.el' functions
 and variables.  If for some reason you do load both libraries,
 then load `palette.el' after `eyedropper.el'.

 ** Use with Icicles **

 If you use this library with Icicles (`icicles.el' and associated
 files), which I recommend, then `c' is bound in the palette to an
 Icicles multi-command that lets you choose colors by name.  After
 you hit `c', you can hit `TAB' or `S-TAB' to use Icicles
 completion.  During completion, you can use `C-next', `C-prior',
 `C-down', `C-up', and `C-RET' to change the current color to
 different colors, by name, successively.  This lets you browse
 colors by name, seeing what they look like immediately.

 ** Do Re Mi **

 See also my library `doremi-frm.el' to incrementally adjust face
 and frame properties, including colors, using the arrow keys or a
 mouse wheel.  The color changes are applied instantly to the
 face(s) or frames, so you see the result as you make the changes.


 User options defined here:

   `palette-change-color-hook', `palette-exit-hook',
   `palette-font', `palette-hex-rgb-digits',
   `palette-message-info', `palette-save-color-hook',
   `palette-update-cursor-color-flag', `palette-verbose-flag'.

 Commands defined here:

   `background-color', `colors', `complement',
   `eyedrop-background-at-mouse', `eyedrop-background-at-point',
   `eyedrop-foreground-at-mouse', `eyedrop-foreground-at-point',
   `eyedrop-pick-background-at-mouse',
   `eyedrop-pick-background-at-point',
   `eyedrop-pick-foreground-at-mouse',
   `eyedrop-pick-foreground-at-point', `eyedropper-background',
   `eyedropper-foreground', `foreground-color', `hsv', `palette',
   `palette-background-at-mouse', `palette-background-at-point',
   `palette-brightness-scale', `palette-current-color',
   `palette-current-rgb-to-kill-ring', `palette-decrease-blue',
   `palette-decrease-green', `palette-decrease-hue',
   `palette-decrease-red', `palette-decrease-saturation',
   `palette-decrease-value', `palette-down', `palette-down+pick',
   `palette-exit', `palette-foreground-at-mouse',
   `palette-foreground-at-point', `palette-help',
   `palette-hex-info', `palette-hsv-info', `palette-increase-blue',
   `palette-increase-green', `palette-increase-hue',
   `palette-increase-red', `palette-increase-saturation',
   `palette-increase-value', `palette-left', `palette-left+pick',
   `palette-pick-background-at-mouse',
   `palette-pick-background-at-point', `palette-pick-color-by-hsv',
   `palette-pick-color-by-name', `palette-pick-color-by-rgb',
   `palette-pick-color-complement',
   `palette-pick-foreground-at-mouse',
   `palette-pick-foreground-at-point', `palette-popup-menu',
   `palette-quit', `palette-refresh', `palette-restore-old-color',
   `palette-rgb-info', `palette-right', `palette-right+pick',
   `palette-save-new-color', `palette-swap-last-color',
   `palette-swatch', `palette-toggle-cursor-color',
   `palette-toggle-verbose', `palette-up', `palette-up+pick',
   `palette-where-is-color', `pick-background-color',
   `pick-foreground-color', `rgb', `toggle-palette-cursor-color',
   `toggle-palette-verbose'.

 Non-interactive functions defined here:

   `eyedrop-color-message', `eyedrop-face-at-point',
   `palette-barf-if-outside-palette', `palette-color-message',
   `palette-complement-or-alternative', `palette-face-at-point',
   `palette-pick-by-name-action', `palette-set-current-color',
   `palette-update-blink-cursor-mode'.

 Internal variables defined here:

   `eyedrop-last-picked-color', `eyedrop-picked-background',
   `eyedrop-picked-foreground', `palette-action',
   `palette-current-color', `palette-last-color',
   `palette-last-picked-color', `palette-mode-map',
   `palette-old-color', `palette-picked-background',
   `palette-picked-foreground', `palette-popup-map',
   `palette-saved-blink-cursor-mode'.

 Do NOT try to use this library without a window manager.
 That is, do not try to use this with `emacs -nw'.

 Compatibility: You really need Emacs 22 for this, but reduced
 functionality is available for Emacs 20 and 21.
