This is a simple package that allows Emacs users to compete on [VimGolf][1]
using the One True Editor. Competition can be commenced utilizing `M-x
vimgolf`. When finished with a challenge, `C-c C-v C-c` should finish your
editing, ensure correctness, and submit your score and keystrokes to
[VimGolf][1].

On second thought, let's not go to Camelot. It's a silly place.

Patches are accepted at https://github.com/timvisher/vimgolf

[1]: http://vimgolf.com/

Installation:

Use package.el. You'll need to add Marmalade to your archives:

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

If you use a version of Emacs prior to 24 that doesn't include
package.el, you can get it from http://bit.ly/pkg-el23. If you have
an older package.el installed from tromey.com, you should upgrade
in order to support installation from multiple sources.
