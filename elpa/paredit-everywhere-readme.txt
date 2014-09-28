It turns out that a lot of the paredit key bindings work as
expected in non-lisp buffers, since many major modes provide
reasonable sexp-oriented navigation.

This library, then, provides a minor mode which enables a subset
of the `paredit' library's editing commands in non-lisp buffers.

Usage:

(add-hook 'prog-mode-hook 'paredit-everywhere-mode)
