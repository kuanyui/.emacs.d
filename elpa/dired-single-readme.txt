 This package provides a way to reuse the current dired buffer to visit
 another directory (rather than creating a new buffer for the new directory).
 Optionally, it allows the user to specify a name that all such buffers will
 have, regardless of the directory they point to.

Installation:

 Put this file on your Emacs-Lisp load path and add the following to your
 ~/.emacs startup file

    (require 'dired-single)

 or you can load the package via autoload:

    (autoload 'dired-single-buffer "dired-single" "" t)
    (autoload 'dired-single-buffer-mouse "dired-single" "" t)
    (autoload 'dired-single-magic-buffer "dired-single" "" t)
    (autoload 'dired-single-toggle-buffer-name "dired-single" "" t)

 To add a directory to your load-path, use something like the following:

     (setq load-path (cons (expand-file-name "/some/load/path") load-path))

 See below for key-binding suggestions.
