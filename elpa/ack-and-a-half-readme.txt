ack-and-a-half.el provides a simple compilation mode for the perl
grep-a-like ack (http://petdance.com/ack/).

Add the following to your .emacs:

    (add-to-list 'load-path "/path/to/ack-and-a-half")
    (require 'ack-and-a-half)
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

Run `ack' to search for all files and `ack-same' to search for
files of the same type as the current buffer.

`next-error' and `previous-error' can be used to jump to the
matches.

`ack-find-file' and `ack-find-same-file' use ack to list the files
in the current project.  It's a convenient, though slow, way of
finding files.
