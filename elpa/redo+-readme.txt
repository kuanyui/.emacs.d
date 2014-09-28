redo+.el is bug fix and extended version of XEmacs' redo package.

Emacs' normal undo system allows you to undo an arbitrary
number of buffer changes.  These undos are recorded as ordinary
buffer changes themselves.  So when you break the chain of
undos by issuing some other command, you can then undo all
the undos.  The chain of recorded buffer modifications
therefore grows without bound, truncated only at garbage
collection time.

The redo/undo system is different in two ways:
  1. The undo/redo command chain is only broken by a buffer
     modification.  You can move around the buffer or switch
     buffers and still come back and do more undos or redos.
  2. The `redo' command rescinds the most recent undo without
     recording the change as a _new_ buffer change.  It
     completely reverses the effect of the undo, which
     includes making the chain of buffer modification records
     shorter by one, to counteract the effect of the undo
     command making the record list longer by one.


Installation:

Save this file as redo+.el, byte compile it and put the
resulting redo.elc file in a directory that is listed in
load-path.

In your .emacs file, add
  (require 'redo+)
and the system will be enabled.

In addition, if you don't want to redo a previous undo, add
  (setq undo-no-redo t)
You can also use a function `undo-only' instead of `undo'
in GNU Emacs 22 or later.


History:
2009-01-07  S. Irie
        * Delete unnecessary messages
        * Bug fix
        * Version 1.15

2008-05-23  S. Irie
        * Bug fix
        * Version 1.14

2008-05-11  S. Irie
        * record unmodified status entry when redoing
        * Version 1.13

2008-05-10  S. Irie
        * Bug fix
        * Version 1.12

2008-05-09  S. Irie
        * Bug fix
        * Version 1.11

2008-04-02  S. Irie
        * undo-no-redo available
        * GNU Emacs menu-bar and tool-bar item
        * Bug fix
        * Version 1.10

ToDo:

- undo/redo in region
