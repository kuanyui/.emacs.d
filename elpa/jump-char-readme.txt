Navigate by char.  The best way to "get" it is to try it.

Interface (while jumping):

  <char>   :: move to the next match in the current direction.
  ;        :: next match forward (towards end of buffer)
  ,        :: next match backward (towards beginning of buffer)
  C-c C-c  :: invoke ace-jump-mode if available (also <M-/>)

Any other key stops jump-char and edits as normal.

The behaviour is strongly modeled after `iy-go-to-char' with the following
differences:

  * point always stays before match

  * point during search is same as after exiting

  * lazy highlighting courtesy of isearch




This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 3, or
