This implements the jump cursor feature found in vim.

A jump is added every time you insert a character on a different
line.

Jumps are remembered in a jump list.  With the C-o and C-i
command you can go to cursor positions before older jumps, and back
again.  Thus you can move up and down the list.

Jumps are read and saved in the same configuration file as vim so
you can switch back and forth between the two editors
