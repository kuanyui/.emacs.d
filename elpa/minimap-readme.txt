This file is an implementation of a minimap sidebar, i.e., a
smaller display of the current buffer on the left side. It
highlights the currently shown region and updates its position
automatically. You can navigate in the minibar by dragging the
active region with the mouse, which will scroll the corresponding
edit buffer.

Usage:
 * Put minimap.el in your load path.
 * (require 'minimap)
 * Use 'M-x minimap-create' in a buffer you're currently editing.
 * Use 'M-x minimap-kill' to kill the minimap.
 * Use 'M-x customize-group RET minimap RET' to adapt minimap to your needs.

Download:
 You can always get the latest version from the git repository:
      git://randomsample.de/minimap.git
 or   http://randomsample.de/minimap.git
