This package provides integration with jedi virtualenv and buildout.
When working within a virtualenv, configure python sys.path passed
to `jedi:server-args' such jedi commands `jedi:complete',
`jedi:goto-definition' and `jedi:doc' show the correct sources.

Installation:
If not using ELPA (i.e list-packages), then add the following to
you init.el/.emacs:

(add-to-list 'load-path 'path-to-this-file)
(require 'pungi)

Usage:
  When you'd like project specific variables to be taken into account,
  e.g python-mode specific changes, you can place a file at the root
  of the project directory called .dir-locals.el, in which
  you can set variables on a per-mode, or global basis.
  See http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
  for documentation.
  Set the `pungi-setup-jedi' to a non-nil value in order for `jedi:setup' to
  take those settings into account.

  If jedi has been required, then jedi:setup will be triggered when
  python-mode-hook is fired.

Testing:
  When visiting a python buffer, move the cursor over a symbol
  and check that invoking M-x `jedi:goto-definition' opens a
  new buffer showing the source of that python symbol.
