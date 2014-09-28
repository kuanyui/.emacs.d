If you have not installed this from a package such as those on
Marmalade or MELPA, then save kanban.el to a directory in your
load-path and add

(require 'kanban)

to your Emacs start-up files.

Usage:

* Zero state Kanban: Directly displaying org-mode todo states as kanban board

Use the functions kanban-headers and kanban-zero in TBLFM lines to
get your org-mode todo states as kanban table. Update with C-c C-c
on the TBLFM line.

Example:

|   |   |   |
|---+---+---|
|   |   |   |
|   |   |   |
#+TBLFM: @1='(kanban-headers $#)::@2$1..@>$>='(kanban-zero @# $# "TAG" '(list-of-files))
"TAG" and the list of files are optional

* Stateful Kanban: Use org-mode to retrieve tasks, but track their state in the Kanban board

|   |   |   |
|---+---+---|
|   |   |   |
|   |   |   |
#+TBLFM: @1='(kanban-headers $#)::@2$1..@>$1='(kanban-todo @# @2$2..@>$> "TAG" '(list-of-files))
"TAG" and the list of files are optional

TODO: The links don’t yet work for tagged entries. Fix that. There
has to be some org-mode function to retrieve the plain header.

TODO: kanban-todo sometimes inserts no tasks at all if there are multiple tasks in non-standard states.

TODO: bold text in headlines breaks the parser (*bold*).
