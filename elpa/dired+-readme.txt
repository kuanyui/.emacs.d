   Extensions to Dired.

 This file extends functionalities provided by standard GNU Emacs
 files `dired.el', `dired-aux.el', and `dired-x.el'.

 Key bindings changed.  Menus redefined.  `diredp-mouse-3-menu'
 popup menu added.  New commands.  Some commands enhanced.

 All of the new functions, variables, and faces defined here have
 the prefix `diredp-' (for Dired Plus) in their names.

 Additional suggested key bindings:

   (define-key ctl-x-map   "d" 'diredp-dired-files)
   (define-key ctl-x-4-map "d" 'diredp-dired-files-other-window)


 Fontification Level
 -------------------

 If you want a maximum or minimum fontification for Dired mode,
 then customize option `font-lock-maximum-decoration'.  If you want
 a different fontification level for Dired than for other modes,
 you can do this too by customizing
 `font-lock-maximize-decoration'.


 Act on All Files
 ----------------

 Most of the commands (such as `C' and `M-g') that operate on the
 marked files have the added feature here that multiple `C-u' use
 not the files that are marked or the next or previous N files, but
 *all* of the files in the Dired buffer.  Just what "all" files
 means changes with the number of `C-u', as follows:

   `C-u C-u'         - Use all files present, but no directories.
   `C-u C-u C-u'     - Use all files and dirs except `.' and `..'.
   `C-u C-u C-u C-u' - use all files and dirs, `.' and `..'.

   (More than four `C-u' act the same as two.)

 This feature can be particularly useful when you have a Dired
 buffer with files chosen from multiple directories.

 Note that this behavior is described only in the doc string of
 function `dired-get-marked-files'.  It is *not* described in the
 doc strings of the various commands, because that would require
 redefining each command separately here.  Instead, we redefine
 only macro `dired-map-over-marks' and function
 `dired-get-filename' in order to achieve this effect.


 Act on Marked (or All) Files Here and Below
 -------------------------------------------

 The prefix argument behavior just described does not apply to the
 `diredp-*-recursive' commands.  These commands act on the marked
 files in the current Dired buffer or on all files in the directory
 if none are marked.

 But these commands also handle marked subdirectories recursively,
 in the same way.  That is, they act also on the marked files in
 any marked subdirectories, found recursively.  If there is no
 Dired buffer for a given marked subdirectory then all of its files
 and subdirs are acted on.

 With a prefix argument, all marks are ignored.  The commands act
 on all files in the current Dired buffer and all of its
 subdirectories, recursively.

 All of the `diredp-*-recursive' commands are on prefix key `M-+',
 and they are available on submenu `Marked Here and Below' of the
 `Multiple' menu-bar menu.

 If you use library `Icicles' then you have these additional
 commands/keys that act recursively on marked files.  They are in
 the `Icicles' submenu of menu `Multiple' > `Marked Here and
 Below'.

 * `M-+ M-s M-s' or `M-s M-s m' - Use Icicles search (and its
                 on-demand replace) on the marked files.

 * Save the names of the marked files:

   `M-+ C-M->' - Save as a completion set, for use during
                 completion (e.g. with `C-x C-f').

   `M-+ C->'   - Add marked names to the names in the current saved
                 completion set.

   `M-+ C-}'   - Save persistently to an Icicles cache file, for
                 use during completion in another session.

   `icicle-dired-save-marked-to-fileset-recursive' - Like `M-+
                 C-}', but save persistently to an Emacs fileset.

   `M-+ C-M-}' - Save to a Lisp variable.


 In the other direction, if you have a saved set of file names then
 you can use `C-M-<' (`icicle-dired-chosen-files-other-window') in
 Dired to open a Dired buffer for just those files.  So you can
 mark some files and subdirs in a hierarchy of Dired buffers, use
 `M-+ C-}' to save their names persistently, then later use `C-{'
 to retrieve them, and `C-M-<' (in Dired) to open Dired on them.


 Inserted Subdirs, Multiple Dired Buffers, Files from Anywhere,...
 -----------------------------------------------------------------

 These two standard Dired features are worth pointing out:

 * You can insert multiple subdirectory listings into a single
   Dired buffer using `i' on each subdir line.  Use `C-u i' to
   specify `ls' switches.  Specifying switch `R' inserts the
   inserted subdirectory's subdirs also, recursively.  You can also
   use `i' to bounce between a subdirectory line and its
   inserted-listing header line.  You can delete a subdir listing
   using `C-u k' on its header line.  You can hide/show an inserted
   subdir using `$' and `M-$'.  You can use `C-_' to undo any of
   these operations.

 * You can open a Dired buffer for an arbitrary set of files, from
   different directories.

   First, you can pass a glob pattern with wildcards to `dired'
   interactively, as the file name.

   Beyond that, you can invoke `dired' non-interactively, passing
   it a cons of buffer name and file names.  Relative file names
   are interpreted relative to the value of `default-directory'.
   Use absolute file names if appropriate.

 Some other libraries, such as `Bookmark+' and `Icicles', make it
 easy to create or re-create Dired buffers that list specific files
 and have a particular set of markings.  This can be handy for
 using Dired buffers to manage projects.  In such use cases you
 might have multiple Dired buffers that have quite specific
 contents and that you want to keep around during a session.

 This is one motivation for the Dired+ `diredp-*-recursive'
 commands, which act on the marked files in marked subdirectories,
 recursively.  In one sense these commands are an alternative to
 using a single Dired buffer with inserted subdirectories.  They
 let you use the same operations on the files in a set of Dired
 directories, without inserting those directories into an ancestor
 Dired buffer.

 So you might have some subdirectories inserted in the same Dired
 buffer, and you might have separate Dired buffers for some
 subdirectories.  Sometimes it is useful to have both for the same
 subdirectory.  And sometimes it is useful to move from one
 presentation to the other.

 You can use command `diredp-dired-inserted-subdirs' to open a
 separate Dired buffer for each of the subdirs that is inserted in
 the current Dired buffer.  Markings and Dired switches are
 preserved.

 In the opposite direction, if you use `Icicles' then you can use
 multi-command `icicle-dired-insert-as-subdir', which lets you
 insert any number of directories you choose interactively into a
 Dired ancestor directory listing.  If a directory you choose to
 insert already has its own Dired buffer, then its markings and
 switches are preserved for the new, subdirectory listing in the
 ancestor Dired buffer.


 Faces defined here:

   `diredp-compressed-file-suffix', `diredp-date-time',
   `diredp-deletion', `diredp-deletion-file-name',
   `diredp-dir-heading', `diredp-dir-priv', `diredp-display-msg',
   `diredp-exec-priv', `diredp-executable-tag', `diredp-file-name',
   `diredp-file-suffix', `diredp-flag-mark',
   `diredp-flag-mark-line', `diredp-get-file-or-dir-name',
   `diredp-ignored-file-name', `diredp-link-priv',
   `diredp-mode-line-flagged', `diredp-mode-line-marked'
   `diredp-no-priv', `diredp-number', `diredp-other-priv',
   `diredp-rare-priv', `diredp-read-priv', `diredp-symlink',
   `diredp-write-priv'.

 Commands defined here:

   `diredp-bookmark-this-file', `diredp-byte-compile-this-file',
   `diredp-capitalize', `diredp-capitalize-recursive',
   `diredp-capitalize-this-file', `diredp-chgrp-this-file',
   `diredp-chmod-this-file', `diredp-chown-this-file',
   `diredp-compress-this-file',
   `diredp-copy-filename-as-kill-recursive',
   `diredp-copy-tags-this-file', `diredp-copy-this-file',
   `diredp-delete-this-file', `diredp-describe-file',
   `diredp-describe-mode', `diredp-dired-files',
   `diredp-dired-files-other-window', `diredp-dired-for-files',
   `diredp-dired-for-files-other-window',
   `diredp-dired-inserted-subdirs', `diredp-dired-plus-help',
   `diredp-dired-this-subdir', `diredp-dired-union',
   `diredp-dired-union-other-window', `diredp-do-bookmark',
   `diredp-do-bookmark-in-bookmark-file',
   `diredp-do-bookmark-in-bookmark-file-recursive',
   `diredp-do-bookmark-recursive', `diredp-do-chmod-recursive',
   `diredp-do-chgrp-recursive', `diredp-do-chown-recursive',
   `diredp-do-copy-recursive', `diredp-do-decrypt-recursive',
   `diredp-do-encrypt-recursive',
   `diredp-do-find-marked-files-recursive', `diredp-do-grep',
   `diredp-do-grep-recursive', `diredp-do-hardlink-recursive',
   `diredp-do-isearch-recursive',
   `diredp-do-isearch-regexp-recursive',
   `diredp-do-move-recursive', `diredp-do-paste-add-tags',
   `diredp-do-paste-replace-tags', `diredp-do-print-recursive',
   `diredp-do-query-replace-regexp-recursive',
   `diredp-do-redisplay-recursive',
   `diredp-do-relsymlink-recursive', `diredp-do-remove-all-tags',
   `diredp-do-search-recursive', `diredp-do-set-tag-value',
   `diredp-do-shell-command-recursive', `diredp-do-sign-recursive',
   `diredp-do-symlink-recursive', `diredp-do-tag',
   `diredp-do-touch-recursive', `diredp-do-untag',
   `diredp-do-verify-recursive', `diredp-downcase-recursive',
   `diredp-downcase-this-file', `diredp-ediff', `diredp-fileset',
   `diredp-find-a-file', `diredp-find-a-file-other-frame',
   `diredp-find-a-file-other-window',
   `diredp-find-file-other-frame',
   `diredp-find-file-reuse-dir-buffer',
   `diredp-flag-region-files-for-deletion',
   `diredp-grep-this-file', `diredp-hardlink-this-file',
   `diredp-image-dired-comment-files-recursive',
   `diredp-image-dired-delete-tag-recursive',
   `diredp-image-dired-display-thumbs-recursive',
   `diredp-image-dired-tag-files-recursive',
   `diredp-insert-as-subdir', `diredp-insert-subdirs',
   `diredp-insert-subdirs-recursive',
   `diredp-list-marked-recursive', `diredp-load-this-file',
   `diredp-marked', `diredp-marked-other-window',
   `diredp-marked-recursive',
   `diredp-marked-recursive-other-window',
   `diredp-mark-files-tagged-all', `diredp-mark-files-tagged-none',
   `diredp-mark-files-tagged-not-all',
   `diredp-mark-files-tagged-some',
   `diredp-mark-files-tagged-regexp', `diredp-mark-region-files',
   `diredp-mark/unmark-extension', `diredp-mouse-3-menu',
   `diredp-mouse-backup-diff', `diredp-mouse-copy-tags',
   `diredp-mouse-describe-file', `diredp-mouse-diff',
   `diredp-mouse-do-bookmark', `diredp-mouse-do-byte-compile',
   `diredp-mouse-do-chgrp', `diredp-mouse-do-chmod',
   `diredp-mouse-do-chown', `diredp-mouse-do-compress',
   `diredp-mouse-do-copy', `diredp-mouse-do-delete',
   `diredp-mouse-do-grep', `diredp-mouse-do-hardlink',
   `diredp-mouse-do-load', `diredp-mouse-do-print',
   `diredp-mouse-do-remove-all-tags', `diredp-mouse-do-rename',
   `diredp-mouse-do-set-tag-value',
   `diredp-mouse-do-shell-command', `diredp-mouse-do-symlink',
   `diredp-mouse-do-tag', `diredp-mouse-do-untag',
   `diredp-mouse-downcase', `diredp-mouse-ediff',
   `diredp-mouse-find-file', `diredp-mouse-find-file-other-frame',
   `diredp-mouse-find-file-reuse-dir-buffer',
   `diredp-mouse-flag-file-deletion', `diredp-mouse-mark',
   `diredp-mouse-mark-region-files', `diredp-mouse-mark/unmark',
   `diredp-mouse-unmark', `diredp-mouse-upcase',
   `diredp-mouse-view-file',
   `diredp-multiple-w32-browser-recursive',
   `diredp-nb-marked-in-mode-name', `diredp-omit-marked',
   `diredp-omit-unmarked', `diredp-paste-add-tags-this-file',
   `diredp-paste-replace-tags-this-file', `diredp-print-this-file',
   `diredp-relsymlink-this-file',
   `diredp-remove-all-tags-this-file', `diredp-rename-this-file',
   `diredp-send-bug-report',
   `diredp-set-bookmark-file-bookmark-for-marked',
   `diredp-set-bookmark-file-bookmark-for-marked-recursive',
   `diredp-set-tag-value-this-file',
   `diredp-shell-command-this-file', `diredp-symlink-this-file',
   `diredp-tag-this-file', `diredp-toggle-find-file-reuse-dir',
   `diredp-touch-this-file', `diredp-toggle-marks-in-region',
   `diredp-unmark-files-tagged-all',
   `diredp-unmark-files-tagged-none',
   `diredp-unmark-files-tagged-not-all',
   `diredp-unmark-files-tagged-some', `diredp-unmark-region-files',
   `diredp-untag-this-file', `diredp-upcase-recursive',
   `diredp-upcase-this-file', `diredp-w32-drives',
   `diredp-w32-drives-mode', `toggle-diredp-find-file-reuse-dir'.

 User options defined here:

   `diff-switches', `diredp-prompt-for-bookmark-prefix-flag',
   `diredp-w32-local-drives'.

 Non-interactive functions defined here:

   `derived-mode-p' (Emacs < 22), `diredp-all-files',
   `diredp-ancestor-dirs', `diredp-bookmark',
   `diredp-create-files-non-directory-recursive',
   `diredp-directories-within',
   `diredp-dired-files-interactive-spec',
   `diredp-dired-plus-description',
   `diredp-dired-plus-description+links',
   `diredp-dired-plus-help-link', `diredp-dired-union-1',
   `diredp-dired-union-interactive-spec',
   `diredp-do-chxxx-recursive', `diredp-do-create-files-recursive',
   `diredp-do-grep-1', `diredp-ensure-mode',
   `diredp-fewer-than-2-files-p', `diredp-find-a-file-read-args',
   `diredp-files-within', `diredp-files-within-1',
   `diredp-get-confirmation-recursive', `diredp-get-files',
   `diredp-get-files-for-dir', `diredp-internal-do-deletions',
   `diredp-list-files', `diredp-make-find-file-keys-reuse-dirs',
   `diredp-make-find-file-keys-not-reuse-dirs', `diredp-maplist',
   `diredp-marked-here', `diredp-mark-files-tagged-all/none',
   `diredp-mark-files-tagged-some/not-all',
   `diredp-paste-add-tags', `diredp-paste-replace-tags',
   `diredp-read-bookmark-file-args', `diredp-remove-if-not',
   `diredp-set-tag-value', `diredp-tag',
   `diredp-this-file-marked-p', `diredp-this-file-unmarked-p',
   `diredp-this-subdir', `diredp-untag', `diredp-y-or-n-files-p'.

 Variables defined here:

   `diredp-file-line-overlay', `diredp-files-within-dirs-done',
   `diredp-font-lock-keywords-1', `diredp-loaded-p',
   `diredp-menu-bar-immediate-menu',
   `diredp-menu-bar-immediate-bookmarks-menu',
   `diredp-menu-bar-mark-menu', `diredp-menu-bar-operate-menu',
   `diredp-menu-bar-operate-bookmarks-menu',
   `diredp-menu-bar-recursive-marked-menu',
   `diredp-menu-bar-regexp-menu', `diredp-menu-bar-subdir-menu',
   `diredp-re-no-dot', `diredp-w32-drives-mode-map'.


 ***** NOTE: The following macros defined in `dired.el' have
             been REDEFINED HERE:

 `dired-map-over-marks'    - Treat multiple `C-u' specially.
 `dired-mark-if'           - Better initial msg - Emacs bug #8523.

 ***** NOTE: The following functions defined in `dired.el' have
             been REDEFINED HERE:

 `dired-do-delete'         - Display message to warn that marked,
                             not flagged, files will be deleted.
 `dired-do-flagged-delete' - Display message to warn that flagged,
                             not marked, files will be deleted.
 `dired-find-file'         - Allow `.' and `..' (Emacs 20 only).
 `dired-get-filename'      - Test `./' and `../' (like `.', `..').
 `dired-goto-file'         - Fix Emacs bug #7126.
                             Remove `/' from dir before compare.
 `dired-insert-directory'  - Compute WILDCARD arg for
                             `insert-directory' for individual file
                             (don't just use nil). (Emacs 23+, and
                             only for MS Windows)
 `dired-insert-set-properties' - `mouse-face' on whole line.
 `dired-mark-files-regexp' - Add regexp to `regexp-search-ring'.
 `dired-mark-pop-up'       - Delete the window or frame popped up,
                             afterward, and bury its buffer. Do not
                             show a menu bar for pop-up frame.
 `dired-pop-to-buffer'     - Put window point at bob (bug #12281).
