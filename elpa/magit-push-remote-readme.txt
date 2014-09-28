This plug-in provides support for an additional default remote
which when pushing is used instead of the "merge" default specified
by the git-config(1) option `branch.<name>.remote'.

Together `branch.<name>.remote' and `branch.<name>.merge' set the
default used by git-pull(1) and git-push(1).  Like their git
counterparts `magit-push' and `magit-pull' use these options. So
does `magit-status' which displays commits not pushed to or not
pulled from the default remote/branch.

This works nicely if commits most often flow like this:

  +------------+            +------------+
  |   remote   | -- pull -> |   local    |
  |    repo    | <- push -- |    repo    |
  +------------+            +------------+

But it is inconventient if commits most often flow through your
local repository like this:

  +------------+            +------------+            +------------+
  |  upstream  | -- pull -> |   local    |            |    your    |
  |    repo    |            |    repo    | -- push -> |   public   |
  |            |            +------------+            |    repo    |
  |            | <- merge pull reguest -------------- |            |
  +------------+                                      +------------+

This package modifies magit to automatically detect whether the
latter workflow is used; and if so provide additional information
related to that "personal" or "push" remote and push to it by
default.

Loading this library redefines the commands `magit-push',
`magit-push-tags', and `magit-refresh-status'.

When `magit-push-remote-mode' is turned on and the repository has a
push-remote `magit-push' and `magit-push-tags' now by default push
to the push-remote.  Otherwise they behave mostly like the original
versions defined in `magit.el'.  (The `magit-push' defined here
actually differs a bit in that it is more carefull about when to
--set-upstream.)

When `magit-push-remote-mode' is turned on and the repository has a
push-remote `magit-status' shows information related to both the
push and pull (git's default) remote.  Otherwise it behaves like
the version in `magit.el'.

`magit-push-remote-mode' should be turned on in all Magit buffers;

  (add-hook 'magit-mode-hook 'turn-on-magit-push-remote)

The push-remote is determined based on it's name.  A good name is
e.g. your username.  Again it makes sense to set this globally:

  git config --global magit.defaultpushremote <REMOTE_NAME>

If you want to use a different name in some repositories, that is
also possible:

  git config magit.pushremote <REMOTE_NAME>

Now read `magit-push's doc-string and you are ready to go.
