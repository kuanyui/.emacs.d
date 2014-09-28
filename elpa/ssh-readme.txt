Support for remote logins using `ssh'.
This program is layered on top of shell.el; the code here only accounts
for the variations needed to handle a remote process, e.g. directory
tracking and the sending of some special characters.

If you wish for ssh mode to prompt you in the minibuffer for
passwords when a password prompt appears, just enter m-x send-invisible
and type in your line, or add `comint-watch-for-password-prompt' to
`comint-output-filter-functions'.
