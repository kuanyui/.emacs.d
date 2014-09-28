js-comint.el let's you run an inferior javascript process in emacs,
and defines a few functions for sending javascript input to it quickly.

 Usage:
 Put js-comint.el in your load path
 Add (require 'js-comint) to your .emacs
 Set inferior-js-program-command to the execution command for running your javascript REPL
 (setq inferior-js-program-command "/path/to/executable <args>")
 Do: M-x run-js
 Away you go.

 I've added the following couple of lines to my .emacs to take advantage of
 cool keybindings for sending things to the javascript interpreter inside
 of Steve Yegge's most excellent js2-mode.

(add-hook 'js2-mode-hook '(lambda ()
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))

 This is version 0.0.1, so I've only tested it on my own version of emacs which is currently:
 GNU Emacs 22.0.90.1 (i386-apple-darwin8.8.1, Carbon Version 1.6.0) of 2006-10-28
 Not sure if it'll work anywhere else, but it doesn't require anything apple-ish, just emacs-ish.

Additionally, I've only tested this with rhino.  I'm sure it'll probably work with spidermonkey,
though if it barfs let me know, and I'll update it.

I'm a newbie elisper, so please let me know if I'm a. doing things the wrong way, b.
making things work they way they shouldn't in the elisp world.
