update-diredp:
	wget --output-document lisps/dired+.el  "https://www.emacswiki.org/emacs/download/dired%2b.el"
	emacs -batch -f batch-byte-compile lisps/dired+.el
