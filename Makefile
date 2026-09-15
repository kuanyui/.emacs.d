## Which Emacs to use. Default: `emacs` from PATH. Example: `make recompile EMACS=emacs-31`
EMACS ?= emacs

.PHONY: default help recompile update-diredp

default: help

define print_title
printf "\n"; \
printf "\033[36m=============================================================\033[0m\n"; \
printf "\033[36m %s \033[0m\n" $(1); \
printf "\033[36m=============================================================\033[0m\n" ;
endef

define __DOC__
# ~/.emacs.d
- Something broke (e.g. odd errors after an Emacs upgrade)? Run `make recompile`. Then restart Emacs.
- New machine, fresh clone? Run `make recompile` once.

endef
export __DOC__

# suppress `perl: warning: Setting locale failed.` (DO NOT move this comment at the EOL of export PERL_BADLANG = 0, otherwise a trailing space will be added to the value)
export PERL_BADLANG = 0

help:  ## Show this help.
	@# Some operating system / Linux distro may use `mawk` (e.g. Ubuntu), so prefer more portable `perl` over `awk`.
	@if command -v perl >/dev/null 2>&1; then \
	    $(call print_title,"Quick Start") \
	    printf '%s' "$$__DOC__" | perl -pe 's/`([^`]+)`/\033[34m`\1`\033[0m/g; s/^(#+ .+)/\033[32m\1\033[0m/g; s/(https?:\/\/[a-zA-Z0-9.\/_#?-]+)/\033[33m\1\033[0m/g'; \
	    $(call print_title,"Variables") \
	    perl -ne 'if (/^## /) { $$comment = $$_; $$next = <>; if ($$next =~ /^([A-Za-z0-9_]+)\s*[:?+]?=\s*(.*)$$/) { $$v = $$1; $$c = substr($$comment, 3); $$c =~ s/`([^`]+)`/\033[34m`\1`\033[0m/g; printf "\033[35m%-16s\033[0m %s", $$v, $$c; } }' $(MAKEFILE_LIST); \
	    $(call print_title,"Targets") \
	    perl -ne 'if (/^([a-zA-Z0-9_-]+):.*?## (.*)$$/) { $$t = $$1; $$d = $$2; $$d =~ s/`([^`]+)`/\033[34m`\1`\033[0m/g; printf "\033[32m%-16s\033[0m %s\n", $$t, $$d; } elsif (/^[ \t]*### *(.*)/) { print "\033[34m$$1\033[0m\n"; }' $(MAKEFILE_LIST); \
	else \
	    $(call print_title,"Quick Start") \
	    printf '%s' "$$__DOC__" | gawk '{gsub(/`[^`]+`/, "\033[34m&\033[0m"); gsub(/^#+ .+/, "\033[32m&\033[0m"); print}'; \
	    $(call print_title,"Variables") \
	    gawk '/^## /{comment=$$0; gsub(/`[^`]+`/, "\033[34m&\033[0m", comment); getline; if ($$1 ~ /^[A-Za-z0-9_]+$$/ && match($$0, /^[^:?+]*[:?+]?=/, m)) printf "\033[35m%-16s\033[0m %s\n", $$1, substr(comment, 4)}' $(MAKEFILE_LIST); \
	    $(call print_title,"Targets") \
	    gawk 'match($$0, /^([a-zA-Z0-9_-]+):.*?## (.*)$$/, m) { d=m[2]; gsub(/`[^`]+`/, "\033[34m&\033[0m", d); printf "\033[32m%-16s\033[0m %s\n", m[1], d } match($$0, /^[ \t]*### *(.*)/, m) { printf "\033[34m%s\033[0m\n", m[1] }' $(MAKEFILE_LIST); \
	fi

# Needs Emacs 29+ (for `package-recompile-all`).
recompile:  ## When something breaks, run this. It rebuilds all `.elc` with the current Emacs. Then restart Emacs.
	$(EMACS) --batch -L lisps \
	  --eval '(setq package-user-dir (expand-file-name "elpa"))' \
	  --eval '(package-initialize)' \
	  --eval '(package-recompile-all)' \
	  --eval '(dolist (elc (directory-files-recursively "lisps" (rx ".elc" eos))) (when (file-exists-p (substring elc 0 -1)) (delete-file elc)))' \
	  --eval '(byte-recompile-directory (expand-file-name "lisps") 0)'
	@echo "Done. Please restart Emacs."

update-diredp:  ## Download the latest dired+ from EmacsWiki and compile it.
	wget --output-document lisps/dired+.el  "https://www.emacswiki.org/emacs/download/dired%2b.el"
	$(EMACS) -batch -f batch-byte-compile lisps/dired+.el
