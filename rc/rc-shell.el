;;; rc-shell.el ---                                  -*- lexical-binding: t; -*-

;;======================================================
;; shell-script-mode
;;======================================================

;;較完整地支援shell script語法高亮。
;; (defface font-lock-system-command-face
;; '((((class color)) (:foreground "purple")))
;; "I am comment"
;; :group 'font-lock-faces)

;; (defun font-lock-system-command (&optional limit)
;; ""
;; (and (search-forward-regexp "\\<[a-zA-Z\\-]+\\>" limit t)
;;      (executable-find
;;       (buffer-substring-no-properties (car (bounds-of-thing-at-point 'word))
;;                                       (cdr (bounds-of-thing-at-point 'word))))))
;;
;; (font-lock-add-keywords 'sh-mode
;;                         '((font-lock-system-command . 'font-lock-system-command-face)))

;; One-key to run script with Bash
;; (require 'sh-script)
(with-eval-after-load 'sh-script
  (define-key sh-mode-map (kbd "<f5>") 'run-current-sh)
  (defun run-current-sh ()
    (interactive)
    (save-buffer)(shell-command (format "bash %s" (buffer-real-name))))
  )

(defvar my/sh-external-commands
  '(;; version control
    "git" "hg" "svn"
    ;; containers / orchestration
    "docker" "docker-compose" "podman" "podman-compose" "buildah" "skopeo"
    "kubectl" "helm" "kustomize" "k9s" "minikube" "kind"
    ;; cloud / infra
    "terraform" "tofu" "ansible" "ansible-playbook" "ansible-vault"
    "vagrant" "packer" "consul" "nomad" "vault"
    "aws" "gcloud" "az" "doctl" "flyctl" "heroku"
    ;; networking / transfer
    "curl" "wget" "ssh" "scp" "sftp" "rsync" "ftp" "telnet" "nc" "ncat" "socat"
    "ping" "traceroute" "mtr" "dig" "nslookup" "host" "whois"
    "ip" "ifconfig" "route" "ss" "netstat" "tcpdump" "nmap"
    "iptables" "nft" "ufw"
    ;; text processing
    "grep" "egrep" "fgrep" "rg" "ag" "ack"
    "sed" "awk" "gawk" "cut" "tr" "sort" "uniq" "wc" "head" "tail"
    "tee" "paste" "join" "comm" "diff" "patch" "cmp"
    "jq" "yq" "xq" "fx" "miller" "mlr"
    ;; file / dir
    "find" "fd" "fdfind" "locate" "which" "whereis" "xargs"
    "tree" "stat" "file" "du" "df" "lsof"
    "cp" "mv" "rm" "ln" "mkdir" "rmdir" "chmod" "chown" "chgrp" "touch"
    "ls" "cat" "bat" "less" "more" "tac" "nl"
    "realpath" "readlink" "dirname" "basename"
    ;; archives / compression
    "tar" "gzip" "gunzip" "bzip2" "bunzip2" "xz" "unxz" "zstd"
    "zip" "unzip" "7z" "7za" "rar" "unrar"
    ;; build / package (system)
    "make" "cmake" "ninja" "meson" "autoconf" "automake" "configure"
    "apt" "apt-get" "apt-cache" "dpkg" "aptitude"
    "yum" "dnf" "rpm" "zypper" "pacman" "yay" "paru"
    "apk" "emerge" "xbps-install"
    "brew" "port" "snap" "flatpak"
    "nix" "nix-env" "nix-shell" "nix-build" "nix-store"
    ;; languages / runtimes
    "python" "python2" "python3" "pip" "pip2" "pip3" "pipx" "pipenv" "poetry"
    "uv" "ruff" "black" "mypy" "pytest" "tox" "virtualenv" "conda" "mamba"
    "node" "nodejs" "npm" "npx" "yarn" "pnpm" "bun" "deno"
    "ruby" "gem" "bundle" "bundler" "rake" "rails"
    "perl" "cpan" "cpanm"
    "php" "composer"
    "lua" "luarocks"
    "java" "javac" "jar" "javap" "jshell" "kotlin" "kotlinc" "scala" "sbt" "gradle" "mvn"
    "go" "gofmt" "golint"
    "rustc" "cargo" "rustup"
    "clang" "clang++" "gcc" "g++" "cc" "ld" "ar" "as" "objdump" "nm" "strip"
    "swift" "swiftc"
    "dotnet" "csc" "fsharpc"
    "erl" "erlc" "rebar3" "elixir" "mix" "iex"
    "ghc" "runghc" "cabal" "stack"
    "ocaml" "opam" "dune"
    "raku" "zef"
    "R" "Rscript"
    "julia"
    ;; databases / data
    "sqlite3" "mysql" "psql" "pg_dump" "pg_restore" "redis-cli" "mongo" "mongosh"
    ;; systemd / services
    "systemctl" "journalctl" "loginctl" "machinectl" "timedatectl" "hostnamectl"
    "service" "chkconfig" "update-rc.d"
    "supervisorctl" "rc-service" "rc-update"
    ;; processes / monitoring
    "ps" "top" "htop" "btop" "atop" "iotop" "iftop"
    "kill" "killall" "pkill" "pgrep" "pidof" "nice" "renice" "nohup" "timeout"
    "uptime" "free" "vmstat" "iostat" "mpstat" "sar" "dstat"
    "strace" "ltrace" "perf"
    ;; users / auth
    "sudo" "su" "doas" "passwd" "useradd" "userdel" "usermod"
    "groupadd" "groupdel" "groupmod" "chage" "id" "who" "w" "last" "finger"
    "ssh-keygen" "ssh-add" "ssh-agent" "ssh-copy-id"
    "gpg" "gpg2" "age" "openssl" "step" "certbot"
    ;; shells / terminals
    "bash" "zsh" "fish" "dash" "ksh" "tcsh" "sh"
    "tmux" "screen" "byobu"
    ;; editors / pagers
    "vim" "nvim" "vi" "nano" "emacs" "code" "subl" "micro" "helix" "hx"
    ;; CI / dev tools
    "gh" "glab" "hub" "lazygit" "tig" "delta"
    "shellcheck" "shfmt" "hadolint" "yamllint"
    "pre-commit" "act"
    ;; misc utilities
    "env" "printenv" "watch" "yes" "seq" "date" "cal" "sleep" "true" "false"
    "tput" "stty" "reset" "clear" "script"
    "uuidgen" "base64" "md5sum" "sha1sum" "sha256sum" "sha512sum" "cksum" "crc32"
    "iconv" "recode" "expand" "unexpand" "fold" "fmt" "column" "pr"
    "getent" "ldd" "ldconfig" "modprobe" "lsmod" "dmesg" "lsblk" "lscpu" "lspci" "lsusb"
    "mount" "umount" "fdisk" "parted" "mkfs" "fsck" "blkid"
    "crontab" "at" "anacron" "batch")
  "External commands to highlight in shell scripts.
Add or remove freely; this list is independent of any actual $PATH lookup.")

(defun my/sh-extra-font-lock ()
  "Beef up shell-script-mode highlighting."
  (let ((builtin-keywords (regexp-opt '("declare" "local" "readonly" "export" "unset" "typeset" "shift" "trap" "set" "eval" "exec" "source" "alias" "unalias") 'symbols))
        (external-cmds (regexp-opt my/sh-external-commands 'symbols)))
    (font-lock-add-keywords
     nil
     `(("\\(\\$[A-Za-z_][A-Za-z0-9_]*\\)" 1 font-lock-variable-name-face t) ; $VAR (also inside strings)
       ("\\(\\${[^}]+}\\)" 1 font-lock-variable-name-face t) ; ${VAR}, ${VAR:-default}, etc.
       ("\\(\\$[0-9@*#?$!-]\\)" 1 font-lock-variable-name-face t) ; positional/special params: $1 $@ $# $? $$ $!
       ("\\(\\$(\\|)\\)" 1 font-lock-preprocessor-face t) ; $(...) command substitution delimiters
       ("`\\([^`]*\\)`" 0 font-lock-preprocessor-face t) ; backtick command substitution
       ("\\([<>]&?[0-9]*\\|[0-9]*>>?\\|<<<?\\)" 1 font-lock-warning-face) ; redirection operators
       (,builtin-keywords . font-lock-builtin-face) ; common builtins
       (,external-cmds . font-lock-function-name-face) ; external commands
       ("\\_<\\(--?[A-Za-z][A-Za-z0-9-]*\\)" 1 font-lock-type-face) ; -a, --verbose, --some-flag
       ))))

(add-hook 'sh-mode-hook #'my/sh-extra-font-lock)
(add-hook 'shell-script-mode #'my/sh-extra-font-lock)
(add-hook 'bash-ts-mode-hook #'my/sh-extra-font-lock)

(provide 'rc-shell)
;;; rc-shell.el ends here
