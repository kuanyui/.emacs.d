function run-installer () {
    git clone https://github.com/kuanyui/moe-theme.el.git git/moe-theme
    git clone https://github.com/kuanyui/moedict.el git/moedict
    git clone https://github.com/kuanyui/twittering-myfav.el.git git/twittering-myfav
    git clone https://github.com/kuanyui/writing-utils.el git/writing-utils
    mkdir -p private
    touch private/flickr.el
    touch private/twittering-filter-users.el
    if [-f ~/.emacs] ; then
        mv ~/.emacs ~/.emacs_backup
    fi
    emacs --batch ./auto-install-packages.el
}

while true; do
    read -p "Do you really want to install kuanyui's Emacs config?" yn
    case $yn in
        [Yy]* ) run-installer; break;;
        [Nn]* ) exit;;
        * ) echo "Please answer yes or no.";;
    esac
done
