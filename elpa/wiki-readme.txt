Wiki is a hypertext and a content management system: Normal users are
encouraged to enhance the hypertext by editing and refactoring
existing pages and by adding more pages.  This is made easy by
requiring a certain way of writing pages.  It is not as complicated
as a markup language such as HTML.  The general idea is to write
plain ASCII.  Word with mixed case such as ThisOne are WikiNames --
they may be a Link or they may not.  If they are, clicking them will
take you to the page with that WikiName; if they are not, clicking
them will create an empty page for you to fill out.

This mode does all of this for you without using a web browser, cgi
scripts, databases, etc.  All you need is Emacs!  In order to
install, put wiki.el on you load-path, and add the following to your
.emacs file:

(require 'wiki)

This will activate WikiMode for all files in `wiki-directories' as soon
as they are opened.  This works by adding `wiki-maybe' to
`find-file-hooks'.

Emacs provides the functionality usually found on Wiki web sites
automatically: To find out how many pages have links to your page,
use `grep' or `dired-do-search'.  To get an index of all wikis, use
`dired'.  To keep old versions around, use `version-control' or use
`vc-next-action'.  To edit wikis, use Emacs!

You can publish a wiki using `wiki-publish', or you can use
`dired-do-wiki-publish' to publish marked wikis from dired, or you
can use `wiki-publish-all' to publish all wikis and write an index
file.  This will translate your plain text wikis into HTML according
to the rules defined in `wiki-pub-rules'.

Find out more: Take a look at http://c2.com/cgi/wiki?StartingPoints

XEmacs

XEmacs users will have to get easy-mmode.el, I'm afraid.  You should
be able to get easy-mmode.el from a web site carrying the Emacs
sources.  It loads fine, so that shouldn't be a problem.  Put
`easy-mmode' somewhere in your load-path and install as follows:

(require 'easy-mmode)
(require 'wiki)

What about a Major Mode?

By default, wiki files will be in `fundamental-mode'.  I prefer to be
in `text-mode', instead.  You can do this either for all files that
have WikiNames by changing `auto-mode-alist', or you can make
text-mode the default mode instead of fundamental mode.  Example:

(setq default-major-mode 'text-mode)

This puts wiki files in `text-mode'.  One problem remains, however.
Text mode usually means that the apostrophe is considered to be part
of words, and some WikiNames will not be highlighted correctly, such
as "WikiName''''s".  In that case, change the syntax table, if you
don't mind the side effects.  Example:

(modify-syntax-entry ?' "." text-mode-syntax-table)

Thanks

Frank Gerhardt <Frank.Gerhardt@web.de>, author of the original wiki-mode.
  His latest version is here: http://www.s.netic.de/fg/wiki-mode/wiki.el
Thomas Link <t.link@gmx.at>
John Wiegley <johnw@gnu.org>, author of emacs-wiki.el.
  His latest version is here: http://www.emacswiki.org/emacs/EmacsWikiMode
and evolved into Emacs Muse: http://www.emacswiki.org/emacs/EmacsMuse
