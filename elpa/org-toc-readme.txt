org-toc is a utility to have an up-to-date table of contents in the
org files without exporting (useful primarily for readme files on
GitHub).

To enable this functionality put into your .emacs file something
like

(add-hook 'org-mode-hook 'org-toc-enable)

After that, every time you'll be saving an org file the first
headline with a :TOC: tag will be updated with the current table of
contents.

For details, see https://github.com/snosov1/org-toc/README.org

