pydoc-info provides routines to better search and browse the Python
documentation in Info.

Features:

- Improved Info-lookup support for Python allows quick access to
  the documentation using `info-lookup-symbol' (C-h S) with symbol
  completion for every Python object documented in the standard
  library.

- Hide superfluous *note: references.  Prevent Info from displaying
  "*note:" or "see" in front of cross-references when browsing the
  Python documentation.  This does not affect how other Info
  documents are displayed and is controlled by the variable
  `pydoc-info-hide-note-references'.

Installation:

Before using this package, you may need to download and install the
latest Python Info files:

    wget https://bitbucket.org/jonwaltman/pydoc-info/downloads/python.info.gz
    gunzip python.info
    sudo cp python.info /usr/share/info
    sudo install-info --info-dir=/usr/share/info python.info

Then add the following to your ~/.emacs.d/init.el:

    (add-to-list 'load-path "/path/to/pydoc-info")
    (require 'pydoc-info)

Extending:

The Info-lookup support provided by this package is not limited to
the standard Python documentation.  It can easily be extended to
work with other Python packages that also use Sphinx for
documentation and have generated the necessary Info files.

For example, the documentation for Sphinx can be compiled to the
Info file "sphinx.info" and you have added it to your `INFOPATH'.
By adding the following code to your `init.el', you can use
`info-lookup-symbol' to search the documentation for both Sphinx
and the standard Python modules.

    (pydoc-info-add-help '("python" "sphinx"))

Notes:

pydoc-info is designed to work with Info files produced from the
"new" Python documentation (v2.6 and above).  The "new"
documentation is written in ReStructuredText and built using
Sphinx.  The previous Python Info files were generated from the old
documentation written in LaTeX.

Texinfo support is a recent addition to Sphinx and is currently
limited to the latest development branch.  The Info files
referenced here are not part of the official Python distribution.

The current `info-lookup' support in "python.el" (circa v24.0) is
based on the older Info files and doesn't work with the newer
versions.

Please email bug reports and suggestions to the author, or submit
them at https://bitbucket.org/jonwaltman/pydoc-info/issues
