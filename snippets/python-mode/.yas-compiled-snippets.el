;;; Compiled snippets and support files for `python-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
		     '(("qtclass"
			(prognclass ${1:ClassName} nil : def __init__
				    (self
				     (\, parent=None))
				    : super
				    ($1
				     (\, self))
				    \.__init__
				    (parent)
				    $0)
			"qtclass" nil nil
			((yas/indent-line 'auto))
			nil "direct-keybinding" nil)))


;;; Do not edit! File generated at Sat Aug 16 13:34:14 2014
