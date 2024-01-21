;; -*- coding: utf-8; lexical-binding: t; -*-


(eval-when-compile
  (require 'cl-lib))

(setq prettify-symbols-patterns
      (let ((lispen '(emacs-lisp-mode
                      inferior-lisp-mode
                      lisp-interaction-mode
                      lisp-mode))
            (mlen    '(sml-mode
                       inferior-sml-mode))
            (texen   '(latex-mode
                       org-mode))
            (c-like '(c-mode
                      c++-mode
                      perl-mode
                      sh-mode)))
        `(
          (?× "*"                    (,@mlen))
          (?\\ "\\textbackslash"     nil)
          (?| "\\textbar"            nil)
          (?¡ "!!"                   nil)
          (?¢ "cents?"               nil)
          (?£ "pounds?"              nil)
          (?¥ "yen"                  nil)
          (?© "&copy;"               nil)
          (?± "plus-minus"           nil)
          (?² "square"               nil)
          (?³ "cube"                 nil)
          (?· "dot-product"          nil)
          (?¼ "one-quarter"          nil)
          (?½ "one-half"             nil)
          (?¾ "three-quarters"       nil)
          (?÷ "/"                    nil)
          (?• "\\cdot"               (,@texen))
          (?° "\\textdegree"         (,@texen))
          (?☹ ":-("                  (erc-mode))
          (?☺ ":-)"                  (ec-mode))
          (?α "\\<alpha\\>"          (haskell-mode))
          (?β "\\<beta\\>"           (haskell-mode))
          (?γ "\\<gamma\\>"          (haskell-mode))
          (?δ "\\<delta\\>"          (haskell-mode))
          (?λ "\\<fn\\>"             (,@mlen))

          (?′ "[^']'[^']"            ( haskell-mode))
          (?″ "''"                   ( haskell-mode))
          (?‼ "!!"                   (haskell-mode))
          (?‽ "!?"                   nil)
          (?← ":="                   (smalltalk-mode))
          (?← "<-"                   ( haskell-mode))
          (?→ "->"                   (perl-mode  haskell-mode ,@mlen))
          (?⇒ "=>"                   (perl-mode ,@mlen))
          (?∓ "minus-plus"           nil)
          (?√ "\\<sqrt\\>"           ())
          (?∞ "\\<infinity\\>"       nil)
          (?∧ "\\<andalso\\>"        (,@mlen))
          (?∨ "\\<orelse\\>"         (,@mlen))
          (?≈ "~="                   (perl-mode))

          (?∅ "\\<NULL\\>"           (,@c-like))
          (?¬ "[( ]\\(!\\b\\)"       (,@c-like))
          (?√ " +\\(sqrt\\)("        (,@c-like))
          (?∧ "&&"                   (,@c-like ))
          (?∨ "||"                   (,@c-like ))
          (?≠ "\\!="                 (,@c-like ))



          ,@(map 'list (lambda (c p) (list c (format "\\(\\\\%s\\({}\\)?\\)" p) `(,@texen)))
                 "$%_}&#{†‡¶©§…£^~*\\|{}•©†‡$…—–¡><ªº¶·¿“”‘’®§£™_〈〉"
                 '("$" "%" "_" "}" "&" "#" "{" "dag" "ddag" "P" "copyright" "S" "dots" "pounds" "textasciicircum"
                   "textasciitilde" "textasteriskcentered" "textbackslash" "textbar" "textbraceleft" "textbraceright"
                   "textbullet" "textcopyright" "textdagger" "textdaggerdbl" "textdollar" "textellipsis" "textemdash"
                   "textendash" "textexclamdown" "textgreater" "textless" "textordfeminine" "textordmasculine"
                   "textparagraph" "textperiodcentered" "textquestiondown" "textquotedblleft" "textquotedblright"
                   "textquoteleft" "textquoteright" "textregistered" "textsection" "textsterling" "texttrademark"
                   "textunderscore" "textlangle" "textrangle"))

          ;; LaTeX math symbols.
          ,@(map 'list (lambda (c p) (list c (format "\\(\\\\%s\\(?:{}\\)?\\)" p) `(,@texen)))
                 "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩαβγδεζηθϑικλμνξοπϖρςστυϒφχψω"
                 '("Alpha" "Beta" "Gamma" "Delta" "Epsilon" "Zeta" "Eta" "Theta"
                   "Iota" "Kappa" "Lambda" "Mu" "Nu" "Xi" "Omicron" "Pi" "Rho"
                   "Sigma" "Tau" "Upsilon" "Phi" "Chi" "Psi" "Omega" "alpha" "beta"
                   "gamma" "delta" "epsilon" "zeta" "eta" "theta" "thetasym" "iota"
                   "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "piv" "rho"
                   "sigmaf" "sigma" "tau" "upsilon" "upsih" "phi" "chi" "psi"
                   "omega"))

          ;; This list is taken from the HTML4 spec.
          ,@(map 'list (lambda (c p) (list c (format "&%s;" p) '(html-mode web-mode)))
                 "¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿƒΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩαβγδεζηθϑικλμνξοπϖρςστυϒφχψω•…′″‾⁄℘ℑℜ™ℵ←↑→↓↔↵⇐⇑⇑⇓⇔∀∂∃∅∇∈∉∋∏∑−∗√∝∞∠∧∨∩∪∫∴∼≅≈≠≡≤≥⊂⊃⊄⊆⊇⊕⊗⊥⋅⌈⌉⌊⌋〈〉◊♠♣♥♦\"&<>ŒœŠšŸ^~   ‌‍‎‏–—‘’‚“”„†‡‰‹›€"
                 '("iexcl" "cent" "pound" "curren" "yen" "brvbar" "sect" "uml"
                   "copy" "ordf" "laquo" "not" "shy" "reg" "macr" "deg" "plusmn"
                   "sup2" "sup3" "acute" "micro" "para" "middot" "cedil" "sup1"
                   "ordm" "raquo" "frac14" "frac12" "frac34" "iquest" "Agrave"
                   "Aacute" "Acirc" "Atilde" "Auml" "Aring" "AElig" "Ccedil"
                   "Egrave" "Eacute" "Ecirc" "Euml" "Igrave" "Iacute" "Icirc"
                   "Iuml" "ETH" "Ntilde" "Ograve" "Oacute" "Ocirc" "Otilde" "Ouml"
                   "times" "Oslash" "Ugrave" "Uacute" "Ucirc" "Uuml" "Yacute"
                   "THORN" "szlig" "agrave" "aacute" "acirc" "atilde" "auml"
                   "aring" "aelig" "ccedil" "egrave" "eacute" "ecirc" "euml"
                   "igrave" "iacute" "icirc" "iuml" "eth" "ntilde" "ograve"
                   "oacute" "ocirc" "otilde" "ouml" "divide" "oslash" "ugrave"
                   "uacute" "ucirc" "uuml" "yacute" "thorn" "yuml" "fnof" "Alpha"
                   "Beta" "Gamma" "Delta" "Epsilon" "Zeta" "Eta" "Theta" "Iota"
                   "Kappa" "Lambda" "Mu" "Nu" "Xi" "Omicron" "Pi" "Rho" "Sigma"
                   "Tau" "Upsilon" "Phi" "Chi" "Psi" "Omega" "alpha" "beta" "gamma"
                   "delta" "epsilon" "zeta" "eta" "theta" "thetasym" "iota" "kappa"
                   "lambda" "mu" "nu" "xi" "omicron" "pi" "piv" "rho" "sigmaf"
                   "sigma" "tau" "upsilon" "upsih" "phi" "chi" "psi" "omega" "bull"
                   "hellip" "prime" "Prime" "oline" "frasl" "weierp" "image" "real"
                   "trade" "alefsym" "larr" "uarr" "rarr" "darr" "harr" "crarr"
                   "lArr" "uArr" "rArr" "dArr" "hArr" "forall" "part"
                   "exist" "empty" "nabla" "isin" "notin" "ni" "prod" "sum" "minus"
                   "lowast" "radic" "prop" "infin" "ang" "and" "or" "cap" "cup"
                   "int" "there4" "sim" "cong" "asymp" "ne" "equiv" "le" "ge" "sub"
                   "sup" "nsub" "sube" "supe" "oplus" "otimes" "perp" "sdot"
                   "lceil" "rceil" "lfloor" "rfloor" "lang" "rang" "loz" "spades"
                   "clubs" "hearts" "diams" "quot" "amp" "lt" "gt" "OElig" "oelig"
                   "Scaron" "scaron" "Yuml" "circ" "tilde" "ensp" "emsp"
                   "thinsp" "zwnj" "zwj" "lrm" "rlm" "ndash" "mdash" "lsquo"
                   "rsquo" "sbquo" "ldquo" "rdquo" "bdquo" "dagger" "Dagger"
                   "permil" "lsaquo" "rsaquo" "euro")))))


(defun prettify-symbols--establish-list ()
  "`fin' is ((mode ((\"string\" . ?sym)
                    (\"string\" . ?sym)
                    ...))
             (mode ((\"x\" . ?x)
                    (\"y\" . ?y))))"
  (let* ((fin)
         (push-fin (function
                    (lambda (pair mode)
                      (condition-case exception ;try
                          (pushnew pair (cadr (assoc mode fin)))
                        ('error         ;catch exception
                         (push (list mode (list pair)) fin)
                         ))
                      ))))
    (mapc (lambda (pattern-line)
            (mapc
             (lambda (mode)
               (funcall push-fin (cons (nth 1 pattern-line) (nth 0 pattern-line)) mode))
             (nth 2 pattern-line)))  ;mode list
          prettify-symbols-patterns) ;main list ((s str (mode)) (s str (mode))...)
    fin)
  )

(defun prettify-symbols-initialize ()
  (interactive)
  (mapc (lambda (x)
          (prettify-symbols--def-init-function (car x) (cadr x)))
        (prettify-symbols--establish-list)))

(defun prettify-symbols--def-init-function (mode pair-list)
  (let ((function-sym (intern (format "prettify-symbols-init/%s" mode))))
    ;; == defun
    (defalias function-sym
      (function (lambda ()
		  (set 'prettify-symbols-alist pair-list))))
    (add-hook (intern (format "%s-hook" mode)) (symbol-function function-sym))
    ))

(prettify-symbols-initialize)

(provide 'rc-prettify-symbols)
