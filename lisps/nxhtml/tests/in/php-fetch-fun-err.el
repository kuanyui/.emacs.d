(setq x (byte-compile
(lambda
  (body)
  (make-local-variable 'parse-sexp-lookup-properties)
  (let
      ((sgml-xml-mode nil)
       (font-lock-extend-after-change-region-function 'c-extend-after-change-region)
       )))))
