(defpackage #:advanced-readtable
  (:use #:cl)
  (:shadow
   #:find-package
   #:find-symbol)
  (:export
   #:set-macro-symbol
   #:get-macro-symbol
   #:activate
   #:! #:find-package #:find-symbol
   #:package-finders
   #:symbol-finders
   #:*package-finders*
   #:*symbol-finders*
   #:*extra-finders*
   #:*advanced-readtable*
   #:*disable-symbol-readmacro*
   #:push-import-prefix
   #:push-local-nickname
   #:push-local-package
   #:set-handler))
