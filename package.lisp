(defpackage #:advanced-readtable
  (:use #:cl #:named-readtables)
  (:import-from #:named-readtables #:define-api #:=>)
  (:shadow
   #:find-package
   #:find-symbol
   #:in-package
   #:defpackage)
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
   #:*enable-symbol-readmacro*
   #:push-import-prefix
   #:push-local-nickname
   #:push-local-package
   #:set-handler
   #:read-token-with-colons))

(pushnew :advanced-readtable *features*)