(named-readtables:in-readtable :advanced)

(5am:in-suite* :advanced-readtable)

(defpackage a (:use cl))
(in-package a)
(5am:test hierarchy1
  (5am:is (string= (package-name *package*) "A")))

(defpackage .b (:use cl))
(in-package .b)
(5am:test hierarchy2
  (5am:is (string= (package-name *package*) "A.B")))

(in-package ..)
(5am:test hierarchy3
  (5am:is (string= (package-name *package*) "A")))

(defun foo () 1)

(in-package a.b)

(defun foo () 2)

(5am:test hierarchy4
  (5am:is (= (+ (foo) (..::foo) 3)))
  (5am:is (= (+ (foo) (..b::foo) 4)))
  (5am:is (eq 'foo '..b::foo)))
;  (5am:is (eq 'foo '........b::foo)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (advanced-readtable:push-import-prefix :a))

(5am:test import-prefix
  (5am:is (eq 'foo 'b::foo)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (advanced-readtable:push-local-nickname :cl :alias))

(5am:test local-nickname
  (5am:is (eq 'car 'alias:car)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (advanced-readtable:push-local-nickname :cl :alias))
