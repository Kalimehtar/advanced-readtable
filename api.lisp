(in-package #:advanced-readtable)

;;;; This file contains main API

(defmacro %set-handler (handler-list key name &body handler-body)
  "Local macros for push-* functions. No gensyms intended."
  `(set-handler ,handler-list ,key
                (lambda (,name package)
                  (declare (ignore package)) . ,handler-body)))

(define-api push-import-prefix (prefix &optional (package *package*))
    ((or string symbol) &optional package-designator => package-handler)
  "Enables using package name omitting prefix.
For example, you have packages com.clearly-useful.iterator-protocol, com.clearly-useful.reducers, ...
You may use them as
 (push-import-prefix :com.clearly-useful)
 (iterator-protocol:do-iterator ...)
 (reducers:r/map #'1+ data)
and so on.
Package prefix is enabled per package so it is safe to use it in your package.

If there is package, which name coincides with shortcut, package name has priority.

So, if you make
 (defpackage :reducers ...)

after that reducers:... will refer to new package, not com.clearly-useful.reducers.
"
  (%set-handler (package-finders package) `(:prefix ,prefix) name
    (|CL|:find-package (concatenate 'string (string prefix) "." name))))

(defvar *global-nicknames* (make-hash-table :test 'equal) ; to find by string=
  "Package aliases. Hash nickname: string -> package")
(declaim (type hash-table *global-nicknames*))

(define-api push-local-nickname 
    (long-package nick 
                  &optional (current-package *package*))
    (package-designator package-designator 
                        &optional package-designator => package-handler)
  "Enables package nickname in CURRENT-PACKAGE.
For example, you found COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST package and want to use
it. But don't want to USE-PACKAGE them, because some exported symbols from it are clashing 
with yours.

You may do it right:
 (push-local-nickname :com.informatimago.common-lisp.cesarum.list :ilist)
 (ilist:circular-length l)

Local-nicknames are local, so you may use it freely.

If package A wants  package LIB version 1, and package B wants package 
LIB version 2, one can simply rename LIB version 1 to LIB1 and rename LIB 
version 2 to LIB2 and make
 (push-local-nickname :lib1 :lib :a)
 (push-local-nickname :lib2 :lib :b)

If enabled global-nicknames via enable-global-nicknames,
then also created alias in current package.

For example,
 (push-local-nickname :lib1 :lib :a), states, that package A.LIB is eq to LIB1.
"
  (let ((dpackage (find-package long-package))
        (s-nick (string nick)))
    (when *global-nicknames*
      (setf (gethash (format nil "~a.~a"
                             (package-name current-package) s-nick) 
                     *global-nicknames*)
            dpackage))
    (%set-handler (package-finders current-package) 
                  `(:nick ,(string long-package) ,s-nick) name
      (when (string= name s-nick) dpackage))))

(define-api push-local-package (symbol local-package)
    (symbol package-designator => symbol-handler)
  "Sets local-package for a symbol. Many macroses use there own clauses. 
For example, ITERATE uses FOR, COLLECT and so on. 
If you don't want to USE-PACKAGE iterate, this function will help.
 (push-local-package 'iter:iter :iterate)
 (iter:iter (for i from 1 to 10) (collect i))

Caution: this function enables package substitution in all cases, 
where SYMBOL is the car of a list.
For example, this will be error: 
 (let (iter:iter for) (list iter:iter for))
, because first for is in ITERATE package, but second -- is not.
"
  (let ((dpackage (find-package local-package)))
    (%set-handler (extra-finders symbol) `(:local ,symbol ,local-package) name
      (multiple-value-bind (symbol status) (|CL|:find-symbol name dpackage)
        (if (eq status :external) 
            (values symbol status)
            (values nil nil))))))