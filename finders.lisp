(in-package #:advanced-readtable)

;;;; This file contains my version of FIND-PACKAGE and FIND-SYMBOL
;;;; and settings for them: package-finders, symbol-finders, extra-finders

(deftype package-handler ()
  "Type of handlers for package finding.
First arg is a name to find. Second -- current package"
  `(function (string package) package))

(deftype symbol-handler ()
  "Type of handlers for symbol finding
First arg is a name to find. Second -- current package"
  `(function (string package) symbol))

(deftype package-designator ()
  "Argument of CL:FIND-PACKAGE"
  `(or string symbol character package))

(deftype macro-symbol-handler ()
  "Handler for SET/GET-MACRO-SYMBOL"
  `(function (stream symbol) t))

(defvar *local-package-finders* (make-hash-table :test 'eq)
  "Hash package -> alist (key . package-handler).
Contains per-package handlers for FIND-PACKAGE.")
(declaim (type hash-table *per-package-finders*))

(defvar *package-finders* nil 
  "Alist (equal key . package-handler).
Contains global handlers for FIND-PACKAGE.")
(declaim (type list *package-finders*))

(defvar *local-symbol-finders* (make-hash-table :test 'eq)
  "Hash package -> alist (equal key . symbol-handler).
Contains per-package handlers for FIND-SYMBOL.")
(declaim (type hash-table *local-symbol-finders*))

(defvar *symbol-finders* nil 
  "Alist (equal key . symbol-handler).
Contains global handlers for FIND-SYMBOL.")
(declaim (type list *symbol-finders*))

(defvar *extra-finders* (make-hash-table :test 'eq)
  "Hash symbol -> alist (key . symbol-handler)
Used for PUSH-LOCAL-PACKAGE. It will be searched before CL:FIND-SYMBOL.
Will be checked inside list, where car of list eq SYMBOL.")
(declaim (type hash-table *extra-finders*))

(defvar *symbol-readmacros* (make-hash-table :test 'eq)
  "Hash symbol -> macro-symbol-handler
Contans handlers for macro-symbols.")
(declaim (type hash-table *symbol-readmacros*))

(define-api set-macro-symbol (symbol func)
    (symbol macro-symbol-handler => macro-symbol-handler)
  "Syntax is like set-macro-character, 
except that FUNC is binded to SYMBOL, not character"
  (setf (gethash symbol *symbol-readmacros*) func))

(define-api get-macro-symbol (symbol)
    (symbol => macro-symbol-handler)
  "Syntax is like get-macro-character.
Returns function, assigned by set-macro-symbol"
  (let ((func (gethash symbol *symbol-readmacros*)))
    func))

(macrolet ((define-accessor (name var doc)
             `(progn 
                (define-api ,name (&optional (package *package*))
                    (&optional package => list)
                  ,(format nil "Returns ~a" doc)
                  (values (gethash (find-package package) ,var)))
                (define-api (setf ,name) (value &optional (package *package*))
                    (list &optional package => list)
                  ,(format nil "Sets ~a" doc)
                  (setf (gethash (find-package package) ,var) value)))))
  (define-accessor package-finders *local-package-finders* 
    "alist (key . package-handler) for package")
  (define-accessor symbol-finders *local-symbol-finders*
    "alist (key . symbol-handler) for package"))

(define-api (setf extra-finders) (value symbol)
    (list symbol => list)
  "Sets alist (key . symbol-handler) for symbol"
  (setf (gethash symbol *extra-finders*) value))

(define-api extra-finders (symbol)
    (symbol => list)
  "Returns alist (key . symbol-handler) for symbol"
  (values (gethash symbol *extra-finders*)))

(defmacro set-handler (handler-list key function)
  "This is middle-level public API for changing handlers for
find-symbol and find-package. There are five lists:
  *package-finders* -- global for find-package
  *symbol-finders* -- global for find-symbol
  (package-finders package) -- per-package for find-package
  (symbol-finders package) -- per-package for find-symbol
  (extra-finders symbol) -- per-symbol for (symbol ....) package substitution

Key should be uniq in the sense of EQUAL in the list. SET-HANDLER adds
new handler if it is not already there.
" 
  `(let ((key ,key) (function ,function))
     (let ((found (assoc key ,handler-list :test #'equal)))
       (if found (cdr found)
           (prog1
               function
             (push (cons key function)
                   ,handler-list))))))

(declaim (ftype (function (list string (or null package)) (or null package))
                funcall-first))
(defun funcall-first (handlers-list name package)
  "HANDLERS-LIST -- alist (key . package-handler).
The function calls handlers until one of them returns not null.
Then the result of last call is returned"
  (when handlers-list
    (or (funcall (cdar handlers-list) name package)
        (funcall-first (cdr handlers-list) name package))))

(define-api find-package (name &optional (current-package *package*))
    (package-designator &optional package-designator => (or null package))
  "We try to find package.
1. By full name with CL:FIND-PACKAGE.
2. By per-package handlers. Here we wil try local-nicknames and so on.
3. By global handlers. Here we may use, for example, hierarchical packages."
  (when (packagep name)
    (return-from find-package name))
  (setf current-package (find-package current-package))
  (let ((sname (string name)))
    (or
     (cl:find-package name)
     (funcall-first (package-finders current-package) 
                    sname current-package)
     (funcall-first *package-finders* 
                    sname current-package))))

;;; Internal special variables. Do not export

(defvar *current-extra-finders* nil 
  "Alist (key . symbol-handler). Used in PUSH-LOCAL-PACKAGE processing")
(declaim (type list *extra-symbol-finders*))

(defvar *local-packages* nil "List of packages: for pack:( ... pack2:(...))")
(declaim (type list *local-packages*))

(declaim (ftype (function (list string (or null package)) 
                          (values symbol symbol))
                funcall-first-mv))
(defun funcall-first-mv (handlers-list name package)
  "HANDLERS-LIST -- alist (key . package-handler).
The function calls handlers until one of them returns not null.
Then the result of last call is returned"
  (if handlers-list
    (multiple-value-bind (symbol status)
        (funcall (cdar handlers-list) name package)
      (if status
          (values symbol status)
          (funcall-first-mv (cdr handlers-list) name package)))
    (values nil nil)))

(declaim (ftype (function (list string) 
                          (values symbol symbol))
                find-local-packages))
(defun find-local-packages (packages name)
  (if packages
    (multiple-value-bind (symbol status) (cl:find-symbol name (car packages))
      (if symbol 
          (values symbol status)
          (find-local-packages (cdr packages) name)))
    (values nil nil)))

(define-api find-symbol (name &optional dpackage)
    (string &optional (or null package-designator) 
            => symbol (member :inherited :internal :external nil))
  "We try to find symbol
1. In package set with car of list, for example, PUSH-LOCAL-PACKAGE
2. By CL-FIND-SYMBOL, when package explicitly given
3. By packages added with package:(...)
4. By per-package finders
5. By global finders
6. By CL-FIND-SYMBOL"
  (let ((package (if dpackage (find-package dpackage) *package*)))
    (macrolet ((mv-or (&rest clauses)
                 (if clauses
                     `(multiple-value-bind (symbol status) ,(car clauses)
                        (if status (values symbol status)
                            (mv-or . ,(cdr clauses))))
                     `(values nil nil))))
      (mv-or
       (funcall-first-mv *current-extra-finders* name package)
       (when dpackage (cl:find-symbol name package))
       (unless dpackage (find-local-packages *local-packages* name))
       (funcall-first-mv (symbol-finders package) name package)
       (funcall-first-mv *symbol-finders* name package)
       (unless dpackage (cl:find-symbol name package))))))