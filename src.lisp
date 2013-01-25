(in-package #:advanced-readtable)

;;;; Advanced-readtable
;;;; 
;;;; per-package aliases for packages
;;;; per-package shortcuts for package hierarchies
;;;; extendable find-package and find-symbol
;;;; local use package in form package:(here form where package used)
;;;; local intern package like in SBCL: package::(symbol1 symbol2) will intern
;;;;                                    package::symbol1 and package::symbol2

(defvar *per-package-finders* (make-hash-table :test 'eq)
  "Hash package -> list of handlers. Each handler is a cons (key . function)
function = (lambda (name package) ...) -> package")

(defvar *package-finders* nil
  "List of handlers. Each handler is a cons (key . function) 
function = (lambda (name package) ...) -> package")

(defvar *global-nicknames* nil
  "Placeholder for global nicknames, when not null, it is an alias hash")

;;;
;;; Prepare readtables
;;;

(defvar *colon-readtable* (copy-readtable nil) 
  "Support readtable with colon as whitespace")
(set-syntax-from-char #\: #\Space *colon-readtable* *colon-readtable*)

;;;
;;; Readtable handlers
;;; 

(defpackage #:advanced-readtable.junk)

(defun try-funcall (handlers-list name package)
  (declare (type list handlers-list)
           (type string name)
           (type (or null package) package))
  (when handlers-list
    (or (funcall (cdr (car handlers-list)) name package)
        (try-funcall (cdr handlers-list) name package))))

(defun find-package (name &optional (current-package *package*))
  "We try to find package.
1. By full name with CL:FIND-PACKAGE.
2. By per-package handlers. Here we wil try local-nicknames and so on.
3. By global handlers. Here we may use, for example, hierarchical packages."
  (declare (type package current-package))
  (if (typep name 'package) name
      (let ((sname (string name)))
        (or
         (cl:find-package name)
         (try-funcall (package-finders current-package) 
                      sname current-package)
         (try-funcall *package-finders* sname current-package)))))

(defvar *package-symbol-finders* (make-hash-table :test 'eq)
  "Hash package -> list of handlers. Each handler is a cons (key . function)
function =  (lambda (name package) ...) -> symbol")

(defvar *symbol-finders* nil
  "List of handlers. Each handler is a cons (key . function) 
function =  (lambda (name package) ...) -> symbol")

(defvar *extra-finders* (make-hash-table :test 'eq)
  "Hash symbol -> list of handlers. Each handler is a cons (key . function) 
function = (lambda (name package) ...) -> symbol
These will be used before CL:FIND-SYMBOL")

(defvar *symbol-readmacros* (make-hash-table :test 'eq))

(defvar *disable-symbol-readmacro* nil 
  "Disables processing of symbol-readmacro.")

(defun def-symbol-readmacro (symbol func)
  (setf (gethash symbol *symbol-readmacros*) func))

(defun set-macro-symbol (symbol func)
  "Syntax is like set-macro-character, 
except that FUNC is binded to SYMBOL, not character"
  (setf (gethash symbol *symbol-readmacros*) func))

(defun get-macro-symbol (symbol)
  "Syntax is like get-macro-character.
Returns function, assigned by set-macro-symbol"
  (gethash symbol *symbol-readmacros*))

(defun process-symbol-readmacro (symbol stream)
  (let ((func (gethash symbol *symbol-readmacros*)))
    (if func (funcall func stream symbol) symbol)))

;;; Internal special variables. Do not export

(defvar *extra-symbol-finders* nil 
  "List of handlers: handlers for symbol, car of list")
(defvar *car-list* nil "Boolean: iff reader in list and car is not read")
(defvar *local-packages* nil "List of packages: for pack:( ... pack2:(...))")

(defun try-local-packages (packages name)
  (when packages
    (multiple-value-bind (symbol status) (cl:find-symbol name (car packages))
      (if symbol 
          (values symbol status)
          (try-local-packages (cdr packages) name)))))

(defun try-mv-funcall (handlers-list name package)
  "Returns symbol, status"
  (declare (type list handlers-list)
           (type string name)
           (type (or null package) package))
  (when handlers-list
    (multiple-value-bind (symbol status)
        (funcall (cdr (car handlers-list)) name package)
      (if symbol 
          (values symbol status)
          (try-funcall (cdr handlers-list) name package)))))

(defun find-symbol (name &optional dpackage)
  "We try to find symbol
1. In package set with car of list, for example, PUSH-LOCAL-PACKAGE
2. By CL-FIND-SYMBOL, when package explicitly given
3. By packages added with package:(...)
4. By per-package finders
5. By global finders
6. By CL-FIND-SYMBOL"
  (declare (type string name))
;  (when (string= name "NIL")
;    (return-from find-symbol (cl:find-symbol name (or dpackage *package*))))
  (let ((package (if dpackage (find-package dpackage) *package*)))
    (macrolet ((mv-or (&rest clauses)
                 (if clauses
                     `(multiple-value-bind (symbol status) ,(car clauses)
                        (if status (values symbol status)
                            (mv-or . ,(cdr clauses))))
                     `(values nil nil))))      
      (mv-or
       (try-mv-funcall *extra-symbol-finders* name package)
       (when dpackage (cl:find-symbol name package))
       (unless dpackage (try-local-packages *local-packages* name))
       (try-mv-funcall (symbol-finders package) name package)
       (try-mv-funcall *symbol-finders* name package)
       (unless dpackage (cl:find-symbol name package))))))

(defun read-token (stream)
  "
DO: Reads from STREAM a symbol or number up to whitespace or colon
RETURN: symbols name or numbers value"
  (let ((*readtable* *colon-readtable*)
        (*package* (cl:find-package '#:advanced-readtable.junk)))
    (read-preserving-whitespace stream nil)))

(defun count-colons (stream)
  "
DO: Reads colons from STREAM
RETURN: number of the colons"
  (do ((n 0 (1+ n)) 
       (c (read-char stream nil) (read-char stream nil))) 
      ((char/= c #\:) (unread-char c stream) n)))

(defun read-after-colon (stream maybe-package colons)
  "Read symbol package:sym or list package:(...)"
  (declare (type stream stream)
           (type integer colons))
  (check-type colons (integer 0 2))
  (when (= colons 0) ; no colon: this is a symbol or an atom
    (return-from read-after-colon 
      (if (symbolp maybe-package)
          (prog1
              (let ((name (symbol-name maybe-package)))
                (or (find-symbol name) (intern name)))
            (unintern maybe-package))
          maybe-package)))

  (let ((package (find-package maybe-package)))
    (assert package (package) "No package ~a" maybe-package)
    (unintern maybe-package)
    (when (eql (peek-char t stream) #\()
      ;; package:(...) or package::(...)
      (ecase colons
        (1 (let ((*local-packages* (cons package *local-packages*)))
             (return-from read-after-colon 
               (read stream nil))))
        (2 (let ((*package* package))
             (return-from read-after-colon 
               (read stream nil))))))

    (let ((token (read-token stream)))
      (check-type token symbol)
      (multiple-value-bind (symbol status) 
          (find-symbol (symbol-name token) package)
        (unless status
          (if (= colons 1) (error "No external symbol ~S in ~S" 
                                  (symbol-name token) package)
              (progn
                (cerror "Intern ~S in ~S" "No such symbol ~S in package ~S" 
                        (symbol-name token) package)
                (setf symbol (intern (symbol-name token) package)))))
        (unintern token)
        (when (and (= colons 1) (not (eq status :external))) 
          (cerror "Use anyway" 
                  "Symbol ~A not external" symbol))
        symbol))))

(defun read-token-with-colons (stream char)
  "Reads token, then analize package part if needed"
  (unread-char char stream)
  (when *read-suppress* 
    (let ((*readtable* (copy-readtable nil)))
      (read stream))
    (return-from read-token-with-colons))
  (let* ((token (read-token stream))
         ;; We have read something. 
         ;; It may represent either symbol or package designator. 
         ;; Looking after it: do we have a colon?
         (colons (count-colons stream))
         (object (read-after-colon stream token colons)))
    
    (when (or *disable-symbol-readmacro* 
              (not (symbolp object)) 
              (eql char #\|))
        (return-from read-token-with-colons object))
        
    (let ((object (process-symbol-readmacro object stream)))
      (when *car-list*
        (setf *car-list* nil
              *extra-symbol-finders* 
              (append (extra-finders object) *extra-symbol-finders*)))
      object)))
          
(let ((default-open-paren-reader 
       (get-macro-character #\( (copy-readtable nil))))
  (defun open-paren-reader (stream char)
    (let ((*car-list* t) (*extra-symbol-finders* *extra-symbol-finders*))
      (funcall default-open-paren-reader stream char))))

(defun (setf package-finders) (value &optional (package *package*))
  (setf (gethash (find-package package) *per-package-finders*) value))

(defun package-finders (&optional (package *package*))
  (gethash (find-package package) *per-package-finders*))

(defun (setf symbol-finders) (value &optional (package *package*))
  (setf (gethash (find-package package) *package-symbol-finders*) value))

(defun symbol-finders (&optional (package *package*))
  (gethash (find-package package) *package-symbol-finders*))

(defun (setf extra-finders) (value symbol)
  (setf (gethash symbol *extra-finders*) value))

(defun extra-finders (symbol)
  (gethash symbol *extra-finders*))

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
  (let ((key-var (gensym "key")))
    `(let ((,key-var ,key))
       (unless (assoc ,key-var ,handler-list :test #'equal)
         (push (cons ,key-var ,function)
               ,handler-list)))))
                      
(defmacro %set-handler (handler-list key name &body handler-body)
  "Local macros for push-* functions. No gensyms intended."
  `(set-handler ,handler-list ,key
                (lambda (,name package)
                  (declare (ignore package)) . ,handler-body)))

(defun push-import-prefix (prefix &optional (package *package*))
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
    (cl:find-package (concatenate 'string (string prefix) "." name))))

(defun push-local-nickname (long-package nick 
                            &optional (current-package *package*))
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
    (%set-handler (package-finders current-package) 
                  `(:nick ,(string long-package) ,s-nick) name
      (when (string= name s-nick) dpackage))
    (when *global-nicknames*
      (setf (gethash (concatenate 'string
                                  (package-name current-package)
                                  "." s-nick) *global-nicknames*)
            dpackage))))

(defun push-local-package (symbol local-package)
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
      (multiple-value-bind (symbol status) (cl:find-symbol name dpackage)
        (when (eq status :external) symbol)))))

(flet ((parent (name)
         (let ((pos (position #\. name :from-end t)))
           (if pos (subseq name 0 pos) "")))
       (relative-to (parent name)
         (if (string= parent "") name
             (concatenate 'string parent "." name))))
  (defun enable-hierarchy-packages ()
    (set-handler *package-finders* :hierarchy
                 (lambda (name package)
                   (when (char= (char name 0) #\.)
                     (do ((i 1 (1+ i))
                          (p (package-name package) (parent p)))
                         ((char/= (char name i) #\.) 
                          (relative-to p (subseq name i)))))))))

(defun enable-global-nicknames ()
  (setf *global-nicknames* (make-hash-table :test 'equal))
  (%set-handler *package-finders* :global-nicknames name
    (gethash name *global-nicknames*)))

(enable-hierarchy-packages)
(enable-global-nicknames)

;;;
;;; Readtable analysis and change
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-case (case &body body)
    (let ((save (gensym)))
      `(let ((,save (readtable-case *readtable*)))
         (setf (readtable-case *readtable*) ,case)
         (unwind-protect
              (progn ,@body)
           (setf (readtable-case *readtable*) ,save)))))

  (defun does-not-terminate-token-p (c) 
    (ignore-errors
      (let ((str (format nil "a~Ab" c)))
        (string= str (symbol-name 
                      (with-case :preserve 
                        (read-from-string (format nil "#:~A" str))))))))

  (defun whitespace-p (c)
    (ignore-errors 
      (= 2 (length (read-from-string (format nil "(#:a~A#:b)" c))))))

  (defun multiple-escape-p (c)
    (ignore-errors 
      (string= "qQ" (symbol-name
                     (with-case :upcase
                       (read-from-string (format nil "#:~AqQ~A" c c)))))))

  (defun single-escape-p (c)
    (ignore-errors 
      (string= (symbol-name '#:\ ) (symbol-name
                                    (read-from-string 
                                     (format nil "#:~A'" c))))))

  (defun macro-char-p (c)
    "If C is macro-char, return GET-MACRO-CHARACTER"
    #+allegro (unless 
                  (eql (get-macro-character c) #'excl::read-token)
                (get-macro-character c))
    #-allegro (get-macro-character c))

  (defun to-process (c)
    (cond
      ((eql c #\:) nil)
      ((macro-char-p c) nil)
      ((does-not-terminate-token-p c) t)
      ((whitespace-p c) nil)
      ((multiple-escape-p c) t)
      ((single-escape-p c) t)
      (t nil)))
  
  (defparameter +additional-chars+ ""
    "Fill this, if you need extra characters for packages to begin with")

  (defun chars-to-process ()
    (nconc
     (loop :for i :from 1 :to 127
        :for c = (code-char i)
        :when (to-process c) :collect c)
     (loop :for c :across +additional-chars+
        :collect c))))

(macrolet ((def-advanced-readtable ()
             (when (cl:find-package "NAMED-READTABLES")
               `(named-readtables:defreadtable :advanced
                  (:merge :standard)
                  ,@(dolist (c (chars-to-process))
                            `(:macro-char ,c #'read-token-with-colons t))
                  (:macro-char #\( #'open-paren-reader nil)))))
  (def-advanced-readtable))

(defun activate ()
  (dolist (c (chars-to-process))
    (set-macro-character c #'read-token-with-colons t))
  (set-macro-character c #'read-token-with-colons t))

(defun ! () (activate))
