(in-package #:advanced-readtable)

;;; Advanced-readtable
;;; 
;;; per-package aliases for packages
;;; per-package shortcuts for package hierarchies
;;; extendable find-package and find-symbol
;;; local use pcakage in form package:(here form where package used)
;;; local intern package like in SBCL: package::(symbol1 symbol2) will intern
;;;                                    package::symbol1 and package::symbol2

(defvar *per-package-finders* (make-hash-table :test 'eq)
  "Hash package -> list of handlers")
(defvar *package-finders* nil
  "List of handlers (lambda (name package) ...) -> package")




;;;
;;; Prepare readtables
;;;



(defvar *advanced-readtable* (copy-readtable nil))
(defvar *colon-readtable* (copy-readtable nil) 
  "Support readtable with colon as whitespace")

;;;
;;; Readtable handlers
;;; 

(defpackage #:advanced-readtable.junk)



(defun try-funcall (handlers-list name package)
  (declare (type list handlers-list)
           (type string name)
           (type (or null package) package))
  (when handlers-list
    (or (funcall (car handlers-list) name package)
        (try-funcall (cdr handlers-list) name package))))

(defun find-package (name &optional (current-package *package*))
  (declare (type (or null package) current-package))
  (if (typep name 'package) name
      (let ((sname (string name)))
        (or 
         (when current-package
           (try-funcall (package-finders current-package) sname current-package))
         (try-funcall *package-finders* sname current-package)
         (cl:find-package name)))))

(defvar *package-symbol-finders* (make-hash-table :test 'eq)
  "Hash package -> list of handlers")
(defvar *symbol-finders* nil
  "List of handlers (lambda (name package) ...) -> symbol")
(defvar *extra-finders* (make-hash-table :test 'eq)
  "Hash symbol -> list of handlers (lambda (name package) ...) -> symbol
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
        (funcall (car handlers-list) name package)
      (if symbol 
          (values symbol status)
          (try-funcall (cdr handlers-list) name package)))))


(defun find-symbol (name &optional dpackage)
  (declare (type string name))
  (let ((package (find-package dpackage)))
    (macrolet ((mv-or (&rest clauses)
                 (if clauses
                     `(multiple-value-bind (symbol status) ,(car clauses)
                        (if symbol (values symbol status)
                            (mv-or . ,(cdr clauses))))
                     `(values nil nil))))
      
      (mv-or
       (try-mv-funcall *extra-symbol-finders* name package)
       (unless package (try-local-packages *local-packages* name))
       (when package (try-mv-funcall (symbol-finders package) name package))
       (try-mv-funcall *symbol-finders* name package)
       (if package 
           (cl:find-symbol name package)
           (cl:find-symbol name))))))

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
  (let ((c (read-char stream nil)))
    (if (eql c #\:) 
        (+ 1 (count-colons stream))
        (progn (unread-char c stream) 0))))

(defun read-after-colon (stream maybe-package colons)
  "Read symbol package:sym or list package:(...)"
  (when (= colons 0) 
    (return-from read-after-colon 
      (if (symbolp maybe-package)
          (let ((name (symbol-name maybe-package)))
            (or (find-symbol name)(intern name)))
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
      (multiple-value-bind (symbol status) 
          (find-symbol token package)
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
  (push (lambda (name package)
          (declare (ignore package))
          (or (cl:find-package name)
              (cl:find-package (concatenate 'string prefix "." name))))
        (package-finders package)))

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

Local-nickname shadows any package, which name is NICK, so if package A wants 
package LIB version 1, and package B wants package LIB version 2, one can simply 
rename LIB version 1 to LIB1 and make
 (push-local-nickname :lib1 :lib :a)
"
  (let ((dpackage (find-package long-package)))
    (push (lambda (name package)
            (declare (ignore package))
            (when (string= name (string nick)) dpackage))
        (package-finders current-package))))

(defun push-local-package (symbol local-package)
  "Sets local-package for a symbol. Many macroses use the own clauses. 
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
    (push (lambda (name package)
            (declare (ignore package))
            (multiple-value-bind (symbol status) (cl:find-symbol name dpackage)
              (when (eq status :external) symbol)))
        (extra-finders symbol))))

;;;
;;; Readtable analysis and change
;;;

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
                                  (read-from-string (format nil "#:~A'" c))))))



(defun macro-char-p (c)
  "If C is macro-char, return GET-MACRO-CHARACTER"
  #+allegro (unless 
                (eql (get-macro-character c) #'excl::read-token)
              (get-macro-character c))
  #-allegro (get-macro-character c))

(defun fill-char-table ()
  "Returns simple-vector with character syntax classes"
  (let ((*readtable* (copy-readtable nil))
        (char-table (make-array 127)))
    (dotimes (i (length char-table))
      (let ((c (code-char i)))
        (setf 
         (svref char-table i)
         (cond
           ((eql c #\:) :colon)
           ((macro-char-p c) :macro)
           ((does-not-terminate-token-p c) :does-not-terminate-token)
           ((whitespace-p c) :whitespace)
           ((multiple-escape-p c) :multiple-escape)
           ((single-escape-p c) :single-escape)))))
    char-table))

(let (initialized)
  (defun activate (&optional force)
    "Inits *advanced-readtable* and *colon-readtable*."
    (when (or force (not initialized))
      (setq initialized t)
      (let ((char-table (fill-char-table)))
        (dotimes (i (length char-table))
          (let ((b (svref char-table i))
                (c (code-char i)))
            (unless (char= #\# c)
              (when (member b '(:does-not-terminate-token 
                                :multiple-escape :single-escape))
                ;; will make it non-terminating macro character
                ;;    = potentially beginning of the package-name
                (set-macro-character c #'read-token-with-colons 
                                     t *advanced-readtable*))))))
  
      (set-syntax-from-char #\: #\Space *colon-readtable* *colon-readtable*)
      (set-macro-character #\( #'open-paren-reader))
    (setf *readtable* *advanced-readtable*)))

(defun ! () (activate))
