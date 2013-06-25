(in-package #:advanced-readtable)

;;;; Advanced-readtable
;;;; 
;;;; per-package aliases for packages
;;;; per-package shortcuts for package hierarchies
;;;; extendable find-package and find-symbol
;;;; local use package in form package:(here form where package used)
;;;; local intern package like in SBCL: package::(symbol1 symbol2) will intern
;;;;                                    package::symbol1 and package::symbol2

;;;
;;; Prepare readtables
;;;

(defvar *colon-readtable* (copy-readtable nil) 
  "Support readtable with colon as whitespace")
(set-syntax-from-char #\: #\Space *colon-readtable* *colon-readtable*)

;;;
;;; Readtable handlers
;;; 

(cl:defpackage #:advanced-readtable.junk)

(defvar *enable-symbol-readmacro* t
  "Enables processing of symbol-readmacro.")
(declaim (type boolean *enable-symbol-readmacro*))

(declaim (ftype macro-symbol-handler process-symbol-readmacro))
(defun process-symbol-readmacro (stream symbol)
  (let ((func (gethash symbol *symbol-readmacros*)))
    (if (and func *enable-symbol-readmacro*)
        (funcall func stream symbol) symbol)))

;; Internal special variable. Do not export
(defvar *car-list* nil "Boolean: iff reader in list and car is not read")

(defun collect-dots (stream)
  (do ((n 0 (1+ n)) 
       (c (read-char stream nil) (read-char stream nil)))
      ((or (null c) (char/= c #\.))
       (when c 
         (unread-char c stream))
       (if (and (plusp n) (member c '(nil #\Space #\) #\( #\Tab #\Newline #\:)))
         (intern (make-string n :initial-element #\.))
         (dotimes (foo n) (unread-char #\. stream))))))

(defun read-token (stream)
  "
DO: Reads from STREAM a symbol or number up to whitespace or colon
RETURN: symbols name or numbers value"
  (let ((*readtable* *colon-readtable*)
        (*package* (|CL|:find-package '#:advanced-readtable.junk)))
    (or (collect-dots stream)
        (read-preserving-whitespace stream nil))))

(defun count-colons (stream)
  "
DO: Reads colons from STREAM
RETURN: number of the colons"
  (do ((n 0 (1+ n)) 
       (c (read-char stream nil) (read-char stream nil))) 
      ((or (null c) (char/= c #\:)) 
       (when c (unread-char c stream)) n)))

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
                (warn "No such symbol ~S in package ~S. Interning..." 
                        (symbol-name token) package)
                (setf symbol (intern (symbol-name token) package)))))
        (unintern token)
        (when (and (= colons 1) (not (eq status :external))) 
          (cerror "Use anyway" 
                  "Symbol ~A not external" symbol))
        symbol))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun read-token-with-colons (stream char)
    "Reads token, then analize package part if needed"
    (unread-char char stream)
    (let* ((token (read-token stream))
           ;; We have read something. 
           ;; It may represent either symbol or package designator. 
           ;; Looking after it: do we have a colon?
           (colons (count-colons stream))
           (object (read-after-colon stream token colons)))
      
      (when (or (not (symbolp object)) 
                (eql char #\|))
        (return-from read-token-with-colons (and (not *read-suppress*) object)))
      
      (let ((object (process-symbol-readmacro stream object)))
        (when *car-list*
          (setf *car-list* nil
                *current-extra-finders* 
                (append (extra-finders object) *current-extra-finders*)))
        (and (not *read-suppress*) object))))
          
  (let ((default-open-paren-reader 
         (get-macro-character #\( (copy-readtable nil))))
    (defun open-paren-reader (stream char)
      (let ((*car-list* t) (*current-extra-finders* *current-extra-finders*))
        (funcall default-open-paren-reader stream char)))))

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
    (let ((*readtable* (copy-readtable nil)))
      (nconc
       (loop :for i :from 0 :to 127
          :for c = (code-char i)
          :when (to-process c) :collect c)
       (loop :for c :across +additional-chars+
          :when (to-process c) :collect c)))))

(macrolet ((def-advanced-readtable ()
             `(defreadtable :advanced
                (:merge :standard)
                ,@(mapcar (lambda (c) 
                            (list :macro-char c 
                                  '(function read-token-with-colons) t))
                          (chars-to-process))
                (:macro-char #\( #'open-paren-reader nil))))
  (def-advanced-readtable))

(defun activate ()
  (dolist (c (chars-to-process))
    (set-macro-character c #'read-token-with-colons t))
  (set-macro-character #\( #'open-paren-reader nil))

(defun ! () (in-readtable :advanced))
