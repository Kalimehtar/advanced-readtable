(in-package #:advanced-readtable)

;;;; This file contains hierarchy packages implementation

;;; process nicknames in hierarchy
;;; ex: cl-user.test == common-lisp-user.test
;;; cl-user.test.a == common-lisp-user.test.a

(defun normalize-package (name)
  "Returns nil if already normalized.
Replace first section of hierarchy with proper name"
  (let ((pos (position #\. name)))
    (when pos
      (if (= pos 0)  ; .subpackage
          (concatenate 'string (package-name *package*) name)
          (let* ((base (subseq name 0 pos))
                 (p (find-package base)))
            (when (and p (string/= (package-name p) base))
              (concatenate 'string (package-name p) "." 
                           (subseq name (1+ pos)))))))))

(flet ((parent (name)
         (let ((pos (position #\. name :from-end t)))
           (if pos (subseq name 0 pos) "")))
       (relative-to (parent name)
         (cond 
           ((string= parent "") name)
           ((string= name "") parent)
           (t (concatenate 'string parent "." name)))))
  (defun hierarchy-find-package (name package)
    (when (string= name "")
      (return-from hierarchy-find-package package))
    (if (char= (char name 0) #\.)
      (do ((i 1 (1+ i))
           (p (package-name package) (parent p)))
          ((or (= i (length name)) (char/= (char name i) #\.))
           (find-package (relative-to p (subseq name i)))))
      (let ((normalized (normalize-package name)))
        (when normalized
          (find-package normalized package))))))

(defun correct-package (designator)
  (let ((p (find-package designator)))
    (if p (package-name p) designator)))

(defmacro in-package (designator)
  `(|CL|:in-package ,(correct-package (string designator))))

(defmacro defpackage (package &rest options)
  (let ((normalized (normalize-package (string package)))
        (options 
         (mapcar (lambda (option)
                   (cons (car option)
                         (case (car option)
                           (:use (mapcar #'correct-package (cdr option)))
                           ((:import-from :shadowing-import-from)
                            (cons (correct-package (second option))
                                  (cddr option)))
                           (t (cdr option)))))
                 options)))
    `(|CL|:defpackage ,(or normalized package) . ,options)))

(defun substitute-symbol (stream symbol)
  (declare (ignore stream))
  (find-symbol (symbol-name symbol) #.*package*))

(set-handler *package-finders* :hierarchy #'hierarchy-find-package)
(set-macro-symbol '|CL|:in-package #'substitute-symbol)
(set-macro-symbol '|CL|:defpackage #'substitute-symbol)
(set-macro-symbol '|CL|:find-package #'substitute-symbol)
(set-macro-symbol '|CL|:find-symbol #'substitute-symbol)

(%set-handler *package-finders* :global-nicknames name
  (gethash name *global-nicknames*))

