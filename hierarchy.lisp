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
           ((string= parent "") nil)
           ((string= name "") parent)
           (t (concatenate 'string parent "." name)))))
  (defun hierarchy-find-package (name package)
    (when (string= name "")
      (return-from hierarchy-find-package nil))
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

(defun process-local-nicknames (package pairs)
  (let (res)
    (dolist (pair pairs (nreverse res))
      (destructuring-bind (sym orig) pair
        (push (list 'push-local-nickname orig sym package) res)))))

(defmacro defpackage (package &rest options)
  (let (post-commands new-options
                      (normalized (or (normalize-package (string package))
                                      package)))
    (dolist (option options)
       (push (cons (car option) 
                   (case (car option)
                     (:use (mapcar #'correct-package (cdr option)))
                     ((:import-from :shadowing-import-from)
                      (cons (correct-package (second option)) (cddr option)))
                     (:local-nicknames
                      (progn 
                        (mapcar 
                         (lambda (command)
                           (push command post-commands))
                         (process-local-nicknames normalized (cdr option)))
                        (go next)))
                     (t (cdr option))))
             new-options)
       next)
    `(prog1
         (|CL|:defpackage ,normalized . ,(nreverse new-options))
       ,@post-commands)))

(defun substitute-symbol (stream symbol)
  (declare (ignore stream))
  (find-symbol (symbol-name symbol) #.*package*))

(set-handler *package-finders* :hierarchy #'hierarchy-find-package)

(defun activate-cl-substitutes ()
  (set-macro-symbol '|CL|:in-package #'substitute-symbol)
  (set-macro-symbol '|CL|:defpackage #'substitute-symbol)
  (set-macro-symbol '|CL|:find-package #'substitute-symbol)
  (set-macro-symbol '|CL|:find-symbol #'substitute-symbol))

(defun !! ()
  (!) (activate-cl-substitutes))

(%set-handler *package-finders* :global-nicknames name
  (gethash name *global-nicknames*))

