(defpackage #:bqncards
  (:use #:cl))
(in-package #:bqncards)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:bqncards.templates
    (:use #:cl #:ten #:bqncards)))

(defun parse-file (file)
  (with-open-file (s file :direction :input)
    (cmark:parse-stream s)))

;; From the docs
(defun print-node (node &optional (level 0))
  "Recursively print each node and its children at progressively deeper
  levels"
  (format t "~&~A~A"
          (make-string (* 2 level) :initial-element #\Space)
          (class-name (class-of node)))
  (dolist (child (cmark::node-children node))
    (print-node child (1+ level))))

(defun write-operator-page (md target)
  (with-open-file (f target :direction :output :if-exists :overwrite)
    (write-string (bqncards.templates:operator-page md) f)))

#+nil
(write-operator-page (parse-file #p"doc/group.md") #p"dist/op/group.html")
#+nil
(print-node (parse-file #p"doc/group.md"))
#+nil
(inspect (parse-file #p"doc/group.md"))
#+nil
(ten:compile-template #p"src/template.html" :bqncards.templates)
