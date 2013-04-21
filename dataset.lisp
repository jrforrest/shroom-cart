(in-package :common-lisp-user)
(defpackage :dataset
  (:use :common-lisp)
  (:export :make-dataset))
(in-package :dataset)
(defun make-dataset ()
  (make-array 0 :element-type 'list :fill-pointer 0 :adjustable t))
