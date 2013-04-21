(in-package :common-lisp-user)
(defpackage :alist
  (:use :common-lisp)
  (:export
    :make-alist
    :alist-update
    :alist-set
    :alist-get))
(in-package :alist)

(defun make-alist (&rest tokens)
  (if (oddp (length tokens)) 
    (error "Odd number of key-value pairs given"))
  (loop with alist = (list)
        for (key value) on tokens by #'cddr
        when alist do (push (cons key value) alist)
        when (not alist) do (setf alist (list (cons key value)))
        finally (return alist)))

(defun alist-update (fun key alist &key)
  (let ((found nil))
    (dolist (pair alist)
      (if (equal (car pair) key)
        (progn
          (setf (cdr pair) (funcall fun (cdr pair)))
          (return-from alist-update (cdr pair)))))))

(defun alist-set (key value alist)
  (setf (cdr (last alist)) (cons (cons key value) nil)))

(defmacro alist-each (assignments alist &rest body)
  (unless (equal (length assignments) 2)
    (error "ASSIGNMENTS (ARG 1) should contain a KEY assignment
           and a VALUE assignment"))
  (let ((keysym (first assignments))
        (valuesym (second assignments))
        (pairsym (gensym)))
    `(loop for ,pairsym in ,alist
           for ,keysym = (car ,pairsym)
           for ,valuesym = (cdr ,pairsym)
           do (progn ,@body))))

(defun alist-get (lookup-key alist)
  (alist-each (key value) alist
    (if (equal lookup-key key) (return-from alist-get value))))

