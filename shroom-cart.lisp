(in-package :common-lisp-user)
(load "alist.lisp")
(load "classification-tree.lisp")
(load "dataset.lisp")
(defpackage :shroom-cart
  (:use :alist
        :common-lisp
        :classification-tree
        :dataset)
  (:export :tree-accuracy))
(in-package :shroom-cart)

(defun str-split (str chr)
  (loop for start = 0 then (1+ end)
        for end = (position chr str :start start)
        collect (subseq str start end)
        until (null end)))

(defmacro with-data-file (stream-name &rest body)
    `(with-open-file (,stream-name "./shrooms.csv" :direction :input)
       ,@body))

(defun data-from-file (&key num-lines)
  "Loads the data from the input file, retuning num-lines at a time"
  (with-data-file s
    (loop with shroom-types = (make-dataset)
          for i = 1 then (+ i 1)
          for line = (read-line s nil)
          while line
          until (and num-lines (eq num-lines i))
          do (vector-push-extend (str-split line #\,) shroom-types)
          finally (return shroom-types))))

(defun prediction-rating (result actual)
  (let* ((rating (alist-get actual result)))
    (or rating 0)))

(defun tree-accuracy (num-training-rows)
  (loop with dataset = (data-from-file)
        with training-set = (subseq dataset 0 num-training-rows)
        with checking-set = (subseq dataset num-training-rows)
        with tree = (let ((tree (make-classification-tree training-set)))
                      (prune-classification-tree tree 10)
                      tree)
        for row across checking-set
        for result = (classify-with-classification-tree row tree)
        for actual = (first row)
        sum (prediction-rating result actual) into rating-total
        finally (return (/ rating-total (length checking-set)))))
