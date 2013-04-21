(in-package :common-lisp-user)
(load "alist.lisp")
(load "dataset.lisp")
(defpackage :classification-tree
  (:use :alist
        :common-lisp
        :dataset)
  (:export make-classification-tree
           print-classification-tree
           prune-classification-tree
           classify-with-classification-tree))
(in-package :classification-tree)

(defun incr-key-in-result-set (key result-set)
  (loop for pair in result-set
        do (if (equal (car pair) key) 
             (progn 
               (setf (cdr pair) (+ (cdr pair) 1))
               (return t)))))

(defmacro each-in-assoc-list (lst keysym valuesym &rest body)
  `(loop for pair in ,lst
         for ,keysym = (car pair)
         for ,valuesym = (cdr pair)
         do (progn ,@body)))

(defun classification-counts (dataset)
  (loop with results = (list)
        for row across dataset
        for classification = (car row)
        do (if (not (incr-key-in-result-set classification results))
             (progn (push (cons classification 1) results)))
        finally (return results)))

(defun merged-child-data (node)
  (let* ((true-data (alist-get :true-set (alist-get :split node)))
         (false-data (alist-get :false-set (alist-get :split node)))
         (merged-set (concatenate 'array true-data false-data)))
    merged-set))

(defun merged-impurity-difference (node)
  (let* ((true-data (alist-get :true-set (alist-get :split node)))
         (false-data (alist-get :false-set (alist-get :split node)))
         (merged-set (merged-child-data node))
         (merged-impurity (gini-impurity merged-set))
         (current-impurity (/ (+ (gini-impurity true-data)
                                 (gini-impurity false-data)) 2))
         (delta (- current-impurity merged-impurity)))
    delta))

(defun classification-probabilities (dataset)
  (let* ((class-counts (classification-counts dataset))
         (num-rows (length dataset))
         (probabilities (list)))
    (each-in-assoc-list class-counts key value
      (push (cons key (/ value num-rows)) probabilities))
    probabilities))

(defun gini-impurity (dataset)
  (loop with impurity = 0
        with probabilities = (classification-probabilities dataset)
        for pair in probabilities
        for classification = (car pair)
        for probability = (cdr pair)
        do (loop for inner_pair in probabilities
                 for inner_classification = (car inner_pair)
                 for inner_probability = (cdr inner_pair)
                 when (not (equal inner_classification classification))
                 do (setf impurity 
                          (+ impurity (* probability inner_probability))))
        finally (return impurity)))

(defun make-split (dataset column value)
  (let* ((split (make-alist :column column :value value))
         (result-sets (split-dataset dataset split)))
    (alist-set :true-set (car result-sets) split)
    (alist-set :false-set (cdr result-sets) split)
    split))

(defun split-dataset (dataset split)
  (loop
    with column = (alist-get :column split)
    with value = (alist-get :value split)
    with true-set = (make-dataset)
    with false-set = (make-dataset)
    with comp-func =
      (if (numberp value)
        (lambda (x) (< x value))
        (lambda (x) (equal x value)))
    for row across dataset
    do (vector-push-extend row (if (funcall comp-func (nth column row))
                                 true-set
                                 false-set))
    finally (return (cons true-set false-set))))

(defun value-counts (dataset column)
  (loop 
    with ht = (make-hash-table :test 'equal)
    for row across dataset
    for value = (nth column row)
    do (if (gethash value ht)
         (setf (gethash value ht) (+ (gethash value ht) 1))
         (setf (gethash value ht) 1))
    finally (return ht)))

(defun information-gain (current-score dataset-length split)
  (let* ((true-set (alist-get :true-set split))
         (false-set (alist-get :false-set split))
         (p (/ (length true-set) dataset-length))
         (gain (- current-score (- (* p (gini-impurity true-set))
                                   (* (- 1 p) (gini-impurity false-set))))))
    gain))
    
(defun find-best-split (dataset)
  (loop
    with current-score = (gini-impurity dataset)
    with best-split = nil
    for column upto (- (length (aref dataset 0)) 1)
    when (not (eql column 0))
    do (loop
         with distinct-values = (value-counts dataset column)
         for value being the hash-keys in distinct-values
         for split = (make-split dataset column value)
         for gain = (information-gain current-score (length dataset) split)
         do (format t "Considering split:  ") (print-split split)
         
         ;; We only want splits that have entries in the true 
         ;; and false branches
         when (and (alist-get :true-set split) (alist-get :false-set split))
         do (if (or (not best-split) (> gain (alist-get :gain best-split)))
              (progn (alist-set :gain gain split)
                     (setf best-split split))))
    finally (return best-split)))

(defun make-classification-tree (dataset)
  (unless dataset (return-from make-classfication-tree nil))
  (let* ((split (find-best-split dataset))
         (node (make-alist :split split)))
    (if (<= (alist-get :gain split) 0)
      (progn 
        (alist-set :results (classification-counts dataset) node)
        (return-from make-classification-tree node)))
    (dolist (label '(:true-set :false-set))
      (let ((child-node (make-classification-tree (alist-get label split))))
        (alist-set label child-node node)))
    (if node (print-split (alist-get :split node)))
    (return-from make-classification-tree node)))

(defun print-split (split)
  (format t "~a:~a -> ~a~%"
          (alist-get :column split)
          (alist-get :value split)
          (alist-get :gain split))
  (finish-output))

(defun prune-classification-tree (tree min-gain)
  (labels ((is-leaf (node) (if (alist-get :results node) t nil)))
    (let* ((true-node (alist-get :true-set tree))
           (false-node (alist-get :false-set tree)))

      ;; for each of the child trees, prune it unless it's a leaf node
      (dolist (child-node (list true-node false-node))
        (unless (is-leaf child-node) 
          (prune-classification-tree child-node min-gain)))

      ;; if both the children are leaf nodes
      (if (and (is-leaf true-node) (is-leaf false-node))
        (let* ((delta (merged-impurity-difference tree))
               (merged-data (merged-child-data tree)))
          (if (< delta min-gain)
            (progn
              (alist-set :true-set nil tree)
              (alist-set :false-set nil tree)
              (alist-set :results
                         (classification-counts merged-data)
                         tree)))))))
  tree)

(defun display-classification-tree (node indent-spaces)
  (let* ((split (alist-get :split node))
         (column (alist-get :column split))
         (value (alist-get :value split))
         (gain (alist-get :gain split))
         (true-node (alist-get :true-set node))
         (false-node (alist-get :false-set node))
         (results (alist-get :results node))
         (indentation (make-array indent-spaces 
                                  :element-type 'character
                                  :initial-element #\space)))
    (format t "~a:~a -> ~a~%" column value gain)
    (if results (format t "~a RESULTS: ~a~%" indentation
                        (class-counts-to-probabilities results)))
    (if true-node (progn
                    (format t "~a TRUE ->" indentation)
                    (display-classification-tree
                      true-node (+ indent-spaces 2))))
    (if false-node (progn
                     (format t "~a FALSE ->" indentation) 
                     (display-classification-tree
                       false-node (+ indent-spaces 2))))))

(defun class-counts-to-probabilities (classification-counts)
  (let ((total-occurences
          (loop for pair in classification-counts
                for classification = (car pair)
                for occurences = (cdr pair)
                summing occurences into total
                finally (return total))))
    (mapcar (lambda (pair) 
              (cons (car pair) 
                    (/ (cdr pair) total-occurences) ))
            classification-counts)))

(defun classify-with-classification-tree (data-row node)
  (let* ((split (alist-get :split node))
         (column (alist-get :column split))
         (value (alist-get :value split))
         (true-node (alist-get :true-set node))
         (false-node (alist-get :false-set node))
         (results (alist-get :results node)))
    (if results (return-from classify-with-classification-tree
                             (class-counts-to-probabilities results)))
    ;(format t "Comparison (data-row class? :~a): ~a:~a == ~a~%"
    ;        (first data-row) column value (nth column data-row))
    (if (equal (nth column data-row) value)
      (if true-node (classify-with-classification-tree data-row true-node) 
        (return-from classify-with-classification-tree t))
      (if false-node (classify-with-classification-tree data-row false-node)
        (return-from classify-with-classification-tree nil)))))
