(defpackage :classification-tree
  (:export build-tree
           print-tree
           classify))

(defun str-split (str chr)
  (loop for start = 0 then (1+ end)
        for end = (position chr str :start start)
        collect (subseq str start end)
        until (null end)))

(defmacro with-data-file (stream-name &rest body)
    `(with-open-file (,stream-name "~/Downloads/shrooms.csv" :direction :input)
       ,@body))

(defun make-dataset ()
  (make-array 0 :element-type 'list :fill-pointer 0 :adjustable t))

(defun data-from-file (&key num-lines)
  (with-data-file s
    (loop with shroom-types = (make-dataset)
          for i = 1 then (+ i 1)
          for line = (read-line s nil)
          while line
          until (and num-lines (eq num-lines i))
          do (vector-push-extend (str-split line #\,) shroom-types)
          finally (return shroom-types))))

(defun incr-key-in-result-set (key result-set)
  (loop for pair in result-set
        do (if (string= (car pair) key) 
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
                 when (not (string= inner_classification classification))
                 do (setf impurity 
                          (+ impurity (* probability inner_probability))))
        finally (return impurity)))

(defun make-split (dataset column value)
  (let* ((split (make-alist :column column :value value))
         (result-sets (split-dataset dataset split)))
    (alist-set :true-set (car result-sets) split)
    (alist-set :false-set (cdr result-sets) split)))

(defun split-dataset (dataset split)
  (loop
    with column = (alist-get :column split)
    with value = (alist-get :value split)
    with true-set = (make-dataset)
    with false-set = (make-dataset)
    with comp-func =
      (if (numberp value)
        (lambda (x) (< x value))
        (lambda (x) (string= x value)))
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

(defun build-tree (dataset)
  (unless dataset (return-from build-tree nil))
  (let* ((split (find-best-split dataset))
         (node (make-alist :split split)))
    (if (<= (alist-get :gain split) 0)
      (progn 
        (alist-set :results (classification-counts dataset) node)
        (return-from build-tree node)))
    (dolist (label '(:true-set :false-set))
      (let ((child-node (build-tree (alist-get label split))))
        (alist-set label child-node node)))
    (if node (print-split (alist-get :split node)))
    (return-from build-tree node)))

(defun print-split (split)
  (format t "~a:~a -> ~a~%"
          (alist-get :column split)
          (alist-get :value split)
          (alist-get :gain split))
  (finish-output))

(defun print-tree (node indent-spaces)
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
                        (class-counts-to-probabilities)))
    (if true-node (progn
                    (format t "~a TRUE ->" indentation)
                    (print-tree true-node (+ indent-spaces 2))))
    (if false-node (progn
                     (format t "~a FALSE ->" indentation) 
                     (print-tree false-node (+ indent-spaces 2))))))

(defun class-counts-to-probabilities (classification-counts)
  (let ((total-occurences
          (loop for pair in classification-counts
                for classification = (car pair)
                for occurences = (cdr pair)
                summing occurences into total
                finally (return total))))
    (mapcar (lambda (pair) 
              (cons (car pair) 
                    (coerce (/ (cdr pair) total-occurences) 'float)))
            classification-counts)))

(defun is-shroom-toxic? (shroom node)
  (let* ((split (alist-get :split node))
         (column (alist-get :column split))
         (value (alist-get :value split))
         (true-node (alist-get :true-set node))
         (false-node (alist-get :false-set node))
         (results (alist-get :results node)))
    (if results (return-from is-shroom-toxic?
                             (class-counts-to-probabilities results)))
    ;(format t "Comparison (shroom class? :~a): ~a:~a == ~a~%"
    ;        (first shroom) column value (nth column shroom))
    (if (equal (nth column shroom) value)
      (if true-node (is-shroom-toxic? shroom true-node) 
        (return-from is-shroom-toxic? t))
      (if false-node (is-shroom-toxic? shroom false-node)
        (return-from is-shroom-toxic? nil)))))
