(defun parse-shroom-info ()
  (with-open-file (file "/tmp/shroom-info")
    (loop with shroom-infos = (make-array 0 :element-type 'list
                                          :fill-pointer 0 :adjustable t)
          with buffer = (make-array 0 :element-type 'character
                                    :fill-pointer 0 :adjustable t
          with state = :pre-num
          for chr = (read-char file)



