(let* ((*standard-output* (make-broadcast-stream)) (*error-output* *standard-output*))
  ;;; Load Quicklisp.
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init))))

(let* ((*standard-output* (make-broadcast-stream)) (*error-output* *standard-output*))
  ;;; Load dependencies.
  (ql:quickload 'uiop)
  (ql:quickload 'cl-ppcre)
  (ql:quickload 'split-sequence))


(defun partOne()
  (let ((lines (uiop:read-file-lines "input5.txt"))
        (maxid 0))
    (loop for (row col id) in
                           (loop for line in lines
                                 collect
                                 (let ((row-min 0) (row-max 127) (col-min 0) (col-max 8))
                                   (loop for c across line do
                                        ; (print (list "ROW-MIN" row-min "ROW-MAX" row-max))
                                     (if (char= c #\F)
                                         (setq row-max (floor (/ (+ row-min row-max) 2))))
                                     (if (char= c #\B)
                                         (setq row-min (ceiling (/ (+ row-min row-max) 2))))
                                     (if (char= c #\L)
                                         (setq col-max (floor (/ (+ col-min col-max) 2))))
                                     (if (char= c #\R)
                                         (setq col-min (ceiling (/ (+ col-min col-max) 2)))))
                                   (list row-min col-min (+ (* row-min 8) col-min))
                                   ))
          do
             (if (> id maxid) (setq maxid id)))
    maxid))

(defun partTwo()
  (defun get-row(_row ls)
    (loop for (row col id) in ls
          when (= row _row)
            collect col))
  (defun get-partial-rows (ls)
    (loop for row from 0 to 127
          when (has-empty-col row ls)
            collect row))
  (defun get-missing-cols (_row ls)
    (let ((row (get-row _row ls)))
      (loop for col from 1 to 7
            when (not (member col row))
              collect col)))
  (defun row-len(_row ls)
    (length (get-row _row ls)))
  (defun has-empty-col (_row ls)
    (= (row-len _row ls) 7))
  (defun get-pass (line)
    (let ((row-min 0) (row-max 127) (col-min 0) (col-max 8))
      (loop for c across line do
        (if (char= c #\F)
            (setq row-max (floor (/ (+ row-min row-max) 2))))
        (if (char= c #\B)
            (setq row-min (ceiling (/ (+ row-min row-max) 2))))
        (if (char= c #\L)
            (setq col-max (floor (/ (+ col-min col-max) 2))))
        (if (char= c #\R)
            (setq col-min (ceiling (/ (+ col-min col-max) 2)))))
      (if (and
           (> row-min ) (< row-max 127))
          (list row-min col-min (+ (* row-min 8) col-min)))))
  (let* ((lines (uiop:read-file-lines "input5.txt"))
         (passes (sort (loop for line in lines collect (get-pass line)) #'> :key #'car))
         (my-row (nth 0 (get-partial-rows passes)))
         (my-col (nth 0 (get-missing-cols my-row passes))))
    (+ (* my-row 8) my-col)))

(print (partTwo))
