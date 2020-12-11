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

(defun my-eval(element)
  (destructuring-bind (op arg) element
    (eval ;; call string directly
     (car
      (read-from-string
       (concatenate 'string "((" op " " arg "))"))))))

(defun my-parser(str)
  (let* ((split-regex " ")
         (replace-regex "")
         (replace-to "")
         (replaced (cl-ppcre:regex-replace-all replace-regex str replace-to)))
    ;; (cl-ppcre:split split-regex replaced))
    (loop for c across str collect c)))

(defun parse-file(parser file-name)
  (loop for line in (uiop:read-file-lines file-name)
        collect (funcall parser line)))

(defun count-seats(2dls)
  (length (apply #'append (loop for x in 2dls collect
                      (loop for c in x
                            when (char= c #\#)
                              collect c)))))


(defun partOne(parsed)
  (defun shuffle-seats(2dls row col)
    (defun get-n-surround(2dls row col ttl)
      (let ((n-cols (length (nth 0 2dls)))
            (n-rows (length 2dls)))
        (if (and (< row n-rows) (< col n-cols)
                 (>= row 0) (>= col 0)
                 (char= (nth col (nth row 2dls)) #\#))
            (progn
              (if (> ttl 0)
                  ;; (print (list row col))
                  (let ((_t (get-n-surround 2dls (- row 1) col 0))
                        (b (get-n-surround 2dls (+ row 1) col 0))
                        (l (get-n-surround 2dls row (- col 1) 0))
                        (r (get-n-surround 2dls row (+ col 1) 0))
                        (tl (get-n-surround 2dls (- row 1) (- col 1) 0))
                        (tr (get-n-surround 2dls (- row 1) (+ col 1) 0))
                        (bl (get-n-surround 2dls (+ row 1) (- col 1) 0))
                        (br (get-n-surround 2dls (+ row 1) (+ col 1) 0))
                        (c (if (char= (nth col (nth row 2dls)) #\#) (list col row))))
                    (remove nil (list c _t b l r tl tr bl br)))
                  (if (char= (nth col (nth row 2dls)) #\#) (list row col)))))))
    (defun get-n-surround-empty(2dls row col ttl)
      (let ((n-cols (length (nth 0 2dls)))
            (n-rows (length 2dls)))
        (if (and (< row n-rows) (< col n-cols)
                 (>= row 0) (>= col 0)
                 (char= (nth col (nth row 2dls)) #\L))
            (progn
              (if (> ttl 0)
                  ;; (print (list row col))
                  (let ((_t (get-n-surround 2dls (- row 1) col 0))
                        (b (get-n-surround 2dls (+ row 1) col 0))
                        (l (get-n-surround 2dls row (- col 1) 0))
                        (r (get-n-surround 2dls row (+ col 1) 0))
                        (tl (get-n-surround 2dls (- row 1) (- col 1) 0))
                        (tr (get-n-surround 2dls (- row 1) (+ col 1) 0))
                        (bl (get-n-surround 2dls (+ row 1) (- col 1) 0))
                        (br (get-n-surround 2dls (+ row 1) (+ col 1) 0))
                        (c (if (char= (nth col (nth row 2dls)) #\L) (list col row))))
                    (remove nil (list c _t b l r tl tr bl br)))
                  (if (char= (nth col (nth row 2dls)) #\L) (list row col)))))))

    (loop while T do
      (let* ((n-cols (- (length (nth 0 2dls)) 1))
             (n-rows (- (length 2dls) 1))
             (to-remove (remove nil
                                (loop for i from 0 to n-rows
                                      collect
                                      (remove nil (loop for j from 0 to n-cols
                                                        collect
                                                        (remove nil (if (> (length (get-n-surround 2dls i j 1)) 4) (list i j)))))))))
        (loop for col in to-remove do
          (loop for row in to-remove do
            (loop for (r c) in row do
              (setf (nth c (nth r 2dls)) #\L ))))
        ;; (print (get-n-surround 2dls 0 0 1))
        ;; (print 2dls)
        ;; (print (count-seats 2dls))
        (let ((to-add
                (remove nil (loop for i from 0 to n-rows
                                  collect
                                  (remove nil (loop for j from 0 to n-cols
                                                    if (char= (nth j (nth i 2dls)) #\L)
                                                      collect
                                                      (if (<= (length (get-n-surround-empty 2dls i j 1)) 1) (list i j))
                                                    ))))))

          ;; (print to-add)
          (loop for col in to-add do
            (loop for row in to-add do
              (loop for (r c) in row do
                (setf (nth c (nth r 2dls)) #\# ))))
          ;; (print 2dls)
          (print (count-seats 2dls))
          )
        )))
                                        ; (setf (nth c (nth r 2dls)) #\L ))))
  (print (shuffle-seats parsed 0 0))
  (count-seats parsed))

(defparameter my-hash (make-hash-table))

(defun partTwo(parsed)
  (defun ping-direction (2dls cquery row col irow icol distance)
    ;; (print (list "trace" row col))
     (let ((n-cols (length (nth 0 2dls)))
           (n-rows (length 2dls)))
       (if (and (< row n-rows) (< col n-cols)
                (>= row 0) (>= col 0))
           (progn
             (if (and
                  (or
                   (char= (nth col (nth row 2dls)) #\#)
                   (char= (nth col (nth row 2dls)) #\L))
                  (> distance 0))
                 (progn ; is character to see
                   (if (char= (nth col (nth row 2dls)) cquery)
                       (list row col) ; got match
                       Nil)) ; hit something else
                 (progn ; empty space - or on the starting square
                   (ping-direction 2dls cquery (+ row irow) (+ col icol) irow icol (+ distance 1)))))
           (progn ; gone off the grid
             Nil))))
  (defun get-n-surround-direction(2dls query row col)
    (let ((n-cols (length (nth 0 2dls)))
          (n-rows (length 2dls)))
      (if (and (< row n-rows) (< col n-cols)
               (>= row 0) (>= col 0))
          (let ((_t (ping-direction 2dls query row col -1  0 0))
                (b  (ping-direction 2dls query row col  1  0 0))
                (l  (ping-direction 2dls query row col  0 -1 0))
                (r  (ping-direction 2dls query row col  0  1 0))
                (tl (ping-direction 2dls query row col -1 -1 0))
                (tr (ping-direction 2dls query row col -1  1 0))
                (bl (ping-direction 2dls query row col  1 -1 0))
                (br (ping-direction 2dls query row col  1  1 0)))
            (remove nil (list _t b l r tl tr bl br))))))

  (loop while T do
    (let* ((n-cols (- (length (nth 0 parsed)) 1))
           (n-rows (- (length parsed) 1))
           (to-remove (remove nil
                              (loop for i from 0 to n-rows
                                    collect
                                    (remove nil (loop for j from 0 to n-cols
                                                      if (or (char= (nth j (nth i parsed)) #\L)
                                                             (char= (nth j (nth i parsed)) #\#))
                                                        collect
                                                        (remove nil (if (>= (length (get-n-surround-direction parsed #\# i j)) 5) (list i j)))))))))

      ;; (print parsed)
      ;; (print (count-seats parsed))
      ;; (print "========")
      (loop for col in to-remove do
        (loop for row in to-remove do
          (loop for (r c) in row do
            (setf (nth c (nth r parsed)) #\L ))))
    ;;;; (print (get-n-surround parsed 0 0 1))
      ;; (print parsed)
      ;; (print (count-seats parsed))
      ;; (print "========")
      (let ((to-add (remove nil
                            (loop for i from 0 to n-rows
                                  collect
                                  (remove nil (loop for j from 0 to n-cols
                                                    if (or (char= (nth j (nth i parsed)) #\L)
                                                           (char= (nth j (nth i parsed)) #\#))
                                                      collect
                                                        (remove nil (if (> (length (get-n-surround-direction parsed #\# i j)) 0) Nil (list i j)))))))))

        ;; (print to-add)
        ;; (print (get-n-surround parsed #\L 1 8))
        (loop for col in to-add do
          (loop for row in to-add do
            (loop for (r c) in row do
              (setf (nth c (nth r parsed)) #\# ))))
        ;; (print parsed)
        (print (count-seats parsed))))
    ;; (setf (nth c (nth r 2dls)) #\L ))))
       )
  ;; (length (get-n-surround parsed #\# 0 0))
  )


(let ((parsed (parse-file 'my-parser "input11.txt")))
  (print (partTwo parsed)))
