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

  (defun ping-direction (2dls cquery row col irow icol distance selection)
    (let ((n-cols (length (nth 0 2dls)))
          (n-rows (length 2dls)))
      (if (and (< row n-rows) (< col n-cols)
               (>= row 0) (>= col 0))
          (if (and
               (member (nth col (nth row 2dls)) selection)
               (> distance 0))
              (if (char= (nth col (nth row 2dls)) cquery) ; is character to see
                  (list row col) ; got match
                  Nil) ; hit something else
              ;; empty space - or on the starting square
              (ping-direction 2dls cquery (+ row irow) (+ col icol) irow icol (+ distance 1) selection))
          Nil))) ; gone off the grid
  (defun ping-direction-long (2dls cquery row col irow icol distance)
    (ping-direction 2dls cquery row col irow icol distance (list #\# #\L)))

  (defun ping-direction-short (2dls cquery row col irow icol distance)
    (ping-direction 2dls cquery row col irow icol distance (list #\# #\L #\.)))

  (defun get-n-surround(2dls ping query row col)
    (let ((n-cols (length (nth 0 2dls)))
          (n-rows (length 2dls)))
      (if (and (< row n-rows) (< col n-cols)
               (>= row 0) (>= col 0))
          (let ((_t (funcall ping 2dls query row col -1  0 0))
                (b  (funcall ping 2dls query row col  1  0 0))
                (l  (funcall ping 2dls query row col  0 -1 0))
                (r  (funcall ping 2dls query row col  0  1 0))
                (tl (funcall ping 2dls query row col -1 -1 0))
                (tr (funcall ping 2dls query row col -1  1 0))
                (bl (funcall ping 2dls query row col  1 -1 0))
                (br (funcall ping 2dls query row col  1  1 0)))
            (remove nil (list _t b l r tl tr bl br))))))

  (defun get-n-surround-all(parsed f)
    (let* ((n-cols (- (length (nth 0 parsed)) 1))
           (n-rows (- (length parsed) 1)))
      (loop for i from 0 to n-rows collect
             (remove nil (loop for j from 0 to n-cols
                if (or (char= (nth j (nth i parsed)) #\L)
                       (char= (nth j (nth i parsed)) #\#))
                  collect (remove nil (funcall f i j)))))))

  (defun replace-given-points(2dls ls query)
    (loop for col in ls do
      (loop for row in ls do
        (loop for (r c) in row do
          (setf (nth c (nth r 2dls)) query)))))

(defun partOneTwo(parsed)

  (let ((parts (list (list #'ping-direction-short 4) (list #'ping-direction-long 5))))
    (loop for (ping count) in parts
          collect
          (let ((results Nil)
                ;; deep copy the 2d list
                (2dls (loop for row in parsed collect (loop for col in row collect col))))
            (loop while T do
              (let* ((n-cols (- (length (nth 0 2dls)) 1))
                     (n-rows (- (length 2dls) 1))
                     (to-remove (get-n-surround-all
                                 2dls
                                 #'(lambda (row col)
                                     (if (>= (length (get-n-surround 2dls ping #\# row col)) count)
                                         (list row col)))))
                     (to-add (get-n-surround-all
                              2dls
                              #'(lambda (row col)
                                  (if (> (length (get-n-surround 2dls ping #\# row col)) 0)
                                      Nil
                                      (list row col))))))

                (replace-given-points 2dls to-remove #\L)
                (replace-given-points 2dls to-add #\#)
                (let ((seat-count (count-seats 2dls)))
                  (setq results (append results (list seat-count)))
                  (print seat-count)
                  (if (> (length (loop for r in results when (= r seat-count) collect r)) 5)
                      (return seat-count)))))))))

(let ((parsed (parse-file 'my-parser "input11.txt")))
  (print (partOneTwo parsed)))
