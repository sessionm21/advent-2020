(load "~/quicklisp/setup.lisp")
(ql:quickload "split-sequence")
(ql:quickload "uiop")

                                        ; Answer
                                        ; (82 242 71 67 24)
(defun partOne()
  (let ((lines (uiop:read-file-lines "input.txt")))
    (length
     (loop for line in lines
          for lineNumber from 0 to (length lines)
          when (let ((leftPos (mod (* lineNumber 3) (length line))))
                 (when (char= (char line leftPos) #\#)
                     T))
            collect T))))
(defun partTwo()
  (let ((lines (uiop:read-file-lines "input.txt"))
        (slopes (list (list 1 1) (list 3 1) (list 5 1) (list 7 1) (list 1 2))))
    (loop for (rightInc downInc) in slopes
          collect
          (length
           (loop for line in lines
                 for lineNumber from 0 to (length lines)
                 when (let ((leftPos (mod (* lineNumber (/ rightInc downInc)) (length line))))
                        (when (and (= (mod lineNumber downInc) 0)
                                   (char= (char line leftPos) #\#))
                          T)
                        )
                   collect T)))))
(print (partOne))
(print (reduce #'* (partTwo)))
