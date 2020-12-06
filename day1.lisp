(load "~/quicklisp/setup.lisp")
(ql:quickload "split-sequence")
(ql:quickload "uiop")

(defun parseInts(strings)
  (loop for x in strings collect (read-from-string x)))
;; (defun partOne()
;;   (let ((lines (uiop:read-file-lines "input1.txt")))
;;     (let ((nums (parseInts lines)))
;;       (setq nums (sort nums '<))
;;       (loop for x in nums do
;;         (loop for y in nums do
;;             (when (= (+ x y) 2020) (return-from partOne (* x y))))))
;;     )
;;   )
(defun partTwo()
  (let ((lines (uiop:read-file-lines "input1.txt")))
    (let ((nums (parseInts lines)))
      (setq nums (sort nums '<))
      (loop for x in nums do
        (loop for y in nums do
          (loop for z in nums do
            (when (= (+ (+ x y) z) 2020) (return-from partTwo (* (* x y) z)))))))
    )
  )
(print (partTwo))
