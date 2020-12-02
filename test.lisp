(load "~/quicklisp/setup.lisp")
(ql:quickload "split-sequence")
(ql:quickload "uiop")


(let ((lines (uiop:read-file-lines "input.txt")))
  (defun parseInts(strings)
    (loop for x in strings collect (read-from-string x)))
  (defun makeSum()
    (let ((nums (parseInts lines)))
      (loop for x in nums do
        (loop for y in nums do
          (when (= (+ x y) 2020) (return-from makeSum (* x y)))))
      )
    )
  (print (makeSum)))
