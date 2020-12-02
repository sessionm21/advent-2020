(load "~/quicklisp/setup.lisp")
(ql:quickload "split-sequence")
(ql:quickload "uiop")


(let ((nums (uiop:read-file-lines "input.txt")))
  (defun makeSum()
    (loop for x in nums do
      (loop for y in nums do
        (let ((px (read-from-string x))
              (py (read-from-string y)))
          (when (= (+ px py) 2020) (return-from makeSum (* px py)))))))
    (print (makeSum)))
