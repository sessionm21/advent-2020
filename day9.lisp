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
    (cl-ppcre:split split-regex replaced))
  (parse-integer str))

(defun parse-file(parser file-name)
  (loop for line in (uiop:read-file-lines file-name)
        collect (funcall parser line)))

(defun make-mult(a b c)
  (if (and a b c)
      (= (+ a b ) c)))
(defun partOne(parsed)
  (let* ((original (copy-list parsed))
         (n-preamble 25)
         (preamble (loop repeat n-preamble collect (pop parsed)))
         (current (nth 0 parsed)))

  (print preamble)

    (print current)
    (if (not (member T (loop for x in preamble
                             collect (nth 0 (loop for y in preamble when (make-mult x y current) collect T)))))
        (return-from partOne current))

    (pop original)
    (if (> (length original) 0)
        (partOne original))))

(defun partTwo(left right parsed)
  (let* ((to-find 257342611)
         (current (subseq parsed left right))
         (_sum (apply #'+ current)))

    (if (= _sum to-find) ;; pick smallest and largest
        (let* ((sorted (sort current #'<))
               (answer (+ (nth 0 sorted) (nth (- (length sorted) 1) sorted))))
          (print (list "PROG DONE" _sum left right "answer: " answer 99 99 99 ))))

    (if (< _sum to-find)
        (progn
          (if (< right (length parsed))
              (partTwo left (+ right 1) parsed))
          (if (and (< right (length parsed))
                   (< (+ left 1) right))
              (partTwo (+ left 1) (+ right 1) parsed))
          (if (< (+ left 2) right)
              (partTwo (+ left 1) right parsed))
          )
        )))

(let ((parsed (parse-file 'my-parser "input9.txt")))
  (partTwo 0 0 (loop repeat 576 collect (pop parsed))))
