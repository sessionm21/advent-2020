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
    ;(cl-ppcre:split split-regex replaced))
    (parse-integer str)))

(defun parse-file(parser file-name)
  (loop for line in (uiop:read-file-lines file-name)
        collect (funcall parser line)))

(defun recursive-solve(element ls built-in)
  (if (and ls (> (length ls) 0))
      (let ((original (copy-list ls))
            (first (pop ls)))
        (if (<=(abs (- first element)) 3)
            (append (list (- first element)) (recursive-solve first ls built-in))
            (let ((second (pop ls)))
              (if (<=(abs (- second element)) 3)
                  (append (list (- second element)) (recursive-solve second ls built-in)))
              (let ((third (pop ls)))
                (if (<=(abs (- third element)) 3)
                    (append (list (- third element)) (recursive-solve third ls built-in))
                    )))))
      (list (- built-in element))
      ))


(defun partOne(parsed)
  (let* ((sorted (sort parsed #'<))
         (last (nth (- (length sorted) 1) sorted))
         (built-in (+ last 3)))
    (print sorted)
    (let* ((original-sol (sort (recursive-solve 0 sorted built-in) #'<))
           (no-dupes (remove-duplicates original-sol)))
      (print (apply #'* (loop for e in no-dupes collect (length  (loop for x in original-sol when (= x e) collect x)))))
      (print original-sol)
      (print no-dupes)
      )
  ))


(defparameter my-hash (make-hash-table))
(defun partTwo(parsed)
  (defun recursive-solve-two(element ls built-in)
    (if (<= (abs (- element built-in)) 3)
        (progn
          (setf (gethash element my-hash) 1)
          1)
        (progn
          (let ((query (gethash element my-hash)))
            (if query
                (progn
                  query)
                (let* ((copied ls)
                       (res (apply #'+ (loop for i in (list 0 1 2)
                                                  if (and copied (and (<= (abs (- (nth 0 copied) element)) 3)))
                                                    collect (let ((x (pop copied)))
                                                              (if (and (<= (abs (- x element)) 3))
                                                                  (recursive-solve-two x copied built-in)
                                                                  ))))))
                  (setf (gethash element my-hash) res)
                  res))))))

  (let* ((sorted (sort parsed #'<))
         (last (nth (- (length sorted) 1) sorted))
         (built-in (+ last 3))
         (res (recursive-solve-two 0 sorted built-in)))
    res
    ))


(let ((parsed (parse-file 'my-parser "input10.txt")))
  (print (partTwo parsed)))
