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
    (parse-integer str)))

(defun parse-file(parser file-name)
  (loop for line in (uiop:read-file-lines file-name)
        collect (funcall parser line)))

(defun partOne(parsed)
  (defun recursive-solve(element ls built-in)
    (if (and ls (> (length ls) 0))
        ;; pull from list of elements
        (let ((original (copy-list ls))
              (first element))
          (loop for i in (list 0 1 2)
                for el in ls
                while (<= (abs (- first el)) 3)
                collect
                (let ((e (pop ls)))
                  (return (append (list (- e first)) (recursive-solve e ls built-in))))))
        ;; final element
        (list (- built-in element))))

  (let* ((sorted (sort parsed #'<))
         (last (nth (- (length sorted) 1) sorted))
         (built-in (+ last 3)))
    (let* ((original-sol (sort (recursive-solve 0 sorted built-in) #'<))
           (no-dupes (remove-duplicates original-sol))
           (rows (loop for e in no-dupes collect (list e (length  (loop for x in original-sol when (= x e) collect x)))))
           (multiplied (apply #'* (loop for (n e) in rows collect e))))
      ;; results
      (list "rows x y:" rows
            "multiplied:" multiplied))))


(defparameter my-hash (make-hash-table))

(defun partTwo(parsed)
  (defun recursive-solve-two(element ls built-in)
    (if (<= (abs (- element built-in)) 3)
        (setf (gethash element my-hash) 1)
        (apply #'+ (loop for i in (list 0 1 2)
                         if (and ls (and (<= (abs (- (nth 0 ls) element)) 3)))
                           collect (recursive-solve-two (pop ls) ls built-in)))))
  ;; define memoize for 3 arguments that caches to the hash table
  (defun memoize(f)
    #'(lambda (a &rest args)
        (let ((query (gethash a my-hash)))
          (if query
              query
              (setf (gethash a my-hash) (funcall f a (nth 0 args) (nth 1 args)))))))
  ;; memoize the function
  (setf (fdefinition 'recursive-solve-two) (memoize #'recursive-solve-two))
  ;; set up and call
  (let* ((sorted (sort parsed #'<))
         (last (nth (- (length sorted) 1) sorted))
         (built-in (+ last 3))
         (res (recursive-solve-two 0 sorted built-in)))
    res))


(let ((parsed (parse-file 'my-parser "input10.txt")))
  (print (partTwo parsed)))
