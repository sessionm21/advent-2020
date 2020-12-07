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

(defun generate-tree(input)
  (defun parse-bag(str)
    (let* ((number (parse-integer (nth 0 (cl-ppcre:all-matches-as-strings "^[0-9]* " str))))
           (color (cl-ppcre:regex-replace-all "^[0-9]* " str "")))
      (list number color)))
  (let* ((lines (loop for line in (cl-ppcre:split ".\\n" input) collect (cl-ppcre:split " contain " line)))
         (pairs (loop for (k v) in lines
                      collect (list k (loop for bag in (cl-ppcre:split ", " v)
                                            if (not (string= bag "no other bag"))
                                              collect (parse-bag bag))))))
    pairs))

(defun part-one-recurs(query tree)
  (defun custom-test(a b)
    (destructuring-bind (n color) b
      (string= query color)))
  (let ((get-left (loop for (color val) in tree if (find query val :test #'custom-test) collect color)))
    (apply #'append get-left (loop for color in get-left collect (part-one-recurs color tree)))))

(defun partOne(query tree)
  (length (remove-duplicates (part-one-recurs query tree) :test #'string-equal)))

(defun part-two-recurs(query tree)
  (defun custom-test(a b)
    (destructuring-bind (color v) b
      (string= color query)))
  (let* ((sub-bags (find query tree :test #'custom-test))
         (n-sub-bags (reduce #'+ (destructuring-bind (c v) sub-bags (loop for (n _c) in v collect n))))
         (recurse
           (destructuring-bind (c v) sub-bags
             (loop for (n _c) in v
                   collect (loop repeat n collect (part-two-recurs _c tree))))))
    (apply #'append (list n-sub-bags) (apply #' append recurse))))

(defun partTwo (query tree)
  (reduce #'+ (part-two-recurs query tree)))

(print (let ((input (cl-ppcre:regex-replace-all "bags" (uiop:read-file-string "input.txt") "bag")))
         (partTwo "shiny gold bag" (generate-tree input))))
