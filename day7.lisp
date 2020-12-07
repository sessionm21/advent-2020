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


(defun partOne()
  (defun find-references(query input)
    (loop for line in (cl-ppcre:split ".\\n" input)
          when (cl-ppcre:all-matches (concatenate 'string "^.* " query) line)
            collect (cl-ppcre:regex-replace-all " contain" (nth 0 (cl-ppcre:all-matches-as-strings "^.*bag contain" line)) "")))

  (defun recursive-solve(count query input)
    (let ((references (find-references query input)))
      (apply #'append
             (list query)
             (loop for reference in references collect (recursive-solve (+ count 1) reference input)))))

  (let* ((input (cl-ppcre:regex-replace-all "bags" (uiop:read-file-string "input.txt") "bag"))
         (query "shiny gold")
         (qpath (remove-duplicates (recursive-solve 0 query input) :test #'string-equal)))
    (print (- (length qpath) 1))))

(defun partTwo()
  (defun parse-bag(str)
    (let* ((number (parse-integer (nth 0 (cl-ppcre:all-matches-as-strings "^[0-9]* " str))))
           (color (cl-ppcre:regex-replace-all "^[0-9]* " str "")))
      (list number color)))

  (defun get-subbags(query input)
    (let* ((line (nth 0 (cl-ppcre:all-matches-as-strings (concatenate 'string query ".*contain.*\.") input)))
           (bags (loop for bag in (cl-ppcre:split ", " (cl-ppcre:regex-replace-all "(.*contain )|(\\.)" line ""))
                       when (not (string= bag "no other bag"))
                         collect (parse-bag bag)))
           (n-bags (reduce #'+ (loop for (count color) in bags collect count))))
      (+ (reduce #'+ (loop for (count color) in bags
                           collect (reduce #'+ (loop repeat count collect (get-subbags color input)))))
         n-bags)))

  (let* ((input (cl-ppcre:regex-replace-all "bags" (uiop:read-file-string "input.txt") "bag"))
         (query "shiny gold"))
    (print (get-subbags query input))))

(partTwo)
