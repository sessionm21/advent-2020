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

(defun my-parser(input)
  (let* ((split-regex " ")
        (replace-regex "")
        (replaced (cl-ppcre:regex-replace-all replace-regex input "")))
    (cl-ppcre:split split-regex replaced)))

(defun parse-file(name)
  (let ((file-lines (uiop:read-file-lines name)))
    (loop for line in file-lines
          collect (my-parser line))))

(defun partOne(parsed)
  (print parsed))

(defun partTwo(parsed)
  (print parsed))

(let ((parsed (parse-file "input.txt")))
  (partOne parsed))
