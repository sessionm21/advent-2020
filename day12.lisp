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
    (list (subseq str 0 1) (subseq str 1 (length str)))))

(defun parse-file(parser file-name)
  (loop for line in (uiop:read-file-lines file-name)
        collect (funcall parser line)))

(defun partOne(parsed)
  (let ((n 0) (w 0) (directions (list "N" "E" "S" "W" "N" "E" "S" "W" "N")) (direction "E"))
    (defun N(a) (setq n (+ n a)))
    (defun S(a) (setq n (- n a)))
    (defun E(a) (setq w (- w a)))
    (defun W(a) (setq w (+ w a)))
    (defun L(a)
      (let ((n-dirs (mod (/ a 90) 4)))
        (cond
          ((string= direction "N") (setq direction (nth (- 4 n-dirs) directions)))
          ((string= direction "E") (setq direction (nth (- 5 n-dirs) directions)))
          ((string= direction "S") (setq direction (nth (- 6 n-dirs) directions)))
          ((string= direction "W") (setq direction (nth (- 7 n-dirs) directions))))))
    (defun R(a)
      (let ((n-dirs (mod (/ a 90) 4)))
        (cond
          ((string= direction "N") (setq direction (nth (+ 0 n-dirs) directions)))
          ((string= direction "E") (setq direction (nth (+ 1 n-dirs) directions)))
          ((string= direction "S") (setq direction (nth (+ 2 n-dirs) directions)))
          ((string= direction "W") (setq direction (nth (+ 3 n-dirs) directions))))))
    (defun F(a)
      (cond
        ((string= direction "N") (setq n (+ n a)))
        ((string= direction "S") (setq n (- n a)))
        ((string= direction "E") (setq w (- w a)))
        ((string= direction "W") (setq w (+ w a)))))
    (loop for e in parsed do
      (my-eval e))
    (list "n:" (abs n) "w:" (abs w) "dir:" direction "sum:" (+ (abs n) (abs w)))))

(defun partTwo(parsed)
  (let ((n 1) (w -10) (ship-n 0) (ship-w 0))
    (defun N(a) (setq n (+ n a)))
    (defun S(a) (setq n (- n a)))
    (defun E(a) (setq w (- w a)))
    (defun W(a) (setq w (+ w a)))
    (defun R(a)
      (let ((n-dirs (mod (/ a 90) 4)))
        (cond
          ((= n-dirs 1) (let ((n-t n)) (setq n w) (setq w (- 0 n-t))))
          ((= n-dirs 2) (let ((n-t n)) (setq n (- 0 n)) (setq w (- 0 w))))
          ((= n-dirs 3) (let ((n-t n)) (setq n (- 0 w)) (setq w n-t))))))
    (defun L(a)
      (let ((n-dirs (mod (/ a 90) 4)))
        (cond
          ((= n-dirs 3) (let ((n-t n)) (setq n w) (setq w (- 0 n-t))))
          ((= n-dirs 2) (let ((n-t n)) (setq n (- 0 n)) (setq w (- 0 w))))
          ((= n-dirs 1) (let ((n-t n)) (setq n (- 0 w)) (setq w n-t))))))
    (defun F(a)
      (setq ship-n (+ ship-n (* n a)))
      (setq ship-w (+ ship-w (* w a))))
    (loop for e in parsed do
      (print (list e "ship-n:" ship-n "ship-w:" ship-w "n:" n "w:" w "sum:" (+ (abs ship-w) (abs ship-n))))
      (my-eval e))
    (list "ship-n:" ship-n "ship-w:" ship-w "n:" n "w:" w "sum:" (+ (abs ship-w) (abs ship-n)))))

(let ((parsed (parse-file 'my-parser "input12.txt")))
  (print (partTwo parsed)))
