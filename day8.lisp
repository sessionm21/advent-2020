(let* ((*standard-output* (make-broadcast-stream)) (*error-output* *standard-output*))
  ;;; Load Quicklisp.
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init))))

(let* ((*standard-output* (make-broadcast-stream)) (*error-output* *standard-output*))
  ;;; Load dependencies.
  (ql:quickload 'uiop)
  (ql:quickload 'cl-ppcre))

(defun system(lines)
  (let ((original-instructions (cl-ppcre:split "\\n" (cl-ppcre:regex-replace-all "\\n\\n" lines "")))
        (accumulator 0))
    (defun nop(arg) 1)
    (defun acc(arg) (setq accumulator (+ accumulator arg)) 1)
    (defun jmp(arg) arg)
    (defun custom-test(a b)
      (destructuring-bind (n c) a
        (destructuring-bind (_n _c) b
          (and (= n _n) (string= c _c)))))
    (defun is-jump(a)
      (cl-ppcre:all-matches-as-strings "jmp" a))
    (defun _run-program(instructions)
      (let ((line-number 0) (lines-run (list (list 0 "START"))) (running T))
        (setq accumulator 0)
        (loop while running do
          (let ((line (nth line-number instructions)))
            (if (or (member (list line-number line) lines-run :test #'custom-test))
                (progn ; kill loop
                  (setq lines-run (append lines-run (list (list line-number "kill +0"))))
                  (setq running Nil))
                (progn  ; normal operation
                  (setq lines-run (append lines-run (list (list line-number line))))
                  (setq line-number (+ line-number ;; move from line number (jump)
                                       (eval ;; call string directly
                                        (car (read-from-string (concatenate 'string "((" line "))"))))))))
            (if (>= line-number (length instructions)) (setq running Nil))))
        (list lines-run accumulator)))

    (destructuring-bind (lines-run acc) (_run-program original-instructions)
      (print (list "part one:" acc))
      (loop for (n i) in lines-run
            when (is-jump i) ; part two
              do
                 (let ((instructions (copy-list original-instructions)))
                   (setf (nth n instructions) "nop +0")
                   (destructuring-bind (lines-run acc) (_run-program instructions)
                     (destructuring-bind (n op) (nth (- (length lines-run) 1) lines-run)
                       (if (not (string= op "kill +0"))
                           (print (list "part two: " acc))))))))))

(let ((lines (uiop:read-file-string "input.txt")))
  (system lines)) ; part one and two
