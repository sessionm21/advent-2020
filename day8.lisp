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

(defun partOne(lines)
  (let ((original-instructions (cl-ppcre:split "\\n" (cl-ppcre:regex-replace-all "\\n\\n" lines "")))
        (running T)
        (line-number 0)
        (accumulator 0)
        (lines-run (list (list 0 "START"))))
    (defun nop(arg)
      1)
    (defun acc(arg)
      (setq accumulator (+ accumulator arg))
      1)
    (defun jmp(arg)
      ; (print (list "jump" arg))
      arg)
    (defun custom-test(a b)
      (destructuring-bind (n c) a
        (destructuring-bind (_n _c) b
          (and (= n _n) (string= c _c)))))
    (defun is-jump(a)
      (cl-ppcre:all-matches-as-strings "jmp" a))
    (defun do-loop(instructions)
      (loop while running do
        (let ((line (nth line-number instructions)))
          ; (print (list "DEBUG" line-number line))
          (if (or (member (list line-number line) lines-run :test #'custom-test))
                                        ; kill loop
              (progn
                (setq lines-run (append lines-run (list (list line-number "kill +0"))))
                (setq running Nil))
                                        ; normal operation
              (progn
                (setq lines-run (append lines-run (list (list line-number line))))
                (setq line-number (+ line-number (eval (car (read-from-string (concatenate 'string "((" line "))")))))))
                                        ; (print lines-run))
              )
          (if (>= line-number (length instructions)) (setq running Nil))
          ))
      )
    (do-loop original-instructions)
    (print lines-run)

    (loop for (n i) in lines-run
          when (is-jump i)
            do
               (setq lines-run (list (list 0 "START")))
               (setq line-number 0)
               (setq running T)
               (setq accumulator 0)
               (print (list "SETTING:" n i))
               (let ((instructions (copy-list original-instructions)))
                 (setf (nth n instructions) "nop +0")
                 ; (print instructions)
                 (do-loop instructions)
                 ; (print lines-run)
                 (print (nth (- (length lines-run) 1 ) lines-run))
                 (print accumulator)
                 (print "==================================")
                 ))

    (print "==TEST=======================")
    (do-loop original-instructions)
    (print lines-run)
    (print (list "ACCUMULATOR: " accumulator))
    )
  )

(defun partTwo(lines)
  (print lines))

(let ((lines (uiop:read-file-string "input.txt")))
  (partOne lines))
