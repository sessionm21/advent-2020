(let* ((*standard-output* (make-broadcast-stream)) (*error-output* *standard-output*))
  ;;; Load Quicklisp.
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init))))

(let* ((*standard-output* (make-broadcast-stream)) (*error-output* *standard-output*))
  ;;; Load dependencies.
  (ql:quickload 'uiop)
  (ql:quickload 'cl-ppcre))

(defun my-parser(str)
  (let* ((split-regex " ")
         (replace-regex "")
         (replace-to "")
         (replaced (cl-ppcre:regex-replace-all replace-regex str replace-to)))
    (cl-ppcre:split split-regex replaced)))

(defun parse-file(parser file-name)
  (loop for line in (uiop:read-file-lines file-name)
        collect (funcall parser line)))

(defun system(original-instructions)
  (defun nop(arg) (list 1 0))
  (defun acc(arg) (list 1 arg))
  (defun jmp(arg) (list arg 0))
  (defun custom-test(a b)
    (cond ((listp b)
           (destructuring-bind ((_an (_aop _aarg)) (_bn (_bop _barg)))
               (list a b)
             (and (= _an _bn) (string= _aop _bop) (string= _aarg _barg))))))
  (defun _run-program(lines-run line-number accumulator instructions)
    (let* ((line (nth line-number instructions))
           (op (nth 0 line))
           (arg (nth 1 line)))
      (if (>= line-number (length instructions))
          (list lines-run accumulator)
          (if (member (list line-number line) lines-run :test #'custom-test)
              (list (append lines-run (list (list line-number (list "kill" "0")))) accumulator)
              (progn
                (destructuring-bind (_jmp _acc)
                    (funcall (read-from-string op) (read-from-string arg))
                  (_run-program
                   (append lines-run (list (list line-number line)))
                   (+ line-number _jmp)
                   (+ accumulator _acc)
                   instructions)))))))
  (destructuring-bind (lines-run acc)
      (_run-program Nil 0 0 original-instructions)
    (list "part one:" acc "part two:"
          (remove-duplicates
           (loop for (n (op arg)) in lines-run
                 when (string= op "jmp")
                   collect (let ((instructions (copy-list original-instructions)))
                             (setf (nth n instructions) (list "nop" "0"))
                             (destructuring-bind (lines-run _acc)
                                 (_run-program (list 0 "START") 0 0 instructions)
                               (destructuring-bind (n (op arg))
                                   (nth (- (length lines-run) 1) lines-run)
                                 (if (not (string= op "kill"))
                                     _acc)))))))))

(print (let ((parsed (parse-file 'my-parser "input.txt")))
         (system parsed)))
