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

(defmacro split (a b)
  ('split-sequence:SPLIT-SEQUENCE a b ))

(defun partOne()
  (defun validate-passport(str)
    (defun has-key(key ls)
      (> (length (loop for (k v) in ls
                       when (string= k key)
                         collect T)) 0))
    (let ((content (loop for pair in (split #\Space str)
                         collect
                         (destructuring-bind (key value) (split #\: pair)
                           (list key value)))))
      (and (has-key "byr" content)
           (has-key "iyr" content)
           (has-key "eyr" content)
           (has-key "hgt" content)
           (has-key "hcl" content)
           (has-key "ecl" content)
           (has-key "pid" content)
            ; (has-key "cid" content)
           )
      )
    )
  (length (let ((data (uiop:read-file-string "input4.txt")))
            (loop for line in (split #\^ (cl-ppcre:regex-replace-all "\\n\\n" data "^" ))
                  when (validate-passport (cl-ppcre:regex-replace-all " $" (cl-ppcre:regex-replace-all "\\n" line " ") ""))
                    collect T))))

(defun partTwo()
  (defun validate-passport(str)
    (defun get-key-value(key ls)
      (nth 0 (loop for (k v) in ls
                   when (string= k key)
                     collect v)))
    (defun has-key(key ls)
      (get-key-value key ls))
    (defun year-in-range(start end _year)
      (and (= (length _year) 4)
           (let ((year (parse-integer _year)))
             (and (<= start year) (<= year end)))))
    (defun valid-measure(_measure)
      (let ((measure (cl-ppcre:regex-replace-all "cm$" (cl-ppcre:regex-replace-all "in$" _measure "#in") "#cm")))
        (let ((measures (split #\# measure)))
          (when (= (length measures) 2)
            (destructuring-bind (_m u) (split #\# measure)
              (let ((m (parse-integer _m)))
                (or (when (string= u "cm")
                      (and (<= 150 m) (<= m 193)))
                    (when (string= u "in")
                      (and (<= 59 m) (<= m 76)))
                    )))))))
    (defun valid-rgb(str)
      (and (char= (char str 0) #\#)
           (= (length (cl-ppcre:regex-replace-all "([0-9]|[a-f])*" (subseq str 1) "")) 0)))
    (defun valid-eye-color(str)
      (or  (string= str "amb")
           (string= str "blu")
           (string= str "brn")
           (string= str "gry")
           (string= str "grn")
           (string= str "hzl")
           (string= str "oth")))
    (defun valid-passport-id(str)
      (and (= (length str) 9)
           (= (length (cl-ppcre:regex-replace-all "([0-9])*" str "")) 0)))

    (let ((content (loop for pair in (split #\Space str)
                         collect
                         (destructuring-bind (key value) (split #\: pair)
                           (list key value)))))
      (and (and (has-key "byr" content) (year-in-range 1920 2002 (get-key-value "byr" content)))
           (and (has-key "iyr" content) (year-in-range 2010 2020 (get-key-value "iyr" content)))
           (and (has-key "eyr" content) (year-in-range 2020 2030 (get-key-value "eyr" content)))
           (and (has-key "hgt" content) (valid-measure (get-key-value "hgt" content)))
           (and (has-key "hcl" content) (valid-rgb (get-key-value "hcl" content)))
           (and (has-key "ecl" content) (valid-eye-color (get-key-value "ecl" content)))
           (and (has-key "pid" content) (valid-passport-id (get-key-value "pid" content)))
           ; (has-key "cid" content)
           )
      )
    )
  (let ((data (uiop:read-file-string "input4-2.txt")))
    (loop for line in (split #\^ (cl-ppcre:regex-replace-all "\\n\\n" data "^" ))
          when (validate-passport (cl-ppcre:regex-replace-all " $" (cl-ppcre:regex-replace-all "\\n" line " ") ""))
            collect T)))

(print (length (partTwo)))
