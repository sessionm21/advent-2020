(load "~/quicklisp/setup.lisp")
(ql:quickload "split-sequence")
(ql:quickload "uiop")

(defun partOne(range _letter phrase)
  (let ((min 0)
        (max 0)
        (letter 0)
        (matches 0))
    (destructuring-bind (smin smax) (split-sequence:SPLIT-SEQUENCE #\- range)
      (setq min (read-from-string smin))
      (setq max (read-from-string smax)))
    (destructuring-bind (sletter _) (split-sequence:SPLIT-SEQUENCE #\: _letter)
      (setq letter (char sletter 0)))
    (loop for c across phrase do
      (when (char= c letter)
        (setq matches (+ matches 1))))
    (and (>= matches min) (<= matches max)))
  )

(defun partTwo(range _letter phrase)
  (let ((min 0)
        (max 0)
        (letter 0)
        (matches 0)
        (len (length phrase)))
    (destructuring-bind (smin smax) (split-sequence:SPLIT-SEQUENCE #\- range)
      (setq min (- (nth-value 0 (parse-integer smin)) 1))
      (setq max (- (nth-value 0 (parse-integer smax)) 1)))
    (destructuring-bind (sletter _) (split-sequence:SPLIT-SEQUENCE #\: _letter)
      (setq letter (char sletter 0)))
    (when (and (and (>= min 0) (>= max 0)) (<= min len) (<= max len))
      (not (equal (char= (char phrase min) letter) (char= (char phrase max) letter)))
      ))
  )

(defun run()
  ; length of list is the number of matches
  (length (loop for line in (uiop:read-file-lines "input2.txt")
                if (destructuring-bind
                     ; break down line by spaces
                       (range letter phrase)
                       (split-sequence:SPLIT-SEQUENCE #\Space line)
                     ; call vadidation function
                     (partTwo range letter phrase))
                  collect T)))

(print (run))
