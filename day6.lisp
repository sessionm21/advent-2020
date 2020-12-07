(defun partOne()
  (let* ((lines (uiop:read-file-string "input6.txt"))
         (groups (cl-ppcre:split "\\n\\n" lines)))
    (reduce '+ (loop for group in groups collect
      (length (remove-duplicates (cl-ppcre:regex-replace-all "\\n" group "")))))))

(defun partTwo()
  (let* ((lines (uiop:read-file-string "input6.txt"))
         (groups (cl-ppcre:split "\\n\\n" lines)))
    (reduce '+ (loop for sgroup in groups collect
      (let* ((clipped (cl-ppcre:regex-replace-all "\\n$" sgroup ""))
             (n-members (length (cl-ppcre:all-matches-as-strings "\\n" clipped)))
             (sregex (concatenate 'string "([a-zA-Z])\\1{" (write-to-string n-members) "}")))
        (length (cl-ppcre:all-matches-as-strings sregex (sort clipped #'char-lessp)))
        )))))

(print (partOne))
(print (partTwo))
