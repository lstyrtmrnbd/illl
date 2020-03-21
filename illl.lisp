
;;; Rewriting

(defun string-like (thing)
  (etypecase thing
    (character (make-string 1 :initial-element thing))
    (string thing)))

(defun map-string (fun string)
  (dotimes (n (length string))
    (funcall fun (char string n))))

(defun rewrite (string rules)
  "Rewrite a string once, rules is a hash-map of char-string mappings"
  (let ((result ""))
    (labels ((add-to-result (str)
               (setf result
                     (concatenate 'string
                                  result
                                  (string-like str))))
             (transform (char)
               (multiple-value-bind (new found) (gethash char rules)
                 (if found
                     new
                     char)))
             (transform-into-result (char)
               (add-to-result (transform char))))
      (map-string #'transform-into-result
                  string))
    result))
