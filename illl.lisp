(defpackage :illl
  (:use :cl :cl-cairo2)
  (:export :make-rules
           :do-rewrite))

(in-package :illl)

;;; Cairo

(defvar *png-dir* (merge-pathnames "output/"))
(defvar *png-width* 400)
(defvar *png-height* 400)

(defun png-pathname-string (fname &optional (check-existance nil))
  (let ((pngdir  *png-dir*))
    (if (probe-file pngdir)
        (let ((path (namestring (merge-pathnames (concatenate 'string fname ".png") pngdir))))
          (if check-existance
              (if (probe-file path)
                  path
                  (error "~A doesn't exist" path))
              path))
        (error "~A (see *png-dir*) doesn't exist" pngdir))))

(defmacro with-png-surface-rgba ((fname width height) &body body)
  `(with-png-file (,fname :argb32 ,width ,height)
     (clear-surface-rgba)
     (set-source-rgb 0 0 0)
     ;(scale ,width ,height)
     ,@body))

(defmacro write-to-png ((verb) &body body)
  `(with-png-surface-rgba ((png-pathname-string ,verb) *png-width* *png-height*)
     ,@body))

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

(defun do-rewrite (string rules n)
  "Does rewrite of string by rules n iterations"
  (labels ((recur (i string)
             (if (<= i 0)
                 string
                 (recur (1- i) (rewrite string rules)))))
    (recur n string)))

(defun make-rules (rulelist)
  "Turns a list of form ((char str) ...) into a rules hash-table"
  (let ((result (make-hash-table :test 'equal)))
    (dolist (rule rulelist)
      (setf (gethash (car rule) result)
            (car (cdr rule))))
    result))
