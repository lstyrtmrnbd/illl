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
        (let ((path (namestring (merge-pathnames (concatenate 'string fname ".png")
                                                 pngdir))))
          (if check-existance
              (if (probe-file path)
                  path
                  (error "~A doesn't exist" path))
              path))
        (error "~A (see *png-dir*) doesn't exist" pngdir))))

(defun clear-surface-rgba ()
  (save)
  (set-source-rgba 1.0 1.0 1.0 1.0)
  (set-operator :source)
  (paint)
  (restore))

(defmacro with-png-surface-rgba ((fname width height) &body body)
  `(with-png-file (,fname :argb32 ,width ,height)
     (clear-surface-rgba)
     (set-source-rgb 0 0 0)
     ;(scale ,width ,height)
     ,@body))

(defmacro write-to-png ((verb) &body body)
  `(with-png-surface-rgba ((png-pathname-string ,verb) *png-width* *png-height*)
     ,@body))

;;; Turtle

(defvar *turtle-pos* '(0 0))
(defvar *turtle-heading* 0)

;; Turtle Motion Queries

(defun pos ()
  *turtle-pos*)

(defun xcor ()
  (car *turtle-pos*))

(defun ycor ()
  (car (cdr *turtle-pos*)))

(defun heading ()
  *turtle-heading*)

(defun toward (pos) ; TODO
  "outputs a number, the heading at which the turtle should be
 facing so that it would point from its current position to 
 the position given as the input.")

;; Turtle Motion

(defun deg->rad (x) (* x (/ pi 180)))
(defun rad->deg (x) (* x (/ 180 pi)))

(defun move (dist heading)
  "Calculates new coords for forward and back"
  (let ((x-off (* dist (cos (deg->rad heading))))
        (y-off (* dist (sin (deg->rad heading)))))
    (list x-off y-off)))

(defun forward (dist)
  (let* ((newpos (move dist (heading)))
         (newx (+ (xcor) (first newpos)))
         (newy (+ (ycor) (second newpos))))
    (setxy newx newy)))

(defun back (dist)
  (let* ((newpos (move dist (+ 180 (heading))))
         (newx (+ (xcor) (first newpos)))
         (newy (+ (ycor) (second newpos))))
    (setxy newx newy)))

(defun setpos (pos)
  (setf *turtle-pos* pos))

(defun setxy (xcor ycor)
  (setf *turtle-pos* (list xcor ycor)))

(defun setx (xcor)
  (setf (car *turtle-pos*) xcor))

(defun sety (ycor)
  (setf (car (cdr *turtle-pos*)) ycor))

(defun setheading (degrees)
  (setf *turtle-heading* degrees))

(defun seth (degrees)
  (setheading degrees))

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
