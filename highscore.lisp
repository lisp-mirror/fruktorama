;; highscore.lisp
;; Copyright Parasite Network 2018
;; GPL3

(defpackage :highscore
  (:use :cl)
  (:export
    :worthy?
    :add
    :load-from
    :save-to
    :set-defaults
    :reset
    :delete-data
    :get-entry
    :number-of-entries))

(in-package :highscore)

(defparameter *DEFAULT-HIGHSCORE-LIST* nil)

(defparameter *HIGHSCORE-LIST* nil)

(defun number-of-entries ()
  (length *HIGHSCORE-LIST*))

(defun set-defaults (defaults)
  "Must be an assoc list with each entry being (points . name)."
  (setf *DEFAULT-HIGHSCORE-LIST* defaults))

(defun worthy? (points)
  "Checks if the points are enough to be on the highscore list."
  (when *HIGHSCORE-LIST*
    (position points *HIGHSCORE-LIST* :key #'car :test #'>=)))

(defun add (points name)
  "Adds the new entry to the highscore list, sorts it and then drops the last entry."
  (setf *HIGHSCORE-LIST*
        (butlast
          (sort
            (cons (cons points name) *HIGHSCORE-LIST*)
            #'> 
            :key #'car))))

(defun delete-data (path)
  (when (probe-file path)
    (delete-file path)))

(defun reset ()
  (setf *HIGHSCORE-LIST* (copy-list *DEFAULT-HIGHSCORE-LIST*)))

(defun load-from (path)
  (reset)
  (if (probe-file path)
    (with-open-file (in path :direction :input)
                    (when in
                      (let ((scores (read in)))
                        (when scores
                          (setf *HIGHSCORE-LIST* scores)))))))

(defun save-to (path)
  (when *HIGHSCORE-LIST*
    (delete-data path)
    (with-open-file (out path :direction :output)
                    (write *HIGHSCORE-LIST* :stream out))))
   
(defun get-entry (n)
  (nth n *HIGHSCORE-LIST*))