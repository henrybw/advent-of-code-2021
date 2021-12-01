(in-package #:advent-of-code-2021)

(defun ./ (path)
  (asdf:system-relative-pathname :advent-of-code-2021 path))

(defun day-file (n file)
  (./ (make-pathname :directory (list :relative "day" (write-to-string n))
                     :name file)))
