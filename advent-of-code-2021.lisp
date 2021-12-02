(in-package #:advent-of-code-2021)

(defun ./ (path)
  (asdf:system-relative-pathname :advent-of-code-2021 path))

(defun day-pathname (n path)
  (./ (make-pathname :directory (list :relative "day" (write-to-string n))
                     :name path)))

(defgeneric parse (day input))

(defmethod parse ((day integer) (input string))
  (with-open-file (stream (day-pathname day input) :if-does-not-exist nil)
    (when stream
      (parse day stream))))

(defgeneric solve (day part input))
