(in-package #:advent-of-code-2021)

(defun ./ (path)
  (asdf:system-relative-pathname :advent-of-code-2021 path))

(defun day-pathname (n path)
  (./ (make-pathname :directory (list :relative "day" (write-to-string n))
                     :name path)))

;; Returns all points in the 2D array MATRIX that are adjacent to the given
;; point at ROW, COL using the given list of DIRECTIONS, where each direction is
;; a pair of (ROW-OFFSET COL-OFFSET) offsets.
(defun adjacent-points (matrix row col directions)
  (loop for (row-offset col-offset) in directions
        for adj-row = (+ row row-offset)
        for adj-col = (+ col col-offset)
        when (array-in-bounds-p matrix adj-row adj-col)
          collect (list adj-row adj-col)))

(defgeneric parse (day input))

(defmethod parse ((day integer) (input string))
  (with-open-file (stream (day-pathname day input) :if-does-not-exist nil)
    (when stream
      (parse day stream))))

(defgeneric solve (day part input))
