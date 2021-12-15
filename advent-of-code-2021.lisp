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

;; Returns a 2D matrix of integers from the given list of strings LINES, each
;; line consisting of digit characters.
(defun parse-matrix (lines)
  (let* ((rows (length lines))
         (cols (length (first lines)))
         (matrix (make-array (list rows cols))))
    (loop for line in lines for row below rows
          do (loop for char across line for col below cols
                   do (setf (aref matrix row col)
                            (parse-integer (string char)))))
    matrix))

;; Returns all points in the 2D array MATRIX that are adjacent to the given
;; point at ROW, COL using the given list of DIRECTIONS, where each direction is
;; a pair of (ROW-OFFSET COL-OFFSET) offsets.
(defun adjacent-points (matrix row col directions)
  (loop for (row-offset col-offset) in directions
        for adj-row = (+ row row-offset)
        for adj-col = (+ col col-offset)
        when (array-in-bounds-p matrix adj-row adj-col)
          collect (list adj-row adj-col)))

;; Splits the string STR separated by the substring SEPARATOR into a list of two
;; strings: the substring before the separator and the substring after.
(defun split-by (str separator)
  (let ((start (search separator str)))
    (list (subseq str 0 start) (subseq str (+ start (length separator))))))

;; Prints all key/value pairs in HASH-TABLE.
(defun printhash (hash-table)
  (maphash (lambda (k v) (format t "~a -> ~a~%" k v)) hash-table))
