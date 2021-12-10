(in-package #:advent-of-code-2021)

(defparameter *day* 9)

;; Returns a 2D matrix of integers representing the given heightmap.
(defmethod parse ((day (eql *day*)) (input stream))
  (let* ((lines (uiop:slurp-stream-lines input))
         (rows (length lines))
         (cols (length (first lines)))
         (heightmap (make-array (list rows cols))))
    (loop for line in lines for row below rows
          do (loop for char across line for col below cols
                   do (setf (aref heightmap row col)
                            (parse-integer (string char)))))
    heightmap))

;; Your first goal is to find the low points - the locations that are lower than
;; any of its adjacent locations.
(defun find-low-points (heightmap)
  (destructuring-bind (rows cols) (array-dimensions heightmap)
    (flet ((adjacent-points (row col)
             ;; up, down, left, right
             (loop for (row-offset col-offset) in '((-1 0) (1 0) (0 -1) (0 1))
                   for adj-row = (+ row row-offset)
                   for adj-col = (+ col col-offset)
                   unless (or (minusp adj-row) (minusp adj-col)
                              (>= adj-row rows) (>= adj-col cols))
                     collect (aref heightmap adj-row adj-col))))
      (loop for row below rows
            append (loop for col below cols
                         when (let ((this-value (aref heightmap row col))
                                    (adjacent (adjacent-points row col)))
                                (every (lambda (x) (< this-value x)) adjacent))
                           collect (list row col))))))

;; The risk level of a low point is 1 plus its height. What is the sum of the
;; risk levels of all low points on your heightmap?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (loop for (row col) in (find-low-points input) sum (1+ (aref input row col))))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 15)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))))
