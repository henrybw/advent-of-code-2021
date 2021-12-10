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

;; Returns all points in HEIGHTMAP that are to the up, down, left, and right of
;; the given point at ROW, COL.
(defun adjacent-points (heightmap row col)
  (destructuring-bind (rows cols) (array-dimensions heightmap)
    (loop for (row-offset col-offset) in '((-1 0) (1 0) (0 -1) (0 1))
          for adj-row = (+ row row-offset)
          for adj-col = (+ col col-offset)
          unless (or (minusp adj-row) (minusp adj-col)
                     (>= adj-row rows) (>= adj-col cols))
            collect (list adj-row adj-col))))

;; Your first goal is to find the low points - the locations that are lower than
;; any of its adjacent locations.
(defun low-point-p (heightmap row col)
  (let ((this-value (aref heightmap row col))
        (adjacent (loop for (row col) in (adjacent-points heightmap row col)
                         collect (aref heightmap row col))))
    (every (lambda (x) (< this-value x)) adjacent)))

(defun find-low-points (heightmap)
  (destructuring-bind (rows cols) (array-dimensions heightmap)
    (loop for row below rows
          append (loop for col below cols
                       when (low-point-p heightmap row col)
                         collect (list row col)))))

;; The risk level of a low point is 1 plus its height. What is the sum of the
;; risk levels of all low points on your heightmap?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (loop for (row col) in (find-low-points input) sum (1+ (aref input row col))))

;; A basin is all locations that eventually flow downward to a single low point.
;; Locations of height 9 do not count as being in any basin, and all other
;; locations will always be part of exactly one basin.
(defun basin-size (heightmap low-point)
  (let ((visited (make-array (array-dimensions heightmap)
                             :element-type 'boolean :initial-element nil)))
    (labels ((explore (point)
               (destructuring-bind (row col) point
                 (unless (aref visited row col)
                   (let ((val (aref heightmap row col))
                         (adjacents (adjacent-points heightmap row col)))
                     (unless (= val 9)
                       (setf (aref visited row col) t)
                       (dolist (adjacent adjacents)
                         (explore adjacent))))))))
      (explore low-point)
      (loop for i below (array-total-size visited)
            count (row-major-aref visited i)))))

(defun basin-sizes (heightmap)
  (loop for low-point in (find-low-points heightmap)
        collect (basin-size heightmap low-point)))

;; What do you get if you multiply together the sizes of the three largest
;; basins?
(defmethod solve ((day (eql *day*)) (part (eql 2)) input)
  (reduce #'* (subseq (sort (basin-sizes input) #'>) 0 3)))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 15))
  (assert (= (solve *day* 2 example) 1134)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))
    (format t "day~a-part2: ~a~%" *day* (solve *day* 2 input))))
