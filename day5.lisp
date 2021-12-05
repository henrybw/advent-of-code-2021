(in-package #:advent-of-code-2021)

(defparameter *day* 5)

;; Returns a list of line segments, each represented as two endpoints:
;; ((X1 Y1) (X2 Y2)).
(defmethod parse ((day (eql *day*)) (input stream))
  (loop for line in (uiop:slurp-stream-lines input)
        collect (mapcar (lambda (endpoint)
                          (mapcar #'parse-integer
                                  (uiop:split-string endpoint :separator ",")))
                        ;; filter empty strings
                        (remove-if (lambda (s) (= (length s) 0))
                                   (uiop:split-string line :separator "->")))))

(defun bounds (segments)
  (loop for ((x1 y1) (x2 y2)) in segments
        maximize (max x1 x2) into width
        maximize (max y1 y2) into height
        finally (return (mapcar #'1+ (list width height)))))

(defun diagonalp (segment)
  (destructuring-bind ((x1 y1) (x2 y2)) segment
    (not (or (= x1 x2) (= y1 y2)))))

(defun overlapping-points (segments)
  (let ((points (make-array (bounds segments))))
    (dolist (segment segments)
      (destructuring-bind ((x1 y1) (x2 y2)) segment
        (if (diagonalp segment)
            (loop for x = x1 then (if (> x2 x1) (incf x) (decf x))
                  for y = y1 then (if (> y2 y1) (incf y) (decf y))
                  do (incf (aref points x y))
                  until (and (= x x2) (= y y2)))
            (loop for x from (min x1 x2) to (max x1 x2)
                  do (loop for y from (min y1 y2) to (max y1 y2)
                           do (incf (aref points x y)))))))
    (loop for i below (array-total-size points)
          count (> (row-major-aref points i) 1))))

;; Consider only horizontal and vertical lines. At how many points do at least
;; two lines overlap?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (overlapping-points (remove-if #'diagonalp input)))

;; Consider all of the lines. At how many points do at least two lines overlap?
(defmethod solve ((day (eql *day*)) (part (eql 2)) input)
  (overlapping-points input))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 5))
  (assert (= (solve *day* 2 example) 12)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))
    (format t "day~a-part2: ~a~%" *day* (solve *day* 2 input))))
