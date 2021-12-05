(in-package #:advent-of-code-2021)

(defparameter *day* 1)

;; Each line is a measurement of the sea floor depth as the sweep looks further
;; and further away from the submarine.
(defmethod parse ((day (eql *day*)) (input stream))
  (loop for line = (read-line input nil) while line
        collect (parse-integer line)))

;; Count the number of times a depth measurement increases from the previous
;; measurement.
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (loop for (prev next) on input while next
        count (> next prev)))

;; Consider sums of a three-measurement sliding window. Count the number of
;; times the sum of measurements in this sliding window increases from the
;; previous sum.
(defmethod solve ((day (eql *day*)) (part (eql 2)) input)
  (let ((sums (loop for (m1 m2 m3) on input while m3
                    collect (+ m1 m2 m3))))
    (solve day 1 sums)))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 7))
  (assert (= (solve *day* 2 example) 5)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))
    (format t "day~a-part2: ~a~%" *day* (solve *day* 2 input))))
