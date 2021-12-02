(in-package #:advent-of-code-2021)

;; Each line is a measurement of the sea floor depth as the sweep looks further
;; and further away from the submarine.
(defmethod parse ((day (eql 1)) (input stream))
  (loop for line = (read-line input nil) while line
        collect (parse-integer line)))

;; Count the number of times a depth measurement increases from the previous
;; measurement.
(defun day1-part1 (input)
  (loop for (prev next) on input while next
        count (> next prev)))

;; Consider sums of a three-measurement sliding window. Count the number of
;; times the sum of measurements in this sliding window increases from the
;; previous sum.
(defun day1-part2 (input)
  (let ((sums (loop for (m1 m2 m3) on input while m3
                    collect (+ m1 m2 m3))))
    (day1-part1 sums)))

(let ((example (parse 1 "example")))
  (assert (= (day1-part1 example) 7))
  (assert (= (day1-part2 example) 5)))

(let ((input (parse 1 "input")))
  (when input
    (format t "day1-part1: ~a~%" (day1-part1 input))
    (format t "day1-part2: ~a~%" (day1-part2 input))))
