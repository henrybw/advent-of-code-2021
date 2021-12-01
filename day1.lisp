(in-package #:advent-of-code-2021)

;; Each line is a measurement of the sea floor depth as the sweep looks further
;; and further away from the submarine.
(defun day1-input (file)
  (with-open-file (stream (day-input 1 file))
    (loop for line = (read-line stream nil)
          while line
          collect (parse-integer line))))

;; Count the number of times a depth measurement increases from the previous
;; measurement.
(defun day1-part1 (input)
  (loop for (prev next) on input while next
        count (> next prev)))

;; TODO macro-ify this or something
(let ((example (day1-input "example"))
      (input (day1-input "input")))
  (assert (= (day1-part1 example) 7))
  (format t "day1-part1: ~a~%" (day1-part1 input)))
