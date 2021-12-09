(in-package #:advent-of-code-2021)

(defparameter *day* 8)

;; Returns a list of (PATTERNS DIGITS), where PATTERNS is a set of 10 unique
;; signal patterns, and DIGITS is a set of 4 digits comprising the output value.
;; Patterns and digits are sets of characters, where each character specifies a
;; wire that is turned on ('a' through 'g').
(defmethod parse ((day (eql *day*)) (input stream))
  (let ((lines (uiop:slurp-stream-lines input)))
    (mapcar
     (lambda (line)
       (let* ((pattern-set (uiop:split-string line :separator "|"))
              (pattern-strings (mapcar
                                (lambda (patterns) (uiop:split-string
                                                    (string-trim " " patterns)
                                                    :separator " "))
                                pattern-set)))
         (mapcar (lambda (str)
                   (loop for pattern in str collect (coerce pattern 'list)))
                 pattern-strings)))
     lines)))

;; In the output values, how many times do digits 1, 4, 7, or 8 appear?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (loop for (nil digits) in input
        sum (loop for digit in digits
                  count (member (length digit) '(2 4 3 7))))) ; digits 1 4 7 8

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 26)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))))
