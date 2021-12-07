(in-package #:advent-of-code-2021)

(defparameter *day* 7)

;; Returns a list of the horizontal position of each crab.
(defmethod parse ((day (eql *day*)) (input stream))
  (mapcar #'parse-integer
          (uiop:split-string (first (uiop:slurp-stream-lines input))
                             :separator ",")))

(defun median (list)
  (let ((len (length list))
        (sorted (sort (copy-list list) #'<)))
    (if (oddp len)
        (nth (1- (/ (1+ len) 2)) sorted)
        (/ (+ (nth (1- (/ len 2)) sorted)
              (nth (/ len 2) sorted))
           2))))

;; Determine the horizontal position that the crabs can align to using the least
;; fuel possible. How much fuel must they spend to align to that position?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (let ((med (median input)))
    (loop for x in input sum (abs (- med x)))))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 37)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))))
