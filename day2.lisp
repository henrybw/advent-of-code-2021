(in-package #:advent-of-code-2021)

(defparameter *day* 2)

;; Returns a list of (dx dy) pairs.
(defmethod parse ((day (eql *day*)) (input stream))
  (flet ((parse-line (line)
           (destructuring-bind (direction value)
               (uiop:split-string line :separator " ")
             (let ((value (parse-integer value)))
               (cond ((string= direction "forward") (list value 0))
                     ((string= direction "down")    (list 0 value))
                     ((string= direction "up")      (list 0 (- value))))))))
    (loop for line in (uiop:slurp-stream-lines input)
          collect (parse-line line))))

;; It seems like the submarine can take a series of commands like "forward 1",
;; "down 2", or "up 3":
;;
;;  - "forward X" increases the horizontal position by X units.
;;  - "down X" increases the depth by X units.
;;  - "up X" decreases the depth by X units.
;;
;; What do you get if you multiply your final horizontal position by your final
;; depth?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (loop for (dx dy) in input while dy
        sum dx into x
        sum dy into y
        finally (return (* x y))))

;; In addition to horizontal position and depth, you'll also need to track a
;; third value, aim, which also starts at 0. The commands also mean something
;; entirely different than you first thought:
;;
;;  - "down X" increases your aim by X units.
;;  - "up X" decreases your aim by X units.
;;  - "forward X" does two things:
;;    - It increases your horizontal position by X units.
;;    - It increases your depth by your aim multiplied by X.
;;
;; Using this new interpretation of the commands, calculate the horizontal
;; position and depth you would have after following the planned course. What do
;; you get if you multiply your final horizontal position by your final depth?
(defmethod solve ((day (eql *day*)) (part (eql 2)) input)
  (loop for (dx dy) in input while dy
        sum dx into x
        sum dy into aim
        sum (* aim dx) into y
        finally (return (* x y))))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 150))
  (assert (= (solve *day* 2 example) 900)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))
    (format t "day~a-part2: ~a~%" *day* (solve *day* 2 input))))
