(in-package #:advent-of-code-2021)

(require :asdf)

;; Returns a list of (dx dy) pairs.
(defmethod parse ((day (eql 2)) (input stream))
  (flet ((parse-line (line)
           (destructuring-bind (direction value)
               (uiop:split-string (string-trim '(#\Newline) line)
                                  :separator " ")
             (let ((value (parse-integer value)))
               (cond ((string= direction "forward") (list value 0))
                     ((string= direction "down")    (list 0 value))
                     ((string= direction "up")      (list 0 (- value))))))))
    (loop for line = (read-line input nil) while line
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
(defmethod solve ((day (eql 2)) (part (eql 1)) input)
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
(defmethod solve ((day (eql 2)) (part (eql 2)) input)
  (loop for (dx dy) in input while dy
        sum dx into x
        sum dy into aim
        sum (* aim dx) into y
        finally (return (* x y))))

(let ((example (parse 2 "example")))
  (assert (= (solve 2 1 example) 150))
  (assert (= (solve 2 2 example) 900)))

(let ((input (parse 2 "input")))
  (when input
    (format t "day2-part1: ~a~%" (solve 2 1 input))
    (format t "day2-part2: ~a~%" (solve 2 2 input))))
