(in-package #:advent-of-code-2021)

(defparameter *day* 6)

;; Returns a list of lanternfish timers.
(defmethod parse ((day (eql *day*)) (input stream))
  (mapcar #'parse-integer
          (uiop:split-string (first (uiop:slurp-stream-lines input))
                             :separator ",")))

;; Each day, a 0 becomes a 6 and adds a new 8 to the end of the list, while each
;; other number decreases by 1 if it was present at the start of the day.
(defun tick (timers)
  (loop for timer in timers
        if (= timer 0)
          collect 6 into next and collect 8 into spawns
        else
          collect (1- timer) into next
        finally (return (append next spawns))))

;; How many lanternfish would there be after 80 days?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (let ((state (copy-list input)))
    (dotimes (i 80)
      (setf state (tick state)))
    (length state)))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 5934)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))))
