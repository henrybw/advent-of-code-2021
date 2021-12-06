(in-package #:advent-of-code-2021)

(defparameter *day* 6)

;; Returns a list of lanternfish timers.
(defmethod parse ((day (eql *day*)) (input stream))
  (mapcar #'parse-integer
          (uiop:split-string (first (uiop:slurp-stream-lines input))
                             :separator ",")))

;; Returns a hash table of each possible lanternfish timer value (0..8) ->
;; the number of occurrences of that timer value in TIMERS. If TIMERS is
;; omitted, returns a hash table with all counts set to 0.
(defun make-timer-counts (&optional timers)
  (let ((timer-counts (make-hash-table)))
    (dotimes (i 9)
      (setf (gethash i timer-counts) 0))
    (when timers
      (dolist (timer timers)
        (incf (gethash timer timer-counts))))
    timer-counts))

;; Each day, a 0 becomes a 6 and adds a new 8 to the end of the list, while each
;; other number decreases by 1 if it was present at the start of the day.
(defun tick (timer-counts)
  (let ((zeros (gethash 0 timer-counts)))
    (setf (gethash 0 timer-counts) 0)
    ;; shift the counts for each timer value n into n-1
    (loop for i from 1 to 8
          when (> (gethash i timer-counts) 0)
            do (setf (gethash (1- i) timer-counts) (gethash i timer-counts))
               (setf (gethash i timer-counts) 0))
    (setf (gethash 6 timer-counts) (+ zeros (gethash 6 timer-counts)))
    (setf (gethash 8 timer-counts) (+ zeros (gethash 8 timer-counts)))))

(defun lanternfish-after (days initial-timers)
  (let ((timer-counts (make-timer-counts initial-timers)))
    (dotimes (i days)
      (tick timer-counts))
    (loop for timer being the hash-key using (hash-value count) of timer-counts
          sum count)))

;; How many lanternfish would there be after 80 days?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (lanternfish-after 80 input))

;; How many lanternfish would there be after 256 days?
(defmethod solve ((day (eql *day*)) (part (eql 2)) input)
  (lanternfish-after 256 input))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 5934))
  (assert (= (solve *day* 2 example) 26984457539)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))
    (format t "day~a-part2: ~a~%" *day* (solve *day* 2 input))))
