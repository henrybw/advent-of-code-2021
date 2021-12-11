(in-package #:advent-of-code-2021)

(defparameter *day* 11)

;; Returns a 2D matrix of integers representing the octopus energy levels.
(defmethod parse ((day (eql *day*)) (input stream))
  (parse-matrix (uiop:slurp-stream-lines input)))

;; Returns all points in LEVELS that are adjacent (including diagonals) to the
;; given point at ROW, COL.
(defun neighbors (levels row col)
  (adjacent-points levels row col '((-1 -1) (-1 0) (-1 1)
                                    (0 -1) (0 1)
                                    (1 -1) (1 0) (1 1))))

;; During a single step, the following occurs:
;;
;;  - First, the energy level of each octopus increases by 1.
;;  - Then, any octopus with an energy level greater than 9 flashes. This
;;    increases the energy level of all adjacent octopuses by 1, including
;;    octopuses that are diagonally adjacent. If this causes an octopus to have
;;    an energy level greater than 9, it also flashes. This process continues as
;;    long as new octopuses keep having their energy level increased beyond 9.
;;    (An octopus can only flash at most once per step.)
;;  - Finally, any octopus that flashed during this step has its energy level
;;    set to 0, as it used all of its energy to flash.
(defun energy-step (levels)
  (dotimes (i (array-total-size levels))
    (incf (row-major-aref levels i)))
  (let ((flashed (make-array (array-dimensions levels)
                             :element-type 'boolean :initial-element nil)))
    (labels ((pending-flashes-p ()
               (loop for i below (array-total-size levels)
                       thereis (and (> (row-major-aref levels i) 9)
                                    (not (row-major-aref flashed i)))))
             (inc-adjacent (row col)
               (loop for (adj-row adj-col) in (neighbors levels row col)
                     unless (aref flashed adj-row adj-col)
                       do (incf (aref levels adj-row adj-col))))
             (propagate-flashes ()
               (destructuring-bind (rows cols) (array-dimensions levels)
                 (loop for row below rows
                       do (loop for col below cols
                                when (and (> (aref levels row col) 9)
                                          (not (aref flashed row col)))
                                  do (setf (aref flashed row col) t)
                                     (inc-adjacent row col))))
               (loop for i below (array-total-size levels)
                     when (row-major-aref flashed i)
                       do (setf (row-major-aref levels i) 0))))
      (loop initially (propagate-flashes)
            while (pending-flashes-p) do (propagate-flashes))
      (loop for i below (array-total-size flashed)
            count (row-major-aref flashed i)))))

;; How many total flashes are there after 100 steps?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (let ((levels (alexandria:copy-array input)))
    (loop repeat 100 sum (energy-step levels))))

;; What is the first step during which all octopuses flash?
(defmethod solve ((day (eql *day*)) (part (eql 2)) input)
  (let ((levels (alexandria:copy-array input)))
    (loop count t until (= (energy-step levels) (array-total-size levels)))))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 1656))
  (assert (= (solve *day* 2 example) 195)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))
    (format t "day~a-part2: ~a~%" *day* (solve *day* 2 input))))
