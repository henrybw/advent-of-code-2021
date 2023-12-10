(in-package #:advent-of-code-2021)

(defparameter *day* 15)

;; Returns a 2D matrix of integers representing the given risk map.
(defmethod parse ((day (eql *day*)) (input stream))
  (parse-matrix (uiop:slurp-stream-lines input)))

;; Returns the minimal risk (cost of the shortest path) from the top left to the
;; bottom right of MAP, a 2D adjacency matrix of risk levels.
(defun minimize-risk (map)
  (let ((pos '(0 0))
        (goal (mapcar #'1- (array-dimensions map)))
        (frontier (make-instance 'pairing-heap))
        (costs (make-array (array-dimensions map)
                           :initial-element most-positive-fixnum))
        (visited (make-array (array-dimensions map)
                             :element-type 'boolean :initial-element nil)))
    (setf (apply #'aref costs pos) 0)
    (insert frontier 0 pos)
    (loop until (equal pos goal)
          do (progn
               (setf pos (extract-min frontier))
               (setf (apply #'aref visited pos) t)
               (let ((path-cost (apply #'aref costs pos)))
                 (loop for neighbor in (apply #'adjacent-cardinal map pos)
                       unless (apply #'aref visited neighbor)
                         do (let* ((edge-cost (apply #'aref map neighbor))
                                   (new-cost (+ path-cost edge-cost)))
                              (when (< (+ path-cost edge-cost)
                                       (apply #'aref costs neighbor))
                                (setf (apply #'aref costs neighbor) new-cost)
                                (insert frontier new-cost neighbor))))))
          finally (return (apply #'aref costs goal)))))

(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (minimize-risk input))

(defun scale-map (map times)
  ;; scaled value is original value + (tile-x + tile-y):
  ;;
  ;; 0,0     1,0     2,0     3,0      4,0
  ;; 0,1     1,1     2,1     3,1      4,1
  ;; 0,2     1,2     2,2     3,2      4,2
  ;; 0,3     1,3     2,3     3,3      4,3
  ;; 0,4     1,4     2,4     3,4      4,4
  ;;
  ;; +0   (0,0)
  ;; +1   (0,1), (1,0)
  ;; +2   (0,2), (1,1), (2,0)
  ;; +3   (0,3), (1,2), (2,1), (3,0)
  ;; +4   (0,4), (1,3), (2,2), (3,1), (4,0)
  ;; +5   (1,4), (2,3), (3,2), (4,1)
  ;; +6   (2,4), (3,3), (4,2)
  ;; +7   (3,4), (4,3)
  ;; +8   (4,4)
  (let ((scaled (make-array (mapcar (lambda (x) (* x times))
                                    (array-dimensions map)))))
    (dotimes (tile-x times)
      (dotimes (tile-y times)
        (destructuring-bind (rows cols) (array-dimensions map)
          (loop for row below rows
                do (loop for col below cols
                         do (let* ((scaled-row (+ (* tile-x rows) row))
                                   (scaled-col (+ (* tile-y cols) col))
                                   (orig-val (aref map row col))
                                   (new-val (1+ (mod (+ (+ tile-x tile-y)
                                                        (1- orig-val))
                                                     9))))
                              (setf (aref scaled scaled-row scaled-col)
                                    new-val)))))))
    scaled))

(defmethod solve ((day (eql *day*)) (part (eql 2)) input)
  (minimize-risk (scale-map input 5)))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 40))
  (assert (= (solve *day* 2 example) 315)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))
    (format t "day~a-part2: ~a~%" *day* (solve *day* 2 input))))
