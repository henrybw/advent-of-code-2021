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

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 40)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))))
