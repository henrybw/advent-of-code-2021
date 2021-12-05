(in-package #:advent-of-code-2021)

(defparameter *day* 5)

;; Returns a list of line segments, each represented as two endpoints:
;; ((X1 Y1) (X2 Y2)).
(defmethod parse ((day (eql *day*)) (input stream))
  (loop for line in (uiop:slurp-stream-lines input)
        collect (mapcar (lambda (endpoint)
                          (mapcar #'parse-integer
                                  (uiop:split-string endpoint :separator ",")))
                        ;; filter empty strings
                        (remove-if (lambda (s) (= (length s) 0))
                                   (uiop:split-string line :separator "->")))))

(defun bounds (segments)
  (loop for ((x1 y1) (x2 y2)) in segments
        maximize (max x1 x2) into width
        maximize (max y1 y2) into height
        finally (return (mapcar #'1+ (list width height)))))

;; Consider only horizontal and vertical lines. At how many points do at least
;; two lines overlap?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (let* ((nondiagonals (remove-if-not (lambda (segment)
                                        (destructuring-bind ((x1 y1) (x2 y2))
                                            segment
                                          (or (= x1 x2) (= y1 y2))))
                                      input))
         (points (make-array (bounds nondiagonals))))
    (dolist (segment nondiagonals)
      (destructuring-bind ((x1 y1) (x2 y2)) segment
        (loop for x from (min x1 x2) to (max x1 x2)
              do (loop for y from (min y1 y2) to (max y1 y2)
                       do (incf (aref points x y))))))
    (loop for i below (array-total-size points)
          count (> (row-major-aref points i) 1))))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 5)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))))
