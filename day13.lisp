(in-package #:advent-of-code-2021)

(defparameter *day* 13)

(defun make-grid (dimensions)
  (make-array dimensions :element-type 'boolean :initial-element nil))

(defun reflect (grid folded x y new-x new-y)
  (unless (aref folded new-y new-x)
    (setf (aref folded new-y new-x) (aref grid y x))))

(defun fold-x (grid coord)
  (destructuring-bind (height width) (array-dimensions grid)
    (let ((folded (make-grid (list height coord))))
      (loop for y below height
            do (loop for x below width
                     if (< x coord)
                       do (reflect grid folded x y x y)
                     else
                       unless (= x coord)
                         do (reflect grid folded x y (- width (1+ x)) y)))
      folded)))

(defun fold-y (grid coord)
  (destructuring-bind (height width) (array-dimensions grid)
    (let ((folded (make-grid (list coord width))))
      (loop for y below height
            do (loop for x below width
                     if (< y coord)
                       do (reflect grid folded x y x y)
                     else
                       unless (= y coord)
                         do (reflect grid folded x y x (- height (1+ y)))))
      folded)))

;; Returns a pair of (GRID FOLDS), where GRID is a 2D array of booleans
;; representing points in row/column order, i.e. accessed like (AREF GRID Y X),
;; and FOLDS is a list of (FOLD-FN COORD) pairs, where FOLD-FN is one of FOLD-X
;; or FOLD-Y, and COORD is an integer coordinate to fold along that axis.
(defmethod parse ((day (eql *day*)) (input stream))
  (multiple-value-bind (points folds)
      (loop for line in (uiop:slurp-stream-lines input)
            unless (uiop:emptyp line)
              when (digit-char-p (uiop:first-char line))
                collect (mapcar #'parse-integer
                                (uiop:split-string line :separator ","))
                  into points
            when (uiop:string-prefix-p "fold along" line)
              collect (destructuring-bind (axis coord)
                          (uiop:split-string (subseq line
                                                     (length "fold along "))
                                             :separator "=")
                        (list (if (string= axis "x") #'fold-x #'fold-y)
                              (parse-integer coord)))
                into folds
            finally (return (values points folds)))
    (let* ((dimensions (loop for (x y) in points
                             maximize x into x-max maximize y into y-max
                             finally (return (mapcar #'1+ (list y-max x-max)))))
           (grid (make-grid dimensions)))
      (loop for (x y) in points do (setf (aref grid y x) t))
      (list grid folds))))

;; How many dots are visible after completing just the first fold instruction on
;; your transparent paper?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (destructuring-bind (grid folds) input
    (destructuring-bind (fold-fn coord) (first folds)
      (setf grid (funcall fold-fn grid coord))
      (loop for i below (array-total-size grid)
            count (row-major-aref grid i)))))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 17)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))))
