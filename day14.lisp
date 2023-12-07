(in-package #:advent-of-code-2021)

(defparameter *day* 14)

(defun ensure-incf (table key)
  (alexandria:ensure-gethash key table 0)
  (incf (gethash key table)))

(defun ensure-add (table key val)
  (alexandria:ensure-gethash key table 0)
  (setf (gethash key table) (+ (gethash key table) val)))

;; Returns a list of (ELEM-COUNTS PAIR-COUNTS PAIR-EXPANSIONS), where
;; ELEM-COUNTS is a hash table mapping characters to a count of their
;; appearances in the polymer template, PAIR-COUNTS is a hash table mapping
;; character pairs (e.g. (#\A . #\B)) to a count of their appearances in the
;; polymer template, and PAIR-EXPANSIONS is hash table mapping character pairs
;; to a list of the two other character pairs they expand to.
(defmethod parse ((day (eql *day*)) (input stream))
  (let* ((lines (uiop:slurp-stream-lines input))
         (template (coerce (first lines) 'list))
         (elem-counts (make-hash-table))
         (pair-counts (make-hash-table :test #'equal))
         (pair-expansions (make-hash-table :test #'equal)))
    (loop for e in template
          do (ensure-incf elem-counts e))
    (loop for (e1 e2) on template while e2
          do (ensure-incf pair-counts (cons e1 e2)))
    (mapcar (lambda (rule) (destructuring-bind (pair insertion)
                               (split-by rule " -> ")
                             (setf (gethash (cons (char pair 0) (char pair 1))
                                            pair-expansions)
                                   (list
                                    (cons (char pair 0) (char insertion 0))
                                    (cons (char insertion 0) (char pair 1))))))
            (subseq lines 2))
    (list elem-counts pair-counts pair-expansions)))

(defun apply-pair-insertions (steps elem-counts pair-counts pair-expansion)
  (let ((pair-changes (make-hash-table :test #'equal)))
    (dotimes (i steps)
      (clrhash pair-changes)
      (maphash
       (lambda (old-pair count)
         (assert (>= count 0))
         (let ((expansion (gethash old-pair pair-expansion)))
           (when expansion
             (destructuring-bind (new-pair1 new-pair2) expansion
               (ensure-add pair-changes new-pair1 count)
               (ensure-add pair-changes new-pair2 count)
               (ensure-add pair-changes old-pair (- count))

               (assert (eql (cdr new-pair1) (car new-pair2)))
               (ensure-add elem-counts (cdr new-pair1) count)))))
       pair-counts)
      (maphash
       (lambda (pair delta)
         (multiple-value-bind (old-count presentp) (gethash pair pair-counts)
           (setf (gethash pair pair-counts)
                 (if presentp (+ old-count delta) delta))))
       pair-changes))

    (loop for occurs in (alexandria:hash-table-values elem-counts)
          maximize occurs into most
          minimize occurs into least
          finally (return (- most least)))))

;; Apply 10 steps of pair insertion to the polymer template and find the most
;; and least common elements in the result. What do you get if you take the
;; quantity of the most common element and subtract the quantity of the least
;; common element?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (destructuring-bind (elem-counts pair-counts pair-expansions) input
    (apply-pair-insertions 10
                           (alexandria:copy-hash-table elem-counts)
                           (alexandria:copy-hash-table pair-counts)
                           (alexandria:copy-hash-table pair-expansions))))

;; 40 steps of pair insertion
(defmethod solve ((day (eql *day*)) (part (eql 2)) input)
  (destructuring-bind (elem-counts pair-counts pair-expansions) input
    (apply-pair-insertions 40
                           (alexandria:copy-hash-table elem-counts)
                           (alexandria:copy-hash-table pair-counts)
                           (alexandria:copy-hash-table pair-expansions))))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 1588))
  (assert (= (solve *day* 2 example) 2188189693529)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))
    (format t "day~a-part2: ~a~%" *day* (solve *day* 2 input))))
