(in-package #:advent-of-code-2021)

(defparameter *day* 14)

;; Returns a list of (POLYMER-TEMPLATE PAIR-INSERTIONS), where POLYMER-TEMPLATE
;; is a string, and PAIR-INSERTIONS is hash table mapping pairs of characters,
;; e.g. (#\A . #\B), to the character to insert between them.
(defmethod parse ((day (eql *day*)) (input stream))
  (let* ((lines (uiop:slurp-stream-lines input))
         (polymer-template (first lines))
         (pair-insertions (make-hash-table :test #'equal)))
    (mapcar (lambda (rule) (destructuring-bind (pair insertion)
                               (split-by rule " -> ")
                             (setf (gethash (cons (char pair 0) (char pair 1))
                                            pair-insertions)
                                   (char insertion 0))))
            (subseq lines 2))
    (list polymer-template pair-insertions)))

(defun ensure-incf (table key)
  (alexandria:ensure-gethash key table 0)
  (incf (gethash key table)))

(defun remove-decf (table key)
  (if (= (gethash key table) 1)
      (remhash key table)
      (decf (gethash key table))))

(defun most-least-diff-after (steps polymer rules)
  (let ((elem-counts (make-hash-table))
        (pair-counts (make-hash-table :test #'equal)))
    (loop for elem in polymer
          do (ensure-incf elem-counts elem))
    (loop for (a b) on polymer
          when b do (ensure-incf pair-counts (cons a b)))
    (dotimes (i steps)
      (let (pairs-inserted pairs-removed)
        (loop for pair being the hash-key using (hash-value insertion) of rules
              do (let ((occurs (gethash pair pair-counts)))
                   (when occurs
                     (let ((insertion (gethash pair rules)))
                       (when insertion
                         (dotimes (j occurs)
                           (ensure-incf elem-counts insertion)
                           (push (cons (car pair) insertion) pairs-inserted)
                           (push (cons insertion (cdr pair)) pairs-inserted)
                           (push pair pairs-removed)))))))
        (dolist (pair pairs-inserted)
          (ensure-incf pair-counts pair))
        (dolist (pair pairs-removed)
          (remove-decf pair-counts pair))))
    (loop for occurs in (alexandria:hash-table-values elem-counts)
          maximize occurs into most
          minimize occurs into least
          finally (return (- most least)))))

;; Apply 10 steps of pair insertion to the polymer template and find the most
;; and least common elements in the result. What do you get if you take the
;; quantity of the most common element and subtract the quantity of the least
;; common element?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (destructuring-bind (template rules) input
    (let ((polymer (coerce template 'list)))
      (most-least-diff-after 10 polymer rules))))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 1588)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))))
