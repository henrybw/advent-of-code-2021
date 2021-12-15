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

(defun step-grow (polymer rules)
  (labels ((insert (a b rules)
             (let ((insertion (gethash (cons a b) rules)))
               ;; second char will be appended as A in the loop's next ON clause
               (if insertion
                   (list a insertion)
                   (list a)))))
    (loop for (a b) on polymer
          if b
            append (insert a b rules)
          else
            collect a)))

;; Apply 10 steps of pair insertion to the polymer template and find the most
;; and least common elements in the result. What do you get if you take the
;; quantity of the most common element and subtract the quantity of the least
;; common element?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (destructuring-bind (template rules) input
    (let ((polymer (coerce template 'list)))
      (dotimes (i 10)
        (setf polymer (step-grow polymer rules)))
      (loop for element in (remove-duplicates polymer)
            maximize (count element polymer) into most
            minimize (count element polymer) into least
            finally (return (- most least))))))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 1588)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))))
