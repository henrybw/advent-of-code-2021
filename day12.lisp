(in-package #:advent-of-code-2021)

(defparameter *day* 12)

;; Returns a hash table of nodes mapped to their adjacent edges.
(defmethod parse ((day (eql *day*)) (input stream))
  (let ((edges (mapcar (lambda (line) (uiop:split-string line :separator "-"))
                       (uiop:slurp-stream-lines input)))
        (node->adjacent (make-hash-table :test #'equal)))
    (loop for (a b) in edges
          do (pushnew a (gethash b node->adjacent) :test #'equal)
             (pushnew b (gethash a node->adjacent) :test #'equal))
    node->adjacent))

;; Find the number of distinct paths that start at "start", end at "end", and
;; don't visit small caves more than once.
(defmethod solve ((day (eql *day*)) (part (eql 1)) node->adjacent)
  (labels ((explore (node visited)
             (when (string= node "end")
               (return-from explore 1))
             (unless (every #'upper-case-p node)
               (pushnew node visited :test #'equal))
             (loop for adjacent in (gethash node node->adjacent)
                   unless (member adjacent visited :test #'equal)
                     sum (explore adjacent visited))))
    (explore "start" nil)))

;; After reviewing the available paths, you realize you might have time to visit
;; a single small cave twice. However, the caves named "start" and "end" can
;; only be visited exactly once each. Given these new rules, how many paths
;; through this cave system are there?
(defmethod solve ((day (eql *day*)) (part (eql 2)) node->adjacent)
  (labels ((explore (node visited revisit-exception)
             (when (string= node "end")
               (return-from explore 1))
             (unless (every #'upper-case-p node)
               (pushnew node visited :test #'equal))
             (loop for adjacent in (gethash node node->adjacent)
                   if (and (member adjacent visited :test #'equal)
                           (null revisit-exception)
                           (string/= adjacent "start"))
                     sum (explore adjacent visited adjacent)
                   else
                     unless (member adjacent visited :test #'equal)
                       sum (explore adjacent visited revisit-exception))))
    (explore "start" nil nil)))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 10))
  (assert (= (solve *day* 2 example) 36)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))
    (format t "day~a-part2: ~a~%" *day* (solve *day* 2 input))))
