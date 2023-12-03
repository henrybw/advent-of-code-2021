(in-package #:advent-of-code-2021)

(defparameter *day* 8)

;; Returns a list of (PATTERNS DIGITS), where PATTERNS is a set of 10 unique
;; signal patterns, and DIGITS is a set of 4 digits comprising the output value.
;; Patterns and digits are sets of characters, where each character specifies a
;; wire that is turned on ('a' through 'g').
(defmethod parse ((day (eql *day*)) (input stream))
  (let ((lines (uiop:slurp-stream-lines input)))
    (mapcar
     (lambda (line)
       (let* ((pattern-set (uiop:split-string line :separator "|"))
              (pattern-strings (mapcar
                                (lambda (patterns) (uiop:split-string
                                                    (string-trim " " patterns)
                                                    :separator " "))
                                pattern-set)))
         (mapcar (lambda (str)
                   (loop for pattern in str collect (coerce pattern 'list)))
                 pattern-strings)))
     lines)))

;; In the output values, how many times do digits 1, 4, 7, or 8 appear?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (loop for (nil digits) in input
        sum (loop for digit in digits
                  count (member (length digit) '(2 4 3 7))))) ; digits 1 4 7 8

(defun decode (patterns digits)
  ;;   0:      1:      2:      3:      4:
  ;;  aaaa    ....    aaaa    aaaa    ....
  ;; b    c  .    c  .    c  .    c  b    c
  ;; b    c  .    c  .    c  .    c  b    c
  ;;  ....    ....    dddd    dddd    dddd
  ;; e    f  .    f  e    .  .    f  .    f
  ;; e    f  .    f  e    .  .    f  .    f
  ;;  gggg    ....    gggg    gggg    ....
  ;;
  ;;   5:      6:      7:      8:      9:
  ;;  aaaa    aaaa    aaaa    aaaa    aaaa
  ;; b    .  b    .  .    c  b    c  b    c
  ;; b    .  b    .  .    c  b    c  b    c
  ;;  dddd    dddd    ....    dddd    dddd
  ;; .    f  e    f  .    f  e    f  .    f
  ;; .    f  e    f  .    f  e    f  .    f
  ;;  gggg    gggg    ....    gggg    gggg
  (let ((segment->wire (make-hash-table))
        (digit->pattern (make-hash-table))
        (patterns-235 (loop for wires in patterns when (= (length wires) 5)
                            collect wires))
        (patterns-069 (loop for wires in patterns when (= (length wires) 6)
                            collect wires)))
    (flet ((segments->wires (segments)
             (loop for segment across segments
                   collect (gethash segment segment->wire)))
           (map-segment->wire (segment wire-set)
             (assert (= (length wire-set) 1))
             (setf (gethash segment segment->wire) (first wire-set))))

      ;; digits (1 4 7 8) each have a unique number of wires
      (loop for (wire-count . digit) in '((2 . 1) (4 . 4) (3 . 7) (7 . 8))
            do (setf (gethash digit digit->pattern)
                     (find-if (lambda (wires) (= (length wires) wire-count))
                              patterns)))

      ;; (diff 7 1) = (a)
      (let ((diff-7-1 (set-difference (gethash 7 digit->pattern)
                                      (gethash 1 digit->pattern))))
        (map-segment->wire #\a diff-7-1))
      ;; known: (a)

      ;; (intersect 2 3 5) = (a d g)
      ;; (intersect 4 (intersect 2 3 5)) = (d)
      (let* ((intersect-235 (reduce #'intersection patterns-235))
             (intersect-4-235 (intersection (gethash 4 digit->pattern)
                                            intersect-235)))
        (map-segment->wire #\d intersect-4-235)
        ;; known: (a d)

        ;; (diff (intersect 2 3 5) (a d)) = (g)
        (let ((diff-1-ad (set-difference intersect-235 (segments->wires "ad"))))
          (map-segment->wire #\g diff-1-ad)))
      ;; known: (a d g)

      ;; (intersect 0 6 9) = (a b f g)
      ;; (intersect 1 (intersect 0 6 9)) = (f)
      (let* ((intersect-069 (reduce #'intersection patterns-069))
             (intersect-1-069 (intersection (gethash 1 digit->pattern)
                                            intersect-069)))
        (map-segment->wire #\f intersect-1-069)
        ;; known: (a d f g)

        ;; (diff (intersect 0 6 9) (a f g)) = (b)
        (let ((diff-069-afg (set-difference intersect-069
                                            (segments->wires "afg"))))
          (map-segment->wire #\b diff-069-afg)))
      ;; known: (a b d f g)

      ;; (diff 1 (f)) = (c)
      (let ((diff-1-f (set-difference (gethash 1 digit->pattern)
                                      (list (gethash #\f segment->wire)))))
        (map-segment->wire #\c diff-1-f))
      ;; known: (a b c d f g)

      ;; (diff 8 (a b c d f g)) = (e)
      (let ((diff-8-abcdfg (set-difference (gethash 8 digit->pattern)
                                           (segments->wires "abcdfg"))))
        (map-segment->wire #\e diff-8-abcdfg))
      ;; all segments are now known

      ;; now fill in the rest of digit->pattern
      (loop for (digit . segments) in '((0 . "abcefg") (2 . "acdeg")
                                        (3 . "acdfg") (5 . "abdfg")
                                        (6 . "abdefg") (9 . "abcdfg"))
            do (setf (gethash digit digit->pattern)
                     ;; effectively behaves like FIND-IF on PATTERNS would, if a
                     ;; SET-EQUAL predicate existed...
                     (find-if-not
                      (lambda (pattern)
                        (set-exclusive-or pattern (segments->wires segments)))
                      patterns)))

      ;; finally we can decode the output digits into an integer
      (flet ((pattern-key (pattern)
               (sort (copy-list pattern) #'char<)))
        (let ((pattern->digit (make-hash-table :test #'equalp)))
          (maphash (lambda (digit pattern)
                     (setf (gethash (pattern-key pattern) pattern->digit)
                           digit))
                   digit->pattern)
          (reduce (lambda (a b) (+ (* a 10) b))
                  (loop for pattern in digits
                        collect (gethash (pattern-key pattern)
                                         pattern->digit))))))))

;; What do you get if you add up all of the output values?
(defmethod solve ((day (eql *day*)) (part (eql 2)) input)
  (loop for (patterns digits) in input sum (decode patterns digits)))

(with-input-from-string
    (test (format nil "~a~a"
           "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | "
           "cdfeb fcadb cdfeb cdbaf"))
  (destructuring-bind (patterns digits) (first (parse *day* test))
    (assert (= (decode patterns digits) 5353))))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 26))
  (assert (= (solve *day* 2 example) 61229)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))
    (format t "day~a-part2: ~a~%" *day* (solve *day* 2 input))))
