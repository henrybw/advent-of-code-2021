(in-package #:advent-of-code-2021)

(defparameter *day* 3)

;; Returns a 2D array of bits.
(defmethod parse ((day (eql *day*)) (input stream))
  (let ((lines (uiop:slurp-stream-lines input)))
    (destructuring-bind (rows cols)
        (loop for line in lines
              count line into rows
              maximize (length line) into cols
              finally (return (list rows cols)))
      (let ((bit-matrix (make-array (list rows cols) :element-type 'bit)))
        (loop for line in lines
              count line into row
              do (loop for char across line
                       count char into col
                       do (setf (bit bit-matrix (- row 1) (- col 1))
                                (if (char= char #\1) 1 0))))
        bit-matrix))))

(defun column-major-bits (bit-matrix col)
  (let* ((rows (array-dimension bit-matrix 0))
         (col-major (make-array rows :element-type 'bit)))
    (loop for row below rows
          for b = (bit bit-matrix row col)
          do (setf (bit col-major row) b))
    col-major))

(defun row-major-bits (bit-matrix row)
  (let* ((cols (array-dimension bit-matrix 1))
         (row-major (make-array cols :element-type 'bit)))
    (loop for col below cols
          for b = (bit bit-matrix row col)
          do (setf (bit row-major col) b))
    row-major))

(defun count-bits (bits)
  (loop for b across bits
        count (= b 1) into ones
        count (= b 0) into zeros
        finally (return (list ones zeros))))

;; Returns DEFAULT if 1 and 0 are equally common.
(defun common-bit (ones-zeros-cmp bits default)
  (destructuring-bind (ones zeros) (count-bits bits)
    (if (= ones zeros) default
        (if (funcall ones-zeros-cmp ones zeros) 1 0))))

(defun most-common-bit (bits default)
  (common-bit #'> bits default))

(defun least-common-bit (bits default)
  (common-bit #'< bits default))

(defun integer-from-bits (bits)
  (reduce (lambda (a b) (+ (ash a 1) b)) bits))

;; Each bit in the gamma rate can be determined by finding the most common bit
;; in the corresponding position of all numbers in the diagnostic report. The
;; epsilon rate is calculated in a similar way; rather than use the most common
;; bit, the least common bit from each position is used.
;;
;; Use the binary numbers in your diagnostic report to calculate the gamma rate
;; and epsilon rate, then multiply them together. What is the power consumption
;; of the submarine?
(defmethod solve ((day (eql *day*)) (part (eql 1)) input)
  (let* ((cols (array-dimension input 1))
         (gamma (make-array cols :element-type 'bit)))
    (dotimes (col cols)
      (let ((col-major (column-major-bits input col)))
        (setf (bit gamma col) (most-common-bit col-major 1))))
    (let ((epsilon (bit-not gamma)))
      (* (integer-from-bits gamma) (integer-from-bits epsilon)))))

;; To find oxygen generator rating, determine the most common value (0 or 1) in
;; the current bit position, and keep only numbers with that bit in that
;; position. If 0 and 1 are equally common, keep values with a 1 in the position
;; being considered.
;;
;; To find CO2 scrubber rating, determine the least common value (0 or 1) in the
;; current bit position, and keep only numbers with that bit in that position.
;; If 0 and 1 are equally common, keep values with a 0 in the position being
;; considered.
;;
;; Use the binary numbers in your diagnostic report to calculate the oxygen
;; generator rating and CO2 scrubber rating, then multiply them together. What
;; is the life support rating of the submarine?
(defmethod solve ((day (eql *day*)) (part (eql 2)) input)
  (destructuring-bind (rows cols) (array-dimensions input)
    (flet ((find-matching-number (which-common-bit default-bit)
             (let ((numbers (loop for row below rows
                                  collect (row-major-bits input row))))
               (loop for col below cols
                     until (= (length numbers) 1)
                     do (let ((matrix (make-array (list (length numbers) cols)
                                                  :element-type 'bit)))
                          (loop for number in numbers
                                for row below (length numbers)
                                do (loop for b across number
                                         for col below (length number)
                                         do (setf (bit matrix row col) b)))
                          (let* ((col-major (column-major-bits matrix col))
                                 (b (funcall which-common-bit col-major
                                             default-bit)))
                            (setf numbers
                                  (remove-if-not (lambda (n) (= (bit n col) b))
                                                 numbers)))))
               (integer-from-bits (first numbers)))))

      (let ((generator (find-matching-number #'most-common-bit 1))
            (scrubber (find-matching-number #'least-common-bit 0)))
        (* generator scrubber)))))

(let ((example (parse *day* "example")))
  (assert (= (solve *day* 1 example) 198))
  (assert (= (solve *day* 2 example) 230)))

(let ((input (parse *day* "input")))
  (when input
    (format t "day~a-part1: ~a~%" *day* (solve *day* 1 input))
    (format t "day~a-part2: ~a~%" *day* (solve *day* 2 input))))
