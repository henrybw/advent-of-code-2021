(in-package #:advent-of-code-2021)

;; Returns a 2D array of bits.
(defmethod parse ((day (eql 3)) (input stream))
  (destructuring-bind (rows cols lines)
      (loop for line = (read-line input nil) while line
            count line into rows
            maximize (length line) into cols
            collect line into lines
            finally (return (list rows cols lines)))
    (let ((bit-matrix (make-array (list rows cols) :element-type 'bit)))
      (loop for line in lines
            count line into row
            do (loop for char across line
                     count char into col
                     do (setf (bit bit-matrix (- row 1) (- col 1))
                              (if (char= char #\1) 1 0))))
      bit-matrix)))

(defun most-common-bit (bits)
  (loop for b across bits
        count (= b 1) into ones
        count (= b 0) into zeros
        finally (return (if (> ones zeros) 1 0))))

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
(defmethod solve ((day (eql 3)) (part (eql 1)) input)
  (destructuring-bind (rows cols) (array-dimensions input)
    (let ((gamma (make-array cols :element-type 'bit)))
      (dotimes (col cols)
        (let ((col-major (make-array rows :element-type 'bit)))
          (loop for row from 0 below rows
                for b = (bit input row col)
                do (setf (bit col-major row) b))
          (setf (bit gamma col) (most-common-bit col-major))))
      (let ((epsilon (bit-not gamma)))
        (* (integer-from-bits gamma) (integer-from-bits epsilon))))))

(let ((example (parse 3 "example")))
  (assert (= (solve 3 1 example) 198)))

(let ((input (parse 3 "input")))
  (when input
    (format t "day3-part1: ~a~%" (solve 3 1 input))))
