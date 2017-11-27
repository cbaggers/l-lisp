(in-package :l-systems)

;;
;; Density functions
;;
(defun standard-normal-distribution (x)
  (/ (exp (- (* x x 0.5d0)))
     (sqrt (* 2 pi))))

(defun normal-distrbution (x mean standard-deviation)
  (/ (standard-normal-distribution (/ (- x mean) standard-deviation))
     standard-deviation))

(defun fast-normal-distribution (x)
  (declare (optimize (speed 3) (safety 0))
           (double-float x))
  (exp (- (* x x 0.5d0))))

;;
;; Numerical integration
;;
(defparameter *integrate-intervals* 50)

(defun integrate (func a b &optional (intervals *integrate-intervals*))
  "Integrate function numerically using Simpson's 1/3 rule."
  (declare (optimize (speed 3) (safety 0))
           (double-float a b)
           (fixnum intervals)
           (type compiled-function func))
  (let* ((sum 0.0d0)
         (n (if (evenp intervals)
             intervals
             (1+ intervals)))
         (h (/ (- b a) n))
         (n/2 (/ n 2)))
    (declare (double-float sum h)
             (fixnum n n/2))
    ;; Add 4f(x1) + 2f(x2) + 4f(x3) + ... + 4f(xn-1)
    (dotimes (i n/2)
      (let ((x (+ a h (* 2 h i))))
        (incf sum (* 4 (the double-float (funcall func x))))
        (when (< i (- n/2 1))
          (incf sum (* 2 (the double-float (funcall func (+ x h))))))))
    ;; Add f(a) and f(b)
    (incf sum (+ (the double-float (funcall func a))
                 (the double-float (funcall func b))))
    ;; Return sum*h/3
    (* sum h 1/3)))

;;
;; Finding probabilities
;;
(defun standard-probability-between (a b)
  "Find an approximation for Pr(A < Z < B) for the standard normal
distribution."
  ;; Integrate numerically from a to b
  ;;(integrate #'standard-normal-distribution a b intervals))
  ;; use the fast normal-distribution function
  (declare (double-float a b))
  (/ (integrate #'fast-normal-distribution a b)
     (sqrt (* 2 pi))))

(defun standard-probability-< (x)
  "Find an approximation for Pr(Z < X) for the standard normal
distribution."
  ;; integrate from a reasonably small number (<= -10) to x
  (declare (optimize (speed 3) (safety 0))
           (double-float x))
  (let ((startnum (if (< x 0d0)
                    (- x 10d0)
                    -10d0)))
    (min (/ (the double-float
              (integrate #'fast-normal-distribution
                         startnum x))
            (sqrt (* 2 pi)))
         1d0)))

(defun standard-probability-> (x)
  "Find an approximation for Pr(Z > X) for the standard normal
distribution."
  (declare (double-float x))
  (- 1d0 (standard-probability-< x)))

(defun probability-between (a b mean standard-deviation)
  "Find an approximation for Pr(A < Z < B) for a normal distribution."
  (declare (double-float a b mean standard-deviation))
  (let ((new-a (/ (- a mean) standard-deviation))
        (new-b (/ (- b mean) standard-deviation)))
    (standard-probability-between new-a new-b)))

(defun probability-< (x mean standard-deviation)
  "Find an approximation for Pr(Z < X) for a normal distribution."
  (declare (double-float x mean standard-deviation))
  (standard-probability-< (/ (- x mean) standard-deviation)))

(defun probability-> (x mean standard-deviation)
  "Find an approximation for Pr(Z > X) for a normal distribution."
  (declare (double-float x mean standard-deviation))
  (- 1d0 (probability-< x mean standard-deviation)))

;;
;; Finding roots by the regula falsi method
;;
(defparameter *tolerance* 1.0d-9)
(declaim (double-float *tolerance*))

(declaim (inline small-enough))
(defun small-enough (x)
  (declare (optimize (speed 3) (safety 0))
           (double-float x))
  (< (abs x) *tolerance*))

(defun find-root (func x0 x1)
  "Find a root of FUNC between X0 and X1 using the regula falsi method."
  (declare (optimize (speed 3) (safety 0))
           (compiled-function func)
           (double-float x0 x1))
  (let ((fx0 (funcall func x0))
        (fx1 (funcall func x1))
        (i 0))
    (declare (double-float fx0 fx1)
             (fixnum i))
    (when (plusp (* fx0 fx1))
      (error "func(x0) and func(x1) must be of opposite signs"))
    (loop
     (incf i)
     (let* ((x2 (- x0 (* fx0 (/ (- x0 x1)
                                (- fx0 fx1)))))
            (fx2 (funcall func x2)))
       (declare (double-float x2 fx2))
       (when (small-enough fx2)
         (return (values x2 i)))
       (if (minusp (* fx0 fx2))
         (setq x1 x2
               fx1 fx2)
         (setq x0 x2
               fx0 fx2))))))

;;
;; Finding variables for given probabilities
;;
(defun find-standard-random-variable (probability)
  "Return an X such that Pr(Z < X) approximates PROBABILITY for the standard
normal distribution."
  (declare (double-float probability))
  (let (a b)
    (cond ((= probability 0.5d0)
           (return-from find-standard-random-variable 0d0))
          ((< probability 0.5d0)
           (setq a -10d0
                 b 0d0))
          (t
           (setq a 0d0
                 b 10d0)))
    (find-root (lambda (x) (- (standard-probability-< x)
                              probability))
               a b)))

(defun find-random-variable (probability mean standard-deviation)
  "Return an X such that Pr(Z < X) approximates PROBABILITY for a normal
distribution with the given MEAN and STANDARD-DEVIATION."
  (declare (double-float probability mean standard-deviation))
  (multiple-value-bind (std-answer i)
      (find-standard-random-variable probability)
    (declare (ignorable i))
    ;;(format t "FIND-ROOT iterations: ~A~%" i)
    (+ (* std-answer standard-deviation) mean)))

;;
;; NRANDOM
;;
(defun nrandom (mean standard-deviation)
  "Pick a random number from a normal distribution."
  (find-random-variable (random 1.0d0)
                        (coerce mean 'double-float)
                        (coerce standard-deviation 'double-float)))
