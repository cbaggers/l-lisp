;;; *** examples.lisp ***
;;;
;;; This file is part of L-Lisp by Knut Arild Erstad.
;;; Examples of L-systems in L-Lisp

(in-package :l-system-examples)

;; Fibonacci l-system
(defclass fibonacci (l-system)
  ((axiom :initform '(a))
   (depth :initform 4)))

(defmethod l-productions ((ls fibonacci))
  (choose-production ls
    (a (--> b))
    (b (--> a b))))

;; Snowflake curve
(defclass snowflake (l-system)
  ((axiom :initform '((+ 90) F - - F - - F))
   (depth :initform 1)
   (angle-increment :initform 60.0)))

(defmethod l-productions ((ls snowflake))
  (choose-production ls
    (F (--> F + F - - F + F))))

;; Parametric version of the snowflake curve
(defclass snowflake-param (l-system)
  ((axiom :initform '((F 1) (- 120) (F 1) (- 120) (F 1)))
   (depth :initform 0)))

(defmethod l-productions ((ls snowflake-param))
  (choose-production ls
    ((F x)
     (let ((newx (/ x 3)))
       (--> (F newx) (+ 60) (F newx) (- 120) (F newx) (+ 60) (F newx))))))

;; Occult L-system ;->
(defclass occult (l-system)
  ((axiom :initform '((- 90) F + F + F + F + F))
   (depth :initform 3)
   (angle-increment :initform 72)))

(defmethod l-productions ((ls occult))
  (choose-production ls
    (F (--> F F + F + F + F + F + F F))))

(defmethod l-productions ((ls occult))
  (choose-production ls
    (F (--> F F [ + F + F + F + F ] F))))

;; Tree structure
(defclass tree1 (l-system)
  ((axiom :initform '(F))
   (depth :initform 3)
   (angle-increment :initform 20.0)))

(defmethod l-productions ((ls tree1))
  (choose-production ls
    (F (--> F - [ - F + F + F ] + [ + F - F - F ] F))))

;; 3D tree
(defclass tree3d (l-system)
  ((axiom :initform '((! 1.0) (F 1.0)))
   (depth :initform 4)))

(defmethod l-productions ((ls tree3d))
  (choose-production ls
    ((F s) (--> (F s)
                [ (/ 35) (& 35) (! 1.0) (F (* s 0.8)) ]
                [ (/ 215) (& 35) (! 1.0) (F (* s 0.75)) ]))
    ((! x) (--> (! (1+ x))))))

;; Parametric 3d tree
(defclass partree1 (l-system)
  ((axiom :initform '((! 1.0) (F 1.0) (A 1.0)))
   (depth :initform 6)))

(defmethod l-productions ((ls partree1))
  (choose-production ls
    ((! x) (--> (! (1+ x))))
    ((A s) (let ((s1 (* s 0.85))
                 (s2 (* s 0.75)))
             (--> [ (/ 137) (& 25) (! 1.0) (F s1) (A s1) ]
                  [ (\\ 43) (& 25) (! 1.0) (F s2) (A s2) ])))))

(defclass partree2 (l-system)
  ((axiom :initform '((:set-width 1.0) (:forward 1.0) (apex 1.0)))
   (depth :initform 6)))

(defmethod l-productions ((ls partree2))
  (choose-production ls
    ((:set-width x) (--> (:set-width (1+ x))))
    ((apex s) (let ((s1 (* s 0.85))
                    (s2 (* s 0.75)))
                (--> [ (:roll-right 137) (:pitch-down 25)
                     (:set-width 1.0) (:forward s1) (apex s1) ]
                     [ (:roll-left 43) (:pitch-down 25)
                     (:set-width 1.0) (:forward s2) (apex s2) ])))))

;; Tree with randomness
(defclass rtree (l-system)
  ((axiom :initform '((! 10.0) (F 1.0) (A 10.0 1.0)))
   (depth :initform 100)))

(defmethod l-productions ((ls rtree))
  (choose-production ls
    ((A width length)
     (stochastic-choice
       (30 (--> (/ (random 10.0))
                (& (- (random 10.0) 5.0))
                (! (* width 0.99))
                (F length)
                (A (* width 0.99) length)))
       (1 (--> [ (- 25) (F length) (A width (* length 0.6)) ]
               [ (+ 25) (F length) (A width (* length 0.6)) ]))))))


;; Example 4.2.3, "Exploration of parameter space"
;; from "Visual models of plant development"
(defclass partree (l-system)
  ((axiom :initform '(S))
   (depth :initform 11 :initarg :depth)
   (r1  :initarg :r1  :initform 0.75)
   (r2  :initarg :r2  :initform 0.77)
   (a1  :initarg :a1  :initform 35)
   (a2  :initarg :a2  :initform -35)
   (d1  :initarg :d1  :initform 0)
   (d2  :initarg :d2  :initform 0)
   (w0  :initarg :w0  :initform 30)
   (q   :initarg :q   :initform 0.50)
   (e   :initarg :e   :initform 0.40)
   (min :initarg :min :initform 0.0)))

(defmethod l-productions ((ls partree))
  (with-slots (r1 r2 a1 a2 d1 d2 w0 q e min n) ls
    (choose-production ls
      (S (--> (A 100 w0)))
      ((A s w)
       (if (>= s min)
           (--> (! w) (F s)
                [ (+ a1) (/ d1)	(A (* s r1) (* w (expt q e))) ]
                [ (+ a2) (/ d2) (A (* s r2) (* w (expt (- 1 q) e))) ]))))))

;; Simple stochastic tree
(defclass stree (l-system)
  ((axiom :initform '((! 1) F A))
   (depth :initform 10)
   (cylinder-width :initform 0.02)))

(defmethod l-productions ((ls stree))
  (choose-production ls
    ((! n) (--> (! (1+ n))))
    (A (stochastic-choice
         (1 (--> (/ 137)
                 [ (+ 20) (! 1) F A ]
                 (- 20) (! 1) F A))
         (1 (--> (/ 137)
                 (- 20) (! 1) F A))))))

;; Stochastic tree model from VMoPD (modified)
(defclass stochastic-tree (l-system)
  ((axiom :initform '((S 0) (A 1)))
   (depth :initform 12)
   (homomorphism-depth :initform 10)))

(defmethod l-productions ((ls stochastic-tree))
  (let ((turn-ang1 10.0)
        (turn-ang2 20.0)
        (roll-ang 137.0))
    (choose-production ls
      ((A k)
       (let ((p (min 1.0 (/ (1+ (* 2.0 k)) (* k k)))))
         (stochastic-choice
           (p
            (--> (/ roll-ang)
                 [ (+ turn-ang1) (S 1) (A (1+ k)) ]
                 (- turn-ang2) (S 1) (A (1+ k))))
           ((- 1 p)
            (--> (/ roll-ang)
                 (- turn-ang2) (S 1) (A (1+ k)))))))
      ((S n) (--> (S (1+ n)))))))

(defmethod homomorphism ((ls stochastic-tree))
  (choose-production ls
    ((S n) (--> (! n) F))))

;; Signal test
(defclass signal-test (l-system)
  ((axiom :initform '(b a a a a a a a a))
   (depth :initform 0)))

(defmethod l-productions ((ls signal-test))
  (choose-production ls
    (a (with-left-context (b) (--> b)))
    (b (--> a))))

;; Context-sensitive L-systems from ABoP p. 34--35
(defclass context-a (l-system)
  ((axiom :initform '(F 1 F 1 F 1))
   (depth :initform 30)
   (angle-increment :initform 22.5)
   (ignore-list :initform '(+ - F))))

(defmethod l-productions ((ls context-a))
  (choose-production ls
    (0 (with-lc (0)
                (with-rc (0) (--> 0))
                (with-rc (1) (--> 1 [ + F 1 F 1 ])))
       (with-lc (1)
                (with-rc (0) (--> 0))
                (with-rc (1) (--> 1 F 1))))
    (1 (with-lc (0)
                (--> 1))
       (with-lc (1)
                (with-rc (0) (--> 0))
                (with-rc (1) (--> 0))))
    (+ (--> -))
    (- (--> +))))

(defclass context-b (l-system)
  ((axiom :initform '(F 1 F 1 F 1))
   (depth :initform 30)
   (angle-increment :initform 22.5)
   (consider-list :initform '(0 1))))

(defmethod l-productions ((ls context-b))
  (choose-production ls
    (0 (with-lc (0)
                (with-rc (0) (--> 1))
                (with-rc (1) (--> 1 [ - F 1 F 1 ])))
       (with-lc (1)
                (with-rc (0) (--> 0))
                (with-rc (1) (--> 1 F 1))))
    (1 (with-lc (0)
                (--> 1))
       (with-lc (1)
                (with-rc (0) (--> 1))
                (with-rc (1) (--> 0))))
    (+ (--> -))
    (- (--> +))))

;; Mycelis muralis (from ABoP, p. 87--90)
(defclass mycelis (l-system)
  ((axiom :initform '((I 20) Fa (A 0)))
   (ignore-list :initform '(+ /))
   (homomorphism-depth :initform 10)
   (frame-delay :initform 0.5)
   (frame-list :initform '((0 100)))
   (limits :initform '((-2 -1 -1) (2 14 1)))))

(defmethod l-productions ((ls mycelis))
  (choose-production ls
    ;; Growing apex
    ((A x)
     (with-lc (S) (--> (T 0)))
     (if (> x 0)
         (--> (A (1- x)))
         (--> [ (+ 30) Fb ] Fa (/ 180) (A 2))))
    ;; Stem segment: propagates signals
    (Fa
     (with-lc (S) (--> Fa S))
     (with-rc ((T c)) (--> (T (1+ c)) Fa)))
    ;; Undeveloped branch segment
    (Fb
     (with-lc ((T c) Fa) (--> (I (1- c)) Fa (A 3))))
    ;; Delayed signal
    ((I c)
     (if (zerop c)
         (--> S)
         (--> (I (1- c)))))
    ;; Signals disappers
    (S (--> nil))
    ((T c) (--> nil))))

(defmethod homomorphism ((ls mycelis))
  (choose-production ls
    (Fa (--> (:color #(0.5 0.2 0.0)) F))
    (Fb (--> (:color #(0.0 0.5 0.0)) F))))

;; Polygon test
(defclass polytest (l-system)
  ((axiom :initform '((:color #(0.6 0.3 0.1)) (! 1) A))
   (depth :initform 7)
   (angle-increment :initform 22.5)
   (line-style :initform :cylinders)
   (cylinder-width :initform 0.015)))

(defmethod l-productions ((ls polytest))
  (let ((green '#(0.3 1.0 0.3)))
    (choose-production ls
      (A (--> [ & F L (! 1) A ]
              / / / / /
              [ & F L (! 1) A ]
              / / / / / / /
              [ & F L (! 1) A ]))
      (F (--> S / / / / / F))
      (S (--> F L))
      ((! x) (--> (! (* 1.7 x))))
      (L (--> [ (:color green) ^ ^
              { - f. + f. + f. - \| - f. + f. + f. } ])))))

;; Cordate leaf
(defclass leaf (l-system)
  ((axiom :initform '((F 5) [ A ] [ B ]))
   (depth :initform 20)
   (angle-increment :initform 10)))

(defmethod l-productions ((ls leaf))
  (choose-production ls
    (A (--> [ + A { ] \. C }))
    (B (--> [ - B { ] \. C }))
    (C (--> \f C))))

;; Mesh test
(defclass mesh-test (l-system)
  ((axiom :initform '(F (:color #(0.5 1.0 0.5))
                      (:roll-left 180) (:pitch-down 45)
                      m{
                      [ (:roll-left 60) (:pitch-up 45) (bow 1 10 10) ]
                      [ m/ (:roll-right 60) (:pitch-up 45) (bow 1 10 10) ]
                      m}))
   (depth :initform 0)
   (angle-increment :initform 45)
   (homomorphism-depth :initform 100)))

(defmethod homomorphism ((ls mesh-test))
  (choose-production ls
    ((bow length angle num)
     (if (> num 0)
         (--> (mf length)
              (:pitch-down angle)
              (bow length angle (1- num)))))))

;; Mesh test 2
(defclass mesh-test-2 (l-system)
  ((axiom :initform '((:color #(0.6 0.3 0.15)) (! 1) A))
   (depth :initform 7)
   (angle-increment :initform 22.5)
   (homomorphism-depth :initform 100)
   (line-style :initform :cylinders)
   (cylinder-width :initform 0.015)))

(defmethod l-productions ((ls mesh-test-2))
  (let ((green '#(0.3 1.0 0.3)))
    (choose-production ls
      (A (--> [ & F L (! 1) A ]
              / / / / /
              [ & F L (! 1) A ]
              / / / / / / /
              [ & F L (! 1) A ]))
      (F (--> S / / / / / F))
      (S (--> F L))
      ((! x) (--> (! (* 1.7 x))))
      (L (--> [ (:color green) ^ ^ leaf ])))))

(defmethod homomorphism ((ls mesh-test-2))
  (choose-production ls
    (leaf (--> (:roll-left 180)
               m{
               [ (:roll-left 50) (:pitch-up 50) (bow 0.4 20 6) ]
               [ m/ (:roll-right 50) (:pitch-up 50) (bow 0.4 20 6) ]
               m} ))
    ((bow length angle num)
     (if (> num 0)
         (--> (mf length)
              (:pitch-down angle)
              (bow length angle (1- num)))))))

;; 2d pruning
(defclass prune1 (l-system)
  ((axiom :initform '([ (! 2)
                      (- 90) (F 4) (+ 90) (F 8) (+ 90) (F 8)
                      (+ 90) (F 8) (+ 90) (F 4) ]
                      F (?P nil)))
   (depth :initform 10)
   (angle-increment :initform 20)
   (sensitive :initform t)))

(defun prune (vec)
  (let ((x (aref vec 0))
        (y (aref vec 1)))
    (or (< x -4) (> x 4) (> y 8) (< y 0))))

(defmethod l-productions ((ls prune1))
  (choose-production ls
    ((?P pos) (if (not (prune pos))
                  (--> [ + F (?P nil) ] [ - F (?P pos) ])))))

;; Two-face pruning
(defclass prune2face (l-system)
  ((axiom :initform '([ (! 2) (- 90) (F 4) (+ 90) (F 8) (+ 90) (F 8)
                      (+ 90) (F 8) (+ 90) (F 4) ]
                      F (?P nil)))
   (mode :accessor mode :initform :grow)
   (depth :initform 10)
   (angle-increment :initform 20)
   (sensitive :initform t)))

(defmethod rewrite :before ((ls prune2face) &optional depth)
  (declare (ignore depth))
  (setf (mode ls) :grow))

(defmethod rewrite1 :before ((ls prune2face))
  (setf (mode ls)
        (if (eq (mode ls) :grow)
            :cut
            :grow)))

(defmethod l-productions ((ls prune2face))
  (choose-production ls
    ((?P pos)
     (when (eq (mode ls) :grow)
       (--> [ + F (?P nil) ]
            [ - F (?P nil) ])))
    (F
     (when (eq (mode ls) :cut)
       (with-rc ((?P pos))
                (when (prune pos)
                  (--> %)))))))

;; Example: 3d pruning
(defun prune-cylinder (pos height radius)
  ;; prune to a cylinder
  (let ((x (aref pos 0))
        (y (aref pos 1))
        (z (aref pos 2)))
    (or (< y 0) (> y height)
        (> (+ (* x x) (* z z)) (* radius radius)))))

(defun prune-cylinder-sphere (pos height radius)
  ;; prune to a cylinder and a (half-)sphere
  (let* ((x (aref pos 0))
         (y (aref pos 1))
         (z (aref pos 2))
         (y2 (- y height)))
    ;; prune if y < 0 or (x y z) is outside both cylinder and sphere
    (or (< y 0)
        (and (or (> y height) (> (+ (* x x) (* z z)) (* radius radius)))
             (> (+ (* x x) (* y2 y2) (* z z)) (* radius radius))))))

(defun prune-sphere (pos radius)
  (let* ((x (aref pos 0))
         (y (aref pos 1))
         (z (aref pos 2))
         (y2 (- y radius)))
    (> (+ (* x x) (* y2 y2) (* z z)) (* radius radius))))

(defun prune-cube (pos size)
  ;; prune to a cube
  (declare (optimize (speed 3) #+cmu (ext:inhibit-warnings 3))
           (type (simple-array double-float (3)) pos))
  (let* ((x (aref pos 0))
         (y (aref pos 1))
         (z (aref pos 2))
         (s (coerce size 'double-float))
         (size2 (/ s 2d0))
         (-size2 (- size2)))
    (declare (double-float x y z s size2 -size2))
    (or (< y 0) (> y s)
        (< x -size2) (> x size2)
        (< z -size2) (> z size2))))

(defun prune-cone (pos height radius)
  (let ((x (aref pos 0))
        (y (aref pos 1))
        (z (aref pos 2)))
    (or (< y 0) (> y height)
        (let ((distance-from-y-axis (sqrt (+ (* x x) (* z z)))))
          (> (* height distance-from-y-axis)
             (* (- height y) radius))))))

(defun prune-square-hedge (pos height inner-size outer-size)
  (declare (optimize (speed 3) #+cmu (ext:inhibit-warnings 3))
           (type (simple-array double-float (3)) pos))
  (let* ((x (aref pos 0))
         (y (aref pos 1))
         (z (aref pos 2))
         (h (coerce height 'double-float))
         (isize (coerce inner-size 'double-float))
         (osize (coerce outer-size 'double-float))
         (-isize (- isize))
         (-osize (- osize)))
    (declare (double-float x y z h isize osize -isize -osize))
    (or (< y 0d0) (> y h)
        (> x osize) (< x -osize)
        (> z osize) (< z -osize)
        (and (< -isize x isize)
             (< -isize z isize)))))

(defclass prune2 (l-system)
  ((axiom :initform '((! 1) F (?P nil)))
   (depth :initform 15)
   (homomorphism-depth :initform 100)
   (sensitive :initform t)))

(defmethod l-productions ((ls prune2))
  (choose-production ls
    ((! n) (--> (! (1+ n))))
    ((?P pos) (when (not (prune-cylinder pos 10 6))
                (stochastic-choice
                  (9 (--> (/ 137)
                          [ (+ 40) (! 1) F (?P pos) leaf ]
                          (- 35) (! 1) F (?P pos) leaf ))
                  (1 (--> (/ 137)
                          (- 35) (! 1) F (?P pos) leaf)))
                (--> permanent-leaf)))
    (leaf (--> nil))))

(defmethod homomorphism ((ls prune2))
  (choose-production ls
    (permanent-leaf (--> leaf))
    (leaf (let* ((n 4)
                 (start-ang 40)
                 (ang (/ (* start-ang 2.0) (1- n)))
                 (distance 1.0)
                 (dist (/ distance n)))
            (--> (:pitch-down 65) (:color '#(0.3 1.0 0.3))
                 m{
                 [ (:roll-left 40) (:pitch-up start-ang) (bow ang n dist) ]
                 [ m/ (:roll-right 40) (:pitch-up start-ang) (bow ang n dist) ]
                 m})))
    ((bow ang n dist) (if (> n 0)
                          (--> (mf dist) (:pitch-down ang)
                               (bow ang (1- n) dist))))))

;; Improved pruning
(defclass prune3 (l-system)
  ((axiom :initform '((:color #(0.8 0.4 0.2)) (?T nil)))
   (depth :initform 18)
   (homomorphism-depth :initform 10)
   (sensitive :initform t)
   (prune-func :accessor prune-func
               :initform #'(lambda (pos) (prune-cylinder pos 15 6)))
   (limits :initform '((-7 -1 -7) (7 15 7)))
   (cylinder-width :initform 0.02)))

(defmethod l-productions ((ls prune3))
  (choose-production ls
    ((?T turtle)
     (let ((prune-func (prune-func ls)))
       ;; try moving one unit forward and see if we need to prune
       (move-forward turtle 1d0)
       (if (funcall prune-func (turtle-pos turtle))
           ;; pruning: use interval halving to find out by how much
           (let ((length nil))
             (move-forward turtle -1d0)
             (do* ((lower 0d0)
                   (upper 1d0)
                   (middle (/ (- upper lower) 2) (/ (- upper lower) 2))
                   (original-pos (copy-seq (turtle-pos turtle)))
                   (n 0 (1+ n)))
                  ((= n 5) (setq length middle))
               (setf (turtle-pos turtle) (copy-seq original-pos))
               (move-forward turtle middle)
               (if (funcall prune-func (turtle-pos turtle))
                   (setq upper middle)
                   (setq lower middle)))
             ;; return a segment that will not grow any more
             (--> (! 1) (F length) permanent-leaf))
           ;; no pruning, normal growth
           (if (< (current-depth ls) 4)
               (--> (! 1) (F 1) leaf (/ 137)
                    [ (- 35) (?T nil) ] (+ 40) (?T nil))
               (stochastic-choice
                 (9 (--> (! 1) (F 1) leaf (/ 137)
                         [ (- 35) (?T nil) ] (+ 40) (?T nil)))
                 (1 (--> (! 1) (F 1) leaf (/ 137) (+ 40) (?T nil))))))))
    ;; leaves move by disappearing and getting recreated...
    (leaf (--> nil))
    ;; branch width
    ((! x) (when (< x 4) (--> (! (+ x 0.5)))))))

(defmethod homomorphism ((ls prune3))
  (choose-production ls
    (permanent-leaf (--> leaf))
    (leaf (let* ((n 4)
                 (start-ang 40)
                 (ang (/ (* start-ang 2.0) (1- n)))
                 (distance 1.0)
                 (dist (/ distance n)))
            (--> (:pitch-down 65) (:color '#(0.3 1.0 0.3))
                 m{
                 [ (:roll-left 40) (:pitch-up start-ang) (bow ang n dist) ]
                 [ m/ (:roll-right 40) (:pitch-up start-ang) (bow ang n dist) ]
                 m})))
    ((bow ang n dist) (if (> n 0)
                          (--> (mf dist) (:pitch-down ang)
                               (bow ang (1- n) dist))))))

;; L-system inherited from PRUNE3:
(defclass prune3-1 (prune3)
  ((axiom :initform '((:color #(0.8 0.4 0.2))
                      [ (@M #(15 0 15)) (?T nil) ]
                      [ (@M #(-15 0 -15)) (?T nil) ]
                      [ (@M #(-15 0 15)) (?T nil) ]
                      [ (@M #(15 0 -15)) (?T nil) ]
                      [ (@M #(0 0 0)) (?T nil) ]))
   (prune-func :initform #'(lambda (pos)
                             (and (prune-square-hedge pos 8 12 18)
                                  (prune-cylinder-sphere pos 10 5))))))

;; Climbing plant
(defun crash-box (box point)
  (let* ((minv (box-pos box))
         (maxv (vec+ minv (box-size box))))
    (and (< (aref minv 0) (aref point 0) (aref maxv 0))
         (< (aref minv 1) (aref point 1) (aref maxv 1))
         (< (aref minv 2) (aref point 2) (aref maxv 2)))))

(defun crash-boxes (boxes point)
  (dotimes (i (length boxes))
    (when (crash-box (svref boxes i) point)
      (return t))))

(defun crash-cylinder-axis (x1 y1 z1 x2 y2 z2 x y z width)
  (assert (and (= y1 y2) (= z1 z2)))
  ;;  (format t "cyl (~A ~A ~A) (~A ~A ~A) ~A, point (~A ~A ~A)~%"
  ;;  x1 y1 z1 x2 y2 z2 width x y z)
  (labels ((** (n) (* n n)))
    (let ((xmin (min x1 x2))
          (xmax (max x1 x2)))
      (and (< xmin x xmax)
           (< (+ (** (- y y1))
                 (** (- z z1)))
              (** width))))))

(defun crash-cylinder (line point)
  (let* ((p1 (line-p1 line))
         (p2 (line-p2 line))
         (x1 (aref p1 0))
         (y1 (aref p1 1))
         (z1 (aref p1 2))
         (x2 (aref p2 0))
         (y2 (aref p2 1))
         (z2 (aref p2 2))
         (width (line-width line))
         (x (aref point 0))
         (y (aref point 1))
         (z (aref point 2)))
    (cond ((and (= y1 y2) (= z1 z2))
           (crash-cylinder-axis x1 y1 z1 x2 y2 z2 x y z width))
          ((and (= x1 x2) (= z1 z2))
           (warn "fy")
           (crash-cylinder-axis y1 x1 z1 y2 x2 z2 y x z width))
          ((and (= x1 x2) (= y1 y2))
           (warn "fz")
           (crash-cylinder-axis z1 x1 y1 z2 x2 y2 z x y width))
          (t
           (error "Only works with cylinders parallel to an axis.")))))

(defun crash-cylinders (lines point)
  (dotimes (i (length lines))
    (when (crash-cylinder (svref lines i) point)
      (return t))))

(defparameter *init-boxes* '#((:box #(-50 0 -10) #(100 50 10))))
(defparameter *init-cylinders* '#((:line #(-50 50 -5) #(50 50 -5) 5)))

(defclass climbing1 (l-system)
  ((axiom :initform (concatenate
                     'list
                     '(\[ (:color #(1.0 0.6 0.6))
                       (:produce-spheres nil))
                     *init-boxes*
                     *init-cylinders*
                     '(\]
                       (:color #(0.4 1.0 0.4)) (! 0.15)
                       [ (@M #(-30 0 0))
                       (searching-apex 0 22.5 20) (?P nil) ]
                       [ (@M #(30 0 0))
                       (searching-apex 0 22.5 20) (?P nil) ])))
   (boxes :accessor boxes
          :initform (turtle-interpret *init-boxes*))
   (cylinders :accessor cylinders
              :initform (turtle-interpret *init-cylinders*))
   (line-style :initform :cylinders)
   (frame-list :initform '((0 1000)))
   (frame-delay :initform 0.0)
   (depth :initform 100)
   (sensitive :initform t)
   (homomorphism-depth :initform 100)))

(defmethod l-productions ((ls climbing1))
  (flet ((crash (pos)
           (or (< (aref pos 1) 0)
               (crash-boxes (boxes ls) pos)
               (crash-cylinders (cylinders ls) pos))))
    (let ((rstep 5d0)
          (maxr 95d0)
          (bstep -2d0)
          (branch-distance 20)
          (leaf-distance 10)
          (branch-angle 60d0)
          (random-turn 5d0))
      (choose-production ls
        ((searching-apex roll pitch n)
         (with-rc ((?P pos))
                  (if (crash pos)
                      (progn
                        #+nil(if (>= (aref pos 1) 55)
                                 (format t "crash ~A???~%" pos)
                                 (format t "crash with y=~A~%" (aref pos 1)))
                        (--> (backtracking-apex roll pitch n)))
                      (let ((npitch (+ pitch rstep)))
                        (if (< npitch maxr)
                            (--> (searching-apex roll npitch n))
                            (--> (searching-apex (+ roll 45) 0d0 n)))))))
        ((backtracking-apex roll pitch n)
         (with-rc ((?P pos))
                  (if (crash pos)
                      (--> (backtracking-apex roll (+ pitch bstep) n))
                      (cond ((zerop n)
                             (--> (segment roll pitch)
                                  [ (:turn-left (stochastic-choice
                                                  (1 branch-angle)
                                                  (1 (- branch-angle))))
                                  (searching-apex 0d0 0d0 branch-distance)
                                  (?P nil) ]
                                  (searching-apex 0d0 0d0 branch-distance)))
                            ((zerop (mod (+ n (floor leaf-distance 2))
                                         leaf-distance))
                             (--> (segment roll pitch)
                                  (leaf 6 (+ 35d0 (random 10d0)) 0.1d0)
                                  (:turn-right (nrandom 0d0 random-turn))
                                  (searching-apex 0d0 0d0 (- n 1))))
                            (t
                             (--> (segment roll pitch)
                                  (:turn-right (nrandom 0d0 random-turn))
                                  (searching-apex 0d0 0d0 (- n 1))))))))
        ((leaf n start-ang size)
         (--> (leaf n start-ang (min 5d0 (+ size 0.1)))))))))

(defmethod homomorphism ((ls climbing1))
  (choose-production ls
    ((searching-apex r p n) (--> (segment r p)))
    ((backtracking-apex r p n) (--> (segment r p)))
    ((segment roll pitch) (--> (:roll-left roll) (:pitch-down pitch)
                               :forward))
    ((leaf n start-ang size)
     (let ((ang (/ (* start-ang 2.0) (1- n)))
           (dist (/ size n)))
       (--> [ (:pitch-up 170) (:roll-left 180)
            m{
            [ (:roll-left 60) (:pitch-up start-ang) (bow ang n dist) ]
            [ m/ (:roll-right 60) (:pitch-up start-ang) (bow ang n dist) ]
            m} ])))
    ((bow ang n dist) (if (> n 0)
                          (--> (mf dist) (:pitch-down ang)
                               (bow ang (1- n) dist))))))

;; Simple tropism
(defclass tropism1 (l-system)
  ((axiom :initform '((A 100 12)))
   (depth :initform 11)
   (homomorphism-depth :initform 10)
   (tropism-vector :accessor tropism-vector
                   :initarg :tropism-vector
                   :initform '#(0d0 -1d0 0d0))
   (tropism-angle :accessor tropism-angle
                  :initarg :tropism-angle
                  :initform 1.0)))

(defmethod l-productions ((ls tropism1))
  (choose-production ls
    ((A s w) (--> (S s w)
                  [ (+ 30) (/ 137) (A (* s 0.8) (* w 0.8)) ]
                  [ (- 10) (/ -90) (A (* s 0.9) (* w 0.8)) ]))))

(defmethod homomorphism ((ls tropism1))
  (choose-production ls
    ((S s w) (--> (:rotate-towards (tropism-angle ls) (tropism-vector ls))
                  (! w) (F s)))))

;; Tropism with hvec
(defclass tropism2 (l-system)
  ((axiom :initform '((A 100 12) (?H nil)))
   (depth :initform 11)
   (homomorphism-depth :initform 10)
   (tropism-vector :accessor tropism-vector
                   :initarg :tropism-vector
                   :initform '#(0d0 -1d0 0d0))
   (elasticity-const :accessor elasticity-const
                     :initarg :elasticity-const
                     :initform 15)))

(defmethod l-productions ((ls tropism2))
  (choose-production ls
    ((A s w) (with-rc ((?H hvec))
                      (--> (S s w hvec)
                           [ (+ 30) (/ 137) (A (* s 0.8) (* w 0.8)) (?H nil) ]
                           [ (- 10) (/ -90) (A (* s 0.9) (* w 0.8)) (?H nil) ])))
    ((?H hvec) (--> nil))))

(defmethod elasticity ((ls tropism2) s w)
  (declare (ignorable s w))
  (elasticity-const ls))

(defmethod homomorphism ((ls tropism2))
  (choose-production ls
    ((S s w hvec)
     (let* ((elasticity (elasticity ls s w))
            (tvec (map '(simple-array double-float)
                       #'identity
                       (tropism-vector ls)))
            (HxT (cross-product hvec tvec))
            (angle (* elasticity (vlength HxT))))
       (--> (:set-width w)
            (:rotate-towards angle (tropism-vector ls))
            (:forward s))))))

;; Different elasticiy function: smaller widths -> larger elasticity
(defclass tropism3 (tropism2)
  ((elasticity-const :initform 50)))

(defmethod elasticity ((ls tropism3) s w)
  (declare (ignorable s w))
  (/ (elasticity-const ls) w))

;; Yet another elasticiy function
(defclass tropism4 (tropism2)
  ((elasticity-const :initform 5)
   (param1 :initarg :param1
           :accessor param1
           :initform 20)
   (tropism-vector :initform #(1d0 0d0 0d0))))

(defmethod elasticity ((ls tropism4) s w)
  (declare (ignorable s w))
  (+ (elasticity-const ls)
     (/ (param1 ls) w)))

;; Good-looking tropism tree
(defclass gtropism (l-system)
  ((axiom :initform '((:color #(0.8 0.4 0.2))
                      (A 100 12) (?H nil)))
   (depth :initform 12)
   (homomorphism-depth :initform 10)))

(defmethod l-productions ((ls gtropism))
  (choose-production ls
    ((A s w) (with-rc ((?H hvec))
                      (let ((x (if (<= (current-depth ls) 2)
                                   0
                                   1)))
                        (stochastic-choice
                          (x (--> (S s w hvec)
                                  (/ 137) (A (* s 0.8) (* w 0.8)) (?H nil)))
                          (9 (--> (S s w hvec)
                                  [ (+ 30) (/ 137) (A (* s 0.8) (* w 0.8)) (?H nil) ]
                                  [ (- 10) (/ -90) (A (* s 0.9) (* w 0.8)) (?H nil) ]))))))
    ((?H hvec) (--> nil))))

(defmethod homomorphism ((ls gtropism))
  (choose-production ls
    ((S s w hvec)
     (if (> s 10)
         (let ((s/2 (/ s 2)))
           (--> (S s/2 w hvec) (S s/2 w hvec)))
         (let* ((elasticity (* s (/ 1.5 w)))
                (tvec (vec3 0d0 -1d0 0d0))
                (HxT (cross-product hvec tvec))
                (angle (* elasticity (vlength HxT))))
           (--> (:set-width w)
                (:rotate-towards angle tvec)
                (:forward s)))))))


;; Using homomorphism to create bent branch segments
(defclass bent (l-system)
  ((axiom :initform '((:color #(0.8 0.4 0.2))
                      (bent-branch 0 0 1 8 8 1) (apex 1.0)))
   (depth :initform 6)
   (homomorphism-depth :initform 100)))

(defmethod l-productions ((ls bent))
  (choose-production ls
    ((apex s) (let ((s1 (* s 0.95))
                    (s2 (* s 0.85))
                    (n 8))
                (--> [ (bent-branch 137 25 s1 n n 1.0) (apex s1) ]
                     [ (bent-branch -43 25 s2 n n 1.0) (apex s2) ])))
    ((bent-branch roll pitch length n m width)
     (--> (bent-branch roll pitch length n m (1+ width))))))

(defmethod homomorphism ((ls bent))
  (choose-production ls
    ((bent-branch roll pitch length n m width)
     (if (<= n 0)
         (--> nil)
         (let ((rollf (/ roll n))
               (pitchf (/ pitch n))
               (lengthf (/ length n)))
           (--> (:set-width (+ width (/ n m)))
                (:roll-right rollf)
                (:pitch-down pitchf)
                (:forward lengthf)
                (bent-branch (- roll rollf) (- pitch pitchf)
                             (- length lengthf) (1- n) m width)))))))

;; Cut symbol test
(defclass cut-test (l-system)
  ((axiom :initform '([ + (F 7 0) - (F 10 0) - (F 14 0)
                      - (F 10 0) - (F 7 0) ]
                      (! 1) A))
   (depth :initform 7)
   (frame-list :initform '((1 7)))
   (frame-delay :initform 1)))

(defmethod l-productions ((ls cut-test))
  (choose-production ls
    ((! n) (--> (! (1+ n))))
    ((F x) (--> (F (* x 1.1))))
    (A (--> (F 1)
            [ (+ 25) (! 1) X A ]
            [ (- 25) (! 1) X A ]))
    (X (stochastic-choice
         (19 (--> X))
         (0 (--> A :cut-symbol))))))

;; Sphere test
(defclass sphere-test (l-system)
  ((axiom :initform '(F (:sphere 3) F
                      (:color #(0 0.5 1.0))
                      (:sphere 3) F))))

;; Constant curvature (growing arc)
(defclass const-curvature (l-system)
  ((axiom :initform '((A 1.5) (A 0.0)))
   (decomposition-depth :initform 10)
   (homomorphism-depth :initform 10)
   ;; Animation parameters
   (frame-delay :initform 0.05)
   (frame-list :initform '((0 500)))
   (limits :initform '((-1 -3 -1) (6 3 1)))))

(defmethod l-productions ((ls const-curvature))
  (choose-production ls
    ((A x) (--> (A (* x 1.01))))))

(defmethod decomposition ((ls const-curvature))
  (choose-production ls
    ((A x) (when (> x 1.5)
             ;; Use "golden ratio" to split segment;
             ;; the number of segments will then always be
             ;; a Fibonacci number.
             (let* (;;(factor 0.61803398875)
                    (factor 0.4)
                    (x1 (* x factor))
                    (x2 (* x (- 1 factor))))
               (--> (A x1) (A x2)))))))

(defmethod homomorphism ((ls const-curvature))
  (let ((curvature 10))
    (choose-production ls
      ((A x) (let ((angle (* curvature x)))
               (--> (+ angle) (:sphere 4)
                    (F x) (+ angle)))))))

;; Contact points
(defclass contact-points (l-system)
  ((axiom :initform '([ (! 0.01) (+ 90) (\f 1)
                      (+ 90) (F 0.5) (+ 90) (F 22) (+ 90) (F 1)
                      (+ 90) (F 22) (+ 90) (F 1) ]
                      (- 90) (A 1 0 -1) X))
   (depth :initform 10)
   (decomposition-depth :initform 10)
   (homomorphism-depth :initform 10)
   (growth-rate :accessor growth-rate :initform 1.01)
   (cp-interval :accessor cp-interval :initform 5.0)
   (stem-pos :accessor stem-pos)
   (next-rotation-point :accessor next-rotation-point)
   (max-length :accessor max-length :initform 1.0)
   (frame-list :initform '((1 290)))
   (frame-delay :initform 0.02)))

(defmethod rewrite :before ((ls contact-points) &optional (depth (depth ls)))
  (declare (ignorable depth))
  (setf (next-rotation-point ls) (cp-interval ls)))

(defmethod rewrite1 :before ((ls contact-points))
  (setf (stem-pos ls) 0.0))

(defmethod l-productions ((ls contact-points))
  (choose-production ls
    ((A s p c)
     (with-slots (growth-rate cp-interval stem-pos next-rotation-point) ls
       (let ((new-s (* s growth-rate))
             (this-pos stem-pos))
         (incf stem-pos new-s)
         ;; contact point exists and remains
         (when (>= c this-pos)
           (--> (A new-s this-pos c)))
         ;; CP disappears to the left
         (when (< 0 c this-pos)
           (--> (A new-s this-pos -1)))
         ;; new CP from the right
         (with-rc ((A sr pr cr))
                  (when (< cr stem-pos)
                    (--> (A new-s this-pos cr))))
         ;; create new CP at the apex
         (with-rc (X)
                  (when (> stem-pos next-rotation-point)
                    (let ((new-c next-rotation-point))
                      (incf next-rotation-point cp-interval)
                      (--> (A new-s this-pos new-c)))))
         ;; no CP
         (--> (A new-s this-pos c)))))))

(defmethod decomposition ((ls contact-points))
  (choose-production ls
    ((A s p c)
     (when (> s (max-length ls))
       (let ((s/2 (/ s 2.0))
             (c1 -1)
             (c2 -1))
         (when (>= c 0)
           (if (< c (+ p s/2))
               (setq c1 c)
               (setq c2 c)))
         (--> (A s/2 p c1) (A s/2 (+ p s/2) c2)))))))

(defmethod homomorphism ((ls contact-points))
  (choose-production ls
    ((A s p c)
     (if (< c 0)
         (--> (@O 3) (F s))
         (let* ((s1 (- c p))
                (s2 (- s s1)))
           (--> (@O 3) (F s1) (@O 6) (F s2)))))))

;; Good looking tree (combines randomness, bent branches and mesh leaves)
(defclass gtree (l-system)
  ((axiom :initform '((:color #(0.8 0.4 0.3)) (:tropism 1.2)
                      (S 1 0 0 2) A))
   (depth :initform 10)
   (homomorphism-depth :initform 20)
   (line-style :initform :cylinders)
   (cylinder-width :initform 1.0)
   (sensitive :initform nil)))

(defmethod l-productions ((ls gtree))
  (choose-production ls
    ((S width roll turn length) (--> (S (1+ width) roll turn
                                        (* length 1.15))))
    (A (stochastic-choice
         (3
          (--> [ (S 1 137 (nrandom 30 10) (nrandom 1.5 0.5)) A ]
               (:turn-right 20)
               (S 1 137 (nrandom -30 10) (nrandom 1.5 0.5)) A))
         ((if (< (current-depth ls) 3) 0 1)
          (--> [ (:turn-left 20) L ] ;;(- 20)
               (S 1 137 (nrandom 10 10) (nrandom 1.5 0.5)) A))))))

(defmethod homomorphism ((ls gtree))
  (let ((steps 4)
        (leaf-steps 4))
    (choose-production ls
      ((S width roll turn length)
       ;;(format t "~A~%" (current-module ls))
       (--> (bent-branch (1+ width) (/ roll steps) (/ turn steps)
                         (/ length steps) 0)))
      ((bent-branch width roll turn length n)
       (when (< n steps)
         (--> (wid width) (:roll-left roll) (:turn-left turn)
              (:forward length)
              (bent-branch (- width (/ 0.1 steps)) roll turn length (1+ n)))))
      (L (stochastic-choice
           (1 (--> A))
           (1 (--> (berry 0.8)))))
      ((berry size)
       (--> [ (:turn-left (nrandom 45 10)) (:forward size)
            (:color #(1.0 0.2 0.2)) (:sphere size) ]))
      (A (--> (:color #(0.2 0.7 0.2))
              (:roll-left (random 360))
              (leaf (nrandom 55 10) 60 (nrandom 4 0.2))))
      ((leaf start-roll start-angle size)
       (let ((pitch-step (/ start-angle (1+ leaf-steps) 0.5))
             (size-step (/ size leaf-steps)))
         ;;(format t "~A~%" (current-module ls))
         (--> (wid 0.7) (:forward 0.1)
              (:tropism nil) (:pitch-down 25d0)
              :start-mesh
              [ (:roll-left start-roll) (:pitch-up start-angle)
              :new-strand (bent-leaf pitch-step size-step 0) ]
              [ (:roll-right start-roll) (:pitch-up start-angle)
              :new-strand (bent-leaf pitch-step size-step 0) ]
              :end-mesh)))
      ((bent-leaf pitch-step size-step n)
       (when (< n leaf-steps)
         (--> (:pitch-down pitch-step) (mf size-step)
              (bent-leaf pitch-step size-step (1+ n)))))
      ((wid x)
       (--> (:set-width (* 0.045 x)))))))
