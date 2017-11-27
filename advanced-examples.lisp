(in-package :l-system-examples)

;; *****************************************************
;; *** a schematic plant shaped by a spline function ***
;; *****************************************************
(defun edit-shape (&optional (eps-filename nil))
  (edit-spline :filename "shape.spline"
	       :eps-filename eps-filename))

(defclass shaped-plant (l-system)
  ((axiom :initform '(S))
   (homomorphism-depth :initform 100)
   (spline :accessor spline)))

(defmethod rewrite :before ((ls shaped-plant) &optional (depth 0))
  (declare (ignore depth))
  (setf (spline ls) (input-spline "shape.spline" :make-function t)))

(defmethod homomorphism ((ls shaped-plant))
  (let* ((spline (spline ls))
	 (sfunc (spline-function spline))
	 (length 1.0)
	 (width-mult 1.0)
	 (segments 40)
	 (segment-length (/ length segments)))
    (choose-production ls
      (S (--> (F segment-length) (A 0.0)))
      ((A x) (when (< x length)
	       (let* ((fval (funcall sfunc (/ x length)))
		      (width (* fval width-mult)))
		 (--> [ (+ 90) (F width) ]
		      [ (- 90) (F width) ]
		      (F segment-length)
		      (A (+ x segment-length)))))))))

;; ********************************************************
;; *** a schematic plant shaped by TWO spline functions ***
;; ********************************************************
(defun edit-shape-1 (&optional (eps-filename nil))
  (edit-spline :filename "shape1.spline"
	       :eps-filename eps-filename))

(defun edit-shape-2 (&optional (eps-filename nil))
  (edit-spline :filename "shape2.spline"
	       :eps-filename eps-filename))

(defclass shaped-plant-2 (l-system)
  ((axiom :initform '(S))
   (homomorphism-depth :initform 100)
   (spline1 :accessor spline1)
   (spline2 :accessor spline2)
   ))

(defmethod rewrite :before ((ls shaped-plant-2) &optional (depth 0))
  (declare (ignore depth))
  (setf (spline1 ls) (input-spline "shape1.spline" :make-function t)
	(spline2 ls) (input-spline "shape2.spline" :make-function t)))

(defmethod homomorphism ((ls shaped-plant-2))
  (let* ((spline1 (spline1 ls))
	 (sfunc1 (spline-function spline1))
	 (spline2 (spline2 ls))
	 (sfunc2 (spline-function spline2))
	 (length 1.0)
	 (width-mult 1.0)
	 (angle-mult 90.0)
	 (segments 40)
	 (segment-length (/ length segments)))
    (choose-production ls
      (S (--> (F segment-length) (A 0.0)))
      ((A x) (when (< x length)
	       (let* ((xrel (/ x length))
		      (fval1 (funcall sfunc1 xrel))
		      (width (* fval1 width-mult))
		      (fval2 (funcall sfunc2 xrel))
		      (angle (* fval2 angle-mult)))
		 (--> [ (+ 90) (- angle) (F width) ]
		      [ (- 90) (+ angle) (F width) ]
		      (F segment-length)
		      (A (+ x segment-length)))))))))

;; ****************************************************
;; *** a mesh leaf shaped by three spline functions ***
;; ****************************************************
(defun edit-leaf-width ()
  (edit-spline :filename "leaf-width.spline"))

(defun edit-leaf-curving ()
  (edit-spline :filename "leaf-curving.spline"))

(defun edit-leaf-hcurving ()
  (edit-spline :filename "leaf-hcurving.spline"))

(defclass shaped-leaf (l-system)
  ((axiom :initform '(S))
   (homomorphism-depth :initform 100)
   (line-style :initform :cylinders)
   (cylinder-width :initform 0.005)
   (width-spline :accessor width-spline)
   (curving-spline :accessor curving-spline)
   (hcurving-spline :accessor hcurving-spline)))

(defmethod rewrite :before ((ls shaped-leaf) &optional (depth 0))
  (declare (ignore depth))
  (setf (width-spline ls) (input-spline "leaf-width.spline" :make-function t)
	(curving-spline ls) (input-spline "leaf-curving.spline"
					  :make-function t)
	(hcurving-spline ls) (input-spline "leaf-hcurving.spline"
					   :make-function t)))

(defmethod homomorphism ((ls shaped-leaf))
  (let* ((length 1.0)
	 (segments 40)
	 (segment-length (/ length segments))
	 (width-multiply 1.0)
	 (curving-multiply 10.0)
	 (hcurving-multiply -100.0)
	 (width-func (spline-function (width-spline ls)))
	 (curving-func (spline-function (curving-spline ls)))
	 (hcurving-func (spline-function (hcurving-spline ls))))
    (choose-production ls
      (S (--> (:color #(0.5 1.0 0.5))
	      (:pitch-down 20)
	      (:roll-left 20)
	      (:forward 0.2)
	      :start-mesh [ (L 0.0) ]
	      :new-strand [ (M 0.0) ]
	      :new-strand [ (R 0.0) ]
	      :end-mesh))
      ;; left strand
      ((L x)
       (when (< x length)
	 (let* ((x1 (/ x length))
		(w-val (funcall width-func x1))
		(c-val (funcall curving-func x1))
		(h-val (funcall hcurving-func x1)))
	   (--> [ (:turn-left 90) (:pitch-down (* hcurving-multiply h-val))
		  (:forward-mesh-vertex (* width-multiply w-val)) ]
		(:pitch-down (* curving-multiply c-val))
		(:forward-no-line segment-length)
		(L (+ x segment-length))))))
      ;; middle strand
      ((M x)
       (when (< x length)
	 (let* ((x1 (/ x length))
		(c-val (funcall curving-func x1)))
	   (--> (:pitch-down (* curving-multiply c-val))
		:mesh-vertex
		(:forward-no-line segment-length)
		(M (+ x segment-length))))))
      ;; right strand
      ((R x)
       (when (< x length)
	 (let* ((x1 (/ x length))
		(w-val (funcall width-func x1))
		(c-val (funcall curving-func x1))
		(h-val (funcall hcurving-func x1)))
	   (--> [ (:turn-right 90) (:pitch-down (* hcurving-multiply h-val))
		  (:forward-mesh-vertex (* width-multiply w-val)) ]
		(:pitch-down (* curving-multiply c-val))
		(:forward-no-line segment-length)
		(R (+ x segment-length))))))
      )))


;; ****************************************************
;; *** growth along a 2D curve defined by curvature ***
;; ****************************************************
(defun edit-curvature (&optional eps-filename)
  (edit-spline :filename "curvature.spline"
	       :eps-filename eps-filename))

(defclass spline-curvature (l-system)
  ((axiom :initform '((A 0.08 0 0) X))
   (decomposition-depth :initform 10)
   (homomorphism-depth :initform 10)
   (limits :initform '((-0.5 0 0) (0.5 1 1)))
   (frame-delay :initform 0.01)
   (frame-list :initform '((0 10000)))
   (spline-pos :accessor spline-pos)
   (spline :accessor spline)))

(defun current-curvature (ls)
  (let ((spline-func (spline-function (spline ls))))
    (funcall spline-func (spline-pos ls))))

(defmethod rewrite :before ((ls spline-curvature) &optional (depth 0))
  (declare (ignore depth))
  (setf (spline ls) (input-spline "curvature.spline" :make-function t)))

(defmethod rewrite1 :before ((ls spline-curvature))
  (setf (spline-pos ls) 0.0))

(defmethod l-productions ((ls spline-curvature))
  (let ((spline-func (spline-function (spline ls))))
    (flet ((curvature-at (x) (funcall spline-func x)))
      (choose-production ls
	((A x c1 c2)
	 (if (>= (spline-pos ls) 1.0)
	     (--> nil)
	     (let* ((new-x (* x 1.01))
		    (pos1 (spline-pos ls))
		    (pos2 (+ pos1 new-x)))
	       (when (> pos2 1.0)
		 (setq pos2 1.0
		       new-x (- 1.0 pos1)))
	       (setf (spline-pos ls) pos2)
	       (let ((curv1 (curvature-at pos1))
		     (curv2 (curvature-at pos2)))
		 (--> (A new-x curv1 curv2))))))
	))))

(defmethod decomposition ((ls spline-curvature))
  (choose-production ls
    ((A x c1 c2)
     (when (> x 0.05)
       ;; Use "golden ratio" to split segment;
       ;; the number of segments will then always be
       ;; a Fibonacci number.
       (let* ((factor 0.61803398875)
	      (x1 (* x factor))
	      (x2 (* x (- 1 factor))))
	 (--> (A x1 c1 c2) (A x2 c1 c2)))))))

(defmethod homomorphism ((ls spline-curvature))
  (choose-production ls
    (X (--> (A 0 0 0)))
    ((A x c1 c2)
     (let* ((curvature1 (* c1 500.0))
	    (curvature2 (* c2 500.0))
	    (angle1 (* curvature1 x))
	    (angle2 (* curvature2 x)))
       (--> (+ angle1) [ (:color #(1.0 0.0 0.0))
			 (:sphere 5.0) ]
	    (F x) (+ angle2))))))

;; ********************************************************
;; *** Simple twining plant (adapted from Radomir Mech) ***
;; ********************************************************

(defparameter *st-prad* 10)
(defparameter *st-srad* 1)

(defun in-obstacle (pos)
  (let ((x (aref pos 0))
	(z (aref pos 2))
	(p+r (+ *st-prad* *st-srad*)))
    (< (+ (* x x) (* z z)) (* p+r p+r))))

(defclass simple-twining (l-system)
  ((axiom :initform `([ (! ,*st-prad*) (F 350) ]
		      (! ,*st-srad*) (:color #(0.2 1.0 0.2))
		      (+ 90) (\f ,(+ *st-srad* *st-prad* 2)) (- 90)
		      (S 0 0) (A 1 1)))
   (angle-increment :initform 1.0)
   (line-style :initform :cylinders)
   (homomorphism-depth :initform 10)
   (limits :initform '((-50 -5 0) (50 400 1)))
   (frame-delay :initform 0.1)
   (frame-list :initform '((0 10000)))
   (mode :accessor mode)
   (rotation-point :accessor rotation-point)
   ))

(defmethod rewrite :before ((ls simple-twining) &optional (depth 1))
  (declare (ignore depth))
  (setf (mode ls) :grow
	(rotation-point ls) 0))

(defmethod rewrite1 :after ((ls simple-twining))
  (setf (mode ls) (if (eql (mode ls) :grow)
		      :test
		      :grow)))

(defmethod l-productions ((ls simple-twining))
  (let ((ADEL 1)
	(RSTEP 2))
    (if (eql (mode ls) :grow)
	;; Grow
	(choose-production ls
	  ((A ord delay)
	   (if (plusp delay)
	       (--> (A ord (1- delay)))
	       (--> (S ord 0) (?P nil ord) (A (1+ ord) ADEL))))
	  ((S ord rang)
	   (when (= ord (rotation-point ls))
	     (--> (S ord (+ rang RSTEP))))))
	;; Test
	(choose-production ls
	  ((?P pos ord)
	   (when (in-obstacle pos)
	     (setf (rotation-point ls) ord)
	     (--> (?P nil ord))))))))

(defmethod homomorphism ((ls simple-twining))
  (choose-production ls
    ((S ord rang)
     (--> (/ rang) F +))))

(defun simple-twining-povanim ()
  (povray-animation (make-instance 'simple-twining)
		    :full-scene nil
		    :frames '((100 1400 100))))

;; ******************************
;; *** Twining plant L-system ***
;; ******************************

(defparameter *timesteps-per-hour* 60d0
  "Steps per hour of the simulation")
(defparameter *rotation-per-hour* 180d0
  "Rotation angle in degrees")
(defparameter *max-segment-length* 1d0
  "Maximum length of a stem segment")
(defparameter *grow-area* 20d0
  "Length of the stem's grow area from the tip")
(defparameter *turn-curvature* 2d0
  "Amount of turning of the stem")
(defparameter *rotation-step* (/ *rotation-per-hour* *timesteps-per-hour*)
  "Rotation per timestep")
(defparameter *backtrack-step* (* -0.2d0 *rotation-step*)
  "Backtracking step.")
;; Radii
(defparameter *pole-radius* 1.5d0)
(defparameter *stem-radius* 0.3d0)
(defparameter *pole-xpos* 3.0d0
  "Pole's x position (y and z position is zero)")

(defparameter *pole-color* #(1.0 0.6 0.4))
(defparameter *stem-color* #(0.5 1.0 0.5))

(defparameter *time-between-leaves* 5d0
  "Time in simulated hours between each new leaf forming.")
(defparameter *max-leaf-size* 500 "An integer around 50-500.")

(defun pole-crash (pos)
  (let* ((x (aref pos 0))
	 (z (aref pos 2))
	 (x1 (- x *pole-xpos*))
	 (radius (+ *pole-radius* *stem-radius*)))
    (< (+ (* x1 x1) (* z z))
       (* radius radius))))

(defun edit-growth ()
  (edit-spline :filename "growth.spline"))

(defun grow-segment (segment-length stem-position stem-length
				    spline-function)
  (let ((apex-position (- stem-length stem-position)))
    (if (> apex-position *grow-area*)
      segment-length ;; no growth outside grow area
      (let ((funval (funcall spline-function (/ apex-position *grow-area*))))
	(+ segment-length
	   (* segment-length (/ funval *timesteps-per-hour*)))))))

(defclass twining (l-system)
  ((axiom :initform `([ (:set-width ,*pole-radius*)
			(:set-position #(,*pole-xpos* 0 0))
			(:color ,*pole-color*)
			(:forward 80) ]
		      (:set-width ,*stem-radius*)
		      (:color ,*stem-color*)
		      (:forward 1)
		      (T ,*max-segment-length* 0 0 0)
		      (apex 0)))
   (decomposition-depth :initform 10)
   (homomorphism-depth :initform 10)
   (sensitive :initform t)
   (consider-list :initform '(T pre-leaf))
   (frame-list :initform '((0 10000)))
   (frame-delay :initform 0.01)
   (line-style :initform :cylinders)
   (spline :accessor spline
	   :initform (input-spline "growth.spline" :make-function t))
   (spline-func :accessor spline-func)
   (mode :accessor mode)
   (rotation-point :accessor rotation-point)
   (new-rotation-point :accessor new-rotation-point)
   (exit-backtrack :accessor exit-backtrack)
   (stem-pos :accessor stem-pos)
   (stem-length :accessor stem-length)
   ))

(defmethod rewrite :before ((ls twining) &optional (depth 0))
  (declare (ignore depth))
  (setf (mode ls) :grow
	(rotation-point ls) 0d0
	(new-rotation-point ls) 0d0
	(exit-backtrack ls) nil
	(spline-func ls) (spline-function (spline ls))
	(stem-pos ls) *max-segment-length*))

(defmethod rewrite1 :before ((ls twining))
  (when (eq (mode ls) :grow)
    (setf (stem-length ls) (stem-pos ls) ; length of the whole stem
	  (stem-pos ls) 0d0))            ; position after current segment
  ;(format t "mode: ~A~%" (mode ls))
  )

(defmethod rewrite1 :after ((ls twining))
  (case (mode ls)
    (:grow
     (setf (mode ls) :test))
    (:test
     (setf (mode ls) (if (= (rotation-point ls)
			    (new-rotation-point ls))
		       :grow
		       :backtrack)))
    (:backtrack
     (when (exit-backtrack ls)
       (setf (exit-backtrack ls) nil
	     (rotation-point ls) (new-rotation-point ls)
	     (mode ls) :grow))))
  ;;(format t "depth ~A mode ~A~%" (current-depth ls) (mode ls))
  )

(defmethod l-productions ((ls twining))
  (case (mode ls)
    (:grow
     (choose-production ls
       ((T length position contact-point roll-angle)
	(let* ((new-length (grow-segment length position (stem-length ls)
					 (spline-func ls)))
	       (old-stem-pos (stem-pos ls))
	       (stem-pos (+ old-stem-pos new-length)))
	  (setf (stem-pos ls) stem-pos)
	  ;;(incf (pos ls) new-length)
	  (if contact-point
	    (if (< contact-point old-stem-pos)
	      ;; contact point disappears (to the left)
	      (--> (T new-length old-stem-pos nil nil))
	      ;; contact point remains
	      (let ((new-rotation (if (= contact-point (rotation-point ls))
				    (+ roll-angle *rotation-step*)
				    roll-angle)))
		(--> (T new-length old-stem-pos
			contact-point new-rotation))))
	    (progn
	      ;; new contact point from the right
	      (with-rc ((T rlen rpos rc rangle))
		(when (and rc (< rc stem-pos))
		  (let ((new-rotation (if (= rc (rotation-point ls))
					(+ rangle *rotation-step*)
					rangle)))
		    (--> (T new-length old-stem-pos rc new-rotation)))))
	      ;; just growing
	      (--> (T new-length old-stem-pos nil nil))))))
       ((apex n)
	(if (>= n *time-between-leaves*)
	  (--> pre-leaf (apex 0))
	  (--> (apex (+ n (/ 1d0 *timesteps-per-hour*))))))
       (pre-leaf
	(--> nil))
       ((?P pos)
	(while-considering ls (T leaf pre-leaf)
	  (with-rc ((T a b c d) pre-leaf)
	    (--> (?P nil) (leaf 0 0)))))
       ((leaf size rotation)
	(with-rc ((T len pos contact-point rotation-angle))
	  ;; does the leaf pass through a contact point?
	  (when (and contact-point (< contact-point (stem-pos ls)))
	    ;; yes: update rotation
	    (incf rotation rotation-angle)))
	;; Slowly move leaf rotation back to normal (zero).
	;; Looks a bit strange, but it is pretty much the best
	;; thing I can do with this model.
	;; The X value of (if (< rotation X) ...) decides which way
	;; it will turn, 180 is the obvious value, but smaller values
	;; may look better in some cases...?
	;;(with-rc ((T len pos contact-point rotation-angle))
	;;  (when (> pos (rotation-point ls))
	;;    (incf rotation (* 0.5 *rotation-step*))))
	(setq rotation (mod rotation 360.0))
	(if (< rotation 90.0)
	    (setq rotation (max 0.0 (- rotation (* *rotation-step* 0.5))))
	    (setq rotation (min 360.0 (+ rotation (* *rotation-step* 0.5)))))
	(--> (leaf (min (+ size 1) *max-leaf-size*) rotation)))
       ))
     (:test
     (choose-production ls
       ((?P pos)
	(with-rc ((T length stem-pos cp ra))
	  ;; choose the LOWEST crash point as the new rotation point
	  (when (and (> stem-pos (+ (rotation-point ls)
				    (* *max-segment-length* 1.1)))
		     (= (new-rotation-point ls) (rotation-point ls))
		     (pole-crash pos))
	    ;(format t "crash! ~A~%" pos)
	    (setf (new-rotation-point ls) stem-pos))
	  ;; choose the HIGHEST crash point as the new rotation point
	  #+nil
	  (when (and (> stem-pos (+ (rotation-point ls)
				    (* *max-segment-length* 1.1)))
		     (pole-crash pos))
	    (setf (new-rotation-point ls) stem-pos))
	  ;; don't need to change any modules, so just return T
	  t))))
    (:backtrack
     (choose-production ls
       ((T length stem-pos cp ra)
	(cond ((= stem-pos (new-rotation-point ls))
	       ;;(format t "~A~%" (current-module ls))
	       ;; when the new rotation point is out of obstacle, exit
	       ;; backtrack mode
	       (while-considering ls (?P T)
		 (with-lc ((?P pos))
		   ;;(format t "pos ~A~%" pos)
		   (when (not (pole-crash pos))
		     ;(format t "exit backtracking at ~A~%" pos)
		     (setf (exit-backtrack ls) t))))
	       ;; insert contact point at new rotation point
	       (--> (T length stem-pos stem-pos 0)))
	      ((and cp (= cp (rotation-point ls)))
	       ;; backtrack at (old) rotation point
	       (--> (T length stem-pos cp (+ ra *backtrack-step*))))))))
    ))

(defmethod decomposition ((ls twining))
  (choose-production ls
    ((T length pos cp ra)
     (when (> length *max-segment-length*)
       (let* ((length/2 (/ length 2)))
	 (cond ((null cp)
		(--> (T length/2 pos nil nil) (?P nil)
		     (T length/2 (+ pos length/2) nil nil)))
	       ((< cp (+ pos length/2))
		(--> (T length/2 pos cp ra) (?P nil)
		     (T length/2 (+ pos length/2) nil nil)))
	       (t
		(--> (T length/2 pos nil nil) (?P nil)
		     (T length/2 (+ pos length/2) cp ra)))))))))

(defmethod homomorphism ((ls twining))
  (choose-production ls
    ((T length pos cp ra)
     (if (null cp)
       (let ((angle (* length *turn-curvature*)))
	 (--> (:turn-left angle) (:forward length) (:turn-left angle) S))
       (let* ((len1 (- cp pos))
	      (len2 (- length len1))
	      (ang1 (* len1 *turn-curvature*))
	      (ang2 (* len2 *turn-curvature*)))
	 (--> (:turn-left ang1) (:forward len1) (:turn-left ang1)
	      (:roll-right ra) S2
	      (:turn-left ang2) (:forward len2) (:turn-left ang2) S
	      ))))
    (S (--> [ (:color #(1 0 0)) (:sphere (* *stem-radius* 1.1)) ]))
    (S2 (--> [ (:color #(0 0 1)) (:sphere (* *stem-radius* 1.1)) ]))
    ((leaf size rotation)
     (--> [ (:roll-left (+ 90 rotation)) (:pitch-down 145)
	    (:color #(0.5 1.0 0.5))
	    (:set-width 0.1) (:forward (/ (1+ size) 30)) ] ))
    ))

;; Pretty twining plant (changes homomorphism from TWINING)
;; also contains some code for outputting povray animations
(defclass ptwining (twining)
  ((frame-count :accessor frame-count)
   (frame-list :initform '((0 1600 100)))))

(defmethod rewrite :before ((ls ptwining) &optional (depth 0))
  (declare (ignore depth))
  (setf (frame-count ls) 0))

(defmethod rewrite1 :after ((ls ptwining))
  (when (eql (mode ls) :test)
    (incf (frame-count ls))))

(defmethod homomorphism ((ls ptwining))
  (choose-production ls
    ((T length pos cp ra)
     (if (null cp)
       (let ((angle (* length *turn-curvature*)))
	 (--> (:turn-left angle) (:forward length) (:turn-left angle) S))
       (let* ((len1 (- cp pos))
	      (len2 (- length len1))
	      (ang1 (* len1 *turn-curvature*))
	      (ang2 (* len2 *turn-curvature*)))
	 (--> (:turn-left ang1) (:forward len1) (:turn-left ang1)
	      (:roll-right ra) S
	      (:turn-left ang2) (:forward len2) (:turn-left ang2) S
	      ))))
    (S (--> (:sphere *stem-radius*)))
    ((leaf size rotation)
     (let* ((initial-roll 65)
	    (steps 5)
	    (forward-per-step (/ (1+ size) steps 70))
	    (pitch-per-step 20)
	    (initial-pitch (* pitch-per-step (1- steps) 0.5))
	    (down-pitch (min (* 300 (/ size *max-leaf-size*))
			     180)))
       (--> [ (:roll-left (+ 90 rotation))
	      (:pitch-down 90) (:set-width 0.1) (:forward *stem-radius*)
	      (:pitch-up 90)
	      (:pitch-down down-pitch)
	      (:color #(0.5 1.0 0.5))
	      :start-mesh
	      [ (:roll-left initial-roll) (:pitch-up initial-pitch)
		(mesh-bow pitch-per-step steps forward-per-step) ]
	      :new-strand
	      [ (:roll-right initial-roll) (:pitch-up initial-pitch)
		(mesh-bow pitch-per-step steps forward-per-step) ]
	      ;;(:set-width 0.1) (:forward (/ (1+ size) 30)) ] ))
	      :end-mesh ] )))
    ((mesh-bow p steps len)
     (when (> steps 0)
       (--> (:forward-mesh-vertex len) (:pitch-down p)
	    (mesh-bow p (1- steps) len))))
    ))

(defun twining-povanim (&optional (nframes nil))
  (let ((ls (make-instance 'ptwining)))
    (rewrite ls 0)
    (do* ((framelist (copy-tree (or nframes (frame-list ls)))))
	 ((null framelist) :done)
      (let* ((frame (extract-framelist framelist))
	     (filename (format nil "twining~A.pov" frame)))
	(loop until (<= frame (frame-count ls))
	      do (rewrite1 ls))
	(create-geometry ls)
	(format t "Outputting '~A'..." filename)
	(force-output)
	(output-povray (geometry ls) filename
		       :full-scene nil
		       :width-multiplier 1.0)
	(format t "done.~%")))))

