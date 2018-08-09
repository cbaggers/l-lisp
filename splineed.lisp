;;; *** splineed.lisp ***
;;;
;;; This file is part of L-Lisp by Knut Arild Erstad.
;;; An OpenGL based spline editor.

(in-package :spline-editor)

(defun tri-solve (a d c b)
  "Solve a tridiagonal set of equations.

Input:
    A, D, C:   the diagonal vectors of the matrix
    B:         the right-hand vector

Outputs a solution vector X.

The Matrix is on the form:
|  d0   c0                 : b0   |
|  a1   d1   c1            : b1   |
|       a2   d2   ..       : ..   |
|            ..   ..  cN-2 : bN-2 |
|                aN-1 dN-1 : bN-1 |

The numbers a0 and cN-1 are ignored.
"
  (let ((n (length a)))
    ;; check sizes; will allow that C has length n-1
    (when (or (/= n (length b))
              (/= n (length d))
              (and (/= n (length c))
                   (/= n (1+ (length c)))))
      (error "wrong vector sizes."))
    ;; Gauss elimination (eliminating A)
    (do ((k 1 (1+ k)))
        ((>= k n))
      ;; save multiplicator in A
      (setf (svref a k) (/ (svref a k) (svref d (1- k))))
      ;; update D and B
      (decf (svref d k) (* (svref a k) (svref c (1- k))))
      (decf (svref b k) (* (svref a k) (svref b (1- k)))))
    ;; back substitution (eliminating C)
    (do ((k (1- n) (1- k)))
        ((<= k 0))
      ;; save multiplicator in C
      (setf (svref c (1- k)) (/ (svref c (1- k)) (svref d k)))
      ;; update B
      (decf (svref b (1- k)) (* (svref c (1- k)) (svref b k))))
    ;; final step: b[k] = b[k] / d[k], then return b
    (dotimes (k n b)
      (setf (svref b k) (/ (svref b k) (svref d k))))))

(defun natural-cubic-spline-coefficients (x y)
  "Find the coefficients of a natural cubic spline.

Input:
    X, Y: (n+1)-vectors which define the control points
    X must be sorted.
Output:
    Four n-vectors A, B, C, D which represent the coefficients of the
    natural cubic spline interpolating over X, Y, such that
    p(x) = a_i * (x-x_i)^3 + b_i * (x-x_i)^2 + c_i * (x-x_i) + d_i
      when x_i < x < x_i+1
"
  (let ((n (1- (length x))))
    (when (/= (length y) (1+ n))
      (error "X and Y must be the same size."))
    (when (< n 1)
      (error "too small vectors."))
    (let* ((h (map 'vector #'- (subseq x 1) x))
           (sol (tri-solve (subseq h 0 (1- n))
                           (map 'vector
                                #'(lambda (h0 h1) (* 2 (+ h0 h1)))
                                h (subseq h 1))
                           (subseq h 1)
                           (map 'vector
                                #'(lambda (h0 h1 y0 y1 y2)
                                    (* 6 (- (/ (- y2 y1) h1)
                                            (/ (- y1 y0) h0))))
                                h (subseq h 1) y (subseq y 1) (subseq y 2))))
           (s (concatenate 'vector '(0) sol '(0)))
           (a (map 'vector
                   #'(lambda (h0 s0 s1) (/ (- s1 s0) 6 h0))
                   h s (subseq s 1)))
           (b (map 'vector
                   #'(lambda (s0) (/ s0 2))
                   (subseq s 0 n)))
           (c (map 'vector
                   #'(lambda (h0 y0 y1 s0 s1)
                       (- (/ (- y1 y0) h0)
                          (/ (+ (* 2 h0 s0) (* h0 s1)) 6)))
                   h y (subseq y 1) s (subseq s 1)))
           (d (subseq y 0 n)))
      (values a b c d))))

(defstruct spline
  x y a b c d function
  )

(defun spline-min-x (spline)
  (let ((x (spline-x spline)))
    (svref x 0)))

(defun spline-max-x (spline)
  (let ((x (spline-x spline)))
    (svref x (1- (length x)))))

(defun spline-min-y (spline)
  (let* ((y (spline-y spline))
         (min-y (svref y 0)))
    (do ((i 1 (1+ i)))
        ((>= i (length y)) min-y)
      (setq min-y (min min-y (svref y i))))))

(defun spline-max-y (spline)
  (let* ((y (spline-y spline))
         (max-y (svref y 0)))
    (do ((i 1 (1+ i)))
        ((>= i (length y)) max-y)
      (setq max-y (max max-y (svref y i))))))

(defun natural-cubic-spline (x y &key (make-function nil))
  (multiple-value-bind
        (a b c d) (natural-cubic-spline-coefficients x y)
    (let ((spline (make-spline :x x :y y :a a :b b :c c :d d)))
      (when make-function
        (setf (spline-function spline) (make-spline-function spline)))
      spline)))

(defun recalculate-natural-cubic-spline (spline &key (make-function nil))
  (multiple-value-bind
        (a b c d) (natural-cubic-spline-coefficients
                   (spline-x spline) (spline-y spline))
    (setf (spline-a spline) a
          (spline-b spline) b
          (spline-c spline) c
          (spline-d spline) d
          (spline-function spline)
          (if make-function (make-spline-function spline) nil))
    spline))

(defun spline-value (spline x-value)
  (let* ((i 0)
         (x (spline-x spline))
         (n (length x)))
    (loop (when (or (>= i (1- n)) (>= (svref x (1+ i)) x-value))
            (return))
       (incf i))
    (let* ((h (- x-value (svref x i)))
           (hh (* h h))
           (hhh (* h hh))
           (a (svref (spline-a spline) i))
           (b (svref (spline-b spline) i))
           (c (svref (spline-c spline) i))
           (d (svref (spline-d spline) i)))
      (+ (* a hhh) (* b hh) (* c h) d))))

(defun spline-values (spline &key
                               (start (spline-min-x spline))
                               (end   (spline-max-x spline))
                               (steps 100))
  (let* ((r (make-array steps))
         (x-step (/ (- end start) (1- steps)))
         (x (spline-x spline))
         (i 0)
         (n (1- (length x)))
         (xi (svref x 0))
         (xi+1 (svref x 1))
         (a (spline-a spline))
         (b (spline-b spline))
         (c (spline-c spline))
         (d (spline-d spline))
         (ai (svref a 0))
         (bi (svref b 0))
         (ci (svref c 0))
         (di (svref d 0)))
    (dotimes (j steps r)
      (let ((xpos (+ start (* x-step j))))
        ;; make sure we're in the right interval
        (loop (if (and (> xpos xi+1) (< i (1- n)))
                  (progn (incf i)
                         (setq xi xi+1)
                         (setq xi+1 (svref x (1+ i)))
                         (setq ai (svref a i))
                         (setq bi (svref b i))
                         (setq ci (svref c i))
                         (setq di (svref d i)))
                  (return)))
        ;; calculate spline value
        (let* ((h (- xpos xi))
               (hh (* h h))
               (hhh (* hh h)))
          (setf (svref r j) (+ (* ai hhh) (* bi hh) (* ci h) di)))))))

;; Old version of MAKE-SPLINE-FUNCTION: produces a small but slow function
#+nil
(defun make-spline-function (spline)
  #'(lambda (x)
      (spline-value spline x)))

;; New version of MAKE-SPLINE-FUNCTION: this should be pretty fast for
;; most purposes.  For really big splines (with many control points)
;; binary search might be worthwhile.
;; It is also my first real attempt at on-the-fly code generation
;; and compilation.  Ain't Lisp cool?  ;)
(defun make-spline-function (spline &key (number-type 'double-float))
  (let ((*compile-print* nil)
        (lambda-expr
         `(lambda (x-in)
            (let ((x (coerce x-in ',number-type)))
              (declare (optimize (speed 3) (safety 0)
                                 #+cmu (ext:inhibit-warnings 3))
                       (,number-type x))
              (cond ,@(map 'list
                           #'(lambda (xi xi+1 ai bi ci di)
                               `((< x ,xi+1)
                                 (let* ((h (- x ,xi))
                                        (hh (* h h))
                                        (hhh (* hh h)))
                                   (declare (,number-type h hh hhh))
                                   (+ (* ,ai hhh) (* ,bi hh) (* ,ci h) ,di))))
                           (spline-x spline)
                           (subseq (spline-x spline)
                                   1 (1- (length (spline-x spline))))
                           (spline-a spline) (spline-b spline)
                           (spline-c spline) (spline-d spline))
                    ,(let* ((x (spline-x spline))
                            (a (spline-a spline))
                            (b (spline-b spline))
                            (c (spline-c spline))
                            (d (spline-d spline))
                            (i (1- (length a))))
                       `(t (let* ((h (- x ,(svref x i)))
                                  (hh (* h h))
                                  (hhh (* hh h)))
                             (declare (,number-type h hh hhh))
                             (+ (* ,(svref a i) hhh) (* ,(svref b i) hh)
                                (* ,(svref c i) h) ,(svref d i))))))))))
    (values (compile nil lambda-expr) lambda-expr)))

(defun output-spline (spline filename)
  (let ((*print-readably* t)
        (func (spline-function spline)))
    (setf (spline-function spline) nil)
    (with-open-file (stream filename
                            :direction :output :if-exists :supersede)
      (prin1 spline stream))
    (setf (spline-function spline) func)
    nil))

(defun input-spline (filename &key (make-function nil))
  (let (spline)
    (with-open-file (stream filename)
      (setq spline (read stream)))
    (unless (typep spline 'spline)
      (error "Could not read SPLINE object."))
    (when make-function
      (setf (spline-function spline) (make-spline-function spline)))
    spline))

;; Ouput EPS:
(defun output-spline-to-eps (spline filename &key (steps 100) (border 0.1))
  (let* ((xmin (spline-min-x spline))
         (xmax (spline-max-x spline))
         (xsize (- xmax xmin))
         (xinc (* xsize border))
         (new-xmin (- xmin xinc))
         (new-xmax (+ xmax xinc))
         (new-xsize (- new-xmax new-xmin))
         (ymin (min (spline-min-y spline) 0.0))
         (ymax (max (spline-max-y spline) 0.0))
         (ysize (- ymax ymin))
         (yinc (* ysize border))
         (new-ymin (- ymin yinc))
         (new-ymax (+ ymax yinc))
         (new-ysize (- new-ymax new-ymin))
         (ps-xsize 300)
         (ps-ysize (round (* ps-xsize (/ new-ysize new-xsize))))
         ;;(vals (spline-values spline :steps steps))
         )
    (with-open-file (file filename :direction :output
                          :if-exists :supersede)
      ;; header
      (format file "%!PS-Adobe-3.0 EPSF-3.0
%%BoundingBox: 0 0 ~A ~A
%%Creator: L-Lisp's spline editor by Knut Arild Erstad (knute@ii.uib.no)
%%EndComments~%" ps-xsize ps-ysize)
      ;; spline
      (loop for x from xmin to xmax by (/ xsize steps) do
           (let* ((y (spline-value spline x))
                  (xrel (/ (- x new-xmin) new-xsize))
                  (yrel (/ (- y new-ymin) new-ysize)))
             (format file "~,9F ~,9F ~A~%"
                     (* xrel ps-xsize) (* yrel ps-ysize)
                     (if (= x xmin) "moveto" "lineto"))))
      (format file "stroke~%")
      ;; x axis
      (let* ((yrel-zero (/ (- new-ymin) new-ysize))
             (ps-yzero (* yrel-zero ps-ysize)))
        (format file "[1 2] 0 setdash
~,9F ~,9F moveto ~,9F ~,9F lineto stroke~%"
                0 ps-yzero ps-xsize ps-yzero))
      ;; y axis
      (let* ((xrel-zero (/ (- new-xmin) new-xsize))
             (ps-xzero (* xrel-zero ps-xsize)))
        (format file "~,9F ~,9F moveto ~,9F ~,9F lineto stroke~%"
                ps-xzero 0 ps-xzero ps-ysize)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SDL and OpenGL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (spline-window (:conc-name sw-))
  ;; slots that should be initialized by EDIT-SPLINE
  spline
  window
  steps
  grid
  strong-grid
  draw-points
  draw-axes
  ;; slots that don't need initialization
  (marked-point nil)
  (save-when-quit t)
  ;; slots that should be initialized by SPLINE-RESHAPE
  top
  bottom
  left
  right
  height
  width
  )

(defclass l-lisp-spline-window (kit.sdl2:gl-window)
  ((spline-window
    :initarg :spline-window
    :accessor spline-window)
   (spline
    :initarg :spline
    :accessor spline)
   (filename
    :initarg :filename
    :accessor filename)
   (eps-filename
    :initarg :eps-filename
    :accessor eps-filename)))

(defmethod initialize-instance :after ((window l-lisp-spline-window) &key &allow-other-keys)
  (setf (kit.sdl2:idle-render window) t)
  (with-slots (spline-window) window
    (spline-reshape spline-window (kit.sdl2:window-width window) (kit.sdl2:window-height window))))

(kit.sdl2:define-start-function create-spline-window (&key (spline-window nil) (width 500) (height 300) (filename nil) (eps-filename nil))
  (make-instance 'l-lisp-spline-window :spline-window spline-window :w width :h height :filename filename :eps-filename eps-filename))

(defmethod kit.sdl2:close-window :before ((window l-lisp-spline-window))
  (let ((debug t))
    (when debug (write-line "Closing window")))
  (with-slots (filename eps-filename spline-window) window
    (with-slots (spline) spline-window
      ;; maybe ouput to file
      (when filename
	(if (sw-save-when-quit spline-window)
	    (progn (format t "Writing to file '~A'~%" filename)
		   (output-spline spline filename))
	    (format t "Quitting without saving.~%")))
      ;; maybe output EPS file
      (when eps-filename
	(if (sw-save-when-quit spline-window)
	    (progn (format t "Writing EPS file '~A'~%" eps-filename)
		   (output-spline-to-eps spline eps-filename))
	    (format t "Not writing EPS file.~%")))
      ;; TODO: How to return the spline when edit-spline is being called?
      ;; Should it be a blocking call when creating and setting up the windows for editing the spline?
      spline)))

(defmethod kit.sdl2:render ((window l-lisp-spline-window))
  (with-slots (spline-window) window
    (spline-draw spline-window)))

(defun window-coordinates (spline-window x y)
  (let* ((left (sw-left spline-window))
         (right (sw-right spline-window))
         (top (sw-top spline-window))
         (bottom (sw-bottom spline-window))
         (window-width (sw-width spline-window))
         (window-height (sw-height spline-window))
         (x-size (- right left))
         (y-size (- top bottom))
         (relative-x (/ (- x left) x-size))
         (relative-y (/ (- y bottom) y-size))
         (window-x (* relative-x window-width))
         (window-y (- window-height (* relative-y window-height))))
    (values window-x window-y)))

(defun real-coordinates (spline-window x y)
  (let* ((left (sw-left spline-window))
         (right (sw-right spline-window))
         (top (sw-top spline-window))
         (bottom (sw-bottom spline-window))
         (window-width (sw-width spline-window))
         (window-height (sw-height spline-window))
         (x-size (- right left))
         (y-size (- top bottom))
         (relative-x (/ x window-width))
         (relative-y (/ (- window-height y) window-height))
         (real-x (+ left (* relative-x x-size)))
         (real-y (+ bottom (* relative-y y-size))))
    (values real-x real-y)))

(defun spline-reshape (spline-window width height)
  (let* ((spline (sw-spline spline-window))
         (xmin (spline-min-x spline))
         (xmax (spline-max-x spline))
         (base-xsize (- xmax xmin -0.01))
         ;;(left (1- (spline-min-x spline)))
         ;;(right (1+ (spline-max-x spline)))
         (left (- xmin (* 0.1 base-xsize)))
         (right (+ xmax (* 0.1 base-xsize)))
         (aspect (/ width height))
         (xsize (- right left))
         (ysize (/ xsize aspect))
         ;;(bottom (- (/ ysize 2)))
         ;;(bottom (1- (spline-min-y spline)))
         (ycenter (/ (+ (spline-min-y spline) (spline-max-y spline)) 2))
         (bottom (- ycenter (/ ysize 2)))
         (top (+ bottom ysize)))
    (setf (sw-left spline-window) left
          (sw-right spline-window) right
          (sw-bottom spline-window) bottom
          (sw-top spline-window) top
          (sw-width spline-window) width
          (sw-height spline-window) height)
    (gl:viewport 0 0 width height)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    ;;(gl:gluOrtho2D -1d0 5d0 -1d0 5d0)
    (glu:ortho-2d (coerce left 'double-float)
                   (coerce right 'double-float)
                   (coerce bottom 'double-float)
                   (coerce top 'double-float))
    (gl:matrix-mode :modelview)
    (gl:load-identity)))

(defmacro convert-to (type &body places)
  `(progn
     ,@(mapcar #'(lambda (x) `(setf ,x (coerce ,x ',type)))
               places)))

(defun draw-grid (spline-window grid r g b)
  (let ((left (sw-left spline-window))
        (right (sw-right spline-window))
        (top (sw-top spline-window))
        (bottom (sw-bottom spline-window)))
    (convert-to single-float left right top bottom grid r g b)
    (%gl:color-3f r g b)
    (gl:begin :lines)
    (do ((x-val 0.0 (+ x-val grid)))
        ((> x-val right))
      (%gl:vertex-2f x-val top)
      (%gl:vertex-2f x-val bottom))
    (do ((x-val (- grid) (- x-val grid)))
        ((< x-val left))
      (%gl:vertex-2f x-val top)
      (%gl:vertex-2f x-val bottom))
    (do ((y-val 0.0 (+ y-val grid)))
        ((> y-val top))
      (%gl:vertex-2f left y-val)
      (%gl:vertex-2f right y-val))
    (do ((y-val (- grid) (- y-val grid)))
        ((< y-val bottom))
      (%gl:vertex-2f left y-val)
      (%gl:vertex-2f right y-val))
    (gl:end)))

(defun spline-draw (spline-window)
  (gl:clear-color 1.0 1.0 1.0 1.0)
  (gl:clear :color-buffer-bit)
  ;; Grid
  (let ((grid (sw-grid spline-window))
        (strong-grid (sw-strong-grid spline-window)))
    (when grid
      (draw-grid spline-window grid 0.85 0.85 0.85))
    (when strong-grid
      (draw-grid spline-window strong-grid 0.6 0.6 0.6)))
  ;; Axes
  (when (sw-draw-axes spline-window)
    (%gl:color-3f 0.0 0.0 0.0)
    (gl:begin :lines)
    (%gl:vertex-2f (coerce (sw-left spline-window) 'single-float) 0.0)
    (%gl:vertex-2f (coerce (sw-right spline-window) 'single-float) 0.0)
    (%gl:vertex-2f 0.0 (coerce (sw-top spline-window) 'single-float))
    (%gl:vertex-2f 0.0 (coerce (sw-bottom spline-window) 'single-float))
    (let* ((left (sw-left spline-window))
           (right (sw-right spline-window))
           (top (sw-top spline-window))
           ;;(bottom (sw-bottom spline-window))
           (pixel-size (/ (- right left) (sw-width spline-window)))
           (arrow-size (* pixel-size 5)))
      (convert-to single-float right top arrow-size)
      ;; arrows
      (%gl:Vertex-2f right 0.0)
      (%gl:vertex-2f (- right arrow-size) arrow-size)
      (%gl:vertex-2f right 0.0)
      (%gl:vertex-2f (- right arrow-size) (- arrow-size))
      (%gl:vertex-2f 0.0 top)
      (%gl:vertex-2f arrow-size (- top arrow-size))
      (%gl:vertex-2f 0.0 top)
      (%gl:vertex-2f (- arrow-size) (- top arrow-size))
      )
    (gl:end))
  ;; Spline
  (%gl:color-3f 0.0 0.0 1.0)
  (gl:line-width 1.0)
  (let* ((spline (sw-spline spline-window))
         (steps (sw-steps spline-window))
         (start (spline-min-x spline))
         (end   (spline-max-x spline))
         (x (spline-x spline))
         (x-step (/ (- end start) (1- steps)))
         (y-values (spline-values spline
                                  :start start :end end :steps steps)))
    (gl:begin :line-strip)
    (dotimes (i steps)
      (let ((xpos (+ start (* i x-step)))
            (ypos (svref y-values i)))
        (%gl:vertex-2f (coerce xpos 'single-float)
                       (coerce ypos 'single-float))))
    (gl:end)
    (when (sw-draw-points spline-window)
      (gl:point-size 5.0)
      (gl:begin :points)
      (let ((y (spline-y spline))
            (marked-point (sw-marked-point spline-window)))
        (dotimes (i (length x))
          (if (and marked-point (= marked-point i))
              (%gl:color-3f 0.0 1.0 0.0)
              (%gl:color-3f 0.0 0.0 0.0))
          (%gl:vertex-2f (coerce (svref x i) 'single-float)
                         (coerce (svref y i) 'single-float))))
      (gl:end))))

(defun spline-delete-point (spline-window)
  ;; remove marked point
  (let ((i (sw-marked-point spline-window)))
    (when i
      (let* ((spline (sw-spline spline-window))
             (x (spline-x spline))
             (y (spline-y spline))
             (n (length x)))
        (if (<= n 2)
            (warn "Cannot delete; there must be at least two points.")
            (progn
              (setf (spline-x spline) (concatenate 'vector
                                                   (subseq x 0 i)
                                                   (subseq x (1+ i) n))
                    (spline-y spline) (concatenate 'vector
                                                   (subseq y 0 i)
                                                   (subseq y (1+ i) n)))
              (recalculate-natural-cubic-spline spline)
              (setf (sw-marked-point spline-window) nil)))))))

(defun spline-keypress (window scancode state)
  (when (eq :keydown state)
    (with-slots (spline-window) window
      (case scancode
	(:scancode-escape
	 (setf (sw-save-when-quit spline-window) nil)
	 (kit.sdl2:close-window window))
	((:scancode-delete :scancode-d)
	 (spline-delete-point spline-window))
	(:scancode-q
	 (kit.sdl2:close-window window))
	(:scancode-p
	 (setf (sw-draw-points spline-window)
	       (not (sw-draw-points spline-window))))))))

(defun spline-button1motion (spline-window xpos ypos)
  (let ((marked-point (sw-marked-point spline-window)))
    (when marked-point
      (multiple-value-bind (real-x real-y)
          (real-coordinates spline-window xpos ypos)
        (let* ((spline (sw-spline spline-window))
               (x (spline-x spline))
               (y (spline-y spline))
               (n (length x)))
          ;; make sure a point doesn't get dragged past its neighbours
          (when (and (> marked-point 0)
                     (<= real-x (svref x (1- marked-point))))
            (setf real-x (+ (svref x (1- marked-point)) 0.01)))
          (when (and (< marked-point (1- n))
                     (>= real-x (svref x (1+ marked-point))))
            (setf real-x (- (svref x (1+ marked-point)) 0.01)))
          ;; update and draw spline
          (setf (svref x marked-point) real-x
                (svref y marked-point) real-y)
          (recalculate-natural-cubic-spline (sw-spline spline-window)))))))

(defun spline-button2click (spline-window xpos ypos)
  (multiple-value-bind (real-x real-y)
      (real-coordinates spline-window xpos ypos)
    (let* ((spline (sw-spline spline-window))
           (x (spline-x spline))
           (y (spline-y spline))
           (n (length x))
           (pos (position-if #'(lambda (xi) (<= real-x xi)) x)))
      (if (and pos (= (svref x pos) real-x))
          ;; attempt to insert two points at the same x-value, move old point
          (setf (svref x pos) real-x
                (svref y pos) real-y)
          ;; new point, change vectors
          (progn
            (if (null pos) (setq pos n))
            (setf (spline-x spline)
                  (concatenate
                   'vector (subseq x 0 pos) (vector real-x) (subseq x pos)))
            (setf (spline-y spline)
                  (concatenate
                   'vector (subseq y 0 pos) (vector real-y) (subseq y pos)))))
      (recalculate-natural-cubic-spline spline)
      (setf (sw-marked-point spline-window) pos))))

(defun spline-button1click (spline-window xpos ypos)
  (let* ((spline (sw-spline spline-window))
         (x (spline-x spline))
         (y (spline-y spline))
         (pixel-distance 5))
    (setf (sw-marked-point spline-window) nil)
    (dotimes (i (length x))
      (multiple-value-bind (win-xpos win-ypos)
          (window-coordinates spline-window (svref x i) (svref y i))
        (when (and (<= (abs (- win-xpos xpos))
                       pixel-distance)
                   (<= (abs (- win-ypos ypos))
                       pixel-distance))
          (setf (sw-marked-point spline-window) i)
          (return))))))

(defmethod kit.sdl2:mousebutton-event ((window l-lisp-spline-window) state timestamp button x y)
  (when (eql :mousebuttondown state)
    (with-slots (spline-window) window
      (let ((debug t))
	(write-line (format nil "button state: ~A and button ~A" state button))
	(when (= button 1)
	  (when debug (write-line "Clicked button 1"))
	  (spline-button1click spline-window x y))
	(when (= button 3)
	  (when debug (write-line "Clicked button 3"))
	  (spline-button2click spline-window x y))
	;; When button 3, then quit? (or just don't process it?)
	))))

(defmethod kit.sdl2:keyboard-event ((window l-lisp-spline-window) state timestamp repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (spline-keypress window scancode state)))

(defmethod kit.sdl2:mousemotion-event ((window l-lisp-spline-window) timestamp button-mask x y xrel yrel)
  (with-slots (spline-window) window
    (when (= button-mask 1)
      (spline-button1motion spline-window x y))))

(defmethod kit.sdl2:window-event ((window l-lisp-spline-window) (type (eql :size-changed)) timestamp data1 data2)
  (with-slots (spline-window) window
    (spline-reshape spline-window data1 data2)))

(defun edit-spline (&key (spline nil) (filename nil) (eps-filename nil)
                      (steps 100) (grid 0.1) (strong-grid 1.0)
                      (draw-points t) (draw-axes t))
  (unless spline
    ;; read from file if filename is given, otherwise create a default one
    (setq spline
          (if (and filename (probe-file filename))
              (progn (format t "Reading from file '~A'~%" filename)
                     (input-spline filename))
              (progn (when filename (format t "New file '~A'~%" filename))
                     (natural-cubic-spline '#(0.0 1.0) '#(0.0 0.0))))))
  (let ((spline-window (make-spline-window
                        :spline spline
                        :steps steps
                        :grid grid
                        :strong-grid strong-grid
                        :draw-points draw-points
                        :draw-axes draw-axes)))
    (create-spline-window :spline-window spline-window :width 500 :height 300 :filename filename :eps-filename eps-filename)
    ))
