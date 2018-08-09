;;; *** opengl.lisp ***
;;;
;;; This file is part of L-Lisp by Knut Arild Erstad.
;;; Contains code for OpenGL previews and animations of L-systems
;;;
;;; Ported to SDL2 (2018)

(in-package :l-systems)

(defstruct (l-window (:conc-name lw-))
  ;; L-system
  ls
  ;; line/cylinder settings
  (cylinder-width 1.0d0)
  (line-width 1.0d0)
  (line-style :lines)
  (cylinder-slices 8)
  ;; view settings
  minv maxv zoom width height rotate-angle tilt-angle
  recenter
  (angle-inc 0.1d0)
  (zoom-exp 1.002d0)
  ;; light settings
  (enable-lighting t)
  light-pos
  ;; animation settings
  animate
  frame-delay
  original-frame-list
  frame-list
  clock
  )

(defun gl-initialize (l-win)
  (let* ((ls (lw-ls l-win))
         (geometry (geometry ls)))
    ;; initialize values
    (multiple-value-bind (minv maxv)
        (find-limits geometry)
      (setf (lw-minv l-win) minv)
      (setf (lw-maxv l-win) maxv))
    (setf (lw-zoom l-win) 1.1d0)
    (setf (lw-rotate-angle l-win) 0d0)
    (setf (lw-tilt-angle l-win) 0d0)
    ;; light
    (when (lw-enable-lighting l-win)
      (setf (lw-light-pos l-win)
            (make-array 4 :element-type #-cmu 'fixnum #+cmu '(signed-byte 32)
                        :initial-contents '#(0 1 0 0)))
      (gl:enable :lighting)
      (gl:light-model :light-model-two-side 1)
      ;;(gl:glLightModelf gl:GL_LIGHT_MODEL_LOCAL_VIEWER 0.3)
      (let ((ambient (make-array 4 :element-type 'single-float
                                 :initial-contents #(0.2 0.2 0.2 1.0))))
        (gl:light-model :light-model-ambient ambient))
      (gl:enable :light0))
    ;; OpenGL init
    (gl:depth-func :less)
    (gl:enable :depth-test)))

(defun gl-reshape (l-win &key width height)
  (setf (lw-width l-win) width)
  (setf (lw-height l-win) height)
  (let* ((debug nil)
         (minv (lw-minv l-win))
         (maxv (lw-maxv l-win))
         (zoom (lw-zoom l-win))
         (x1 (aref minv 0))
         (x2 (aref maxv 0))
         (xcenter (/ (+ x1 x2) 2))
         (xsize (max (* (- x2 x1) zoom) 1d-5))
         (y1 (aref minv 1))
         (y2 (aref maxv 1))
         (ycenter (/ (+ y1 y2) 2))
         (ysize (max (* (- y2 y1) zoom) 1d-5))
         (z1 (aref minv 2))
         (z2 (aref maxv 2))
         (zcenter (/ (+ z1 z2) 2))
         (zsize (max (* (- z2 z1) zoom) 1d-10))
         (geometry-aspect (/ xsize ysize))
         (viewport-aspect (/ width height)))
    (when debug (format t "xsize ~A ysize ~A~%" xsize ysize))
    ;; increase either xsize or ysize, depending on aspect
    (if (> geometry-aspect viewport-aspect)
        (setq ysize (/ xsize viewport-aspect))
        (setq xsize (* ysize viewport-aspect)))
    (when debug (format t "after: xsize ~A ysize ~A~%" xsize ysize))
    (let ((half-x (/ xsize 2))
          (half-y (/ ysize 2))
          (z-inc (* 2 (max xsize ysize zsize))))
      (gl:viewport 0 0 width height)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho (- half-x) half-x
		(- half-y) half-y
		(- z-inc) z-inc)
      (gl:matrix-mode :modelview)
      (gl:load-identity)
      ;; rotate model
      (gl:rotate (lw-tilt-angle l-win) 1d0 0d0 0d0)
      (gl:rotate (lw-rotate-angle l-win) 0d0 1d0 0d0)
      ;; move it to center of the window
      (gl:translate (- xcenter) (- ycenter) (- zcenter)))))

(defvar *black* (make-array 4 :element-type 'single-float
                            :initial-contents '#(0.0 0.0 0.0 1.0)))

(defun gl-set-color (l-win color)
  (let ((col (or color *black*)))
    (if (lw-enable-lighting l-win)
        (gl:material :front-and-back
                         :ambient-and-diffuse
                         col)
        (%gl:color-4fv col))))

(defmethod gl-code (l-win elt)
  (declare (ignore l-win))
  (warn "Unkown type ~A passed to GL-CODE." (type-of elt)))

(defmethod gl-code (l-win (line line))
  (case (lw-line-style l-win)
    ;; *** lines ***
    (:lines
     (let* ((color (line-color line))
            (width (line-width line))
            (w (* (lw-line-width l-win) (or width 1.0d0)))
            (p1 (line-p1 line))
            (p2 (line-p2 line)))
       ;; set color
       (gl-set-color l-win color)
       ;; set linewidth
       (gl:line-width (coerce w 'single-float))
       ;; draw line
       (gl:begin :lines)
       (gl:vertex (aref p1 0) (aref p1 1) (aref p1 2))
       (gl:vertex (aref p2 0) (aref p2 1) (aref p2 2))
       (gl:end)))
    ;; *** cylinders ***
    (:cylinders
     (let ((p1 (line-p1 line))
           (p2 (line-p2 line)))
       (unless (equalvec p1 p2)
         (let* ((color (line-color line))
                (width (line-width line))
                (prev-width (line-prev-width line))
                (w1 (coerce (or prev-width width 1d0) 'double-float))
                (w2 (coerce (or width 1d0) 'double-float))
                ;;(p1 (line-p1 line))
                ;;(p2 (line-p2 line))
                (hvec (vec- p2 p1))
                (height (vlength hvec))
                (zvec (vec3 0d0 0d0 1d0))
                (rvec (cross-product zvec (normalize hvec)))
                (dot (aref hvec 2)) ;; [0 0 1] dot hvec = hvec[2]
                (rad-angle (acos dot))
                (deg-angle (rad-to-deg rad-angle))
                (cylinder-width (lw-cylinder-width l-win))
                ;;quad)
                (quad (glu:new-quadric)))
           (declare (optimize (speed 3)
                              #+cmu (ext:inhibit-warnings 3))
                    (type (simple-array double-float (3))
                          p1 p2 hvec zvec rvec)
                    (double-float w1 w2 dot rad-angle deg-angle
                                  cylinder-width))
           ;; set color
           (gl-set-color l-win color)
           ;; push matrix
           (gl:push-matrix)
           ;; translate to p1
           (gl:translate (aref p1 0) (aref p1 1) (aref p1 2))
           ;; rotate to hvec
           (gl:rotate deg-angle (aref rvec 0) (aref rvec 1) (aref rvec 2))
           ;; now the cylinder can be created
           (glu:quadric-normals quad :smooth)
           (glu:quadric-draw-style quad :fill)
           (glu:cylinder quad
                           (* w1 cylinder-width) (* w2 cylinder-width)
                           height (lw-cylinder-slices l-win) 1)
           ;; deallocate quad
           (glu:delete-quadric quad)
           (gl:pop-matrix)))))
    (t
     (error "Unknown LINE-STYLE ~A, should be :LINES or :CYLINDERS."
            (lw-line-style l-win))))
  )

(defmethod gl-code (l-win (sphere sphere))
  (gl-set-color l-win (sphere-color sphere))
  (case (lw-line-style l-win)
    (:cylinders
     (let ((pos (sphere-pos sphere))
           (radius (sphere-radius sphere))
           (quad (glu:new-quadric))
           (cylinder-width (lw-cylinder-width l-win)))
       (declare (optimize (speed 3) (safety 0))
                (double-float radius cylinder-width)
                (type (simple-array double-float (3)) pos))
       (gl:push-matrix)
       (gl:translate (aref pos 0) (aref pos 1) (aref pos 2))
       (glu:quadric-normals quad :smooth)
       (glu:quadric-draw-style quad :fill)
       (let* ((slices (lw-cylinder-slices l-win))
              (stacks (round slices 2)))
         (glu:sphere quad
                     (* cylinder-width radius)
                     slices stacks))
       (glu:delete-quadric quad)
       (gl:pop-matrix)))
    (:lines
     (let ((w (* (sphere-radius sphere) (or (lw-line-width l-win) 1.0d0))))
       (gl:point-size (coerce w 'single-float))
       (gl:begin :points)
       (with-slots (pos) sphere
	 (gl:vertex (aref pos 0) (aref pos 1) (aref pos 2)))
       (gl:end)))))

(defmethod gl-code (l-win (polygon polygon))
  (gl-set-color l-win (polygon-color polygon))
  (with-slots (normal) polygon
    (gl:normal (aref normal 0) (aref normal 1) (aref normal 2)))
  (gl:begin :polygon)
  (dolist (p (polygon-points polygon))
    ;;(declare (optimize (speed 3)))
    ;;(declare (type (simple-array double-float (3)) p))
    (%gl:vertex-3d (aref p 0) (aref p 1) (aref p 2)))
  ;;(gl:glVertex3dv p))
  (gl:end))

(defmethod gl-code (l-win (mesh mesh))
  (gl-set-color l-win (mesh-color mesh))
  (gl:begin :triangles)
  (dolist (triangle (mesh-triangles mesh))
    (dolist (vertex (triangle-vertices triangle))
      (with-slots (normal pos) vertex
	(gl:normal (aref normal 0) (aref normal 1) (aref normal 2))
	(gl:vertex (aref pos 0) (aref pos 1) (aref pos 2)))))
  (gl:end))

(defmethod gl-code (l-win (box box))
  (gl-set-color l-win (box-color box))
  (let* ((pos1 (box-pos box))
         (pos2 (vec+ pos1 (box-size box)))
         (x1 (aref pos1 0))
         (y1 (aref pos1 1))
         (z1 (aref pos1 2))
         (x2 (aref pos2 0))
         (y2 (aref pos2 1))
         (z2 (aref pos2 2)))
    ;; A box is six rectangles (polygons)...
    ;; 1: z = z1, normal = -Z
    (gl:begin :polygon)
    (%gl:normal-3i 0 0 -1)
    (%gl:vertex-3d x1 y1 z1)
    (%gl:vertex-3d x1 y2 z1)
    (%gl:vertex-3d x2 y2 z1)
    (%gl:vertex-3d x2 y1 z1)
    (gl:end)
    ;; 2: z = z2, normal = Z
    (gl:begin :polygon)
    (%gl:normal-3i 0 0 1)
    (%gl:vertex-3d x1 y1 z2)
    (%gl:vertex-3d x1 y2 z2)
    (%gl:vertex-3d x2 y2 z2)
    (%gl:vertex-3d x2 y1 z2)
    (gl:end)
    ;; 3: y = y1, normal = -Y
    (gl:begin :polygon)
    (%gl:normal-3i 0 -1 0)
    (%gl:vertex-3d x1 y1 z1)
    (%gl:vertex-3d x1 y1 z2)
    (%gl:vertex-3d x2 y1 z2)
    (%gl:vertex-3d x2 y1 z1)
    (gl:end)
    ;; 4: y = y2, normal = Y
    (gl:begin :polygon)
    (%gl:normal-3i 0 1 0)
    (%gl:vertex-3d x1 y2 z1)
    (%gl:vertex-3d x1 y2 z2)
    (%gl:vertex-3d x2 y2 z2)
    (%gl:vertex-3d x2 y2 z1)
    (gl:end)
    ;; 5: x = x1, normal = -X
    (gl:begin :polygon)
    (%gl:normal-3i -1 0 0)
    (%gl:vertex-3d x1 y1 z1)
    (%gl:vertex-3d x1 y1 z2)
    (%gl:vertex-3d x1 y2 z2)
    (%gl:vertex-3d x1 y2 z1)
    (gl:end)
    ;; 6: x = x2, normal = X
    (gl:begin :polygon)
    (%gl:normal-3i 1 0 0)
    (%gl:vertex-3d x2 y1 z1)
    (%gl:vertex-3d x2 y1 z2)
    (%gl:vertex-3d x2 y2 z2)
    (%gl:vertex-3d x2 y2 z1)
    (gl:end)))

(defun gl-draw (l-win)
  (let* ((ls (lw-ls l-win))
         (geometry (geometry ls)))
    (when (lw-animate l-win)
      (gl-animate l-win))
    ;; initialization
    (gl:clear-color 1.0 1.0 1.0 0.0)
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:push-matrix)
    (%gl:color-3f 0.0 0.0 0.0)
    (gl:light :light0 :position (lw-light-pos l-win))
    (dotimes (i (length geometry))
      (gl-code l-win (aref geometry i)))
    (gl:pop-matrix)))

(defun gl-mousemotion (l-win x y state)
  (let ((debug nil)
        (width (lw-width l-win))
        (height (lw-height l-win))
        (angle-inc (lw-angle-inc l-win)))
    (when debug (format t "Mouse motion relative (~A ~A) " x y))
    (when (= state 1)
      (when debug (write-line "while button 1 down."))
      (incf (lw-rotate-angle l-win) (* x angle-inc))
      (when (< (lw-rotate-angle l-win) 0d0)
        (incf (lw-rotate-angle l-win) 360d0))
      (when (>= (lw-rotate-angle l-win) 360d0)
        (decf (lw-rotate-angle l-win) 360d0))
      (when debug (format t "New rotate-angle ~A~%"
                          (lw-rotate-angle l-win)))
      (incf (lw-tilt-angle l-win) (* y angle-inc))
      (when (< (lw-tilt-angle l-win) -89.9d0)
        (setf (lw-tilt-angle l-win) -89.9d0))
      (when (> (lw-tilt-angle l-win) 89.9d0)
        (setf (lw-tilt-angle l-win) 89.9d0))
      ;; update viewport
      (gl-reshape l-win :width width :height height))
    (when (= state 4)
      ;; button 2: zooming
      (when debug (write-line "while button 2 down."))
      (setf (lw-zoom l-win)
            (* (lw-zoom l-win) (expt (lw-zoom-exp l-win) y)))
      (gl-reshape l-win :width width :height height))))

(defun current-clock ()
  (/ (get-internal-real-time)
     internal-time-units-per-second))

(defun gl-animate (l-win)
  (let ((ls (lw-ls l-win))
        (frame-delay (lw-frame-delay l-win)))
    ;; if frame list is empty, end animation
    (when (null (lw-frame-list l-win))
      (setf (lw-animate l-win) nil)
      (return-from gl-animate nil))
    ;; if frame delay not reached, return
    (let ((newclock (current-clock)))
      ;; If the clock is nil, then set the clock to avoid runtime error
      (when (eq nil (lw-clock l-win))
	  (setf (lw-clock l-win) newclock))
      (when (< (- newclock (lw-clock l-win)) frame-delay)
        (return-from gl-animate nil))
      (setf (lw-clock l-win) newclock))
    ;; extract from framelist and do the necessary number of rewrites
    (do ((num (extract-framelist (lw-frame-list l-win))))
        ((>= (current-depth ls) num))
      (rewrite1 ls)
      (when (sensitive ls) (create-geometry ls)))
    ;; create geometry
    (when (null (geometry ls))
      (create-geometry ls))
    ;; maybe find new limits
    (when (lw-recenter l-win)
      ;; (format t "recentering~%")
      (multiple-value-bind (minv maxv)
          (find-limits (geometry ls))
        (setf (lw-minv l-win) minv)
        (setf (lw-maxv l-win) maxv))
      (gl-reshape l-win :width (lw-width l-win) :height (lw-height l-win)))))

(defun gl-keypress (window scancode state)
  (when (eq :keydown state)
    (with-slots (l-window) window
      (case scancode
	((:scancode-escape :scancode-q) (kit.sdl2:close-window window))
	(:scancode-a (setf (lw-animate l-window) (not (lw-animate l-window))))
	(:scancode-r
	 (setf (lw-animate l-window) nil)
	 (setf (lw-frame-list l-window)
	       (copy-tree (lw-original-frame-list l-window)))
	 (let ((ls (lw-ls l-window)))
	   (rewrite ls (extract-framelist (lw-frame-list l-window)))
	   (when (null (geometry ls))
	     (create-geometry ls))))
	(:scancode-p
	 (let* ((ls (lw-ls l-window))
		(class-name (class-name (class-of ls)))
		(name (string-downcase (symbol-name class-name)))
		(filename (concatenate 'string name ".pov"))
		(width (or (lw-cylinder-width l-window) 1.0)))
	   (format t "Writing file '~A'...~%" filename)
	   (output-povray (geometry ls) filename :width-multiplier width
						 :full-scene nil)
	   (format t "Done.~%")))))))

(defun gl-show-geometry (l-win
                         &key
                           (width nil)
                           ;;(line-style :lines)
                           ;;(lighting t)
                           (limits nil)
                           (window-width 400)
                           (window-height 400))
  (progn
    (let ((cyl-width (cylinder-width (lw-ls l-win))))
      (when cyl-width
        (setf (lw-cylinder-width l-win)
              (coerce cyl-width 'double-float))))
    (when width
      (if (eq (lw-line-style l-win) :lines)
          (setf (lw-line-width l-win) (coerce width 'double-float))
          (setf (lw-cylinder-width l-win) (coerce width 'double-float))))
    ;; make sure geometry exists
    (let ((ls (lw-ls l-win)))
      (when (null (geometry ls))
        (create-geometry ls)))
    ;; override limits
    (when limits
      (flet ((map-double (seq) (map '(simple-array double-float (*))
                                    #'(lambda (x) (coerce x 'double-float))
                                    seq)))
        (setf (lw-minv l-win) (map-double (first limits))
              (lw-maxv l-win) (map-double (second limits)))))
    (l-lisp-window-preview :l-window l-win :window-width window-width :window-height window-height)))

(defun gl-animation (ls
                     &key
                       (width nil)
                       (line-style (line-style ls))
                       (cylinder-slices (cylinder-slices ls))
                       (lighting t)
                       (limits (limits ls))
                       (recenter nil)
                       (frame-delay (frame-delay ls))
                       (instant-animate t)
                       ;;(frames `((0 ,(depth ls)))))
                       (frames (or (frame-list ls)
                                   `((0 ,(depth ls)))))
                       (window-width 400)
                       (window-height 400))
  (let ((l-win (make-l-window :ls ls
                              :line-style line-style
                              :cylinder-slices cylinder-slices
                              :enable-lighting lighting
                              :animate instant-animate
                              :recenter recenter
                              :original-frame-list frames
                              :frame-list (copy-tree frames)
                              :frame-delay frame-delay)))
    (rewrite ls (extract-framelist (lw-frame-list l-win)))
    (gl-show-geometry l-win :width width :limits limits
                      :window-width window-width
                      :window-height window-height)))

(defun gl-preview (ls
                   &key
                     (width nil)
                     (line-style (line-style ls))
                     (cylinder-slices (cylinder-slices ls))
                     (depth (depth ls))
                     (lighting t)
                     (limits (limits ls))
                     (window-width 400)
                     (window-height 400))
  (when (and (null width) (eq line-style :cylinders))
    (setq width (cylinder-width ls)))
  (gl-animation ls :width width :line-style line-style
                :cylinder-slices cylinder-slices
                :lighting lighting :limits limits
                :instant-animate nil
                :frames (list depth)
                :window-width window-width
                :window-height window-height))

;;;; SDL2
(defclass l-lisp-window (kit.sdl2:gl-window)
  ((l-window
    :initarg :l-window
    :accessor l-window)))

(defmethod initialize-instance :after ((window l-lisp-window) &key &allow-other-keys)
  (setf (kit.sdl2:idle-render window) t)
  (with-slots (l-window) window
    (gl-initialize l-window)
    (gl-reshape l-window :width (kit.sdl2:window-width window) :height (kit.sdl2:window-height window))))

(kit.sdl2:define-start-function l-lisp-window-preview (&key (l-window nil) (window-width 400) (window-height 400))
  (make-instance 'l-lisp-window :l-window l-window :title "L-Lisp" :w window-width :h window-height :resizable t))

(defmethod kit.sdl2:render ((window l-lisp-window))
  ;; Your GL context is automatically active.  FLUSH and
  ;; SDL2:GL-SWAP-WINDOW are done implicitly by GL-WINDOW
  ;; after RENDER.
  (with-slots (l-window) window
    (when (lw-animate l-window)
      (gl-animate l-window))
    (gl-draw l-window)))

(defmethod kit.sdl2:keyboard-event ((window l-lisp-window) state timestamp repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (gl-keypress window scancode state)))

(defmethod kit.sdl2:window-event ((window l-lisp-window) (type (eql :size-changed)) timestamp data1 data2)
  (with-slots (l-window) window
    (gl-reshape l-window :width data1 :height data2)))

(defmethod kit.sdl2:mousemotion-event ((window l-lisp-window) timestamp button-mask x y xrel yrel)
  (with-slots (l-window) window
    (gl-mousemotion l-window xrel yrel button-mask)))
