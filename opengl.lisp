;;; *** opengl.lisp ***
;;;
;;; This file is part of L-Lisp by Knut Arild Erstad.
;;; Contains code for OpenGL previews and animations of L-systems

(in-package :l-systems)

;; Load the allegro-xlib-and-gl Xlib/OpenGL bindings by Richard Mann
;; A CMUCL port is available from http://www.ii.uib.no/~knute/lisp/
;; Note: this has been untested a long time
#+allegro
(eval-when (:compile-toplevel :load-toplevel)
  (require :gl)
  (require :xlib))

#+(or cmu sbcl)
(eval-when (:compile-toplevel :load-toplevel)
  (require :cmucl-xlib-and-gl))

;; global GLX variables
(defvar *display* nil)
(defvar *glxcontext* nil)
;;(defvar *window*)

(defstruct (l-window (:conc-name lw-))
  ;; Xlib/GL variables
  display
  window
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

;; "Callbacks" (not really callbacks, more like event handling functions)
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
      (gl:glEnable gl:GL_LIGHTING)
      (gl:glLightModeli gl:GL_LIGHT_MODEL_TWO_SIDE 1)
      ;;(gl:glLightModelf gl:GL_LIGHT_MODEL_LOCAL_VIEWER 0.3)
      (let ((ambient (make-array 4 :element-type 'single-float
                                 :initial-contents #(0.2 0.2 0.2 1.0))))
        (gl:glLightModelfv gl:GL_LIGHT_MODEL_AMBIENT ambient))
      (gl:glEnable gl:GL_LIGHT0))
    ;; OpenGL init
    (gl:glDepthFunc gl:GL_LESS)
    (gl:glEnable gl:GL_DEPTH_TEST)
    ))

(defun gl-reshape (l-win width height)
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
      (gl:glViewport 0 0 width height)
      (gl:glMatrixMode gl:GL_PROJECTION)
      (gl:glLoadIdentity)
      (gl:glOrtho (- half-x) half-x
                  (- half-y) half-y
                  (- z-inc) z-inc)
      (gl:glMatrixMode gl:GL_MODELVIEW)
      (gl:glLoadIdentity)
      ;; rotate model
      (gl:glRotated (lw-tilt-angle l-win) 1d0 0d0 0d0)
      (gl:glRotated (lw-rotate-angle l-win) 0d0 1d0 0d0)
      ;; move it to center of the window
      (gl:glTranslated (- xcenter) (- ycenter) (- zcenter))
      )))

(defvar *black* (make-array 4 :element-type 'single-float
                            :initial-contents '#(0.0 0.0 0.0 1.0)))

(defun gl-set-color (l-win color)
  (let ((col (or color *black*)))
    (if (lw-enable-lighting l-win)
        (gl:glMaterialfv gl:GL_FRONT_AND_BACK
                         gl:GL_AMBIENT_AND_DIFFUSE
                         col)
        (gl:glColor4fv col))))

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
       (gl:glLineWidth (coerce w 'single-float))
       ;; draw line
       (gl:glBegin gl:GL_LINES)
       (gl:glVertex3dv p1)
       (gl:glVertex3dv p2)
       (gl:glEnd)
       ))
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
                (quad (gl:gluNewQuadric)))
           (declare (optimize (speed 3)
                              #+cmu (ext:inhibit-warnings 3))
                    (type (simple-array double-float (3))
                          p1 p2 hvec zvec rvec)
                    (double-float w1 w2 dot rad-angle deg-angle
                                  cylinder-width))
           ;; set color
           (gl-set-color l-win color)
           ;; push matrix
           (gl:glPushMatrix)
           ;; translate to p1
           (gl:glTranslated (aref p1 0) (aref p1 1) (aref p1 2))
           ;; rotate to hvec
           (gl:glRotated deg-angle (aref rvec 0) (aref rvec 1) (aref rvec 2))
           ;; now the cylinder can be created
           (gl:gluQuadricNormals quad gl:GLU_SMOOTH)
           (gl:gluQuadricDrawStyle quad gl:GLU_FILL)
           (gl:gluCylinder quad
                           (* w1 cylinder-width) (* w2 cylinder-width)
                           height (lw-cylinder-slices l-win) 1)
           ;; deallocate quad
           (gl:gluDeleteQuadric quad)
           (gl:glPopMatrix)))))
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
           (quad (gl:gluNewQuadric))
           (cylinder-width (lw-cylinder-width l-win)))
       (declare (optimize (speed 3) (safety 0))
                (double-float radius cylinder-width)
                (type (simple-array double-float (3)) pos))
       (gl:glPushMatrix)
       (gl:glTranslated (aref pos 0) (aref pos 1) (aref pos 2))
       (gl:gluQuadricNormals quad gl:GLU_SMOOTH)
       (gl:gluQuadricDrawStyle quad gl:GLU_FILL)
       (let* ((slices (lw-cylinder-slices l-win))
              (stacks (round slices 2)))
         (gl:gluSphere quad
                       (* cylinder-width radius)
                       slices stacks))
       (gl:gluDeleteQuadric quad)
       (gl:glPopMatrix)))
    (:lines
     (let ((w (* (sphere-radius sphere) (or (lw-line-width l-win) 1.0d0))))
       (gl:glPointSize (coerce w 'single-float))
       (gl:glBegin GL:GL_POINTS)
       (gl:glVertex3dv (sphere-pos sphere))
       (gl:glEnd)))))

(defmethod gl-code (l-win (polygon polygon))
  (gl-set-color l-win (polygon-color polygon))
  (gl:glNormal3dv (polygon-normal polygon))
  (gl:glBegin gl:GL_POLYGON)
  (dolist (p (polygon-points polygon))
    ;;(declare (optimize (speed 3)))
    ;;(declare (type (simple-array double-float (3)) p))
    (gl:glVertex3d (aref p 0) (aref p 1) (aref p 2)))
  ;;(gl:glVertex3dv p))
  (gl:glEnd))

(defmethod gl-code (l-win (mesh mesh))
  (gl-set-color l-win (mesh-color mesh))
  (gl:glBegin gl:GL_TRIANGLES)
  (dolist (triangle (mesh-triangles mesh))
    (dolist (vertex (triangle-vertices triangle))
      (gl:glNormal3dv (vertex-normal vertex))
      (gl:glVertex3dv (vertex-pos vertex))))
  (gl:glEnd))

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
    (gl:glBegin gl:GL_POLYGON)
    (gl:glNormal3i 0 0 -1)
    (gl:glVertex3d x1 y1 z1)
    (gl:glVertex3d x1 y2 z1)
    (gl:glVertex3d x2 y2 z1)
    (gl:glVertex3d x2 y1 z1)
    (gl:glEnd)
    ;; 2: z = z2, normal = Z
    (gl:glBegin gl:GL_POLYGON)
    (gl:glNormal3i 0 0 1)
    (gl:glVertex3d x1 y1 z2)
    (gl:glVertex3d x1 y2 z2)
    (gl:glVertex3d x2 y2 z2)
    (gl:glVertex3d x2 y1 z2)
    (gl:glEnd)
    ;; 3: y = y1, normal = -Y
    (gl:glBegin gl:GL_POLYGON)
    (gl:glNormal3i 0 -1 0)
    (gl:glVertex3d x1 y1 z1)
    (gl:glVertex3d x1 y1 z2)
    (gl:glVertex3d x2 y1 z2)
    (gl:glVertex3d x2 y1 z1)
    (gl:glEnd)
    ;; 4: y = y2, normal = Y
    (gl:glBegin gl:GL_POLYGON)
    (gl:glNormal3i 0 1 0)
    (gl:glVertex3d x1 y2 z1)
    (gl:glVertex3d x1 y2 z2)
    (gl:glVertex3d x2 y2 z2)
    (gl:glVertex3d x2 y2 z1)
    (gl:glEnd)
    ;; 5: x = x1, normal = -X
    (gl:glBegin gl:GL_POLYGON)
    (gl:glNormal3i -1 0 0)
    (gl:glVertex3d x1 y1 z1)
    (gl:glVertex3d x1 y1 z2)
    (gl:glVertex3d x1 y2 z2)
    (gl:glVertex3d x1 y2 z1)
    (gl:glEnd)
    ;; 6: x = x2, normal = X
    (gl:glBegin gl:GL_POLYGON)
    (gl:glNormal3i 1 0 0)
    (gl:glVertex3d x2 y1 z1)
    (gl:glVertex3d x2 y1 z2)
    (gl:glVertex3d x2 y2 z2)
    (gl:glVertex3d x2 y2 z1)
    (gl:glEnd)
    ))

(defun gl-draw (l-win)
  (let* ((ls (lw-ls l-win))
         (geometry (geometry ls)))
    ;; initialization
    (gl:glClearColor 1.0 1.0 1.0 0.0)
    (gl:glClear (+ gl:GL_COLOR_BUFFER_BIT
                   gl:GL_DEPTH_BUFFER_BIT))
    (gl:glPushMatrix)
    (gl:glColor3f 0.0 0.0 0.0)
    (gl:glLightiv gl:GL_LIGHT0 gl:GL_POSITION (lw-light-pos l-win))
    (dotimes (i (length geometry))
      (gl-code l-win (aref geometry i)))
    (gl:glPopMatrix)
    (gl:glxswapbuffers (lw-display l-win) (lw-window l-win))))

(defun gl-mousemotion (l-win x y state)
  (let ((debug nil)
        (width (lw-width l-win))
        (height (lw-height l-win))
        (angle-inc (lw-angle-inc l-win)))
    (when debug (format t "Mouse motion relative (~A ~A) " x y))
    (unless (zerop (boole boole-and state xlib-gl:Button1Mask))
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
      ;; update viewport and redraw
      (gl-reshape l-win width height)
      (gl-draw l-win))
    (unless (zerop (boole boole-and state xlib-gl:Button2Mask))
      ;; button 2: zooming
      (when debug (write-line "while button 2 down."))
      (setf (lw-zoom l-win)
            (* (lw-zoom l-win) (expt (lw-zoom-exp l-win) y)))
      (gl-reshape l-win width height)
      (gl-draw l-win))
    ))

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
                                        ;(format t "recentering~%")
      (multiple-value-bind (minv maxv)
          (find-limits (geometry ls))
        (setf (lw-minv l-win) minv)
        (setf (lw-maxv l-win) maxv))
      (gl-reshape l-win (lw-width l-win) (lw-height l-win)))
    ;; draw it
    ;;(gl-draw l-win)))
    (send-expose-event l-win)))

(defun gl-keypress (l-win char)
  (case char
    ;; A : Toggle animate
    (#\a (setf (lw-animate l-win) (not (lw-animate l-win))))
    ;; R : Restart animation
    (#\r
     (setf (lw-animate l-win) nil)
     (setf (lw-frame-list l-win)
           (copy-tree (lw-original-frame-list l-win)))
     (let ((ls (lw-ls l-win)))
       (rewrite ls (extract-framelist (lw-frame-list l-win)))
       (when (null (geometry ls))
         (create-geometry ls)))
     (gl-draw l-win))
    ;; P : Output povray
    (#\p
     (let* ((ls (lw-ls l-win))
            (class-name (class-name (class-of ls)))
            (name (string-downcase (symbol-name class-name)))
            (filename (concatenate 'string name ".pov"))
            (width (or (lw-cylinder-width l-win) 1.0)))
       (format t "Writing file '~A'...~%" filename)
       (output-povray (geometry ls) filename :width-multiplier width
                      :full-scene nil)
       (format t "Done.~%")))
    ;; Q : Quit
    (#\q :quit)
    ))

(defun gl-geometry-loop (l-win)
  (setf (lw-clock l-win) (current-clock))
  ;; event loop
  (let ((event (xlib-gl:make-xevent))
        (xpos nil)
        (ypos nil)
        (debug nil)
        (display (lw-display l-win)))
    (loop
       ;; if in animation mode, animate until event occurs
       (when (lw-animate l-win)
         (when debug (format t "Animate..."))
         (loop
            (gl-animate l-win)
            (when (or (> (xlib-gl:xpending display) 0)
                      (not (lw-animate l-win)))
              (return))))
       ;; wait for event
       (when debug (format t "Waiting for event...~%"))
       (xlib-gl:xnextevent display event)
       (when debug (format t "...event~%"))
       (let ((event-type (xlib-gl:xanyevent-type event)))
         (when debug (format t "Event: ~A~%" event-type))
         ;; process event
         (cond
           ((eq event-type xlib-gl:expose)
            (when debug (format t "(expose)~%"))
            ;; expose
            ;; gobble other expose events
            (loop (when (zerop (xlib-gl:xpending display))
                    (return))
               (xlib-gl:xnextevent display event)
               (let ((event-type (xlib-gl:xanyevent-type event)))
                 (unless (eq event-type xlib-gl:expose)
                   (xlib-gl:xputbackevent display event)
                   (return))))
            ;; ... draw after expose
            (gl-draw l-win))
           ((eq event-type xlib-gl:configurenotify)
            ;; resize
            (when debug (format t "(resize)~%"))
            (gl-reshape l-win
                        (xlib-gl:xconfigureevent-width event)
                        (xlib-gl:xconfigureevent-height event)))
           ((eq event-type xlib-gl:buttonpress)
            (when debug (format t "(buttonpress)~%"))
            ;; button press--save position
            (let ((button (xlib-gl:xbuttonevent-button event)))
              (when debug (format t "Button: ~A~%" button))
              (setq xpos (xlib-gl:xbuttonevent-x_root event))
              (setq ypos (xlib-gl:xbuttonevent-y_root event))
              (when debug (format t "pos ~A ~A~%" xpos ypos))
              (cond
                ((eq button xlib-gl:button3)
                 (return)))))
           ((eq event-type xlib-gl:motionnotify)
            ;; mouse moved
            (when debug (format t "(mousemotion)~%"))
            (let ((new-xpos (xlib-gl:xmotionevent-x_root event))
                  (new-ypos (xlib-gl:xmotionevent-y_root event))
                  (state (xlib-gl:xmotionevent-state event)))
              ;; gobble motion events with same state
              (loop (when (zerop (xlib-gl:xpending display))
                      (return))
                 (xlib-gl:xnextevent display event)
                 (let ((event-type (xlib-gl:xanyevent-type event)))
                   ;; if we have the same type of event
                   (if (and (eq event-type xlib-gl:motionnotify)
                            (= (xlib-gl:xmotionevent-state event) state))
                       ;; then gobble it
                       (progn
                         (setq new-xpos (xlib-gl:xmotionevent-x_root event))
                         (setq new-ypos (xlib-gl:xmotionevent-y_root event)))
                       ;; else put it back and jump out of the loop
                       (progn
                         (xlib-gl:xputbackevent display event)
                         (return)))))
              (when debug (format t "Motion: state ~A  pos ~A ~A~%"
                                  state xpos ypos))
              (when xpos
                (gl-mousemotion l-win
                                (- new-xpos xpos) (- new-ypos ypos)
                                state))
              (setq xpos new-xpos)
              (setq ypos new-ypos)))
           ((eq event-type xlib-gl:keypress)
            ;; keypress
            (when debug (format t "(keypress)~%"))
            (let* ((keysym (xlib-gl:xlookupkeysym event 0))
                   (char (if (< keysym 256) (code-char keysym) nil)))
              (when debug (format t "keysym ~A char ~A~%" keysym char))
              (when (= keysym xlib-gl:XK_Escape)
                (return))
              (when char
                (let ((answer (gl-keypress l-win char)))
                  (when (eq answer :quit) (return))))))
           )))
    (xlib-gl:free-xevent event)))

(defun send-expose-event (l-win)
  (let ((display (lw-display l-win))
        (window (lw-window l-win))
        (event (xlib-gl:make-xexposeevent)))
    (unwind-protect
         (progn (xlib-gl:set-xanyevent-type! event xlib-gl:expose)
                (xlib-gl:set-xanyevent-serial! event 0)
                (xlib-gl:set-xanyevent-send_event! event 0)
                (xlib-gl:set-xanyevent-display! event display)
                (xlib-gl:set-xanyevent-window! event window)
                (xlib-gl:xsendevent display window 0 xlib-gl:ExposureMask event))
      (xlib-gl:free-xexposeevent event))))

(defun create-gl-window (display width height name)
  ;; Create a double buffered, RGBA window
  (let* ((screen (xlib-gl:XDefaultScreen display))
         (root (xlib-gl:XRootWindow display screen))
         ;; array of integers, terminated by "None"
         (attrib (make-array 11
                             :element-type
                             #+cmu '(signed-byte 32) #-cmu 'fixnum
                             :initial-contents
                             (list gl:GLX_RGBA
                                   gl:GLX_RED_SIZE  4 gl:GLX_GREEN_SIZE 4
                                   gl:GLX_BLUE_SIZE 4 gl:GLX_DEPTH_SIZE 4
                                   gl:GLX_DOUBLEBUFFER
                                   xlib-gl:None)))
         (visinfo (gl:glXChooseVisual display screen attrib)))
    (when (zerop visinfo)
      (error
       "CREATE-GL-WINDOW: Could not get an RGBA, double-buffered visual."))
    (let ((attr (xlib-gl:make-xsetwindowattributes)))
      (xlib-gl:set-xsetwindowattributes-background_pixel! attr 0)
      (xlib-gl:set-xsetwindowattributes-border_pixel! attr 0)
      (xlib-gl:set-xsetwindowattributes-colormap!
       attr (xlib-gl:XCreateColormap display root
                                     (xlib-gl:XVisualInfo-visual visinfo)
                                     xlib-gl:AllocNone))
      (xlib-gl:set-xsetwindowattributes-event_mask!
       attr (+ xlib-gl:StructureNotifyMask xlib-gl:ExposureMask
               xlib-gl:ButtonPressMask xlib-gl:ButtonReleaseMask
               xlib-gl:Button1MotionMask xlib-gl:Button2MotionMask
               xlib-gl:KeyPressMask))
      (let* ((mask (+ xlib-gl:CWBackPixel xlib-gl:CWBorderPixel
                      xlib-gl:CWColormap xlib-gl:CWEventMask))
             (window (xlib-gl:XCreateWindow display root 0 0 width height
                                            0
                                            (xlib-gl:XVisualInfo-depth visinfo)
                                            xlib-gl:InputOutput
                                            (xlib-gl:XVisualInfo-visual visinfo)
                                            mask attr))
             ;;(context (gl:glXCreateContext display visinfo xlib-gl:NULL 1)))
             (context (setq *glxcontext* (gl:glXCreateContext
                                          display visinfo xlib-gl:NULL 1))))
        (gl:glXMakeCurrent display window context)
        (xlib-gl:XStoreName display window name)
        (xlib-gl:XMapWindow display window)
        window))))

(defun gl-show-geometry (l-win
                         &key
                           (width nil)
                           ;;(line-style :lines)
                           ;;(lighting t)
                           (limits nil)
                           (window-width 400)
                           (window-height 400))
  (unwind-protect
       (progn
         ;; open display/window
         (unless *display* (setq *display* (xlib-gl:xopendisplay "")))
         (let ((window (create-gl-window *display* window-width window-height
                                         "L-Lisp OpenGL window")))
                                        ;    (setq *window* (create-gl-window *display* 300 300
                                        ;                     "L-lisp OpenGL window"))
           ;; fill in some values in l-win
           (setf (lw-display l-win) *display*
                 (lw-window l-win) window))
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
         ;; initialize limits and OpenGL
         (gl-initialize l-win)
         ;; override limits
         (when limits
           (flet ((map-double (seq) (map '(simple-array double-float (*))
                                         #'(lambda (x) (coerce x 'double-float))
                                         seq)))
             (setf (lw-minv l-win) (map-double (first limits))
                   (lw-maxv l-win) (map-double (second limits)))))
         ;; start event loop
         (gl-geometry-loop l-win))
    ;; always cleanup and exit
    (let ((event (xlib-gl:make-xevent))
          (display (lw-display l-win))
          (window (lw-window l-win)))
      (gl:glXDestroyContext display *glxcontext*)
      (xlib-gl:xdestroywindow display window)
      (loop (when (zerop (xlib-gl:xpending display)) (return))
         (xlib-gl:xnextevent display event))
      (xlib-gl:free-xevent event))))


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
