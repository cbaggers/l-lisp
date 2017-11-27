;; *** demo.lisp ***
(in-package :lsx)

(defparameter *demos* nil)
(defparameter *demo-num* 0)

(defun reset-demo ()
  (setf *demos* (make-array 1000 :fill-pointer 0))
  (setf *demo-num* 0))

(defun add-demo (name
		 &key
		 (ls (make-instance name))
		 (depth (depth ls))
		 (frames (frame-list ls))
		 (width 800)
		 (height 800)
		 (delay (frame-delay ls))
		 (line-style (line-style ls))
		 (lwidth nil)
		 (recenter nil)
		 (instant-animate nil)
		 (closure nil))
  (when (null frames)
    (setf frames `((,depth ,depth))))
  (vector-push (lambda ()
		 (when closure
		   (funcall closure))
		 (gl-animation ls
			       :frames frames
			       :window-width width
			       :window-height height
			       :frame-delay delay
			       :line-style line-style
			       :width lwidth
			       :recenter recenter
			       :instant-animate instant-animate))
	       *demos*))

(defun demo (&optional (num *demo-num*))
  (when (>= num (length *demos*))
    (setf num 0))
  (funcall (aref *demos* num))
  (setf *demo-num* (1+ num))
  num)

(defun init-demos ()
  (reset-demo)
  (add-demo 'snowflake :frames '((0 5)) :delay 2 :recenter t)
  (add-demo 'occult :frames '((0 4)) :delay 2 :recenter t)
  (add-demo 'tree1 :frames '((0 4)) :delay 1.5 :recenter t)
  (add-demo 'stree :recenter t)
  (add-demo 'context-b)
  (add-demo 'polytest :depth 6)
  (add-demo 'mesh-test-2 :depth 6)
  (add-demo 'gtree)
  (add-demo 'prune1 :frames '((0 10)))
  (add-demo 'prune2face :frames '((0 19)))
  (add-demo 'prune3 :frames '((0 17)))
  (add-demo 'climbing1 :frames '((0 1000)))
  (add-demo 'tropism4)
  (add-demo 'tropism3)
  (add-demo 'shaped-plant :closure #'edit-shape)
  (add-demo 'shaped-plant-2 :closure (lambda ()
				       (edit-shape-1)
				       (edit-shape-2)))
  (add-demo 'shaped-leaf)
  (add-demo 'spline-curvature :closure #'edit-curvature)
  (add-demo 'ptwining :frames '((0 3500)))
  )

(defun all-demos ()
  (init-demos)
  (dotimes (i (length *demos*))
    (demo i)))
