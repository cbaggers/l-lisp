;;; *** turtle.lisp ***
;;;
;;; This file is part of L-Lisp by Knut Arild Erstad.
;;; Contains routines for:
;;; - 3D vector/matrix functions for double-floats.
;;; - Turtle graphics and interpretation.
;;; - Output geometry to Postscript and Povray

(in-package :l-systems)

;; make sure pi is double-float, no larger
(defconstant d-pi (coerce pi 'double-float))

;; Helper trigonometric functions
(defun deg-to-rad (n)
  (* n (/ d-pi 180.0d0)))

(defun rad-to-deg (n)
  (* n (/ 180.0d0 d-pi)))

(defun cosd (n)
  (cos (deg-to-rad n)))

(defun sind (n)
  (sin (deg-to-rad n)))

(defun tand (n)
  (tan (deg-to-rad n)))

;; Helper 3d vector/matrix functions
(declaim (inline vec3 vlength normalize equalvec zerovec vec- vec+
                 dot-product cross-product))

(defun v3 (elt1 elt2 elt3)
  "Create a 3D vector.  Slow, but works for all real numbers."
  (make-array 3 :element-type 'double-float
              :initial-contents
              (list (coerce elt1 'double-float)
                    (coerce elt2 'double-float)
                    (coerce elt3 'double-float))))

(defun vec3 (elt1 elt2 elt3)
  "Create a 3D vector.  All arguments must be double-floats."
  (declare (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 3))
           (type double-float elt1 elt2 elt3))
  (let ((vec (make-array 3 :element-type 'double-float)))
    (declare (type (simple-array double-float (3)) vec))
    (setf (aref vec 0) elt1)
    (setf (aref vec 1) elt2)
    (setf (aref vec 2) elt3)
    vec))

(defun vlength (vec)
  "Calculate the length of a 3D vector."
  (declare (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 3))
           (type (simple-array double-float (3)) vec))
  (let ((sum 0.0d0))
    (declare (double-float sum))
    (dotimes (i 3)
      (let ((elt (aref vec i)))
        (declare (double-float elt))
        (incf sum (* elt elt))))
    (the double-float (sqrt sum))))

(defun normalize (vec)
  "Change the length of the 3D vector to 1."
  (declare (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 3))
           (type (simple-array double-float (3)) vec))
  (let ((len (vlength vec)))
    (declare (double-float len))
    (dotimes (i 3 vec)
      (setf (aref vec i) (/ (aref vec i) len)))))

(defun equalvec (vec1 vec2)
  "Returns T if two 3D vectors are equal."
  (declare (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 3))
           (type (simple-array double-float (3)) vec1 vec2))
  (and (= (aref vec1 0) (aref vec2 0))
       (= (aref vec1 1) (aref vec2 1))
       (= (aref vec1 2) (aref vec2 2))))

(defun almost-equalvec (vec1 vec2)
  "Returns T if two 3D vectors are almost equal; this is defined to be
true if all distances along axes are smaller than a certain epsilon.
Epsilon = 1.0d-10."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (3)) vec1 vec2))
  (let ((epsilon 1d-10))
    (and (< (abs (- (aref vec1 0) (aref vec2 0))) epsilon)
         (< (abs (- (aref vec1 1) (aref vec2 1))) epsilon)
         (< (abs (- (aref vec1 2) (aref vec2 2))) epsilon))))

(defun zerovec (vec)
  "Returns T if the vector is equal to #(0 0 0)."
  (declare (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 3))
           (type (simple-array double-float (3)) vec))
  (and (zerop (aref vec 0))
       (zerop (aref vec 1))
       (zerop (aref vec 2))))

(defun vec- (vec1 vec2)
  "Returns VEC1 - VEC2."
  (declare (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 3))
           (type (simple-array double-float (3)) vec1 vec2))
  (let ((vec (make-array 3 :element-type 'double-float)))
    (dotimes (i 3 vec)
      (setf (aref vec i) (- (aref vec1 i) (aref vec2 i))))))

(defun vec+ (vec1 vec2)
  "Returns VEC1 + VEC2."
  (declare (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 3))
           (type (simple-array double-float (3)) vec1 vec2))
  (let ((vec (make-array 3 :element-type 'double-float)))
    (dotimes (i 3 vec)
      (setf (aref vec i) (+ (aref vec1 i) (aref vec2 i))))))

(defun dot-product (vec1 vec2)
  "Returns the dot product VEC1 * VEC2."
  (declare (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 3))
           (type (simple-array double-float (3)) vec1 vec2))
  (let ((sum 0.0d0))
    (declare (double-float sum))
    (dotimes (i 3 sum)
      (incf sum (* (aref vec1 i) (aref vec2 i))))))

(defun cross-product (vec1 vec2)
  "Returns the cross product VEC1 x VEC2."
  (declare (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 3))
           (type (simple-array double-float (3)) vec1 vec2))
  (let ((u1 (aref vec1 0))
        (u2 (aref vec1 1))
        (u3 (aref vec1 2))
        (v1 (aref vec2 0))
        (v2 (aref vec2 1))
        (v3 (aref vec2 2))
        (r (make-array 3 :element-type 'double-float)))
    (declare (double-float u1 u2 u3 v1 v2 v3)
             (type (simple-array double-float (3)) r))
    (setf (aref r 0) (- (* u2 v3) (* u3 v2)))
    (setf (aref r 1) (- (* u3 v1) (* u1 v3)))
    (setf (aref r 2) (- (* u1 v2) (* u2 v1)))
    r))

(defun matrix-vector-mult (mat vec)
  "Multiply a 3x3 (rotation) matrix with a vector."
  (declare (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 3))
           (type (simple-array double-float (3 3)) mat)
           (type (simple-array double-float (3)) vec))
  (let ((r (make-array 3 :element-type 'double-float
                       :initial-element 0.0d0)))
    (declare (type (simple-array double-float (3)) r))
    (dotimes (j 3 r)
      (dotimes (i 3)
        (incf (aref r j) (* (aref mat j i) (aref vec i)))))))

;; turtle struct; a "moving coordinate system" with some extra parameters
(defstruct (turtle (:copier nil))
  (pos (make-array 3 :element-type 'double-float
                   :initial-contents '#(0.0d0 0.0d0 0.0d0))
       :type (simple-array double-float (3)))
  (H (make-array 3 :element-type 'double-float
                 :initial-contents '#(0.0d0 1.0d0 0.0d0))
     :type (simple-array double-float (3)))
  (L (make-array 3 :element-type 'double-float
                 :initial-contents '#(-1.0d0 0.0d0 0.0d0))
     :type (simple-array double-float (3)))
  (U (make-array 3 :element-type 'double-float
                 :initial-contents '#(0.0d0 0.0d0 -1.0d0))
     :type (simple-array double-float (3)))
  angle
  width
  prev-width
  color
  texture
  (scale 1.0d0) ;; unused
  (shared-polygon-stack (list nil))
  (shared-mesh (list nil))
  (produce-spheres t)
  )


;; Unlike the other turtle values, the polygon and mesh stuff must be
;; shared between copies.  Some accessors to make this easier:
(defun turtle-polygon-stack (turtle)
  (car (turtle-shared-polygon-stack turtle)))

(defun (setf turtle-polygon-stack) (val turtle)
  (setf (car (turtle-shared-polygon-stack turtle)) val))

(defun turtle-polygon (turtle)
  (car (turtle-polygon-stack turtle)))

(defun (setf turtle-polygon) (val turtle)
  (setf (car (turtle-polygon-stack turtle)) val))

(defun turtle-mesh (turtle)
  (car (turtle-shared-mesh turtle)))

(defun (setf turtle-mesh) (val turtle)
  (setf (car (turtle-shared-mesh turtle)) val))

(defun copy-turtle (turtle)
  "Deep copy of turtle."
  (make-turtle :pos (copy-seq (turtle-pos turtle))
               :H (copy-seq (turtle-H turtle))
               :L (copy-seq (turtle-L turtle))
               :U (copy-seq (turtle-U turtle))
               :angle (turtle-angle turtle)
               :width (turtle-width turtle)
               :prev-width (turtle-prev-width turtle)
               :color (turtle-color turtle)
               :texture (turtle-texture turtle)
               :scale (turtle-scale turtle)
               :shared-polygon-stack
               (turtle-shared-polygon-stack turtle)
               :shared-mesh (turtle-shared-mesh turtle)
               :produce-spheres (turtle-produce-spheres turtle)
               ))

(defun rotate (turtle vec angle)
  "Rotate turtle an angle around a unit vector."
  ;; the algorithm is taken from the comp.graphics.algorithms FAQ:
  ;; "How do I rotate a 3D point?" (works for vectors, too)
  (declare (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 3))
           (type turtle turtle)
           (type (simple-array double-float (3)) vec))
  (let* ((rad-angle (deg-to-rad angle))
         (t-ang (/ rad-angle 2.0d0))
         (cost (cos t-ang))
         (sint (sin t-ang))
         ;; [ x y z w ] quaternation
         (x (* (aref vec 0) sint))
         (y (* (aref vec 1) sint))
         (z (* (aref vec 2) sint))
         (w cost)
         ;; multiples of x, y, z, w
         (wx (* w x)) (wy (* w y)) (wz (* w z))
         (xx (* x x)) (xy (* x y)) (xz (* x z))
         (yy (* y y)) (yz (* y z)) (zz (* z z))
         ;; rotation matrix
         (mat (make-array '(3 3) :element-type 'double-float)))
    (declare (double-float rad-angle t-ang cost sint x y z w
                           wx wy wz xx xy xz yy yz zz)
             (type (simple-array double-float (3 3)) mat))
    ;; fill in matrix
    (setf (aref mat 0 0) (- 1.0d0 (* 2.0d0 (+ yy zz)))
          (aref mat 0 1) (* 2.0d0 (- xy wz))
          (aref mat 0 2) (* 2.0d0 (+ xz wy))
          (aref mat 1 0) (* 2.0d0 (+ xy wz))
          (aref mat 1 1) (- 1.0d0 (* 2.0d0 (+ xx zz)))
          (aref mat 1 2) (* 2.0d0 (- yz wx))
          (aref mat 2 0) (* 2.0d0 (- xz wy))
          (aref mat 2 1) (* 2.0d0 (+ yz wx))
          (aref mat 2 2) (- 1.0d0 (* 2.0d0 (+ xx yy))))
    ;; rotate the three turtle vectors
    (setf (turtle-H turtle) (matrix-vector-mult mat (turtle-H turtle))
          (turtle-U turtle) (matrix-vector-mult mat (turtle-U turtle))
          (turtle-L turtle) (matrix-vector-mult mat (turtle-L turtle)))
    nil))

(defun rotate-towards (turtle vec angle)
  "Rotate the turtle an angle towards a vector."
  ;;(declare (optimize (speed 3) (safety 0)))
  ;;(declare (type (simple-array double-float (3)) vec))
  ;; Find current angle between vec and turtle heading
  (let* ((hvec (turtle-H turtle))
         (dot (dot-product hvec vec))
         (current-rad-angle (acos (/ dot (vlength vec) (vlength hvec))))
         (current-deg-angle (rad-to-deg current-rad-angle))
         (rotate-angle (min angle current-deg-angle)))
    ;;(declare (type (simple-array double-float (3)) hvec))
    ;;(declare (double-float dot current-rad-angle current-deg-angle))
    (when (> rotate-angle 0d0)
      (let ((rvec (cross-product hvec vec)))
        (when (zerovec rvec)
          (let ((newvec (copy-seq vec)))
            (incf (aref newvec 0) 1d-10)
            (setq rvec (cross-product hvec newvec))))
        (rotate turtle (normalize rvec) angle)))))

;; The definitions of turn, pitch and roll has been optimized
;; to avoid slow matrix multiplication.
;; Pencil and paper were used to calculate formulas. :-)
(defun turn (turtle angle)
  (declare (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 3))
           (type turtle turtle))
  (let* ((rad-angle (deg-to-rad (coerce angle 'double-float)))
         (cosa (cos rad-angle))
         (sina (sin rad-angle))
         (H (turtle-H turtle))
         (L (turtle-L turtle)))
    (declare (double-float rad-angle cosa sina)
             (type (simple-array double-float (3)) H L))
    (dotimes (i 3)
      (let ((hi (aref H i))
            (li (aref L i)))
        (declare (double-float hi li))
        (setf (aref H i) (- (* hi cosa) (* li sina))
              (aref L i) (+ (* hi sina) (* li cosa)))))))

(defun pitch (turtle angle)
  (declare (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 3))
           (type turtle turtle))
  (let* ((rad-angle (deg-to-rad (coerce angle 'double-float)))
         (cosa (cos rad-angle))
         (sina (sin rad-angle))
         (H (turtle-H turtle))
         (U (turtle-U turtle)))
    (declare (double-float rad-angle cosa sina)
             (type (simple-array double-float (3)) H U))
    (dotimes (i 3)
      (let ((hi (aref H i))
            (ui (aref U i)))
        (declare (double-float hi ui))
        (setf (aref H i) (+ (* hi cosa) (* ui sina))
              (aref U i) (- (* ui cosa) (* hi sina)))))))

(defun roll (turtle angle)
  (declare (optimize (speed 3) (safety 0) #+cmu (ext:inhibit-warnings 3))
           (type turtle turtle))
  (let* ((rad-angle (deg-to-rad (coerce angle 'double-float)))
         (cosa (cos rad-angle))
         (sina (sin rad-angle))
         (L (turtle-L turtle))
         (U (turtle-U turtle)))
    (declare (double-float rad-angle cosa sina)
             (type (simple-array double-float (3)) L U))
    (dotimes (i 3)
      (let ((li (aref L i))
            (ui (aref U i)))
        (declare (double-float li ui))
        (setf (aref L i) (+ (* li cosa) (* ui sina))
              (aref U i) (- (* ui cosa) (* li sina)))))))

(defun move-forward (turtle &optional (distance 1.0d0))
  (let ((pos (turtle-pos turtle))
        (h (turtle-H turtle)))
    (dotimes (i 3 pos)
      (incf (aref pos i) (* distance (aref h i))))))

;; line struct, for 3d geometry
;; (line/segment/cylinder depending on context)
(defstruct line
  (p1 (make-array 3 :element-type 'double-float
                  :initial-contents '(0.0d0 0.0d0 0.0d0))
      :type (simple-array double-float (3)))
  (p2 (make-array 3 :element-type 'double-float
                  :initial-contents '(0.0d0 0.0d0 0.0d0))
      :type (simple-array double-float (3)))
  width
  prev-width
  sphere
  color
  texture)

;; sphere struct
(defstruct sphere
  pos
  radius
  color
  texture)

;; polygon struct
(defstruct polygon
  points ;; list of points
  normal ;; normal vector
  color
  texture)

;; triangle mesh
(defstruct mesh
  vertices  ;; list of unique vertices
  triangles ;; list of triangles
  (strands (list nil)) ;; nested list of vertices used during contruction
  color
  texture)

(defstruct vertex
  pos       ;; 3d position
  normal)   ;; average of triangles' normal vectors

(defstruct triangle
  vertices ;; three of them
  normal)  ;; normal vector

;; box struct (independent of turtle orientation)
(defstruct box
  pos     ;; #(x y z) position of near lower left corner
  size    ;; #(xsize ysize zsize)
  color
  texture)

(defun mesh-add-vertex (mesh point)
  "Add new vertex to mesh and return it, or if one exists at the same
position, return that one."
  (let ((vertex nil))
    (dolist (obj (mesh-vertices mesh))
      (if (almost-equalvec point (vertex-pos obj))
        (setf vertex obj)))
    (unless vertex
      (setf vertex (make-vertex :pos (copy-seq point)
                                :normal
                                (make-array 3 :element-type 'double-float
                                            :initial-element 0d0)))
      (push vertex (mesh-vertices mesh)))
    (let* ((strands (mesh-strands mesh))
           (strand (first strands)))
      (if (or (null strand)
              (not (eql vertex (first strand))))
        (push vertex (first strands))))
    vertex))

(defun mesh-add-triangle (mesh vertex1 vertex2 vertex3)
  "If all vertices are distinct, create a triangle, add it to the mesh
and return it.  Otherwise, return NIL."
  (if (or (eql vertex1 vertex2)
          (eql vertex1 vertex3)
          (eql vertex2 vertex3))
    nil
    (let* ((pos1 (vertex-pos vertex1))
           (pos2 (vertex-pos vertex2))
           (pos3 (vertex-pos vertex3))
           (v1 (vec- pos2 pos1))
           (v2 (vec- pos3 pos1))
           (normal (normalize (cross-product v1 v2)))
           (triangle (make-triangle
                      :vertices (list vertex1 vertex2 vertex3)
                      :normal normal)))
      ;; add normal to vertices
      (dolist (vertex (list vertex1 vertex2 vertex3))
        (let ((vnormal (vertex-normal vertex)))
          (dotimes (i 3)
            (incf (aref vnormal i) (aref normal i)))))
      ;; add it to the mesh
      (push triangle (mesh-triangles mesh)))))

;; *** Turtle functions ***
(defparameter *turtle-functions*
  (make-hash-table :test 'eq :size 211))

(defmacro def-turtle-function-raw (names parameters &body body)
  "This macro is for defining turtle functions.  The NAMES is
either a single name or a list of names of the turtle command.
The function is stored in the *TURTLE-FUNCTIONS* hash table.
All non-NIL return values will be added to the geometry vector during
turtle interpretation.

The first parameter is the turtle and the second is the list of
parameters.  The elements of this list can be changed, which is unseful
for environmentally sensitive commands.
"
  `(let ((func (lambda ,parameters ,@body)))
    ,@(mapcar (lambda (x)
                `(setf (gethash ',x *turtle-functions*) func))
              (if (atom names) (list names) names))))

(defmacro def-turtle-function (names parameters &body body)
  "This macro is for defining turtle functions.  The NAMES is
either a single name or a list of names of the turtle command.
The function is stored in the *TURTLE-FUNCTIONS* hash table.
All non-NIL return values will be added to the geometry vector during
turtle interpretation.

The first parameter is the turtle (and is required), the rest of the
parameters are module parameters, which can be declared &OPTIONAL.
A &REST parameter is added (and declared to be ignored) unless you supply
one explicitly.

  (def-turtle-command (:foo s) (turtle &optional (angle 90.0))
    (do-something turtle angle)
    nil)
"
  (let* ((temp-param (gensym))
         (junk-param (gensym))
         (turtle-param (first parameters))
         (has-rest (find '&rest parameters))
         (lambda-func (if has-rest
                          `(lambda (,turtle-param &optional ,temp-param)
                            (destructuring-bind ,(rest parameters)
                                ,temp-param
                              ,@body))
                          `(lambda (,turtle-param &optional ,temp-param)
                            (destructuring-bind (,@(rest parameters)
                                                   &rest ,junk-param)
                                ,temp-param
                              (declare (ignore ,junk-param))
                              ,@body)))))
    `(let ((func ,lambda-func))
      ,@(mapcar (lambda (x)
                  `(setf (gethash ',x *turtle-functions*) func))
                (if (atom names) (list names) names)))))

(defun turtle-function (name)
  (gethash name *turtle-functions*))

;;
;; *** Predefined turtle functions ***
;;

(def-turtle-function (:forward F) (turtle &optional (length 1.0))
  (let ((oldpos (copy-seq (turtle-pos turtle))))
    (move-forward turtle length)
    (let ((line (make-line :p1 oldpos
                           :p2 (copy-seq (turtle-pos turtle))
                           :color (turtle-color turtle)
                           :texture (turtle-texture turtle)
                           :width (turtle-width turtle)
                           :prev-width (turtle-prev-width turtle)
                           :sphere (turtle-produce-spheres turtle))))
      ;; since we moved forward, set prev-width to width
      (setf (turtle-prev-width turtle) (turtle-width turtle))
      line)))

(def-turtle-function :produce-spheres (turtle produce-spheres)
  (setf (turtle-produce-spheres turtle) produce-spheres)
  nil)

(def-turtle-function (:forward-no-line \f) (turtle &optional (length 1.0))
  (move-forward turtle length)
  nil)

(def-turtle-function (:turn-left +)
    (turtle &optional (angle (turtle-angle turtle)))
  (turn turtle (- angle))
  nil)

(def-turtle-function (:turn-right -)
    (turtle &optional (angle (turtle-angle turtle)))
  (turn turtle angle)
  nil)

(def-turtle-function (:pitch-down &)
    (turtle &optional (angle (turtle-angle turtle)))
  (pitch turtle angle)
  nil)

(def-turtle-function (:pitch-up ^)
    (turtle &optional (angle (turtle-angle turtle)))
  (pitch turtle (- angle))
  nil)

(def-turtle-function (:roll-left \\)
    (turtle &optional (angle (turtle-angle turtle)))
  (roll turtle angle)
  nil)

(def-turtle-function (:roll-right /)
    (turtle &optional (angle (turtle-angle turtle)))
  (roll turtle (- angle))
  nil)

(def-turtle-function (:turn-around \|) (turtle)
  (let ((H (turtle-H turtle))
        (L (turtle-L turtle)))
    (dotimes (i 3)
      (setf (aref H i) (- (aref H i))
            (aref L i) (- (aref L i))))))

(def-turtle-function (:set-width !) (turtle width) ; required parameter
  (setf (turtle-prev-width turtle) (turtle-width turtle))
  (setf (turtle-width turtle) width)
  nil)

(def-turtle-function (:sphere @O)
    (turtle &optional (radius (turtle-width turtle)))
  (make-sphere :pos (copy-seq (turtle-pos turtle))
               :radius (coerce radius 'double-float)
               :color (turtle-color turtle)
               :texture (turtle-texture turtle)))

(def-turtle-function :box (turtle in-pos in-size)
  (let ((pos (map '(simple-array double-float (3))
                  (lambda (x) (coerce x 'double-float))
                  in-pos))
        (size (map '(simple-array double-float (3))
                   (lambda (x) (coerce x 'double-float))
                   in-size)))
    (make-box :pos pos :size size
              :color (turtle-color turtle)
              :texture (turtle-texture turtle))))

;; Polygon functions
(def-turtle-function (:start-polygon {) (turtle)
  (push (make-polygon :points (list (copy-seq (turtle-pos turtle)))
                      :color (turtle-color turtle)
                      :texture (turtle-texture turtle))
        (turtle-polygon-stack turtle))
  nil)

(def-turtle-function (:add-vertex \.) (turtle)
  (let* ((polygon (turtle-polygon turtle))
         (last-vertex (first (polygon-points polygon)))
         (pos (turtle-pos turtle)))
    (if (not (equalp pos last-vertex))
      (push (copy-seq pos)
            (polygon-points polygon))))
  nil)

(def-turtle-function (:forward-vertex f.) (turtle &optional (length 1.0))
  (move-forward turtle length)
  (funcall (turtle-function :add-vertex) turtle)
  nil)

(def-turtle-function (:end-polygon }) (turtle)
  (funcall (turtle-function :add-vertex) turtle)
  ;; save normal
  (let* ((polygon (turtle-polygon turtle))
         (points (polygon-points polygon))
         (p1 (first points))
         (p2 (second points))
         (p3 (third points))
         (v1 (vec- p2 p1))
         (v2 (vec- p3 p1))
         (normal (cross-product v1 v2)))
    (setf (polygon-normal polygon) (normalize normal)))
  ;; pop polygon
  (pop (turtle-polygon-stack turtle)))

(def-turtle-function :color (turtle in-color)
  ;; prepare color for OpenGL's glColor or glMaterial (use single-floats)
  ;; array [ r g b 1.0 ]
  (let ((color (make-array 4 :element-type 'single-float)))
    (setf (aref color 3) 1.0)
    (dotimes (i 3)
      (setf (aref color i) (coerce (aref in-color i) 'single-float)))
    (setf (turtle-color turtle) color))
  nil)

(def-turtle-function :texture (turtle texture)
  (setf (turtle-texture turtle) texture)
  nil)

;; Mesh functions
(def-turtle-function (:start-mesh m{) (turtle)
  (let ((mesh (make-mesh)))
    (mesh-add-vertex mesh (turtle-pos turtle))
    (setf (turtle-mesh turtle) mesh))
  nil)

(def-turtle-function (:mesh-vertex m.) (turtle)
  (mesh-add-vertex (turtle-mesh turtle) (turtle-pos turtle))
  nil)

(def-turtle-function (:new-strand m/) (turtle)
  (let ((mesh (turtle-mesh turtle)))
    ;; start an empty strand
    (push nil (mesh-strands mesh))
    ;; add current position
    (mesh-add-vertex mesh (turtle-pos turtle)))
  nil)

(defun strands-to-triangles (mesh strand1 strand2)
  (when (and strand1 strand2)
    (mapc (lambda (x y z) (mesh-add-triangle mesh x y z))
          strand1 strand2 (rest strand2))
    (mapc (lambda (x y z) (mesh-add-triangle mesh x y z))
          strand1 (rest strand2) (rest strand1))))

(def-turtle-function (:end-mesh m}) (turtle)
  (let* ((mesh (turtle-mesh turtle))
         (strands (mesh-strands mesh)))
    ;; create triangles
    (if (first strands)
      (mapc #'(lambda (x y) (strands-to-triangles mesh x y))
            strands (cdr strands)))
    ;; calculate average normal vectors
    (dolist (vertex (mesh-vertices mesh))
      (let ((normal (vertex-normal vertex)))
        (unless (zerovec normal)
          (normalize normal))))
    ;; set color
    (setf (mesh-color mesh) (turtle-color turtle))
    ;; mesh is finished, delete redundant fields
    (setf (mesh-vertices mesh) nil)
    (setf (mesh-strands mesh) nil)
    ;; return mesh
    mesh))

(def-turtle-function (:forward-mesh-vertex mf) (turtle &optional (length 1.0))
  (move-forward turtle length)
  (funcall (turtle-function :mesh-vertex) turtle)
  nil)

;; Set position
(def-turtle-function (:set-position @M) (turtle pos)
  (setf (turtle-pos turtle)
        (map '(simple-array double-float 1)
             (lambda (x) (coerce x 'double-float))
             pos))
  nil)

;; Rotate toward a vector
(def-turtle-function (:rotate-towards @R)
    (turtle angle &optional (vec #(0d0 1d0 0d0)))
  (let ((d-angle (coerce angle 'double-float))
        (d-vec (map '(vector double-float)
                   (lambda (x) (coerce x 'double-float))
                   vec)))
    (rotate-towards turtle d-vec d-angle)
    nil))

;; Create lines independently of turtle position
(def-turtle-function :line (turtle p1 p2
                                   &optional (width (turtle-width turtle)))
  (make-line :p1 (map '(simple-array double-float (3))
                      (lambda (x) (coerce x 'double-float))
                      p1)
             :p2 (map '(simple-array double-float (3))
                      (lambda (x) (coerce x 'double-float))
                      p2)
             :width width
             :color (turtle-color turtle)
             :texture (turtle-texture turtle)))

;; Environmentally sensitive functions
(def-turtle-function-raw (:get-position ?P) (turtle params)
  (setf (first params) (copy-seq (turtle-pos turtle)))
  nil)

(def-turtle-function-raw (:get-heading ?H) (turtle params)
  (setf (first params) (copy-seq (turtle-H turtle)))
  nil)

(def-turtle-function-raw (:get-up-vector ?U) (turtle params)
  (setf (first params) (copy-seq (turtle-U turtle)))
  nil)

(def-turtle-function-raw (:get-left-vector ?L) (turtle params)
  (setf (first params) (copy-seq (turtle-L turtle)))
  nil)

(def-turtle-function-raw (:get-turtle ?T) (turtle params)
  (setf (first params) (copy-turtle turtle))
  nil)

;; *** Turtle interpretation ***
(defun turtle-interpret (rstring &key (angle-increment 90.0d0))
  "Create geometry by turtle-interpreting a rewriting string."
  (declare (optimize (speed 3) (safety 0))
           (type simple-vector rstring))
  (let ((geometry (make-buffer))
        (turtle-stack nil)
        (turtle (make-turtle :angle angle-increment)))
    (push turtle turtle-stack)
    (dotimes (pos (length rstring))
      (let* ((module (svref rstring pos))
             (params (if (listp module) (rest module) nil))
             (symbol (if params (first module) module)))
        (cond
         ((eq symbol '\[)
          (setq turtle (copy-turtle turtle))
          (push turtle turtle-stack))
         ((eq symbol '\])
          (pop turtle-stack)
          (setq turtle (car turtle-stack)))
         (t
          (let ((func (gethash symbol *turtle-functions*)))
            (when (functionp func)
              (let ((returnval (funcall func turtle params)))
                (when returnval
                  (buffer-push returnval geometry)))))))))
    (buffer->vector geometry)))

;; Geometry limits
(defun find-limits (geometry)
  "Find the min- and max-values of the geometry coordinates."
  (let* ((bg most-positive-double-float)
         (sm most-negative-double-float)
         (minv (vector bg bg bg))
         (maxv (vector sm sm sm)))
    (dotimes (pos (length geometry))
      (let ((elt (aref geometry pos)))
        (case (type-of elt)
          (line
           (let* ((p1 (line-p1 elt))
                  (p2 (line-p2 elt)))
             (dotimes (i 3)
               (setf (aref minv i) (min (aref minv i)
                                        (aref p1 i)
                                        (aref p2 i)))
               (setf (aref maxv i) (max (aref maxv i)
                                        (aref p1 i)
                                        (aref p2 i))))))
          (polygon
           (dolist (p (polygon-points elt))
             (dotimes (i 3)
               (setf (aref minv i) (min (aref minv i)
                                        (aref p i)))
               (setf (aref maxv i) (max (aref maxv i)
                                        (aref p i))))))
          (box
           (let* ((p1 (box-pos elt))
                  (p2 (vec+ p1 (box-size elt))))
             (dotimes (i 3)
               (setf (aref minv i) (min (aref minv i)
                                        (aref p1 i)
                                        (aref p2 i)))
               (setf (aref maxv i) (max (aref maxv i)
                                        (aref p1 i)
                                        (aref p2 i))))))
          (mesh
           (dolist (triangle (mesh-triangles elt))
             (dolist (vertex (triangle-vertices triangle))
               (let ((pos (vertex-pos vertex)))
                 (dotimes (i 3)
                   (setf (aref minv i) (min (aref minv i)
                                            (aref pos i)))
                   (setf (aref maxv i) (max (aref maxv i)
                                            (aref pos i)))))))))))
    (values minv maxv)))

(defun find-simple-limits (geometry &key (border-percent 10.0))
  "Find 2d limits (min, max) of 3d geometry, ignoring z coordinate."
  (multiple-value-bind (minv maxv) (find-limits geometry)
    (let* ((xmin (aref minv 0))
           (xmax (aref maxv 0))
           (ymin (aref minv 1))
           (ymax (aref maxv 1))
           (x (- xmax xmin))
           (y (- ymax ymin))
           (border (* (max x y) border-percent 0.01)))
      (decf xmin border)
      (decf ymin border)
      (incf xmax border)
      (incf ymax border)
      (values xmin xmax ymin ymax))))

;; *** Outputting Postscript ***
(defun output-simple-eps (geometry filename
                          &key
                          (ps-size 100) (ps-width 0.3) (border-percent 10.0)
                          (sphere-width 0.1))
  "Output simplest possible 2d projection (ignoring z coordinate) as EPS."
  (with-open-file (file filename :direction :output
                        :if-exists :supersede)
    (multiple-value-bind (xmin xmax ymin ymax)
        (find-simple-limits geometry :border-percent border-percent)
      (let* ((xsize (- xmax xmin))
             (ysize (- ymax ymin))
             (line-width 1.0)
             (ps-size-x ps-size)
             (ps-size-y ps-size))
        (if (> xsize ysize)
          (setq ps-size-y (floor (* ps-size ysize) xsize))
          (setq ps-size-x (floor (* ps-size xsize) ysize)))
        (format file "%!PS-Adobe-3.0 EPSF-3.0
%%BoundingBox: 0 0 ~A ~A
%%Creator: L-Lisp (L-systems for CL) by Knut Arild Erstad (knute@ii.uib.no)
%%EndComments~%" ps-size-x ps-size-y)
        (loop for obj across geometry do
              (case (type-of obj)
                (line
                 (let* ((line obj)
                        (p1 (line-p1 line))
                        (x1 (/ (- (aref p1 0) xmin) xsize))
                        (y1 (/ (- (aref p1 1) ymin) ysize))
                        (p2 (line-p2 line))
                        (x2 (/ (- (aref p2 0) xmin) xsize))
                        (y2 (/ (- (aref p2 1) ymin) ysize))
                        (w (line-width line)))
                   (when w
                     (setq line-width w))
                   (format file "~,9F setlinewidth ~,9F ~,9F moveto~%"
                           (* line-width ps-width)
                           (* x1 ps-size-x) (* y1 ps-size-y))
                   (format file "~,9F ~,9F lineto stroke~%"
                           (* x2 ps-size-x) (* y2 ps-size-y))))
                (polygon
                 (let* ((polygon obj)
                        (points (polygon-points polygon))
                        (fpoint (first points))
                        (xf (/ (- (aref fpoint 0) xmin) xsize))
                        (yf (/ (- (aref fpoint 1) ymin) ysize)))
                   (format file "0.01 setlinewidth~%")
                   (format file "newpath ~,9F ~,9F moveto~%"
                           (* xf ps-size-x) (* yf ps-size-y))
                   (dolist (point (rest points))
                     (let ((x (/ (- (aref point 0) xmin) xsize))
                           (y (/ (- (aref point 1) ymin) ysize)))
                       (format file "~,9F ~,9F lineto~%"
                               (* x ps-size-x) (* y ps-size-y))))
                   (format file "closepath fill~%")))
                (sphere
                 (let* ((sphere obj)
                        (pos (sphere-pos sphere))
                        (x (/ (- (aref pos 0) xmin) xsize))
                        (y (/ (- (aref pos 1) ymin) ysize))
                        (radius (sphere-radius sphere)))
                   (format file
                           "newpath ~,9F ~,9F ~,9F 0 360 arc closepath fill~%"
                           (* x ps-size-x) (* y ps-size-y)
                           (* radius sphere-width))))
                ))))))

;; *** Outputting POV code ***
(defmethod povcode (obj stream &rest junk)
  (declare (ignore junk stream))
  (warn "Unknown type ~A passed to POVCODE." (type-of obj))
  nil)

(defun povcode-vector-long (vec stream)
  (format stream "<~,15F,~,15F,~,15F>"
          (aref vec 0) (aref vec 1) (aref vec 2)))

(defun povcode-vector-short (vec stream)
  (format stream "<~,5F,~,5F,~,5F>"
          (aref vec 0) (aref vec 1) (aref vec 2)))

(defun povcode-color (colvec stream)
  (when colvec
    (princ "pigment{ color rgb " stream)
    (povcode-vector-short colvec stream)
    (princ " }" stream)))

(defmethod povcode ((line line) stream &rest args)
  (unless (equalvec (line-p1 line) (line-p2 line))
    (let* ((width (or (line-width line) 1.0))
           (prev-width (or (line-prev-width line) width))
           (width-multiplier (if args (first args) 1.0))
           (width-x-mult (* width width-multiplier))
           (sphere (line-sphere line)))
      (when sphere
        (format stream "union { "))
      (princ "cone{ " stream)
      (povcode-vector-short (line-p1 line) stream)
      (format stream ", ~,5F, " (* prev-width width-multiplier))
      (povcode-vector-short (line-p2 line) stream)
      (format stream ", ~,5F " width-x-mult)
      (when sphere
        (format stream "} sphere {")
        (povcode-vector-short (line-p2 line) stream)
        (format stream ", ~,5F} " width-x-mult))
      (povcode-color (line-color line) stream)
      (write-line "}" stream))))

(defun povcode-points (list stream)
  (povcode-vector-long (car list) stream)
  (dolist (elt (cdr list))
    (princ ", ")
    (povcode-vector-long elt stream)))


(defmethod povcode ((sphere sphere) stream &rest args)
  (let ((radius (or (sphere-radius sphere) 1.0))
        (width-multiplier (if args (first args) 1.0)))
    (princ "sphere { " stream)
    (povcode-vector-short (sphere-pos sphere) stream)
    (format stream ", ~,5F " (* radius width-multiplier))
    (povcode-color (sphere-color sphere) stream)
    (write-line "}" stream)))

(defmethod povcode ((polygon polygon) stream &rest junk)
  (declare (ignore junk))
  (let ((points (polygon-points polygon)))
    (format stream "polygon{ ~A, " (length points))
    (povcode-points points stream)
    (princ " " stream)
    (povcode-color (polygon-color polygon) stream)
    (write-line "}" stream)))

(defun povcode-smooth-triangle (triangle stream)
  (let* ((vertices (triangle-vertices triangle))
         (v1 (first vertices))
         (v2 (second vertices))
         (v3 (third vertices)))
    (princ "smooth_triangle { " stream)
    (povcode-vector-short (vertex-pos v1) stream)
    (princ ", " stream)
    (povcode-vector-short (vertex-normal v1) stream)
    (princ ", " stream)
    (povcode-vector-short (vertex-pos v2) stream)
    (princ ", " stream)
    (povcode-vector-short (vertex-normal v2) stream)
    (princ ", " stream)
    (povcode-vector-short (vertex-pos v3) stream)
    (princ ", " stream)
    (povcode-vector-short (vertex-normal v3) stream)
    (princ " }" stream)))

(defmethod povcode ((mesh mesh) stream &rest junk)
  (declare (ignore junk))
  (princ "mesh { " stream)
  (dolist (elt (mesh-triangles mesh))
    (povcode-smooth-triangle elt stream)
    (princ " " stream))
  (povcode-color (mesh-color mesh) stream)
  (write-line "}" stream))

(defmethod povcode ((box box) stream &rest junk)
  (declare (ignore junk))
  (princ "box { " stream)
  (let* ((pos (box-pos box))
         (pos2 (vec+ pos (box-size box))))
    (povcode-vector-short pos stream)
    (princ ", " stream)
    (povcode-vector-short pos2 stream))
  (povcode-color (box-color box) stream)
  (write-line "}" stream))

(defun output-povray (geometry filename
                      &key (object-name "Lsystem")
                           (full-scene t)
                           (width-multiplier 0.01))
  "Output 3d geometry as an object for the POV ray-tracer."
  (with-open-file (file filename :direction :output
                   :if-exists :supersede)
    (format file "// ~A object generated by L-Lisp L-system framework
// by Knut Arild Erstad (knute@ii.uib.no)~%" object-name)
    (when full-scene
      ;; Put some "dumb" defaults for camera and light source
      (format file "
#include \"colors.inc\"
#include \"woods.inc\"
background { color LightBlue }
camera {
  location <0.7, 1, -1>
  look_at <0, 0.5, 0>
}
light_source { <5, 50, -20> color White }
plane { <0,1,0>, 0 pigment {color White} finish {ambient .3} }~%"))
    (format file "#declare ~A = union {~%" object-name)
    (dotimes (i (length geometry))
      (povcode (aref geometry i) file width-multiplier))
    ;;(format file "~A~%" (povcode (aref geometry i) width-multiplier)))
    (when full-scene
      (multiple-value-bind (minv maxv) (find-limits geometry)
        (declare (ignore minv))
        (let ((yscale (/ 1.0 (aref maxv 1))))
          (format file "scale ~,6F~%" yscale))))
    (format file "} // end of ~A object~%" object-name)
    (when full-scene
      (format file "object { ~A texture {T_Wood1 finish {ambient .5}} }~%"
              object-name))))
