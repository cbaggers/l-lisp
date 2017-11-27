;;; *** lsystem.lisp ***
;;;
;;; This file is part of L-Lisp by Knut Arild Erstad.
;;; Contains the basic L-systems framework, including:
;;; - L-system rewriting
;;; - Homomorphism/decomposition rewriting
;;; - Context matching
;;; - Macros for productions and context sensitivity

(in-package :l-systems)

(defclass l-system ()
  ;;"The base class for L-systems--not to be used directly."
  ((axiom
    :accessor axiom
    :initform nil
    :documentation "Axiom/initiator: sequence of modules/symbols")
   (depth
    :accessor depth
    :initform 0
    :documentation "Depth: (max) number of rewrites.")
   (angle-increment
    :accessor angle-increment
    :initform 90.0
    :documentation "Default angle increment (passed along to turtle).")
   (ignore-list
    :accessor ignore-list
    :initform nil
    :documentation "List of symbols to ignore in context.")
   (consider-list
    :accessor consider-list
    :initform nil
    :documentation
    "List of symbols to consider in context (overrides ignore-list).")
   (homomorphism-depth
    :accessor homomorphism-depth
    :initform 0
    :documentation "Max depth of the homomorphism tree.")
   (decomposition-depth
    :accessor decomposition-depth
    :initform 0
    :documentation "Max depth of the decomposition tree.")
   (sensitive
    :accessor sensitive
    :initform t
    :documentation "Boolean: set to nil to optimize non-environmentally
sensitivite L-systems.")
   ;; The rewriting strings RSTRING and HSTRING (vectors, actually) should
   ;; not be set explicitly.  Neither should GEOMETRY.
   (rstring
    :accessor rstring
    :initform nil
    :documentation "Rewrite string: vector of modules.")
   (hstring
    :accessor hstring
    :initform nil
    :documentation "Homomorphism rewriting string: vector of modules.")
   (geometry
    :accessor geometry
    :initform nil
    :documentation "3D Geometry created by turtle interpretation.")
   (current-depth
    :accessor current-depth
    :initform nil
    :documentation "The number of the current/latest rewrite.")
   ;; Graphical view/animation parameters (for OpenGL)
   (line-style
    :accessor line-style
    :initform :lines
    :documentation "Either :LINES or :CYLINDERS.")
   (cylinder-width
    :accessor cylinder-width
    :initform 1.0d0
    :documentation "Width multiplier for cylinders.")
   (cylinder-slices
    :accessor cylinder-slices
    :initform 8
    :documentation "Number of slices around cylinders.")
   (limits
    :accessor limits
    :initform nil
    :documentation "List of two positions vectors/sequences.")
   (frame-delay
    :accessor frame-delay
    :initform 0.5
    :documentation "Delay between animation frames, in seconds.")
   (frame-list
    :accessor frame-list
    :initform nil
    :documentation "List of frames and frame intervals (nested lists).")
   ;; The rest of the slots are for internal use
   (current-module
    :accessor current-module
    :initform nil
    :documentation "Current module: used internally during rewriting.")
   (pos
    :accessor pos
    :type fixnum
    :initform 0
    :documentation "Position: used internally during context checking.")
   (context-pos
    :accessor context-pos
    :type fixnum
    :initform 0
    :documentation "Used internally during context checking.")
   (warning-msg
    :accessor warning-msg
    :initform nil
    :documentation "Used internally to keep track of warnings.")
   ))

;; *** Methods that should be overridden ***
(defmethod l-productions ((ls l-system))
  "Override this when creating L-systems of greater depth than 0."
  t)

(defmethod homomorphism ((ls l-system))
  "Override this when creating L-systems with homomorphism."
  t)

(defmethod decomposition ((ls l-system))
  "Override this when creating L-systems with decomposition."
  t)

;; *** Helper functions ***
(defun add-bracket-params (vec)
  "Add positions of matching brackets as parameters."
  (declare (type simple-vector vec))
  (let ((bracket-positions nil))
    (dotimes (pos (length vec) vec)
      (declare (optimize (speed 3) (safety 0))
               (type fixnum pos))
      (let* ((module (svref vec pos))
             (symbol (if (consp module) (first module) module)))
        (case symbol
          (\[ (push pos bracket-positions))
          (\] (let ((bpos (pop bracket-positions)))
                (setf (svref vec bpos) (list '\[ pos)
                      (svref vec pos) (list '\] bpos)))))))))

(defun rlist->vector (rlist length)
  "Convert a reverse list RLIST of given length LENGTH to a simple-vector."
  (declare (optimize (speed 3) (safety 0))
           (fixnum length))
  (let ((vec (make-array length)))
    (dolist (elt rlist)
      (setf (svref vec (decf length)) elt))
    vec))

;; *** Rewriting ***
(defmethod rewrite ((ls l-system) &optional (depth (depth ls)))
  "Initialize the rewriting string and rewrite DEPTH times.
If DEPTH is 0, the rewriting string is initialized with the axiom,
but no rewriting is done."
  ;; intialize some slots
  (setf (geometry ls) nil)
  (setf (current-depth ls) 0)
  ;; init rstring with the axiom
  ;;(setf (rstring ls) (apply #'vector (axiom ls)))
  (setf (rstring ls) (map 'simple-vector #'copy-tree (axiom ls)))
  (add-bracket-params (rstring ls))
  ;; for enviro-sensitive systems, create geometry
  (if (sensitive ls) (create-geometry ls))
  ;; do the rewrites
  (dotimes (i depth)
    (rewrite1 ls))
  ;;(if (sensitive ls) (create-geometry ls)))
  (rstring ls))

(defun skip-to-right-bracket (rstring pos)
  "Skip to next unmatched ']', and return the new position,
or NIL if there is no unmatched ']'."
  ;; Note: this could be optimized, but is probably fast enough
  (let ((i pos)
        (len (length rstring)))
    (loop
     (when (>= i len)
       (return nil))
     (let* ((module (svref rstring i))
            (symbol (if (consp module) (first module) module)))
       (cond ((eql symbol '\[)
              (setq i (1+ (skip-to-right-bracket rstring (1+ i)))))
             ((eql symbol '\])
              (return i))
             (t
              (incf i)))))))

(defmethod rewrite1 ((ls l-system))
  "Do a single rewrite of the L-system.
This includes a decomposition rewrite if DECOMPOSITION-DEPTH > 0."
  ;; If geometry exists, remove it
  (setf (geometry ls) nil)
  ;; Update current-depth
  (incf (current-depth ls))
  ;; Use a buffer to fill in new string
  (let* ((oldstr (rstring ls))
         ;;(strlength (length oldstr))
         (newstr (make-buffer)))
    (dotimes (pos (length oldstr))
      (declare (optimize (speed 3) (safety 0))
               (type simple-vector oldstr)
               (type fixnum pos))
      (setf (pos ls) pos)
      (let ((module (svref oldstr pos)))
        ;; check for cut symbol
        (when (member module '(\% :cut-symbol) :test #'eq)
          (let ((jump-pos (skip-to-right-bracket oldstr pos)))
            (if jump-pos
              (setf pos jump-pos
                    (pos ls) jump-pos
                    module (svref oldstr pos))
              (return))))
        (setf (current-module ls) module)
        (let ((answer (l-productions ls)))
          (if (eq answer t)
            ;; T means identity production (no change)
            (buffer-push module newstr)
            ;; otherwise we should have a list
            (dolist (obj answer)
              ;; add all non-nil objects to new rstring
              (unless (null obj)
                (buffer-push obj newstr)))))))
    ;; convert buffer to simple-vector
    (setf (rstring ls) (buffer->vector newstr))
    ;; set some other slots
    (setf (pos ls) 0
          (current-module ls) nil)
    ;; do decomposition if it exists
    (when (> (decomposition-depth ls) 0)
      (decomposition-rewrite ls))
    ;; add bracket parameters (for context checking in next step)
    (add-bracket-params (rstring ls))
    ;; create geometry if it is environmentally sensitive
    (when (sensitive ls)
      (create-geometry ls))
    ;; return new rstring
    (rstring ls)))

(defun tree-rewrite (ls production-func max-depth)
  "Common algorithm for homomorphism and decomposition (a bit like
Chomsky grammar rewriting)."
  (let* ((oldstr (rstring ls))
         (strlength (length oldstr))
         (newstring (make-buffer)))
    (labels ((expand-module (max-depth)
               (declare (optimize (speed 3) (safety 0))
                        (type fixnum max-depth)
                        (type compiled-function production-func))
               (if (= max-depth 0)
                 (progn (when (not (warning-msg ls))
                          (setf (warning-msg ls) "Maximum depth reached."))
                        (buffer-push (current-module ls) newstring))
                 (let ((x (funcall production-func ls)))
                   (if (eq x t)
                     (buffer-push (current-module ls) newstring)
                     (dolist (obj x)
                       (setf (current-module ls) obj)
                       (expand-module (1- max-depth))))))))
      (dotimes (pos strlength)
        (declare (optimize (speed 3) (safety 0))
                 (type fixnum pos))
        (setf (current-module ls) (svref oldstr pos))
        (expand-module max-depth)))
    (buffer->vector newstring)))

;; *** Homomorphism rewriting ***
(defmethod homomorphism-rewrite ((ls l-system))
  "Do a homomorphism (visual) rewrite from the current rewriting string."
  (let ((newstr (tree-rewrite ls #'homomorphism
                              (homomorphism-depth ls))))
    (when (warning-msg ls)
      (format t "Homomorphism warning: ~A" (warning-msg ls))
      (setf (warning-msg ls) nil))
    (setf (hstring ls) newstr)))

;; *** Decomposition rewriting ***
(defmethod decomposition-rewrite ((ls l-system))
  "Do a decomposition rewrite on the current rewriting string."
  (let ((newstr (tree-rewrite ls #'decomposition
                              (decomposition-depth ls))))
    (when (warning-msg ls)
      (format t "Decomposition warning: ~A" (warning-msg ls))
      (setf (warning-msg ls) nil))
    (setf (rstring ls) newstr)))

;; *** Context checking ***
(declaim (inline next-module-left))
(defun next-module-left (ls)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (= (the fixnum (context-pos ls)) 0)
    :empty
    (svref (rstring ls) (decf (the fixnum (context-pos ls))))))

(defun next-context-module-left (ls ignore-list consider-list)
  "Find next context module to the left, ignoring symbols and skipping
brackets as necessary."
  (if consider-list
    (let ((new-consider-list (cons :empty consider-list)))
      (do* ((module (next-module-left ls)
                    (next-module-left ls))
            (symbol (if (consp module) (first module) module)
                    (if (consp module) (first module) module)))
          ((member symbol new-consider-list) module)
        (if (eq symbol '\]) ;; skip brackets
          (setf (context-pos ls) (second module)))))
    (let ((new-ignore-list (append '(\[ \]) ignore-list)))
      (do* ((module (next-module-left ls)
                    (next-module-left ls))
            (symbol (if (consp module) (first module) module)
                    (if (consp module) (first module) module)))
          ((not (member symbol new-ignore-list)) module)
        (if (eq symbol '\]) ;; skip brackets
          (setf (context-pos ls) (second module)))))))

(defun match-context-left (ls context-pattern ignore consider)
  "If the symbols and lengths in context-pattern match, return t and a
parameter list."
  (setf (context-pos ls) (pos ls))
  (let ((param-list nil))
    (dolist (obj (reverse context-pattern) (values t param-list))
      (let* ((module (next-context-module-left ls ignore consider))
             (params (if (consp module) (rest module) nil))
             (symbol (if params (first module) module)))
        (if (and (eql symbol (car obj)) (= (length params) (cdr obj)))
          (setf param-list (append params param-list))
          (return (values nil nil)))))))

(declaim (inline next-module-right))
(defun next-module-right (ls)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (>= (the fixnum (context-pos ls))
          (the fixnum (1- (length (the simple-array (rstring ls))))))
    :empty
    (svref (rstring ls) (incf (the fixnum (context-pos ls))))))

(defun next-context-module-right (ls ignore-list consider-list)
  "Find next context module to the right, ignoring symbols as necessary,
but not skipping brackets."
  (if consider-list
    (let ((new-consider-list (append '(:empty \[ \]) consider-list)))
      (do* ((module (next-module-right ls)
                    (next-module-right ls))
            (symbol (if (consp module) (first module) module)
                    (if (consp module) (first module) module)))
          ((member symbol new-consider-list) module)))
    (do* ((module (next-module-right ls)
                  (next-module-right ls))
          (symbol (if (consp module) (first module) module)
                  (if (consp module) (first module) module)))
        ((not (member symbol ignore-list)) module))))

(defun match-context-right (ls context-pattern ignore consider)
  "If the symbols, lengths and brackets in context-pattern match, return a
parameter list and t."
  (setf (context-pos ls) (pos ls))
  (let ((param-list nil)
        (mstack nil))
    ;; local mstack function
    (flet ((mstack-empty? () (null mstack))
           (mstack-push (elt) (push elt mstack))
           (mstack-pop () (pop mstack)))
      (dolist (obj context-pattern (values t (nreverse param-list)))
        (do ((try-again t))
            ((not try-again))
          (let* ((module (next-context-module-right ls ignore consider))
                 (params (if (consp module) (rest module) nil))
                 (symbol (if params (first module) module)))
            (setq try-again nil)
            (cond ((eql (car obj) '\])
                   ;; skip to unmatched ], or fail if there is none
                   (if (mstack-empty?)
                     (return-from match-context-right (values nil nil))
                     (setf (context-pos ls) (mstack-pop))))
                  ((eql (car obj) '\[)
                   ;; if brackets match, then push end position on
                   ;; mstack, else fail
                   (if (eql symbol '\[)
                     (mstack-push (first params))
                     (return-from match-context-right (values nil nil))))
                  ((and (eql symbol (car obj))
                        (= (length params) (cdr obj)))
                   ;; found matching module, add params to list
                   (dolist (param params) (push param param-list)))
                  ((eql symbol '\[)
                   ;; no match, but skip brackets and try again
                   (setq try-again t)
                   (setf (context-pos ls) (first params)))
                  (t ;; no match, no brackets, fail
                   (return-from match-context-right (values nil nil))))))))))

;; *** Geometry methods ***
(defmethod create-geometry ((ls l-system))
  "Create geometry from current rewriting string."
  (let ((turtle-string (if (> (homomorphism-depth ls) 0)
                           (homomorphism-rewrite ls)
                           (rstring ls))))
    (setf (geometry ls)
      (turtle-interpret turtle-string
                        :angle-increment (angle-increment ls)))))

(defmethod rewrite-and-preview ((ls l-system) filename
                                &key
                                (depth (depth ls))
                                (width 0.3)
                                (sphere-width 0.1))
  "Rewrite, create geometry, output EPS file and view it in Ghostview."
  (rewrite ls depth)
  (if (null (geometry ls)) (create-geometry ls))
  (output-simple-eps (geometry ls) filename
                     :ps-width width
                     :sphere-width sphere-width)
  (asdf:run-shell-command (format nil "gv ~A -scale 10" filename)))

(defmethod rewrite-and-raytrace ((ls l-system) filename
                                 &key
                                 (depth (depth ls))
                                 (width (cylinder-width ls)))
  "Rewrite, create geometry, output POV file and raytrace it."
  (rewrite ls depth)
  (if (null (geometry ls)) (create-geometry ls))
  (output-povray (geometry ls) filename :width-multiplier width)
  (asdf:run-shell-command (format nil "povray +i~A" filename)))

(defmethod timed-raytrace ((ls l-system)
                           &key (filename "foo.pov")
                                (depth (depth ls))
                                (width-multiplier 0.01))
  (format t "~%REWRITE:~%")
  (time (rewrite ls depth))
  (format t "~%CREATE-GEOMETRY:~%")
  (if (geometry ls)
    (write-line "(already exists)")
    (time (create-geometry ls)))
  (format t "~%OUTPUT-POVRAY:~%")
  (time (output-povray (geometry ls) filename
                       :width-multiplier width-multiplier))
  (format t "~%Raytracing:~%")
  (time (asdf:run-shell-command (format nil "povray +i~A" filename))))

(defmethod timed-preview ((ls l-system)
                          &key (filename "foo.eps")
                               (depth (depth ls))
                               (width-multiplier 0.2))
  (format t "~%REWRITE:~%")
  (time (rewrite ls depth))
  (format t "~%CREATE-GEOMETRY:~%")
  (if (geometry ls)
    (write-line "(already exists)")
    (time (create-geometry ls)))
  (format t "~%OUTPUT-SIMPLE-EPS:~%")
  (time (output-simple-eps (geometry ls) filename
                           :ps-width width-multiplier))
  (format t "~%Ghostview:~%")
  (time (asdf:run-shell-command (format nil "gv ~A -scale 10" filename))))

(defun lsys2eps (classname filename &key (depth nil)
                                         (width-multiplier 0.3))
  (let ((lsys (make-instance classname)))
    (rewrite lsys (or depth (depth lsys)))
    (if (null (geometry lsys)) (create-geometry lsys))
    (output-simple-eps (geometry lsys) filename
                       :ps-width width-multiplier)))

(defun lsys2pov (classname filename &key (depth nil)
                                         (width-multiplier 0.02))
  (let ((lsys (make-instance classname)))
    (rewrite lsys (or depth (depth lsys)))
    (if (null (geometry lsys)) (create-geometry lsys))
    (output-povray (geometry lsys) filename
                   :width-multiplier width-multiplier)))

;; *** Animations ***

;; extracting frame numbers from a list of numbers and intervals
;; each element is a number, a (FROM TO) interval or a (FROM TO STEP) interval
;; ((1 3) 5) => (1 2 3 5)
;; (1 (20 50 10)) => (1 20 30 40 50)
(defun next-framelist (list)
  (let ((elt (first list))
        (rest (rest list)))
    (if (atom elt)
        (values elt rest)
        (let* ((step (if (null (cddr elt)) 1 (third elt)))
               (new-num (min (+ step (first elt))
                             (second elt)))
               (newelt (list new-num (second elt) step)))
          (if (>= (first newelt) (second newelt))
              (values (first elt) (cons (first newelt) rest))
              (values (first elt) (cons newelt rest)))))))

(defun top-of-framelist (list)
  (let ((x (car list)))
    (cond ((atom x) x)
          ((atom (car x)) (car x))
          (t (error "Invalid framelist ~A." list)))))

(defmacro extract-framelist (list)
  (let ((num-sym (gensym))
        (newlist-sym (gensym)))
    `(multiple-value-bind (,num-sym ,newlist-sym)
         (next-framelist ,list)
       (setf ,list ,newlist-sym)
       ,num-sym)))

(defmethod eps-animation ((ls l-system)
                          &key
                          (filename-prefix (string-downcase
                                            (class-name (class-of ls))))
                          (frames (or (frame-list ls)
                                      `((0 ,(depth ls)))))
                          (border-percent 10.0)
                          )
  (rewrite ls 0)
  (do* ((framelist (copy-tree frames)))
       ((null framelist) :done)
    (let* ((frame (extract-framelist framelist))
           (filename (format nil "~A~A.eps" filename-prefix frame)))
      (loop until (<= frame (current-depth ls))
            do (rewrite1 ls))
      (create-geometry ls)
      (format t "Outputting '~A'..." filename)
      (force-output)
      (output-simple-eps (geometry ls) filename
                         :border-percent border-percent)
      (format t "done.~%"))))

(defmethod povray-animation ((ls l-system)
                             &key
                             (filename-prefix (string-downcase
                                               (class-name (class-of ls))))
                             (frames (or (frame-list ls)
                                         `((0 ,(depth ls)))))
                             (width (cylinder-width ls))
                             (full-scene t)
                             )
  (rewrite ls 0)
  (do* ((framelist (copy-tree frames)))
       ((null framelist) :done)
    (let* ((frame (extract-framelist framelist))
           (filename (format nil "~A~A.pov" filename-prefix frame)))
      (loop until (<= frame (current-depth ls))
            do (rewrite1 ls))
      (create-geometry ls)
      (format t "Outputting '~A'..." filename)
      (force-output)
      (output-povray (geometry ls) filename
                     :width-multiplier width
                     :full-scene full-scene)
      (format t "done.~%"))))


;; *** Macros ***
(defun expand-prods-body (expr ls blockname)
  "Helper function for the CHOOSE-PRODUCTION macro; makes the necessary
changes to the WITH-*-CONTEXT and --> macros within the expression."
  (cond ((atom expr) expr)
        ;; expand with-contexts with ls
        ((member (first expr)
                 '(with-left-context with-right-context with-lc with-rc))
         `(,(first expr) ,ls ,@(expand-prods-body (rest expr) ls blockname)))
        ;; expand --> to return-->
        ((eq (first expr) '-->)
         `(return-from ,blockname
            (--> ,@(expand-prods-body (rest expr) ls blockname))))
        ;; don't expand nested choose-production expressions
        ((eq (first expr) 'choose-production)
         expr)
        ;; keep searching
        (t (cons (expand-prods-body (first expr) ls blockname)
                 (expand-prods-body (rest expr) ls blockname)))))

(defmacro prod (module module-form &rest body)
  "This macro makes some testing and variable-binding automatic.
Used by CHOOSE-PRODUCTION, don't call it directly."
  (if (consp module-form)
    `(when (and (consp ,module)
              (eq (first ,module) ',(first module-form))
              (= (length ,module) ,(length module-form)))
       (destructuring-bind ,(rest module-form)
           (rest ,module)
         (declare (ignorable ,@(rest module-form)))
         ,@body))
    `(when (eq ',module-form ,module)
       ,@body)))

(defmacro choose-production (ls &rest prods)
  "Choose and evaluate the first matching production.
Productions are given as: (sym expr-returning-list)
or with parameters:       ((sym param-1 ... param-n) expr-returning-list)"
  (let* ((ls-sym (gensym))
         (module-sym (gensym))
         (blockname (gensym))
         (expanded-prods (expand-prods-body prods ls-sym blockname)))
    `(block ,blockname
       (let* ((,ls-sym ,ls)
              (,module-sym (current-module ,ls-sym)))
         (or ,@(mapcar #'(lambda (x) (if (consp x)
                                         `(prod ,module-sym ,@x)
                                         `(prod ,module-sym ,x)))
                       expanded-prods)
             t)))))

(defmacro --> (&rest args)
  "Macro which can be used to specify right hand productions with
parameters as expressions.  Use this within CHOOSE-PRODUCTIONS."
  `(list ,@(mapcar #'(lambda (x) (if (consp x)
                                   `(list ',(car x) ,@(cdr x))
                                   `',x))
                   args)))

(defmacro stochastic-choice (&rest args)
  (let ((sym-list (mapcar #'(lambda (x) (declare (ignore x)) (gensym))
                          args))
        (random-sym (gensym))
        (n (length args)))
    `(let* ((,(first sym-list) ,(caar args))
            ,@(maplist
               #'(lambda (x y) `(,(second x) (+ ,(first x) ,(caar y))))
               sym-list (rest args))
            (,random-sym (random ,(nth (1- n) sym-list))))
       (cond ,@(mapcar
                #'(lambda (x y z) (declare (ignore z))
                          `((< ,random-sym ,x) ,(second y)))
                sym-list args (rest sym-list))
             (t ,(second (nth (1- n) args)))))))

(defmacro with-context (ls match-func-name context-form &body body)
  (let ((context-params-sym (gensym))
        (bool-sym (gensym))
        (bind-params nil)
        (context-pattern (mapcar #'(lambda (x)
                                  (let* ((params
                                          (if (consp x) (rest x) nil))
                                         (symbol
                                          (if params (first x) x)))
                                    (cons symbol (length params))))
                              context-form)))
    (dolist (obj context-form)
      (when (consp obj) (setf bind-params (append bind-params (rest obj)))))
    (if bind-params
        `(multiple-value-bind (,bool-sym ,context-params-sym)
             (,match-func-name ,ls ',context-pattern
                               (ignore-list ,ls) (consider-list ,ls))
           (when ,bool-sym
             (destructuring-bind ,bind-params ,context-params-sym
               (declare (ignorable ,@bind-params))
               ,@body)))
        `(when (,match-func-name ,ls ',context-pattern
                                 (ignore-list ,ls) (consider-list ,ls))
           ,@body))))

(defmacro with-left-context (ls context-form &body body)
  `(with-context ,ls match-context-left ,context-form ,@body))

(defmacro with-right-context (ls context-form &body body)
  `(with-context ,ls match-context-right ,context-form ,@body))

(defmacro with-lc (&rest args)
  `(with-left-context ,@args))

(defmacro with-rc (&rest args)
  `(with-right-context ,@args))

;; ignore/consider macros
(defmacro while-ignoring (ls ignore-list &body body)
  (let ((save-ignore (gensym))
        (save-consider (gensym)))
    `(let ((,save-ignore (ignore-list ,ls))
           (,save-consider (consider-list ,ls)))
       (unwind-protect
           (progn (setf (consider-list ,ls) nil
                        (ignore-list ,ls) ',ignore-list)
                  ,@body)
         (setf (consider-list ,ls) ,save-consider
               (ignore-list ,ls) ,save-ignore)))))

(defmacro while-considering (ls consider-list &body body)
  (let ((save-consider (gensym)))
    `(let ((,save-consider (consider-list ,ls)))
       (unwind-protect
           (progn (setf (consider-list ,ls) ',consider-list)
                  ,@body)
         (setf (consider-list ,ls) ,save-consider)))))
