(in-package :cl-user)

(uiop:define-package :l-systems
  (:nicknames :lsystem :lsys :ls)
  (:use :cl)
  (:export
   ;; trigonometry
   :deg-to-rad :rad-to-deg :cosd :sind :tand
   ;; 3D vectors
   :v3 :vec3 :vlength :normalize :equalvec :almost-equalvec :zerovec :vec- :vec+
   :dot-product :cross-product
   ;; turtle
   :turtle :turtle-pos :turtle-H :turtle-L :turtle-U :turtle-angle :turtle-width
   :turtle-prev-width :turtle-color :turtle-texture :copy-turtle :rotate
   :rotate-towards :turn :pitch :roll :move-forward :turtle-interpret
   :def-turtle-function-raw :def-turtle-function :turtle-function
   ;; geometric shapes
   :line :line-p1 :line-p2 :line-width
   :box :box-pos :box-size
   ;; short names of turtle functions
   :F :\f :+ :- :\& :\^ :\\ :/ :\| :! :@O :\{ :\. :f. :\} :m{ :m. :m/ :m} :mf :@M :@R
   :?P :?H :?U :?L :?T :\[ :\] :\%
   ;; creating images
   :output-simple-eps :output-povray
   ;; l-system class
   :l-system :axiom :depth :angle-increment :ignore-list :consider-list
   :homomorphism-depth :decomposition-depth :sensitive
   :rstring :hstring :geometry :current-depth :current-module
   :line-style :cylinder-width :limits :frame-delay :frame-list
   ;; rewriting
   :l-productions :homomorphism :decomposition
   :rewrite :rewrite1 :homomorphism-rewrite :decomposition-rewrite
   ;; geometry methods
   :create-geometry :rewrite-and-preview :rewrite-and-raytrace
   :timed-raytrace :timed-preview :lsys2eps :lsys2pov
   :eps-animation :povray-animation
   :top-of-framelist :next-framelist :extract-framelist
   ;; l-system macros
   :choose-production :--> :stochastic-choice
   :with-left-context :with-right-context :with-lc :with-rc
   :while-ignoring :while-considering
   ;; OpenGL stuff
   :gl-preview :gl-animation
   ;; Random number generation
   :nrandom))

(uiop:define-package :spline-editor
  (:nicknames :splineed :spline)
  (:use :cl)
  (:export
   :spline :spline-x :spline-y :spline-a :spline-b :spline-b :spline-c :spline-d
   :spline-function :natural-cubic-spline :spline-value :spline-values
   :make-spline-function :output-spline :input-spline :output-spline-to-eps
   :edit-spline))

(uiop:define-package :l-system-examples
  (:nicknames :lsx)
  (:use :common-lisp :l-systems :spline-editor))
