#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))

(asdf:defsystem #:l-lisp
  :description "l-system framework"
  :author "Knut Arild Erstad <knute@ii.uib.no>"
  :license "GPL v3"
  :serial t
  :depends-on (#:cffi
               #:uiop
               #:cl-opengl
               #:cl-glu
               #:sdl2
               #:sdl2kit)
  :components ((:file "packages")
               (:file "buffer")
               (:file "nrandom")
               (:file "turtle")
               (:file "lsystem")
               (:file "opengl")
               (:file "examples")
               (:file "splineed")
               (:file "advanced-examples")
               (:file "demo")))
