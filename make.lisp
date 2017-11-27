(defparameter *l-lisp-dir* "/home/knute/hfag/llisp/")

(make:defsystem l-lisp
  :source-pathname *l-lisp-dir*
  :source-extension "lisp"
  :components ((:file "buffer")
	       (:file "turtle" :depends-on ("buffer"))
	       (:file "lsystem" :depends-on ("buffer" "turtle"))
	       (:file "opengl" :depends-on ("turtle" "lsystem"))
	       (:file "examples" :depends-on ("lsystem"))
	       (:file "splineed")
	       (:file "advanced-examples"
		      :depends-on ("lsystem" "opengl" "splineed"))))
