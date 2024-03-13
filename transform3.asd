(cl:in-package :cl-user)

(asdf:defsystem "transform3"
  :version "0.1.0"
  :description "3D transform tools"
  :license "zlib"
  :depends-on ()
  :serial t
  :components ((:file "package")
	       (:file "matrix")
	       (:file "point")
	       (:file "space")))
