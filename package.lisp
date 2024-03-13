(cl:in-package :cl-user)

(defpackage :transform3
  (:use :cl)
  (:nicknames :xf3)
  (:export #:matrix
	   #:copy-matrix
	   #:make-identity
	   #:multiply
	   #:transpose
	   #:adjoint
	   #:determinant
	   #:inverse
	   #:column-major-write
	   #:row-major-write

	   #:point
	   #:make-point
	   #:point+
	   #:point-
	   #:point*
	   #:point/
	   #:-point
	   #:cross
	   #:dot
	   #:normalize
	   #:transform-point
	   
	   #:space-matrix
	   #:space-inverse
	   #:compose
	   #:move
	   #:move*
	   #:rotate-x
	   #:rotate-y
	   #:rotate-z
	   #:scale
	   #:perspective
	   #:look-at))
