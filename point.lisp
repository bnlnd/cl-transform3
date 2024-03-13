(cl:in-package :transform3)

(deftype point ()
  '(array double-float (4)))

(defun x (v) (aref v 0))
(defun y (v) (aref v 1))
(defun z (v) (aref v 2))
(defun w (v) (aref v 3))

(defun make-point* (v x y z w)
  (setf (aref v 0) x
	(aref v 1) y
	(aref v 2) z
	(aref v 3) w)
  v)

(defun make-point (x y z)
  (make-array 4 :element-type 'double-float
		:initial-contents (list x y z 1.0d0)))

(defun new-point ()
  (make-array 4 :element-type 'double-float
		:initial-contents (list 0.0d0 0.0d0 0.0d0 1.0d0)))

(defun point+ (a b &optional (sum (new-point)))
  (make-point* sum
	       (+ (x a) (x b))
	       (+ (y a) (y b))
	       (+ (z a) (z b))
	       1.0d0))

(defun point- (a b &optional (difference (new-point)))
  (make-point* difference
	       (- (x a) (x b))
	       (- (y a) (y b))
	       (- (z a) (z b))
	       1.0d0))

(defun -point (a &optional (negative (new-point)))
  (make-point* negative
	       (- (x a))
	       (- (y a))
	       (- (z a))
	       1.0d0))

(defun point* (a n &optional (product (new-point)))
  (make-point* product
	       (* n (x a))
	       (* n (y a))
	       (* n (z a))
	       1.0d0))

(defun point/ (a n &optional (product (new-point)))
  (make-point* product
	       (/ (x a) n)
	       (/ (y a) n)
	       (/ (z a) n)
	       1.0d0))

(defun cross (a b &optional (product (new-point)))
  (make-point* product
	       (- (* (y a) (z b)) (* (z a) (y b)))
	       (- (* (z a) (x b)) (* (x a) (z b)))
	       (- (* (x a) (y b)) (* (y a) (x b)))
	       1.0d0))

(defun dot (a b)
  (+ (* (x a) (x b))
     (* (y a) (y b))
     (* (z a) (z b))))

(defun normalize (point &optional (norm (new-point)))
  (let ((mag (sqrt (+ (expt (x point) 2)
		      (expt (y point) 2)
		      (expt (z point) 2)))))
    (unless (= 0.0d0 mag)
      (make-point* norm
		   (/ (x point) mag)
		   (/ (y point) mag)
		   (/ (z point) mag)
		   1.0d0))))

(defun transform-point (point mat &optional (new (new-point)))
  (make-point* new
	       (+ (* (x point) (aref mat 0 0))
		  (* (y point) (aref mat 0 1))
		  (* (z point) (aref mat 0 2))
		  (* (w point) (aref mat 0 3)))
	       (+ (* (x point) (aref mat 1 0))
		  (* (y point) (aref mat 1 1))
		  (* (z point) (aref mat 1 2))
		  (* (w point) (aref mat 1 3)))
	       (+ (* (x point) (aref mat 2 0))
		  (* (y point) (aref mat 2 1))
		  (* (z point) (aref mat 2 2))
		  (* (w point) (aref mat 2 3)))
	       (+ (* (x point) (aref mat 3 0))
		  (* (y point) (aref mat 3 1))
		  (* (z point) (aref mat 3 2))
		  (* (w point) (aref mat 3 3)))))
