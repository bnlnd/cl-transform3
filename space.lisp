(cl:in-package :transform3)

(defun compose (point &rest matrices)
  (reduce #'transform-point matrices :initial-value point))

(defun move (point &optional (m (make-identity)))
  (setf (aref m 0 3) (x point)
	(aref m 1 3) (y point)
	(aref m 2 3) (z point))
  m)

(defun move* (x y z &optional (m (make-identity)))
  (setf (aref m 0 3) x
	(aref m 1 3) y
	(aref m 2 3) z)
  m)

(defun rotate (axis theta &optional (m (make-identity)))		    
  (let ((i (mod (+ 1 axis) 3))
	(j (mod (+ 2 axis) 3))
	(c (cos theta))
	(s (sin theta)))
    (setf (aref m i i) c
	  (aref m i j) (- s)
	  (aref m j i) s
	  (aref m j j) c)
    m))

(defun rotate-x (theta &optional (m (make-identity)))
  (rotate 0 theta m))

(defun rotate-y (theta &optional (m (make-identity)))
  (rotate 1 theta m))

(defun rotate-z (theta &optional (m (make-identity)))
  (rotate 2 theta m))

(defun scale (x y z &optional (m (make-matrix)))
  (setf (aref m 0 0) x
	(aref m 1 1) y
	(aref m 2 2) z
	(aref m 3 3) 1.0d0)
  m)

(defun frustum (m left right bottom top znear zfar)
 (let ((temp1 (* 2.0d0 znear))
       (temp2 (- right left))
       (temp3 (- top bottom))
       (temp4 (- zfar znear)))
   (setf (aref m 0 0) (/ temp1 temp2)
	 (aref m 1 0) 0.0d0
	 (aref m 2 0) 0.0d0
	 (aref m 3 0) 0.0d0
	 (aref m 0 1) 0.0d0
	 (aref m 1 1) (/ temp1 temp3)
	 (aref m 2 1) 0.0d0
	 (aref m 3 1) 0.0d0
	 (aref m 0 2) (/ (+ right left) temp2)
	 (aref m 1 2) (/ (+ top bottom) temp3)
	 (aref m 2 2) (/ (- (- zfar) znear) temp4)
	 (aref m 3 2) -1.0d0
	 (aref m 0 3) 0.0d0
	 (aref m 1 3) 0.0d0
	 (aref m 2 3) (/ (* (- temp1) zfar) temp4)
	 (aref m 3 3) 0.0d0)))

(defun perspective (fov aspect znear zfar &optional (m (make-matrix)))
  (assert (> aspect 0.0d0))
  (assert (> pi fov 0.0d0))
  (assert (> zfar znear 0.001d0))
  (let* ((ymax (* znear (tan (* fov 0.5d0))))
	 (xmax (* ymax aspect)))
    (frustum m (- xmax) xmax (- ymax) ymax znear zfar)
    m))

(defun look-at (here there up &optional (m (make-matrix)))
  (let* ((f (normalize (point- there here)))
	 (s (normalize (cross f up)))
	 (u (cross s f))
	 (w (point- (new-point) f)))
    (setf (aref m 0 0) (x s)
	  (aref m 1 0) (x u)
	  (aref m 2 0) (x w)
	  (aref m 3 0) 0.0d0
	  (aref m 0 1) (y s)
	  (aref m 1 1) (y u)
	  (aref m 2 1) (y w)
	  (aref m 3 1) 0.0d0
	  (aref m 0 2) (z s)
	  (aref m 1 2) (z u)
	  (aref m 2 2) (z w)
	  (aref m 3 2) 0.0d0
	  (aref m 0 3) (- (dot here s))
	  (aref m 1 3) (- (dot here u))
	  (aref m 2 3) (- (dot here w))
	  (aref m 3 3) 1.0d0)
    m))
