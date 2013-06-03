
(in-package #:planets)

(defun sq (X) (* x x))

;;; ---------- physics functions -------------------------------

(defun gamma (vel)
  (destructuring-bind (vx vy vz) vel
	(/ (sqrt (- 1 (+ (sq vx) (sq vy) (sq vz)))))))

(defun trafo (loc vel)
  "Convert coords in rest frame into coords in frame moving at vel relative
to the rest frame"
  (destructuring-bind (t0 x0 y0 z0) loc
    (destructuring-bind (vx vy vz) vel		      
	  (let ((g (gamma vel))
			(rv (apply #'+ (mapcar #'* (list x0 y0 z0) (list vx vy vz))))
			(v (let ((tmp (sqrt (+ (sq vx) (sq vy) (sq vz)))))
				 (if (< tmp 0.00001) 0.00001 tmp))))
		(list (* g (- t0 rv))
			  (+ x0 (* vx (- (/ (- g 1) (sq v)) (* g t0))))
			  (+ y0 (* vy (- (/ (- g 1) (sq v)) (* g t0))))
			  (+ z0 (* vz (- (/ (- g 1) (sq v)) (* g t0)))))))))

;; u' = (v + u|| + u/gamma)/(1 + v.u/c^2)
;; u|| u parallel to v, u u perpendicular to v
(defun vel-trafo (u v)
  "Convert a velocity u in the rest frame into the velocity in the frame moving
at velocity v relative to the rest frame"
  (destructuring-bind (u1 u2 u3) u
    (destructuring-bind (v1 v2 v3) v
      (let ((uv (+ (* u1 v1) (* u2 v2) (* u3 v3)))
			(g (gamma (list v1 v2 v3))))
		(let ((const1 (1+ (/ (* g uv) (1+ g))))
			  (const2 (/ (1+ uv))))
		  (list (* const2 (+ (* const1 v1) (/ u1 g)))
				(* const2 (+ (* const1 v2) (/ u2 g)))
				(* const2 (+ (* const1 v3) (/ u3 g)))))))))


