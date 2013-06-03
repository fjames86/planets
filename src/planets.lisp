
(in-package #:planets)

;; ---------------

;;; objects that sit on the map all share some properties
;;; location, velocity to control position, movement
;;; name is a unique tag

(defparameter *planets* nil)
(defparameter *ships* nil)
(defparameter *home* nil)
(defparameter *ship-speed* 0.02)

(defclass object ()
  ((loc :accessor object-loc :initarg :loc)
   (vel :accessor object-vel :initarg :vel)
   (name :reader object-name :initarg :name))
  (:documentation "Base object for all objects"))

(defclass planet (object)
  ((color :accessor planet-color :initarg :color)
   (radius :accessor planet-radius :initarg :radius)))

(defclass ship (object)
  ((dest-loc :accessor ship-dest-loc :initarg :dest-loc)
   (dest-r :accessor ship-dest-r :initarg :dest-r)))

(defmethod print-object ((o object) stream)
  (format stream "#<OBJECT :LOC ~A :VEL ~A>"
		  (object-loc o) (object-vel o)))

(defgeneric object-time (o))

(defmethod object-time ((o object))
  (car (object-loc o)))
  
(defun make-planet (name x y)
  (let ((p (make-instance 'planet
						  :loc (list 0 x y 0)
						  :vel (list 0 0 0)
						  :name name
						  :color (sdl:color :r (random 255)
											:g (random 255)
											:b (random 255))
						  :radius (+ 5 (random 10)))))
	(push p *planets*)
	(if (null *home*) (setf *home* p))
	p))

(defun lookup-planet (name)
  (find name *planets* :key #'object-name))

(defun make-ship (name origin dest speed)
  (let ((from (lookup-planet origin))
		(to (lookup-planet dest)))
	(destructuring-bind (ft fx fy fz) (object-loc from)
	  (destructuring-bind (dt dx dy dz) (object-loc to)
		(declare (ignore dt))
		(let* ((h (sqrt (+ (sq (- dx fx)) (sq (- dy fy)) (sq (- dz fz)))))
			   (theta (acos (/ (- dz fz) h)))
			   (phi (atan (- dy fy) (- dx fx))))
		  (let ((s (make-instance 'ship
								  :loc (list ft fx fy fz)
								  :vel (list (* speed h (sin theta) (cos phi))
											 (* speed h (sin theta) (sin phi))
											 (* speed h (cos theta)))
								  :name name
								  :dest-loc (list 0 dx dy dz)
								  :dest-r (planet-radius to))))
			(push s *ships*)
			(if (null *home*) (setf *home* s))
			s))))))

(defun ship-arrived-p (ship)
  (destructuring-bind (t0 x y z) (object-loc ship)
	(declare (ignore t0))
	(destructuring-bind (dt dx dy dz) (ship-dest-loc ship)
	  (declare (ignore dt))
	  (let ((r (ship-dest-r ship)))
		(and (< (abs (- dx x)) r)
			 (< (abs (- dy y)) r)
			 (< (abs (- dz z)) r))))))

(defgeneric update-object (o dt))

(defmethod update-object ((o object) dt)
  (destructuring-bind (t0 x y z) (object-loc o)
	(destructuring-bind (vx vy vz) (object-vel o)
	  (setf (object-loc o) (list (+ t0 dt)
								 (+ x (* vx dt))
								 (+ y (* vy dt))
								 (+ z (* vz dt))))))
  o)

(defun update-ships (dt)
  (setf *ships*
		(mapcan (lambda (ship)
				  (update-object ship dt)
				  (unless (ship-arrived-p ship)
					(list ship)))
				*ships*)))
			  
;; ----- drawing -----------------

(defun loc-point (loc)
  (destructuring-bind (t0 x y z) loc
	(declare (ignore t0 z))
	(destructuring-bind (dt dx dy dz) (object-loc *home*)
	  (declare (ignore dt dz))
	  (sdl:point :x (round (+ (/ *window-width* 2) (- x dx)))
				 :y (round (+ (/ *window-height* 2) (- y dy)))))))

(defparameter *default-object-color* sdl:*white*)
(defparameter *default-object-radius* 2)

(defgeneric draw-object (o surface))

(defmethod draw-object ((o object) surface)
  (sdl:draw-filled-circle (loc-point (object-loc o))
						  *default-object-radius*
						  :surface surface
						  :color *default-object-color*))

(defmethod draw-object ((p planet) surface)
  (sdl:draw-filled-circle (loc-point (object-loc p))
						  (planet-radius p)
						  :surface surface
						  :color (planet-color p))
  (destructuring-bind (t0 x y z) (object-loc p)
	(sdl:draw-string-shaded (symbol-name (object-name p))
							  (loc-point (list t0 (+ x (planet-radius p) 2) y z))
							  sdl:*black* sdl:*white*
							  :surface surface)))

(defmethod draw-object ((s ship) surface)
  (sdl:draw-filled-circle (loc-point (object-loc s))
						  *default-object-radius*
						  :surface surface
						  :color *default-object-color*))

(defun draw-objects (surface)
  (mapc (lambda (planet)
		  (draw-object planet surface))
		*planets*)
  (mapc (lambda (ship)
		  (draw-object ship surface))
		*ships*))

(defvar *window-width* 600)
(defvar *window-height* 600)


(defgeneric update-object (o dt))

(defmethod update-object ((o object) dt)
  (destructuring-bind (t0 x y z) (object-loc o)
	(destructuring-bind (vx vy vz) (object-vel o)
	  (setf (object-loc o)
			(list (+ t0 dt)
				  (+ x (* vx dt))
				  (+ y (* vy dt))
				  (+ z (* vz dt))))))
  o)

(defun update-objects (dt)
  (mapc (lambda (planet)
		  (update-object planet dt))
		*planets*)
  (update-ships dt))

(defun set-home (home)
  (let ((vel (object-vel home)))
	(setf *home* home)
	(mapc (lambda (obj)
			(setf (object-loc obj) (trafo (object-loc obj) vel)
				  (object-vel obj) (vel-trafo (object-vel obj) vel)))
		  *planets*)
	(mapc (lambda (ship)
			(setf (object-loc ship) (trafo (object-loc ship) vel)
				  (object-vel ship) (vel-trafo (object-vel ship) vel)
				  (ship-dest-loc ship) (trafo (ship-dest-loc ship) vel)))
		  *ships*)))

(defun destructure-time (x)
  (multiple-value-bind (years days) (round x 1000)
	(if (< days 0)
		(setf years (1- years)
			  days (+ days 1000)))
	(multiple-value-bind (days hours) (round days 100)
	  (if (< hours 0)
		  (setf days (1- days)
				hours (+ hours 100)))
	  (list years days (round hours)))))

(defun planets-main (dt)
  (sdl:with-init ()
	(sdl:window *window-width* *window-height*
				:title-caption "Planets, Frank James"
				:icon-caption "Planets"
				:fps (make-instance 'sdl:fps-timestep))
	(setf (sdl:frame-rate) 60)

	(sdl:initialise-default-font sdl:*font-10x20*)

	(let ((selected nil))
	  (sdl:with-events ()
		(:quit-event () t)
		(:video-expose-event () (sdl:update-display))
		(:key-down-event ()
		  (cond
			((sdl:key-pressed-p :sdl-key-escape)
			 (sdl:push-quit-event))
			((sdl:key-pressed-p :sdl-key-f)
			 (make-ship 'ship
						(object-name *home*)
						(object-name selected)
						*ship-speed*))
			((sdl:key-pressed-p :sdl-key-t)
			 (when selected
			   (set-home selected)
			   (setf selected nil)))))
		(:mouse-button-down-event ()

          (setf selected nil)								  
          (dolist (planet *planets*)
			(let ((p (loc-point (object-loc planet))))
			  (if (and (not (eq planet *home*))
					   (< (abs (- (sdl:mouse-x) (sdl:x p))) (planet-radius planet))
					   (< (abs (- (sdl:mouse-y) (sdl:y p))) (planet-radius planet)))
				  (setf selected planet)))))
		(:idle ()
		  (sdl:clear-display sdl:*black*)
			   
		  (draw-objects sdl:*default-surface*)		  		  
		  (if selected
			  (sdl:draw-circle (loc-point (object-loc selected))
							   (+ (planet-radius selected) 5)
							   :color sdl:*white*))
		
		  (update-objects dt)
		  
		  (sdl:draw-string-shaded-* (symbol-name (object-name *home*))
									(- *window-width* 300)
									(- *window-height* 75)
									sdl:*black* sdl:*white*)
		  
		  (destructuring-bind (year day hour) (destructure-time (object-time *home*))
			(sdl:draw-string-shaded-* (format nil "Year: ~A Day: ~A Time: ~A"
											  year day hour)
									(- *window-width* 300)
									(- *window-height* 50) 
									sdl:*black* sdl:*white*))
		(sdl:update-display))))))

(defun planets (&key (dt 0.05) (background t))
  (if background
	  (bordeaux-threads:make-thread (lambda () (planets-main dt))
									:name 'planets-thread)
	  (planets-main dt)))


(make-planet 'earth 100 200)
(make-planet 'mars 200 100)
