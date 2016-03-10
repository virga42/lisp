
(defclass solar-system ()
  ((planets :accessor planets :initform '())))

(defgeneric add-planet (solar-system planet))
(defmethod add-planet ((s solar-system) planet)
  (setf (planets s) (push planet (planets s))))

(defclass planet ()
   ((image-item :accessor image-item)
    (degree :accessor degree :initform 0)
    (degree-increment :accessor degree-increment :initarg :degree-increment :initform 2)
    (diameter :accessor diameter :initarg :diameter :initform 10)
    (multiplier :accessor multiplier :initarg :multiplier :initform 7)
    (interval :accessor interval :initarg :interval :initform 10)
    (coords :accessor coords)))

(defgeneric orbit (planet canvas))
(defmethod orbit ((p planet) canvas)
  (let* ((new-degree (if (>= (degree p) 360)
			0
		        (+ (degree p) (degree-increment p))))
	 (new-coords (calculate-coords (diameter p) (degree p) (multiplier p))))
    (setf (degree p) new-degree)    (setf (coords p) new-coords)
    (set-coords canvas (image-item p) new-coords)))

(defclass chord ()
  ((image-item :accessor image-item)))

