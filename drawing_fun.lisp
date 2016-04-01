(ql:quickload :lispbuilder-sdl)
(ql:quickload :ltk)
(ql:quickload :lparallel)


(defpackage :sdl-fun
  (:use :common-lisp :lispbuilder-sdl :ltk :lparallel))

(in-package :sdl-fun)

(setf lparallel:*kernel* (lparallel:make-kernel 4))



(defparameter *zoom-width* 80)
(defparameter *zoom-height* 80)

(defclass cross-hairs ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)
   (zoom-box-visibility :accessor zoom-box-visibility :initarg :zoom-box-visibility :initform :off)))

(defgeneric move-cross-hairs (cross-hairs direction))
(defmethod move-cross-hairs (cross-hairs direction)
    (cond ((and (eq direction :up) (> (y cross-hairs) 0))
	   (decf (y cross-hairs)))
	  ((and (eq direction :down) (< (y cross-hairs) *window-height*))
	   (incf (y cross-hairs)))
	  ((and (eq direction :left) (> (x cross-hairs) 0))
	   (decf (x cross-hairs)))
	  ((and (eq direction :right) (< (x cross-hairs) *window-width*))
	   (incf (x cross-hairs)))))



(defgeneric toggle-zoom-visibility (cross-hairs))
(defun toggle-zoom-visibility (cross-hairs)
  (if (eq (zoom-box-visibility cross-hairs) :off)
      (setf (zoom-box-visibility cross-hairs) :on)
      (setf (zoom-box-visibility cross-hairs) :off)))

(defgeneric draw-cross-hairs (cross-hairs surface))
(defmethod draw-cross-hairs (cross-hairs surface)
  (let ((random-color (sdl:color :r (random 255) :g (random 255) :b (random 255))))
    (sdl:draw-vline (x cross-hairs) 0 *window-height* :surface surface :color sdl:*white*)
    (sdl:draw-hline 0 *window-width* (y cross-hairs) :surface surface :color sdl:*white*)
    (when (eq (zoom-box-visibility cross-hairs) :on)
      (draw-surface (draw-zoom-box cross-hairs) :surface surface))))

(defgeneric draw-zoom-box (cross-hairs))
(defmethod draw-zoom-box (cross-hairs)
  (let* ((x (zoom-horizontal-check (x cross-hairs)))
	(y (zoom-vertical-check (y cross-hairs)))
	(surface (create-surface *zoom-width* *zoom-height* :alpha 100 :x x :y y)))
    (sdl:draw-box  (rectangle :x x
			      :y y
			      :w *zoom-width*
			      :h *zoom-height*)
		  :surface surface
		  :stroke-color sdl:*white*
		  :color (sdl:color :r 25 :g 50 :b 75 :a 250))
    surface))

(defun zoom-horizontal-check (x)
  (let* ((half-zoom-width (/ *zoom-width* 2))
	(left-boundary half-zoom-width)
	(right-boundary (- *window-width* half-zoom-width)))
    (cond ((< x left-boundary) 0)
	  ((> x right-boundary) (- *window-width* *zoom-width*))
	  (t x))))

(defun zoom-vertical-check (y)
  (let* ((half-zoom-height (/ *zoom-height* 2))
	 (top-boundary half-zoom-height)
	 (bottom-boundary (- *window-height* half-zoom-height)))
    (cond ((< y top-boundary) 0)
	  ((> y bottom-boundary) (- *window-height* *zoom-height*))
	  (t y))))





(defparameter *window-width* 320)
(defparameter *window-height* 320)

(defun get-random-rectangle ()
  (let ((x (random *window-width*))
	(y (random *window-height*)))
    (sdl:rectangle :x x :y y :w 100 :h 100)))
    
(defun center-rectangle (rect)
  (let* ((w (width rect))
	 (h (height rect))
	 (center-x (floor (- (* .5 *window-width*) (* .5 w))))
	 (center-y (floor (- (* .5 *window-height*) (* .5 h)))))
    (sdl:rectangle :x center-x :y center-y :w w :h h)))
    
(defclass fractal ()
  ((x :initarg :x :accessor x :initform -1.5)
   (y :initarg :y :accessor y :initform -1.0)
   (max-iterations :initarg :max-iterations :accessor max-iterations :initform 255)
   (step-divisor :initarg :step-divisor :accessor step-divisor :initform 100.0)
   (width :initarg :width :accessor width :initform 200)
   (height :initarg :height :accessor height :initform 200)))

(defgeneric fractal-width (fractal))
(defmethod fractal-width ((f fractal))
  (/ (width f) (step-divisor f)))

(defgeneric fractal-height (fractal))
(defmethod fractal-height ((f fractal))
  (/ (height f) (step-divisor f)))

(defun compute-bounds (x y starting-x starting-y step-divisor max-iterations)
  (let ((c (complex (+ (/ x step-divisor) starting-x) (+ (/ y step-divisor) starting-y)))
	(z (complex 0.0 0.0))
	(iteration 0)
	(bounds 0))
    (loop
       (setf z (+ (* z z) c))
       (incf iteration)
       (cond ((< 4 (abs z))
	      (setf bounds iteration)
	      (return))
	     ((>= iteration max-iterations)
	      (setf bounds max-iterations)
	      (return))))
    bounds))

(defun make-pixel-list (width height)
  (loop for y from 0 to (- height 1) append
       (loop for x from 0 to (- width 1) collect
	    (point :x x :y y))))

(defun pmake-mandelbrot (surface &key width height starting-x starting-y max-iterations step-divisor color-fn)
  (let* ((pixels (make-pixel-list width height))
	 (bounding-fn (lambda (p)
			(compute-bounds (x p) (y p) starting-x starting-y step-divisor max-iterations)))
	 (bound-pixels (pmap 'vector bounding-fn pixels)))
    (dotimes (y height)
      (dotimes (x width)
	(let ((i (+ (* y width)  x)))
	  (draw-pixel-* x y :surface surface :color (funcall color-fn (aref bound-pixels i) max-iterations)))))
    surface))
    


(defun make-mandelbrot (surface &key width height starting-x starting-y max-iterations step-divisor color-fn)
  (dotimes (y height)
    (dotimes (x width)
      (let ((c (complex (+ (/ x step-divisor) starting-x) (+ (/ y step-divisor) starting-y)))
	    (z (complex 0.0 0.0))
	    (iteration 0))
	(loop
           (setf z (+ (* z z) c))
           (incf iteration)
           (cond ((< 4 (abs z))
                  (draw-pixel-* x y :color (funcall color-fn iteration max-iterations))
                  (return))
                 ((>= iteration max-iterations)
                  (draw-pixel-* x y :color *red*)
                  (return)))))))
  surface)

(defun to-bluescale (i &key (max-iterations 255))
  (cond ((< i 2) (color :r 240 :g 248 :b 255))
	((< i 4) (color :r 135 :g 206 :b 250))
	((< i 6) (color :r 65 :g 105 :b 225))
	((< i 10) (color :r 0 :g 0 :b 205))
	(t (color :r 25 :g 25 :b 112))))

;(0 255 0) -> (255 0 0) in 100 steps
(defun make-gradient (starting-color ending-color)
  (multiple-value-bind (starting-r starting-g starting-b) (color-* starting-color)
    (multiple-value-bind (ending-r ending-g ending-b) (color-* ending-color)
      (let ((diff-r (- ending-r starting-r))
	    (diff-g (- ending-g starting-g))
	    (diff-b (- ending-b starting-b)))
	(lambda (n max-iterations)
	  (if (>= n (- max-iterations 1))
	      (color :r 0 :g 0 :b 0)
	      (let* ((n (atan (/ n max-iterations)))
		     (new-r (floor (+ (* n diff-r) starting-r)))
		     (new-g (floor (+ (* n diff-g) starting-g)))
		     (new-b (floor (+ (* n diff-b) starting-b))))
		(color :r new-r :g new-g :b new-b))))))))
	

(defun to-grayscale (i &key (max-iterations 255))
  (if (= i max-iterations)
      (color :r 255 :g 255 :b 255)
      (let ((c (- 255 (* (/ i max-iterations) max-iterations))))
	(color :r 255 :g c :b c))))

(defun make-simple-mandelbrot (f surface)	                 
  (let ((green-to-red (make-gradient *blue* *white*)))    
    (pmake-mandelbrot surface :width (width f) :height (height f) :starting-x (x f)
		     :starting-y (y f) :max-iterations (max-iterations f)
		     :step-divisor (step-divisor f) :color-fn green-to-red)))

(defun coords->complex (f x y)
  (let ((new-x (+ (/ x (step-divisor f)) (x f)))
	(new-y (+ (/ y (step-divisor f)) (y f))))
    (complex new-x new-y)))
    
(defun center-on-complex-point (complex-point f)
  (let ((new-x (- (realpart complex-point) (* .5 (fractal-width f))))
	(new-y (- (imagpart complex-point) (* .5 (fractal-height f)))))
    (format t "Width: ~a~%" (fractal-width f))
    (format t "Height: ~a~%" (fractal-height f))
    (complex new-x new-y)))

(defun zoom-on-complex-point (complex-point zoom-factor f)  
  (let ((new-step-divisor (* (step-divisor f) zoom-factor)))
    (setf (step-divisor f) new-step-divisor)
    (let ((center (center-on-complex-point complex-point f)))	 
      (setf (x f) (realpart center))
      (setf (y f) (imagpart center)))))





(defun start-simulation (text-box text-box2)
  (sdl:with-init ()
    (sdl:window *window-width* *window-height* :title-caption "Fractal Viewer v0.01" :icon-caption "")
    (setf (sdl:frame-rate) 30)
    (sdl:clear-display (sdl:color :r 255 :g 255 :b 255) :update t)
    (let ((my-rect (get-random-rectangle))	  
	  (my-fractal (make-instance 'fractal :width *window-width* :height *window-height*
				     :step-divisor 100.0 :y -2 :x -3
				     :max-iterations 50.0))
	  (cross-hairs (make-instance 'cross-hairs :x 160 :y 160))
	  (zoom-box (create-surface 80 80 :alpha 100))
	  (fractal-surface (create-surface *window-width* *window-height*))	    
	  (*random-color* (sdl:color :r (random 255) :g (random 255) :b (random 255))))
      
      (draw-rectangle my-rect :color (sdl:color))
      (make-simple-mandelbrot my-fractal fractal-surface)
      
      (sdl:enable-key-repeat 200 10)
      (draw-cross-hairs cross-hairs *default-surface*)
      (draw-surface fractal-surface)
      (update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:key-down-event (:key key)
			 (cond
			   ((sdl:key= key :sdl-key-up) (move-cross-hairs cross-hairs :up))
			   ((sdl:key= key :sdl-key-down) (move-cross-hairs cross-hairs :down))
			   ((sdl:key= key :sdl-key-left) (move-cross-hairs cross-hairs :left)) 
			   ((sdl:key= key :sdl-key-right) (move-cross-hairs cross-hairs :right))
			   ((sdl:key= key :sdl-key-space)
			    (progn
			      (toggle-zoom-visibility cross-hairs)
			      (when (eq :off (zoom-box-visibility cross-hairs))
				(zoom-on-complex-point
				 (coords->complex my-fractal (x cross-hairs) (y cross-hairs))
				 2
				 my-fractal)
				(setf fractal-surface (make-simple-mandelbrot my-fractal fractal-surface)))
			      ))
			   ((sdl:key= key :sdl-key-escape) (sdl:push-quit-event)))			 
			 (draw-surface fractal-surface)
			 (draw-cross-hairs cross-hairs *default-surface*))
	(:mouse-button-down-event (:x x :y y)
				  (setf (text text-box) (format nil "Mouse: ~a" (coords->string-* x y)))
				  (setf (text text-box2)
					(format nil "Fractal: ~a"
						(coords->complex my-fractal x y)))
				  (zoom-on-complex-point
				   (coords->complex my-fractal x y)
				   2
				   my-fractal)
				  (setf fractal-surface (make-simple-mandelbrot my-fractal fractal-surface))
				  (draw-surface fractal-surface)
				  )
	(:idle ()	      	       
	       (update-display)
	       (process-events)
	       )))))

(defun coords->string (p)
  (coords->string-* (x p) (y p)))
				  
(defun coords->string-* (x y)
  (format nil "(~$, ~$)" x y))


(defun main ()
  (with-ltk ()
    (let* ((frame (make-instance 'frame :width *window-width* :height *window-height*))
	   (mouse-coords-label (make-instance 'message :master frame :text (coords->string-* 0 0)))
	   (fractal-coords-label (make-instance 'message :master frame :text (coords->string-* 0 0)))
	   (button (make-instance 'button :master frame :text "Start")))

      (setf (command button) (lambda () (start-simulation mouse-coords-label fractal-coords-label)))
      (pack frame)
      (pack mouse-coords-label)
      (pack fractal-coords-label)
      (pack button))))
	   


  
  			  
				
		       
    
