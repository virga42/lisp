;;; Planets
;;; by Tom Callahan

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :ltk)
  (ql:quickload :local-time))

(defpackage :planets2
  (:use :common-lisp :ltk)
  (:export :main))

(in-package :planets2)

(load "~/bottega/planets/planet-model.lisp")

(defparameter *update-interval* 10)
(defparameter x-origin 300)
(defparameter y-origin 300)

(defun state-toggle ()
  (let ((state :on))
    (lambda ()
      (if (equalp state :on)
          (setf state :off)
          (setf state :on))
      state)))

(defun rad (degrees)
  (* pi (/ degrees 180.0)))

(defun x-factor (degree)
  (cos (rad degree)))

(defun y-factor (degree)
  (sin (rad degree)))

(defun factor-multiplier (factor multiplier)
  (* factor multiplier))

(defun calculate-radius (diameter)
  (/ diameter 2.0))

(defun calculate-coords (diameter degree multiplier)
  (let* ((radius (calculate-radius diameter))
	(x0 (round (+ x-origin (factor-multiplier (x-factor degree) multiplier) (- radius))))
	(x1 (+ x0 diameter))
	(y0 (round (+ y-origin (factor-multiplier (y-factor degree) multiplier) (- radius))))
	(y1 (+ y0 diameter)))
    (list x0 y0 x1 y1)))

(defun centered-coords (coords)
  (cons (/ (+ (first coords) (third coords)) 2)
	(cons (/ (+ (second coords) (fourth coords)) 2) nil)))

;;; timer functions
;;; attribution to Alex Stults

(defun milliseconds-to-nanoseconds (n)
  (* n 1000000))

(defun ticker-generator ()
  (let ((milliseconds-per-tick 33)
	(time-of-last-tick (local-time:now)))
    (lambda ()
      (let ((now (local-time:now)))
	(if (local-time:timestamp<
	     (local-time:timestamp+ time-of-last-tick
				    (milliseconds-to-nanoseconds 
				     milliseconds-per-tick) :nsec) now)
	    (setf time-of-last-tick now)
	    nil)))))

(defwidget control-panel (frame)
  ((value :accessor value :initform 20)
   (state :accessor state :initform :on)
   (command :accessor command :initform (lambda () ))
   (cmd-action-btn :accessor cmd-action-btn :initform (lambda () )))
  ()
  (let ((chord-queue-title (make-instance 'label 
					  :text "Number of chords"
					  :master self))
	(chord-queue-slider (make-instance 'scale 
			       :orientation :horizontal
			       :from 10
			       :to 400
			       :master self))
	(chord-queue-slider-label (make-instance 'label :master self))
	(action-btn (make-instance 'button :master self
				   :text "Pause")))
    (setf (command chord-queue-slider) (lambda (e) (progn
						     (setf (text chord-queue-slider-label) 
							   (format nil "~a" (floor e)))
						     (setf (value self) e)
						     (funcall (command self) e))))
    (setf (command action-btn) (lambda () (progn
					     (setf (text action-btn) "Go!")
					     (setf (state self) (funcall (cmd-action-btn self))))))
					     
    (setf (text chord-queue-slider-label) (value self))
    (pack chord-queue-title :side :top)
    (pack chord-queue-slider :side :top)
    (pack chord-queue-slider-label :side :top)
    (pack action-btn)))

;;; queueing functions
;;; attribution to Peter Norvig

(defun queue-contents (q) (cdr q))

(defun make-queue ()
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (item q)
  (setf (car q)
	(setf (rest (car q))
	      (cons item nil)))
  q)

(defun dequeue (q)
  (let ((item-dq (pop (cdr q))))
  (if (null (cdr q)) (setf (car q) q))
  item-dq))

(defun front (q) (first (queue-contents q)))

(defun empty-queue-p (q) (null (queue-contents q)))

;;;

(defun update ()
  (when (equalp (state panel) :on)
    (when (funcall my-ticker)
      (orbit outer-planet c)
      (orbit inner-planet c)
      (when (and (> (degree inner-planet) 0) (= 0 (mod (degree inner-planet) 10)))
	(draw-chord a-chord outer-planet inner-planet c)
	(process-events)
	(enqueue (image-item a-chord) chord-queue)
	(when (> (length (queue-contents chord-queue)) (value panel))
	  (dotimes (i 2)
	    (itemdelete c (dequeue chord-queue))))))))  

(defun main ()
  (with-ltk ()
    (let* ((state-fn (state-toggle))
	   (outer-planet (make-instance 'planet :diameter 25 :interval 10 :multiplier 200))
	   (inner-planet (make-instance 'planet :multiplier 125 :degree-increment 5))
	   (a-chord (make-instance 'chord))
	   (mf (make-instance 'frame))
	   (f (make-instance 'frame :master mf))
	   (c (make-instance 'canvas :width 800 :height 600 :master mf))
	   (continuep t)
	   (panel (make-instance 'control-panel :master f)))

      (pack mf)
      (pack f :side :right)
      (setf (cmd-slider panel) (lambda (event) (format t "~a~%" (value panel))))
      (setf (cmd-action-btn panel) (lambda () (funcall state-fn)))
      (focus c)
      (bind c "<KeyPress-q>" (lambda (event) (exit-wish)))
      (pack c :side :left)
      (pack panel :side :top)
      (draw-planet outer-planet c)
      (configure (image-item outer-planet) :fill :blue)
      (draw-planet inner-planet c)
      (configure (image-item inner-planet) :fill :red)

      (let ((my-ticker (ticker-generator))
	    (chord-queue (make-queue))
	    (max-q-length (value panel)))
	(loop while continuep do
	     (update)
	     (process-events))))))





