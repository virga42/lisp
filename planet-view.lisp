;;; Planet view
;;;


(defwidget slider (frame)
  ((label :accessor label :initarg :label :initform "A slider")
   (lower-bound :accessor lower-bound :initarg :lower-bound :initform 10)
   (upper-bound :accessor upper-bound :initarg :upper-bound :initform 100)
   (command :accessor command :initform (lambda () )))
  ()
  (let ((slider-title (make-instance 'text 
				     :text (label self)
				     :master self))
	(slider (make-instance 'scale 
			       :orientation :horizontal
			       :from (lower-bound self)
			       :to (upper-bound self)
			       :master self))
	(slider-value (make-instance 'label :master self)))
    (setf (command slider) (lambda (e) (progn
					 (setf (text slider-value) e)
					 (funcall (command self) e))))
    (pack slider)
    (pack slider-label)))

;; (defun main ()
;;   (with-ltk ()
;;     (let ((c (make-instance 'canvas :height 600 :width 600))
;; 	  (panel (make-instance 'control-panel)))
;;       (setf (command panel) (lambda (e) (format t "~a~%" (value panel))))
;;       (pack c)
;;       (pack panel))))

(defun draw-planet (coords canvas)
    (make-oval canvas (first coords)
	       (second coords)
	       (third coords)
	       (fourth coords)))

(defun draw-chord (object-list canvas)
  (make-line canvas (mapcar #'centered-coords object-list)))
