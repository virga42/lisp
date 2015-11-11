(defparameter suits (list 'D 'H 'S 'C))

(defparameter face-values (list 1 2 3 4 5 6 7 8 9 10 11 12 13))

(defstruct card (value 1) (suit 1))

(defun new-deck-of-cards ()
	(reduce (lambda (first-list second-list) (append first-list second-list)) 
				(mapcar (lambda (s) 
					(mapcar (lambda (v) 
						(make-card :value v :suit s)) FACE-VALUES)) SUITS)))

(defparameter *draw-pile* (shuffle-deck (new-deck-of-cards)))

(defparameter *table* '())

(defparameter *score* '())

(defun get-hand (player))

(defun place-card-on-table (card))

(defun shuffle-deck (cards)
	(labels ((shuffle-deck-helper (cards index)
				(if (= index 0)
					cards
					(shuffle-deck-helper (swap-list cards (random index) index) (- index 1)))))
		(shuffle-deck-helper cards (- (length cards) 1))))

(defun swap (array index1 index2)
	(let ((value1 (aref array index1)))
		(update-array array index1 (aref array index2))
		(update-array array index2 value1))
		array)

(defun swap-list (list index1 index2)
	(array-to-list (swap (list-to-array list) index1 index2)))

(defun list-to-array (list)
	(make-array (length list) :initial-contents list))

(defun array-to-list (array)
	(coerce array 'list))

(defun update-array (array index value)
	(setf (aref array index) value))  

(defun game-overp (turn)
	(>= turn 10))

; (defun want-to-stealp

(defun deal-hands ()
	(list (deal-cards! 5) (deal-cards 5)))

(defun deal-cards! (n)
	(if (= n 0)
		'()
		(cons (pop *draw-pile*) (deal-cards! (- n 1)))))

(defun prompt-player ()
	(values 'STEAL '(2 2 4)))

(defun game ()
	(let ((hands (deal-hands)))
		(do ((turn 1 (1+ turn)))
			((game-overp turn))
			(turn))))
			
(defun turn ()
	(multiple-value-bind (action cards) (prompt-player)
		(cond 
			((eq action 'STEAL) (progn (steal-card cards) (turn)))
			((eq action 'MATCH) (capture-cards cards))
			((eq action 'ADD) (capture-cards cards))
			((eq action 'PLACE) (place-card-on-table cards))



; STEAL 2 2 4 5 6 7
; MATCH 7 J Q
; ADD 5 2 3 6
; PLACE J

