(defun read-sudoku-file (filename)
	(let ((lines '()) (in (open filename)))
  		(when in 
  			(loop for line = (read-line in nil)
  				while line do
  					(let ((first-character (char line 0)))
  					  (if (eq first-character #\Space)
  						(push 0 lines)
  						(push (digit-char-p first-character) lines))))
	  		(close in)
  			(reverse lines))))

(defun make-board (list)
	nil)

(defun pretty-print-board (list height width)
	 (print (repeat-string "-" 39)))

(defun repeat-string (string n)
	(if (= n 0)
		""
		(concatenate 'string string (repeat-string string (- n 1)))))

(defun divide-list (list length-of-sublist)
	(if (null list) 
		'()
		(append (list (up-to-nth length-of-sublist list)) 
				(divide-list (rest-nth length-of-sublist list) length-of-sublist))))

(defun up-to-nth (index lst)
	(if (= index 0)
		'()
		(cons (car lst) (up-to-nth (- index 1) (cdr lst)))))

(defun rest-nth (index lst)
	(if (= index 0)
		lst
		(rest-nth (- index 1) (cdr lst))))

(setf my-file "C:/Users/tcallahan/Dropbox/myDocuments/lisp/raw_sudoku.txt")


		
