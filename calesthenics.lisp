(defpackage :calesthenics
  (:use :common-lisp :md5 :ltk)
  (:export )

(in-package :calesthenics)



(defun flatten-list (lst)
  (if (null lst)
    '()
    (if (atom (car lst))
      (cons (car lst) (flatten-list (cdr lst)))
      (append (flatten-list (car lst)) (flatten-list (cdr lst))))))))

;; create a set from a list
(defun make-set (lst)
  (cond 
    ((null lst) '())
    ((member (car lst) (cdr lst)) (make-set (cdr lst)))
    (t (cons (car lst) (make-set (cdr lst))))))

;; get the intersection of two lists
(defun set-intersection (l1 l2)
  (cond 
    ((null l1) '())
    ((member (car l1) l2) (cons (car l1) (set-intersection (cdr l1) l2)))
    (t (set-intersection (cdr l1) l2))))

;; get the union of two lists
(defun set-union (l1 l2)
  (make-set (append l1 l2)))

;; get the difference of two lists
(defun set-diff (l1 l2)
    (difference-helper (set-union l1 l2) (set-intersection l1 l2)))

(defun difference-helper (u i)
   (cond
     ((null u) '())
     ((member (car u) i) (difference-helper (cdr u) i))
     (t (cons (car u) (difference-helper (cdr u) i)))))

(setf a "all")
(setf b "these")
(setf c "problems")
(setf d '())

(cons a (cons (cons b (cons c d)) d))
; ("all" ("these" "problems"))

(cons a (cons (cons b d) (cons c d)))
; ("all" ("these") "problems")

(cons (cons a (cons b d)) (cons c d))
; (("all" "these") "problems")

(cons (cons a (cons b (cons c d))) d)
; (("all" "these" "problems"))

; xor function
(defun xor-func (a b)
  (cond 
    ((eq a b) nil)
    (t t)))

; (xor-func t t)
; (xor-func t nil)
; (xor-func nil t)
; (xor-func nil nil)

; decimal to binary
(defun decimal-binary (n)
  (do ((power 0 (1+ power)))
      ((> (expt 2 power) n) (cdr (decimal-binary-helper power n)))))

(defun decimal-binary-helper (p n)
  (cond
      ((< p 0) '())
      ((> (expt 2 p) n) (cons 0 (decimal-binary-helper (1- p) n)))
      (t (cons 1 (decimal-binary-helper (1- p) (- n (expt 2 p)))))))

; return decimal result of xOr'ng the binary representations of two decimal numbers
(defun binary-decimal (b)
  (cond
    ((null b) 0)
    ((= (car b) 1) (+ (expt 2 (1- (length b))) (binary-decimal (cdr b))))
    (t (binary-decimal (cdr b)))))

(defun pad-binary (b len)
  (cond 
    ((>= (length b) len) b)
    (t (cons 0 (pad-binary b (1- len))))))

(defun xor-binaries (b1 b2)
  (mapcar #'(lambda (b1 b2) (if (xor-func b1 b2) 1 0)) b1 b2))

(defun xor-binaries-helper (b1 b2)
  (cond 
    ((> (length b1) (length b2)) (xor-binaries b1 (pad-binary b2 (length b1))))
    ((< (length b1) (length b2)) (xor-binaries (pad-binary b1 (length b2)) b2))
    (t (xor-binaries b1 b2))))

(defun xor-decimals (d1 d2)
  (binary-decimal 
    (xor-binaries-helper (decimal-binary d1) 
                         (decimal-binary d2))))

; make a function called encode that takes a key and a string and 
; returns an encoded string

(defun make-circular-list (lst)
  (setf (cdr (last lst)) lst))

(defun stretch (key-phrase n)
  (if (= n 0)
    '()
    (cons (car key-phrase) (stretch (turn key-phrase) (1- n)))))

(defun turn (circ-lst)
  (cdr circ-lst))

(defun encode-msg (s key-phrase)
  (let ((lst-char (coerce s 'list))
        (lst-key (stretch 
                    (make-circular-list (coerce key-phrase 'list)) 
                    (length s))))
    (coerce (mapcar #'(lambda (c k) (code-char 
                                      (xor-decimals (char-code c) 
                                                   (char-code k))))
              lst-char
              lst-key) 'string)))

; decode the following phrase
(setf msg "cGRT@@ZiBDF@NCRtLM\\[FOYDZDF^FanCa@AFpV@^^aCAF^rrR^RYHMpE@Tm@tGLF[BBmay@MCYXgBOWLcSSLBmU[iKY_aF^a\\LcCSUgE@WBJ^UaFYa@AFpbewmatitXSUgLSQiZ_GaCBHZNcQAJBIay@MCYXgdaUDcD@@NCHZNcQrSFCE[DOIrVN_DPiMUGSFAaZLWpFNg]MUPcd[Bgy@Wiw_WatXRGDB^rSB]M]LGprvOTa]ZcDZDgCD@iTY@DCmSUGG__M^m@GBFTrlNCR_PcyrEHmO[]cGSOSmH@iW_rIF[DtHMIrQUHB[G@UBUNBOGiLVrIHZa@Fc@^@^mrAZP]SOg^@]McpHI^JMiWXWOg^IA]cX[RgHXQZcgZXgINtPLErBKBRQiZ_GSgHXQZccGRT@@ZiBCYDCmI]ZcDW@DEDFicc]aSE@@iWXWaUBNYiTY^MgODtLN@FXglUt]KQFaJBLQGWprrR^RYHMpE@TmDZEJWZUBCDP")

; write a function to read a file. when the function is run again it tells whether the file has changed

(defun a-counter ()
  (let ((counter 0))
    (lambda () (incf counter))))

 (setf my-func (a-counter))
 (funcall my-func)

 (defun file-diff (f)
  (let ((chk-sum (md5:md5sum-file f)))
    (lambda () 
      (let ((chk-sum2 (md5:md5sum-file f)))
        (if (equalp chk-sum chk-sum2)
          (print "same")
          (progn
            (setf chk-sum chk-sum2)
            (print "different")))))))

 (setf file-func (file-diff "key.lisp"))
 (funcall file-diff "key.lisp")

 ; make a button appear in ltk
 ;  each time you click it make it update its display text by 1

 (ql:quickload "ltk")

 (defun text-incrementor ()
  (let ((txt 0))
    (lambda () 
      (setf txt (1+ txt)))))

(defun button-exercise ()
  (with-ltk ()
  (let ((b (make-instance 'button 
                         :master nil
                         :text "Press me"
                         ))
         (button-txt (text-incrementor)))
    (setf (command b) (lambda () (setf (text b) button-txt)))
   (pack b))))

; make thirty buttons and have each one increment on their own

