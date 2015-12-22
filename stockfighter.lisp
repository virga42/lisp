(defvar persistent-key "{enter-key-here}")
(defvar first-time-loadp t)
(if first-time-loadp
	(progn
		(load "/home/alex/quicklisp/setup.lisp")
		(load "/home/alex/Documents/lisp/my_key.lisp")
		(sleep 1)
		(ql:quickload :drakma)
		(ql:quickload :cl-json)
		(ql:quickload :cl-interpol)
		(ql:quickload :local-time)
		(cl-interpol:ENABLE-INTERPOL-SYNTAX)
		(sleep 1)
		(setf first-time-loadp nil)))



(defvar *debug* nil)
(defvar base-url "https://api.stockfighter.io/ob/api")
(defvar heart-beat-url (concatenate 'string base-url "/heartbeat"))


(if first-time-loadp
	(progn
		(load "/home/alex/quicklisp/setup.lisp")
		(sleep .1)
		(ql:quickload :drakma)
		(ql:quickload :cl-json)
		(ql:quickload :cl-interpol)
		(cl-interpol:ENABLE-INTERPOL-SYNTAX)
		(setf first-time-loadp nil)))

(defun api-get (request)
	(if *debug* (print request))
	(cl-json:decode-json-from-source (drakma:http-request request
		:additional-headers (list (cons "X-Starfighter-Authorization"  persistent-key))
		:method :get
		:want-stream t)))

(defun api-post (url content)
	(cl-json:decode-json-from-source 
		(drakma:http-request url
			:additional-headers (list (cons "X-Starfighter-Authorization"  persistent-key))
			:method :post
			:content content
			:want-stream t)))

(defun api-delete (url content)
	(if *debug* (print url))
		(cl-json:decode-json-from-source 
			(drakma:http-request url
				:additional-headers (list (cons "X-Starfighter-Authorization"  persistent-key))
				:method :delete
				:content content
				:want-stream t)))

(defun new-order (account venue stock price quantity direction order-type)
	(list 
		`(account . ,account) 
		`(venue . ,venue)
		`(stock . ,stock)
		`(price . ,price)
		`(qty . ,quantity)
		`(direction . ,direction)
		`(orderType . ,order-type)))

(defun get-value-in-list (field-name order)
	(cdr (assoc field-name order)))

(defun new-buy-order (account venue stock price quantity order-type)
	(new-order account venue stock price quantity "buy" order-type))

(defun place-order (order)
	(let* ((venue (get-value-in-list 'venue order))
					(stock (get-value-in-list 'stock order))
					(order-request (cl-json:encode-json-to-string order))
					(content order-request)
				  (url #?"https://api.stockfighter.io/ob/api/venues/${venue}/stocks/${stock}/orders"))
	(api-post url content)))


(defun get-stocks-available (venue)
	(let ((request #?"https://api.stockfighter.io/ob/api/venues/${venue}/stocks"))
		(api-get request)))

(defun get-order-book (venue stock)
	(let ((request #?"https://api.stockfighter.io/ob/api/venues/${venue}/stocks/${stock}"))
		(api-get request)))

(defun get-quote (venue stock)
	(let ((request #?"https://api.stockfighter.io/ob/api/venues/${venue}/stocks/${stock}/quote"))
		(api-get request)))

(defun get-id-in-order (order-response)
	(get-value-in-list ':ID order-response))

(defun get-stock-in-order (order-response)
	(get-value-in-list ':SYMBOL order-response))

(defun get-venue-in-order (order-response)
	(get-value-in-list ':venue order-response))

(defun get-order-status (order-response)
	(let* ((order-id (get-id-in-order order-response))
				 (venue (get-venue-in-order order-response))
				 (stock (get-stock-in-order order-response))
				 (url #?"https://api.stockfighter.io/ob/api/venues/${venue}/stocks/${stock}/orders/${order-id}"))
		(api-get url)))

(defun delete-order (order-response)
	(let* ((order-id (get-id-in-order order-response))
				 (venue (get-venue-in-order order-response))
				 (stock (get-stock-in-order order-response))
				 (order-response (cl-json:encode-json-to-string order-response))
				 (url #?"https://api.stockfighter.io/ob/api/venues/${venue}/stocks/${stock}/orders/${order-id}"))
		(api-delete url order-response)))

(defun get-status-on-all-orders (venue account)
	(let ((url #?"https://api.stockfighter.io/ob/api/venues/${venue}/accounts/${account}/orders"))
		(api-get url)))

(defun get-status-on-all-orders-for-stock (venue account stock)
	(let ((url #?"https://api.stockfighter.io/ob/api/venues/${venue}/accounts/${account}/stocks/${stock}/orders"))
		(api-get url)))

(defun get-asks-from-order-book (order-book)
	(get-value-in-list ':ASKS order-book))

(defun get-bids-from-order-book (order-book)
	(get-value-in-list ':BIDS order-book))

(defun get-price-from-order (order)
	(let ((v (get-value-in-list ':PRICE order)))
		(if v v 0)))

(defun get-quantity-from-order (order)
	(let ((v (get-value-in-list ':QTY order)))
		(if v v 0)))

(defun make-order-for-cheapest-in-order-book (order-book account)
	(let* ((asks (get-asks-from-order-book order-book))
				 (venue (get-venue-in-order order-book))
				 (stock (get-stock-in-order order-book)))
		(when asks
			(let ((price (get-price-from-order (first asks)))
				 	  (quantity   (get-quantity-from-order (first asks))))
				(new-buy-order account venue stock price quantity "limit")))))

(defun make-order-for-cheapest-from-quote (stock-quote account)
	(let ((venue (get-venue-in-order stock-quote))
			  (stock (get-stock-in-order stock-quote))
			  (price (get-value-in-list ':ASK stock-quote))
			  (quantity (get-value-in-list ':ASK-SIZE stock-quote)))
			(new-buy-order account venue stock price quantity "limit")))

(defun update-order (order field-name new-value)
	(rplacd (assoc field-name order) new-value))

(defun order-openp (order-response)
	(eq T (get-value-in-list ':OPEN order-response)))

(defun get-time-from-order (order-response)
	(local-time:parse-timestring (get-value-in-list ':TS order-response)))

(defun get-current-orders (venue account stock)
	(get-value-in-list ':ORDERS 
		(get-status-on-all-orders-for-stock venue account stock)))

(defun get-total-orders-filled (venue account stock)
	(loop for order in (get-current-orders venue account stock)
		sum (get-value-in-list ':TOTAL-FILLED order)))

(defun get-total-orders-requested (venue account stock)
	(loop for order in (get-current-orders venue account stock)
		sum (get-quantity-from-order order)))

(defun current-time ()
	(local-time:now))
	
(defun time-elapsedp (timestamp timeout-in-seconds)
	(let ((max-time (local-time:adjust-timestamp timestamp (offset :sec timeout-in-seconds))))
		(local-time:timestamp> (local-time:now) max-time)))

(defun monitor-order (order-response)
	(let ((order-timeout 5))
		(if (and (order-openp order-response)
						 (time-elapsedp (get-time-from-order order-response) 5))
			(delete-order order-response))))

(defun make-next-order (venue stock account should-buyp)
	(let* ((order (make-order-for-cheapest-from-quote (get-quote venue stock) account)))
		(when (and order (funcall should-buyp order))
			(do ((order-response (place-order order)))
					((not (order-openp order-response)))					
					(monitor-order order-response)
					(sleep .5)
					(setf order-response (get-order-status order-response))
					(pretty-print-order-response order-response)
					))))

(defun stock-to-string (order-response)
	(let ((stock (get-stock-in-order order-response))
			(quantity (get-quantity-from-order order-response))
			(price (get-price-from-order order-response)))			
			#?"${stock}: ${quantity}@${price}"))


(defun pretty-print-order-response (order-response)		
		(let ((stock-string (stock-to-string order-response)))
			(if (order-openp order-response)
				(print #?"${stock-string}....OPEN")
				(print #?"${stock-string}....CLOSED"))))

(defun order-filledp (order-response)
	(> (get-value-in-list ':TOTAL-FILLED order-response) 0))

(defun begin-buy (order fn-buy fn-on-buy fun-on-decline)
	(let* ((pause-interval .2)
				 (max-retries 5)
		 		 (order-response (funcall fn-buy order)))
		(pretty-print-order-response order-response)
		(when (order-openp order-response)
			(do ((tries 1 (+ i 1))
					 (order-response (get-order-status order-response) (get-order-status order-response)))
				  ((> tries max-retries) (not (order-openp order-response)))
				  (pretty-print-order-response order-response)
				  (sleep pause-interval))
			(if (order-filledp order-response)
				(funcall fn-on-buy order-response)
				(funcall fun-on-decline order-response)))))


(defun print-filled-order (order-response)
	(let ((stock-string (stock-to-string order-response)))
		(print #?"${stock-string}....FILLED")))

(defun print-unfilled-order (order-response)
	(let ((stock-string (stock-to-string order-response)))
		(print #?"${stock-string}....UNFILLED")))


(defun generate-buy-function (should-buyp)
	(lambda (order) (if (funcall should-buyp order)
										  (place-order order))))

(defun generate-orders-function (venue stock account)
	(lambda () 
		(make-order-for-cheapest-from-quote (get-quote venue stock) account)))


(defun difference-between-request-and-purchased (venue account stock)
	(- (get-total-orders-requested venue account stock) 
		 (get-total-orders-filled venue account stock)))

		
; (defun filter-on-open-orders (orders)
; 	(remove-if (

; (defun calculate-quantity-required (goal-quantity venue account stock)
; 	(if (= goal-quantity (get-total-orders-filled venue account stock))
; 		0


(defun beat-level2 (venue account stock)
	(let ((total-quantity-to-buy 1000)
				(max-price 10000))				
		(do ((qty-remaining (- total-quantity-to-buy (difference-between-request-and-purchased venue account stock))
												(- total-quantity-to-buy (difference-between-request-and-purchased venue account stock))))
				((<= qty-remaining 0))
				(print #?"Remaining: ${qty-remaining}")
				(let ((order (make-order-for-cheapest-from-quote (get-quote venue stock) account)))
					(if (> (get-quantity-from-order order) qty-remaining)
							(update-order order ':qty qty-remaining))
					(begin-buy 
						order 
						(generate-buy-function
								(lambda (o) (>= max-price (get-price-from-order o))))
						'print-filled-order
						'print-unfilled-order)
					(sleep .5)))))




(setf test-account "EXB123456")
(setf test-venue "TESTEX")
(setf test-stock "FOOBAR")
(setf test-shares-to-buy 100000)
(setf test-order-type "limit")
(setf test-price 100)

(setf account "SMH87162178")
(setf venue "KCTEX")
(setf stock "MPI")
(setf shares-to-buy 100000)
(setf order-type "limit")
(setf price 4600)





; (dotimes (n 100)
; 	(let ((order (new-buy-order account venue stock price shares-to-buy order-type)))
; 		(place-order order)
; 		(print n)
; 		(sleep .2)))

; (dolist (order (get-value-in-list ':ORDERS (get-status-on-all-orders-for-stock venue account stock)))
; 	(delete-order order)
; 	(print "*"))



; (setf my-response (cl-json:decode-json-from-source (drakma:http-request "https://api.stockfighter.io/ob/api/venues/TESTEX/heartbeat"
; 	:method :get
; 	:want-stream t)))

