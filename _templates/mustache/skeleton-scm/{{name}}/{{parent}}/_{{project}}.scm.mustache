; tag::apidocs[]
;;; Echo invocation of function call
(define (echo_invoke func . args)
; end::apidocs[]
	(receive (ans . rst) (apply func args)
		(format "~a: ~a~%" (cons func args)
			(if (null? rst) ans (cons ans rst)))))

; tag::apidocs[]
;;; Make string from nested list of lists
(define (mkstring_nested init sep final lsts)
; end::apidocs[]
	(string-append
		;(fold (lambda (el acc)
		(fold-left (lambda (acc el)
				(if (list? el) 
					(format "~a~a" acc (mkstring_nested "" sep "\n" el))
					(format "~a~a~a" acc el sep)))
			init
			lsts)
		final))

; tag::apidocs[]
;;; Check numbers equality +/- tolerance
(define (in_epsilon? tolerance a b)
; end::apidocs[]
	;(and (<= (- (abs a) tolerance) (abs b))
	;    (>= (+ (abs a) tolerance) (abs b)))
	(let ((delta (abs tolerance)))
		;(and (<= (- a delta) b) (>= (+ a delta) b))
		(and (not (< (+ a delta) b)) (not (< (+ b delta) a)))
		))

; tag::apidocs[]
;;; Cartesian product of 2 lists
(define (cartesian_prod xs ys . rst)
; end::apidocs[]
	(let* ((revlst (reverse (cons xs (cons ys rst)))) (zs (car revlst))
			(ws (cadr revlst)) (lst (cddr revlst)))
		(let iter ((acc (list-ec (: w ws) (: z zs) (cons w z))) (nlst lst))
			(if (null? nlst)
				acc
				(iter (list-ec (: w (car nlst)) (: z acc) z) (cdr nlst))))))

; tag::apidocs[]
;;; Create analysis set of boundary(4n+1) values
(define (bound_values minmax1 . rst)
; end::apidocs[]
	(let* ((avg_vals (map (lambda (tup) (quotient (+ (car tup) (cdr tup)) 2))
				(cons minmax1 rst)))
			(axis_bounds (map (lambda (tup) (list (car tup) (+ (car tup) 1)
				(quotient (+ (car tup) (cdr tup)) 2) (- (cdr tup) 1) (cdr tup)))
				(cons minmax1 rst)))
			(bound_vals (concatenate (map (lambda (tup)
				(let ((ndx (car tup)) (axis (cadr tup)))
					(map (lambda (el) (append (take avg_vals ndx)
						(list el) (drop avg_vals (+ ndx 1)))) axis)))
				(zip (iota (length axis_bounds)) axis_bounds)))))
		(lset-union equal? (list) bound_vals)))

; tag::apidocs[]
;;; Create failure set of robustness((6n+1) - (4n+1)) values
(define (robust_failures minmax1 . rst)
; end::apidocs[]
	(let* ((avg_vals (map (lambda (tup) (quotient (+ (car tup) (cdr tup)) 2))
				(cons minmax1 rst)))
			(axis_robusts (map (lambda (tup) (list (- (car tup) 1)
				(+ (cdr tup) 1))) (cons minmax1 rst)))
			(failure_vals (concatenate (map (lambda (tup)
				(let ((ndx (car tup)) (axis (cadr tup)))
					(map (lambda (el) (append (take avg_vals ndx)
						(list el) (drop avg_vals (+ ndx 1)))) axis)))
				(zip (iota (length axis_robusts)) axis_robusts)))))
		(lset-union equal? (list) failure_vals)))

; tag::apidocs[]
;;; Create analysis set of worst-case(5**n) values
(define (worst_values minmax1 . rst)
; end::apidocs[]
	(let* ((axis_worsts (map (lambda (tup) (list (- (car tup) 1) (car tup)
				(quotient (+ (car tup) (cdr tup)) 2) (cdr tup) (+ (cdr tup) 1)))
				(cons minmax1 rst))))
		(if (> 2 (length (cons minmax1 rst)))
			(lset-union equal? (list) axis_worsts)
			(lset-union equal? (list) (apply cartesian_prod axis_worsts)))))

; tag::apidocs[]
;;; Create failure set of worst-case(5**n - 3**n) values
(define (worst_failures minmax1 . rst)
; end::apidocs[]
	(let* ((axis_passes (map (lambda (tup) (list (car tup)
				(quotient (+ (car tup) (cdr tup)) 2) (cdr tup)))
				(cons minmax1 rst)))
			(worst_passes (lset-union equal? (list)
				(if (> 2 (length (cons minmax1 rst)))
					axis_passes
					(apply cartesian_prod axis_passes)))))
		(lset-difference equal? (apply worst_values (cons minmax1 rst))
			worst_passes)))
