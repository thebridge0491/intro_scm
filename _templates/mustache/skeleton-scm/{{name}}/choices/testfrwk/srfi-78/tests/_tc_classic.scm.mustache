(check-set-mode! 'report-failed) ; 'off, 'summary, 'report-failed, 'report

(define (setUpClass) 
    (display (format "~a~%" "SetUpClass ...")))

(define (tearDownClass) 
    (display (format "~a~%" "... TearDownClass")))

(define (setUp) 
    (display (format "~a~%" "SetUp ...")))

(define (tearDown) 
    (display (format "~a~%" "... TearDown")))

(define (in_epsilon? tolerance a b)
	;(and (<= (- (abs a) tolerance) (abs b))
	;    (>= (+ (abs a) tolerance) (abs b)))
	(let ((delta (abs tolerance)))
		;(and (<= (- a delta) b) (>= (+ a delta) b))
		(and (not (< (+ a delta) b)) (not (< (+ b delta) a)))
		))

(define (bound_values minmax1 . rst)
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

(define epsilon 0.001)

(define (wrap_test start end test_fun)
    (start)
    (test_fun)
    (end))            

(define (suite)
    ;(display (format "~a~%-----~a~%" (make-string 40 #\-) (current-module)))
    (display (format "~a~%-----~a~%" (make-string 40 #\-) mod-sym))
    
    ; (list-ec (:list b '(2 11 20)) (:list n '(3 6 10)) (cons b n))
    (wrap_test setUp tearDown (lambda () (for-each (lambda (tup)
        (let* ((b (car tup)) (n (cadr tup)) (ans (expt b n)))
            (for-each (lambda (fn1)
                (check (in_epsilon? (* epsilon ans) (fn1 b n) ans) => #t))
                (list expt_i expt_lp)))
        ) (bound_values '(2.0 . 20.0) '(3.0 . 10.0)))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (tup)
        (let* ((num (car tup)) (ans (fold * 1 (iota num 1))))
            (for-each (lambda (fn1) (check (fn1 num) => ans))
                (list fact_i fact_lp)))
        ) (bound_values '(0 . 18)))))
    
    
    (check-report)
    )

