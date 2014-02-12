(check-set-mode! 'report-failed) ; 'off, 'summary, 'report-failed, 'report

(define (setUpClass) 
    (display (format "~a~%" "SetUpClass ...")))

(define (tearDownClass) 
    (display (format "~a~%" "... TearDownClass")))

(define (setUp) 
    (display (format "~a~%" "SetUp ...")))

(define (tearDown) 
    (display (format "~a~%" "... TearDown")))

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
                (check (Util:in_epsilon? (* epsilon ans) (fn1 b n) ans) => #t))
                (list expt_i expt_r)))
        ) (Util:bound_values '(2.0 . 20.0) '(3.0 . 10.0)))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (tup)
        (let* ((num (car tup)) (ans (fold * 1 (iota num 1))))
            (for-each (lambda (fn1) (check (fn1 num) => ans))
                (list fact_i fact_r)))
        ) (Util:bound_values '(0 . 18)))))
    
    
    (check-report)
    )
