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
    (let ((lst (iota 5)) (revlst (reverse (iota 5))))
    ;(display (format "~a~%-----~a~%" (make-string 40 #\-) (current-module)))
    (display (format "~a~%-----~a~%" (make-string 40 #\-) mod-sym))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((pred (lambda (el) (= el 3))) (ans (list-index pred l)))
            (for-each (lambda (fn1) (check (fn1 pred l) => ans))
                (list index_i index_do)))
        ) (list lst revlst))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((pred (lambda (el) (= el 3))) (ans (reverse l)))
            (for-each (lambda (fn1) (check (fn1 l) => ans))
                (list reverse_i reverse_r reverse_do)))
        ) (list lst revlst))))
    
    
    (check-report)
    ))

