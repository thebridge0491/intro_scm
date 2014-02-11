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
    
    (wrap_test setUp tearDown (lambda () 
        (check (* 2 2) => 4)
        (check 4.0 (=> (lambda (a b) (Util:in_epsilon? (* epsilon a) a b))) 4.0)
        (check "Hello" => "Hello")
        (check (> 4 5) => #t)
        (check (Util:echo_invoke format "") => "(#<closure format>): \n")
        
        (check-ec (: i 5) (: j 4) (: k 3) (+ (* i j) (* i k)) (=> =)
            (* i (+ j k)))
        (do-ec (: i 5) (: j 4) (: k 3) (check (+ (* i j) (* i k)) (=> =)
            (* i (+ j k))))
        ))
    
    
    (check-report)
    )

