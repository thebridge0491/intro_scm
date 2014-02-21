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
        (let* ((num (car tup)) (ans (expt num 2)))
            (for-each (lambda (fn1) (check (fn1 num) => ans))
                (list square_i square_r square_do square_f square_u
                    square_lc)))
        ) (Util:bound_values '(2 . 20)))))

    (wrap_test setUp tearDown (lambda () (for-each (lambda (tup)
        (let* ((b (car tup)) (n (cadr tup)) (ans (expt b n)))
            (for-each (lambda (fn1)
                (check (Util:in_epsilon? (* epsilon ans) (fn1 b n) ans) => #t))
                (list expt_i expt_r expt_do fast-expt_i fast-expt_r
                    fast-expt_do expt_f expt_u expt_lc)))
        ) (Util:bound_values '(0.0 . 20.0) '(3.0 . 10.0)))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (tup)
        (let* ((hi (car tup)) (lo (cdr tup))
                (ans (fold + lo (iota (- hi lo) (+ lo 1)))))
            (for-each (lambda (fn1) (check (fn1 hi lo) => ans))
                (list sum_to_i sum_to_r sum_to_do sum_to_f sum_to_u
                    sum_to_lc)))
        ) (list-ec (: hi '(15 0 150)) (: lo '(-20 0 -10)) (cons hi lo)))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (tup)
        (let* ((num (car tup)) (ans (fold * 1 (iota num 1))))
            (for-each (lambda (fn1) (check (fn1 num) => ans))
                (list fact_i fact_r fact_do fact_f fact_u fact_lc)))
        ) (Util:bound_values '(0 . 18)))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (tup)
        (let* ((num (car tup))
                (corp (lambda (e a) (cons (+ (car a) (cdr a)) (car a))))
                (ans (car (fold corp '(0 . 1) (iota num)))))
            (for-each (lambda (fn1) (check (fn1 num) => ans))
                (list fib_i fib_r fib_do fib_f fib_u fib_lc)))
        ) (Util:bound_values '(0 . 20)))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (tup)
        (let* ((rows (car tup))
                (corp (lambda (e a) (cons (map + (cons 0 (car a))
                    (append (car a) '(0))) a)))
                (ans (reverse (fold corp '((1)) (iota rows)))))
            (for-each (lambda (fn1) (check (fn1 rows) => ans))
                (list pascaltri_add pascaltri_mult pascaltri_do
                    pascaltri_f pascaltri_u pascaltri_lc)))
        ) (Util:bound_values '(0 . 10)))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (tup)
        (let* ((a (car tup)) (b (cdr tup))
                (ans_q (quotient a b)) (ans_r (remainder a b)))
            (check (quot_m a b) => ans_q)
            (check (rem_m a b) => ans_r))
        ) (list-ec (: a '(10 -10)) (: b '(3 -3)) (cons a b)))))
    #|
    (wrap_test setUp tearDown (lambda () (for-each (lambda (tup)
        (let* ((a (car tup)) (b (cdr tup))
                (ans_d (div a b)) (ans_m (mod a b)))
            (check (div_m a b) (=> (lambda (m n) (Util:in_epsilon?
                (* epsilon (abs n)) m n))) ans_d)
            (check (mod_m a b) (=> (lambda (m n) (Util:in_epsilon?
                (* epsilon (abs n)) m n))) ans_m))
        ) (list-ec (: a '(10.1 -10.2)) (: b '(3.5 -3.8)) (cons a b)))))
    |#
    (wrap_test setUp tearDown (lambda () (for-each (lambda (nums)
        (let* ((ans_g (apply gcd nums)) (ans_l (apply lcm nums)))
            (for-each (lambda (fn_tup)
                (let ((fn_g (car fn_tup)) (fn_l (cdr fn_tup)))
                    (check (apply fn_g nums) => ans_g)
                    (check (apply fn_l nums) => ans_l)))
                (list (cons gcd_i lcm_i) (cons gcd_r lcm_r)
                    (cons gcd_do lcm_do) (cons gcd_f lcm_f)
                    (cons gcd_u lcm_u) (cons gcd_lc lcm_lc))))
        ) '((24 16) (24 16 12) (24 16 32)))))
     
    (wrap_test setUp tearDown (lambda () (for-each (lambda (bs_num)
        (let* ((base (car bs_num)) (num (cdr bs_num))
                (corp (lambda (e a) (cons (quotient (car a) base)
                    (cons (remainder (car a) base) (cdr a)))))
                (ans (fold corp (list num)
                    (iota (+ (round (log num base)) 0)))))
            (for-each (lambda (fn1)
                (check (fn1 base num) => ans))
                (list base_expand_i base_expand_r base_expand_do
                    base_expand_f base_expand_u base_expand_lc)))
        ) '((2 . 11) (4 . 81) (3 . 243) (2 . 16)))))
     
    (wrap_test setUp tearDown (lambda () (for-each (lambda (bs_nums)
        (let* ((base (car bs_nums)) (nums (cadr bs_nums))
                (corp (lambda (i_e a)
                    (+ a (* (cadr i_e) (expt base (car i_e))))))
                (ans (fold corp 0
                    (zip (iota (length nums)) (reverse nums)))))
            (for-each (lambda (fn1) (check (fn1 base nums) => ans))
                (list base_to10_i base_to10_r base_to10_do
                    base_to10_f base_to10_u base_to10_lc)))
        ) '((2 (1 0 1 1)) (4 (1 1 0 1)) (3 (1 0 0 0 0 0)) (2 (1 0 0 0 0))))))
     
    (wrap_test setUp tearDown (lambda () (for-each (lambda (stop_start)
        (let* ((stop (car stop_start)) (start (cdr stop_start))
                (diff (- stop start))
                (ans_pos (iota (if (> diff 0) diff 0) start 1))
                (ans_neg (iota (if (< diff 0) diff 0) start -1)))
            (for-each (lambda (fn_tup)
                (let ((fn (car fn_tup)) (fnStep (cdr fn_tup)))
                    (check (fn start stop) => ans_pos)
                    (check (fnStep 1 start stop) => ans_pos)
                    (check (fnStep -1 start stop) => ans_neg)
                    ))
                (list (cons range_i range_step_i)
                    (cons range_r range_step_r)
                    (cons range_do range_step_do)
                    (cons range_f range_step_f)
                    (cons range_u range_step_u)
                    (cons range_lc range_step_lc))))
        ) '((2 . -1) (11 . -5) (20 . -9)))))
    
    (wrap_test setUp tearDown (lambda ()
        (for-each (lambda (fn1)
            (check ((fn1 square sqrt) 2) (=> (lambda (a b) (Util:in_epsilon?
                (* epsilon 2) a b))) 2)
            (check ((fn1 length iota) 5) => 5))
            (list compose_i compose_r compose_do compose_f compose_u
                compose_lc)))
        )
    
    
    (check-report)
    )
