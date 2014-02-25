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
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (cnt)
        (let* ((proc1 (lambda (e) (quotient 32 (expt 2 e))))
                (ans_id (reverse (fold (lambda (e a)
                    (cons e a)) '() (iota cnt))))
                (ans1 (reverse (fold (lambda (e a)
                    (cons (proc1 e) a)) '() (iota cnt)))))
            (for-each (lambda (fn1)
                (check (fn1 (lambda (e) e) cnt) => ans_id)
                (check (fn1 proc1 cnt) => ans1))
                (list tabulate_i tabulate_r tabulate_do tabulate_f
                    tabulate_u tabulate_lc)))
        ) (list 0 5 10))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (n)
        (let* ((xs (iota n)) (ans (length xs)))
            (for-each (lambda (fn1) (check (fn1 xs) => ans))
                (list length_i length_r length_do length_f length_u
                    length_lc)))
        ) (list 0 3 5 7))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((ans (list-ref l 3)))
            (for-each (lambda (fn1) (check (fn1 3 l) => ans))
                (list nth_i nth_r nth_do nth_f nth_u nth_lc)))
        ) (list lst revlst))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((pred (lambda (el) (= el 3))) (ans (list-index pred l)))
            (for-each (lambda (fn1) (check (fn1 pred l) => ans))
                (list index_i index_do index_f index_u index_lc)))
        ) (list lst revlst))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((pred (lambda (e) (= e 3))) (ans (find pred l)))
            (for-each (lambda (fn1) (check (fn1 pred l) => ans))
                (list find_i find_r find_do find_f find_u find_lc)))
        ) (list lst revlst))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((ans_min (apply min l)) (ans_max (apply max l)))
            (for-each (lambda (fn_tup)
                (let ((fn_min (car fn_tup)) (fn_max (cdr fn_tup)))
                    (check (apply fn_min l) => ans_min)
                    (check (apply fn_max l) => ans_max)))
                (list (cons min_i max_i) (cons min_r max_r)
                    (cons min_do max_do) (cons min_f max_f)
                    (cons min_u max_u) (cons min_lc max_lc))))
        ) (list lst revlst))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((ans (reverse l)))
            (for-each (lambda (fn1) (check (fn1 l) => ans))
                (list reverse_i reverse_r reverse_do reverse_f reverse_u
                    reverse_lc)))
        ) (list lst revlst))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((ans (list-copy l)))
            (for-each (lambda (fn1) (check (fn1 l) => ans))
                (list copy_i copy_r copy_do copy_f copy_u copy_lc)))
        ) (list lst revlst))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((ans_take (take l 3)) (ans_drop (drop l 3)))
            (for-each (lambda (fn_tup)
                (let ((fn_take (car fn_tup)) (fn_drop (cdr fn_tup)))
                    (check (fn_take 3 l) => ans_take)
                    (check (fn_drop 3 l) => ans_drop)))
                (list (cons take_i drop_i) (cons take_do drop_do)
                    (cons take_f drop_f) (cons take_u drop_u)
                    (cons take_lc drop_lc))))
        (let* ((ans (receive (t d) (split-at l 3) (list t d))))
            (for-each (lambda (fn1)
                    (check (receive (t d) (fn1 3 l) (list t d)) => ans))
                (list split-at_i split-at_do split-at_f split-at_u
                    split-at_lc)))
        ) (list lst revlst))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (fn_tup)
        (let ((fn_any (car fn_tup)) (fn_every (cdr fn_tup)))
            (check (fn_any integer? '(1 2 #\a)) => (any integer? '(1 2 #\a)))
            (check (fn_any null? '((1) (2 3) ())) =>
                (any null? '((1) (2 3) ())))
            (check (fn_every integer? '(1 2)) => (every integer? '(1 2)))
            (check (fn_every pair? '((1) (#\a))) =>
                (every pair? '((1) (#\a))))
            )
        ) (list (cons any_i every_i) (cons any_r every_r)
            (cons any_do every_do) (cons any_f every_f)
            (cons any_u every_u) (cons any_lc every_lc)))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((proc (lambda (el) (+ el 2))) (ans (map proc l)))
            (for-each (lambda (fn1) (check (fn1 proc l) => ans))
                (list map_i map_r map_do map_f map_u map_lc)))
        ) (list lst revlst))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((proc (lambda (el) (display (format "~a " el))))
                (ans (for-each proc l)))
            (for-each (lambda (fn1) (check (fn1 proc l) => ans))
                (list for-each_i for-each_r for-each_do for-each_f
                    for-each_u for-each_lc)))
        ) (list lst revlst))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((pred even?)
                (ans_filter (filter pred l)) (ans_remove (remove pred l)))
            (for-each (lambda (fn_tup)
                (let ((fn_filter (car fn_tup)) (fn_remove (cdr fn_tup)))
                    (check (fn_filter pred l) => ans_filter)
                    (check (fn_remove pred l) => ans_remove)))
                (list (cons filter_i remove_i) (cons filter_r remove_r)
                    (cons filter_do remove_do) (cons filter_f remove_f)
                    (cons filter_u remove_u) (cons filter_lc remove_lc))))
        (let* ((pred even?)
                (ans (receive (f r) (partition pred l) (list f r))))
            (for-each (lambda (fn1)
                    (check (receive (f r) (fn1 pred l) (list f r)) => ans))
                (list partition_i partition_r partition_do partition_f
                    partition_u partition_lc)))
        ) (list lst revlst))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((corp1 (lambda (e a) (+ a e))) (corp2 (lambda (e a) (- a e)))
                (ans1 (fold corp1 0 l)) (ans2 (fold corp2 0 l)))
            (for-each (lambda (fn1)
                (check (fn1 corp1 0 l) => ans1)
                (check (fn1 corp2 0 l) => ans2))
                (list fold-left_i fold-left_r fold-left_do)))
        ) (list lst revlst))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((proc1 (lambda (e a) (+ e a))) (proc2 (lambda (e a) (- e a)))
                (ans1 (fold-right proc1 0 l)) (ans2 (fold-right proc2 0 l)))
            (for-each (lambda (fn1)
                (check (fn1 proc1 0 l) => ans1)
                (check (fn1 proc2 0 l) => ans2))
                (list fold-right_i fold-right_r fold-right_do)))
        ) (list lst revlst))))
    
    (wrap_test setUp tearDown (lambda ()
        (let* ((seed1 '(0 . 10)) (pred1 (lambda (tup) (= (cdr tup) 0)))
                (func1 (lambda (tup) (car tup))) (gen1 (lambda (tup)
                    (cons (+ (car tup) 1) (- (cdr tup) (car tup)))))
                (seed2 '(0 . -10)) (gen2 (lambda (tup)
                    (cons (+ (car tup) 1) (+ (cdr tup) (car tup)))))
                (ans1 (unfold-right pred1 func1 gen1 seed1))
                (ans2 (unfold-right pred1 func1 gen2 seed2)))
            (for-each (lambda (fn1)
                (check (fn1 pred1 func1 gen1 seed1) => ans1)
                (check (fn1 pred1 func1 gen2 seed2) => ans2))
                (list unfold-right_i unfold-right_do)))
        ))
    
    (wrap_test setUp tearDown (lambda ()
        (let* ((seed1 '(0 . 10)) (pred1 (lambda (tup) (= (cdr tup) 0)))
                (func1 (lambda (tup) (car tup))) (gen1 (lambda (tup)
                    (cons (+ (car tup) 1) (- (cdr tup) (car tup)))))
                (seed2 '(0 . 2)) (gen2 (lambda (tup)
                    (cons (+ (car tup) 1) (- (car tup) (cdr tup)))))
                (ans1 (unfold pred1 func1 gen1 seed1))
                (ans2 (unfold pred1 func1 gen2 seed2)))
            (for-each (lambda (fn1)
                (check (fn1 pred1 func1 gen1 seed1) => ans1)
                (check (fn1 pred1 func1 gen2 seed2) => ans2))
                (list unfold-left_r unfold-left_do)))
        ))

    (cond-expand
		((library (srfi 95))
			(wrap_test setUp tearDown (lambda () (for-each (lambda (l)
				(for-each (lambda (fn1)
					(check (fn1 l <) => (sorted? l <))
					(check (fn1 l >) => (sorted? l >))
					(check (fn1 '((0 . #\a) (1 . #\b) (2 . #\c)) < car) =>
						(sorted? '((0 . #\a) (1 . #\b) (2 . #\c)) < car))
					(check (fn1 '((2 . #\c) (1 . #\b) (0 . #\a)) > car) =>
						(sorted? '((2 . #\c) (1 . #\b) (0 . #\a)) > car)))
					(list is-ordered_i? is-ordered_r? is-ordered_do?
						is-ordered_f? is-ordered_u?  is-ordered_lc?))
				) (list lst revlst)))))
        (else))

    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((l2 '(9 9 9 9)) (ans (append l l2)))
            (for-each (lambda (fn1) (check (fn1 l l2) => ans))
                (list append_i append_r append_do append_f append_u
                    append_lc)))
        ) (list lst revlst))))
    
    (wrap_test setUp tearDown (lambda ()
        (let* ((l2 '(9 9 9 9)))
            (for-each (lambda (fn1)
                (check (fn1 lst l2) => '(0 9 1 9 2 9 3 9 4))
                (check (fn1 revlst l2) => '(4 9 3 9 2 9 1 9 0)))
                (list interleave_i interleave_r interleave_do
                    interleave_f interleave_u interleave_lc)))
        ))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (l)
        (let* ((proc (lambda (e1 e2) (+ e1 e2 2))) (ans (map proc l l)))
            (for-each (lambda (fn1) (check (fn1 proc l l) => ans))
                (list map2_i map2_r map2_do map2_f map2_u map2_lc)))
        ) (list lst revlst))))
    
    (wrap_test setUp tearDown (lambda ()
        (for-each (lambda (fn1)
            (check (fn1 '(0 1 2) '(20 30)) => (zip '(0 1 2) '(20 30)))
            (check (fn1 '(0 (1 2)) '(20 30)) => (zip '(0 (1 2)) '(20 30))))
            (list zip_i zip_r zip_do zip_m zip_f zip_u zip_lc)))
        )
    
    (wrap_test setUp tearDown (lambda ()
        (let ((lst1 '((0 20) (1 30))) (lst2 '((0 20) (1 30) (#\a #\b)))
                (proc_recv2 (lambda (xs fn)
                    (receive (l1 l2) (fn xs) (list l1 l2)))))
            (for-each (lambda (fn1)
                (check (proc_recv2 lst1 fn1) => (proc_recv2 lst1 unzip2))
                (check (proc_recv2 lst2 fn1) => (proc_recv2 lst2 unzip2)))
                (list unzip_i unzip_do unzip_m unzip_f unzip_u unzip_lc)))
        ))
    
    (wrap_test setUp tearDown (lambda ()
        (for-each (lambda (fn1)
            (check (fn1 '((0 1 2) (20 30))) =>
                (concatenate '((0 1 2) (20 30))))
            (check (fn1 '((0 (1 2)) (20 (30)))) =>
                (concatenate '((0 (1 2)) (20 (30))))))
            (list concat_i concat_r concat_do concat_a concat_f concat_u
                concat_lc)))
        )

    
    (wrap_test setUp tearDown (lambda () (let (
            (pred_any2 (lambda (e1 e2) (or (null? e1) (null? e2))))
            (pred_any3 (lambda (e1 e2 e3) (or (null? e1) (null? e2) 
                (null? e3))))
            (pred_all2 (lambda (e1 e2) (and (pair? e1) (pair? e2))))
            (pred_all3 (lambda (e1 e2 e3) (and (pair? e1) (pair? e2) 
                (pair? e3)))))
        (for-each (lambda (fn_tup)
            (let ((fn_any (car fn_tup)) (fn_every (cdr fn_tup)))
                (check (fn_any pred_any2 '(1 2) '(5 ())) =>
                    (any pred_any2 '(1 2) '(5 ())))
                (check (fn_any pred_any3 '(1 2) '(3 4) '(5 ())) => 
                    (any pred_any3 '(1 2) '(3 4) '(5 ())))
                (check (fn_every pred_all2 '((1)) '((3 4))) => 
                    (every pred_all2 '((1)) '((3 4))))
                (check (fn_every pred_all3 '((1)) '((2)) '((3 4))) =>
                    (every pred_all3 '((1)) '((2)) '((3 4)))))
            ) (list (cons any_iv every_iv) (cons any_rv every_rv)
                (cons any_dov every_dov) (cons any_fv every_fv)
                (cons any_uv every_uv) (cons any_lcv every_lcv))))))
    
    (wrap_test setUp tearDown (lambda () (let (
            (proc2 (lambda (e1 e2) (list (+ e1 2) (+ e2 2))))
            (proc3 (lambda (e1 e2 e3) (list (+ e1 2) (+ e2 2) (+ e3 2)))))
        (for-each (lambda (fn1)
            (check (fn1 proc2 '(0 1) '(2 3)) => (map proc2 '(0 1) '(2 3)))
            (check (fn1 proc3 '(0 1) '(2 3) '(4 5)) => 
                (map proc3 '(0 1) '(2 3) '(4 5)))
        ) (list map_iv map_rv map_dov map_fv map_uv map_lcv)))))
    #|
    (wrap_test setUp tearDown (lambda () (let (
            (proc2 (lambda (el1 el2) (display (format "~a ~a" el1 el2))))
            (proc3 (lambda (el1 el2 el3) (display (format "~a ~a ~a" el1 el2 el3)))))
        (for-each (lambda (fn1)
            (check (fn1 proc2 '(0 1) '(2 3)) =>
                (for-each proc2 '(0 1) '(2 3)))
            (check (fn1 proc3 '(0 1) '(2 3) '(4 5)) =>
                (for-each proc3 '(0 1) '(2 3) '(4 5)))
        ) (list for-each_iv for-each_rv for-each_dov for-each_fv
            for-each_uv for-each_lcv)))))
    |#
    (wrap_test setUp tearDown (lambda () (let (
            (corp2 (lambda (e1 e2 acc) (+ acc e1 e2)))
            (corp3 (lambda (e1 e2 e3 acc) (- acc e1 e2 e3))))
        (for-each (lambda (fn1)
            (check (fn1 corp2 0 '(0 1 2) '(2 3)) => 
                (fold corp2 0 '(0 1 2) '(2 3)))
            (check (fn1 corp3 0 '(0 1 2) '(2 3) '(3 4)) => 
                (fold corp3 0 '(0 1 2) '(2 3) '(3 4)))
        ) (list fold-left_iv fold-left_rv fold-left_dov)))))
    
    (wrap_test setUp tearDown (lambda () (let (
            (proc2 (lambda (e1 e2 acc) (+ (+ e1 e2) acc)))
            (proc3 (lambda (e1 e2 e3 acc) (- (+ e1 e2 e3) acc))))
        (for-each (lambda (fn1)
            (check (fn1 proc2 0 '(0 1 2) '(2 3)) => 
                (fold-right proc2 0 '(0 1 2) '(2 3)))
            (check (fn1 proc3 0 '(0 1 2) '(2 3) '(3 4)) => 
                (fold-right proc3 0 '(0 1 2) '(2 3) '(3 4)))
        ) (list fold-right_iv fold-right_rv fold-right_dov)))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (fn1)
        (check (fn1 '(1) '(2 3) '(4)) => (append '(1) '(2 3) '(4)))
        (check (fn1 '(1) '(2 3) '(4) '(5 6)) =>
            (append '(1) '(2 3) '(4) '(5 6)))
        ) (list append_iv append_rv append_dov append_fv append_uv
            append_lcv))))
    
    (wrap_test setUp tearDown (lambda () (for-each (lambda (fn1)
        (check (fn1 '(0 1 2) '(20 30) '(#\a #\b)) => 
            (zip '(0 1 2) '(20 30) '(#\a #\b)))
        (check (fn1 '(0 1 2) '(20 30) '(#\a #\b) '("Z" "Y")) => 
            (zip '(0 1 2) '(20 30) '(#\a #\b) '("Z" "Y")))
        ) (list zip_iv zip_rv zip_dov zip_fv zip_uv zip_lcv))))
    
        
    (check-report)
    ))
