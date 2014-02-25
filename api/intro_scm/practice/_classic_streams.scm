(cond-expand
	((library (srfi 41))
		(define (squares_strm)
			(stream-map (lambda (x) (* x x)) 
				(stream-iterate (lambda (e) (+ e 1)) 0)))

		(define (expts_strm b)
			(stream-map (lambda (n) (expt b n)) 
				(stream-iterate (lambda (e) (+ e 1)) 0)))


		(define (squares_map2)
			(stream-cons 0.0 (stream-map (lambda (e1 e2) (* e2 e2)) (squares_map2)
				(stream-iterate (lambda (e) (+ e 1)) 1))))
				;(stream-tabulate -1 (lambda (x) (+ x 1))))))

		(define (expts_map2 b)
			(stream-cons 1.0 (stream-map (lambda (e1 e2) (* e1 e2)) (expts_map2 b)
				(stream-iterate values b))))
				;(stream-tabulate -1 (lambda (x) b)))))

		(define (sums_map2 lo)
			(stream-cons lo (stream-map (lambda (e1 e2) (+ e1 e2 lo)) (sums_map2 lo)
				(stream-iterate (lambda (e) (+ e 1)) 1))))
				;(stream-tabulate -1 (lambda (x) (+ x 1))))))

		(define (facts_map2)
			(stream-cons 1 (stream-map * (facts_map2)
				(stream-iterate (lambda (e) (+ e 1)) 1))))
				;(stream-tabulate -1 (lambda (x) (+ x 1))))))

		(define (fibs_map2)
			(stream-cons 0 (stream-cons 1
				(stream-map + (fibs_map2) (stream-cdr (fibs_map2))))))

		(define (pascalrows_map2)
			(stream-cons '(1)
				(stream-map (lambda (row i) (map + (cons 0 row) (append row '(0))))
					(pascalrows_map2) (stream-iterate (lambda (e) (+ e 1)) 0))))
		)
	(gauche
		(define (squares_strm)
			(stream-map (lambda (x) (* x x)) (stream-tabulate -1 values)))

		(define (expts_strm b)
			(stream-map (lambda (n) (expt b n)) (stream-tabulate -1 values)))


		(define (squares_map2)
			(stream-cons 0.0 (stream-map (lambda (e1 e2) (* e2 e2)) (squares_map2)
				(stream-tabulate -1 (lambda (x) (+ x 1))))))

		(define (expts_map2 b)
			(stream-cons 1.0 (stream-map (lambda (e1 e2) (* e1 e2)) (expts_map2 b)
				(stream-tabulate -1 (lambda (x) b)))))

		(define (sums_map2 lo)
			(stream-cons lo (stream-map (lambda (e1 e2) (+ e1 e2 lo)) (sums_map2 lo)
				(stream-tabulate -1 (lambda (x) (+ x 1))))))

		(define (facts_map2)
			(stream-cons 1 (stream-map * (facts_map2)
				(stream-tabulate -1 (lambda (x) (+ x 1))))))

		(define (fibs_map2)
			(stream-cons 0 (stream-cons 1
				(stream-map + (fibs_map2) (stream-cdr (fibs_map2))))))

		(define (pascalrows_map2)
			(stream-cons '(1)
				(stream-map (lambda (row i) (map + (cons 0 row) (append row '(0))))
					(pascalrows_map2) (stream-tabulate -1 values))))
		)
	(else))

(cond-expand
	((library (srfi 41)))
	(gauche 
		(define (stream-unfolds gen seed)
			(stream-unfoldn gen seed 1)))
	(else))

;(define (unfold_strm mapper gen seed)
;    (stream-cons (mapper seed) (unfold_strm mapper gen (gen seed))))

(define (squares_su)
    #|
    (let ((func (lambda (seed) (* (cdr seed) (cdr seed)))))
        (unfold_strm func (lambda (seed) (cons (func seed) (+ (cdr seed) 1))) 
            (cons 0 0))))
    |#
    (stream-unfolds (lambda (seed) (let ((acc (car seed)) (ct (cdr seed)))
        (values (cons (* (+ 1 ct) (+ 1 ct)) (+ 1 ct)) (list acc))))
        (cons 0 0)))

(define (expts_su b)
    ;(unfold_strm car (lambda (seed) (cons (* (car seed) b) (+ (cdr seed) 1))) 
    ;    (cons 1 0)))
    (stream-unfolds (lambda (seed) (let ((acc (car seed)) (ct (cdr seed)))
        (values (cons (* acc b) (+ 1 ct)) (list acc)))) (cons 1 0)))

(define (sums_su lo)
    #|
    (let ((func (lambda (seed) (+ (car seed) (cdr seed)))))
        (unfold_strm func (lambda (seed) (cons (+ (car seed) (cdr seed))
            (+ (cdr seed) 1))) (cons 0 lo))))
    |#
    (stream-unfolds (lambda (seed) (let ((acc (car seed)) (ct (cdr seed)))
        (values (cons (+ acc ct) (+ 1 ct)) (list (+ acc ct)))))
        (cons 0 lo)))

(define (facts_su)
    #|
    (let ((func (lambda (seed) (* (car seed) (cdr seed)))))
        (unfold_strm car
            (lambda (seed) (cons (* (car seed) (cdr seed))
                (+ (cdr seed) 1))) (cons 1 1))))
    |#
    (stream-unfolds (lambda (seed) (let ((acc (car seed)) (ct (cdr seed)))
        (values (cons (* acc ct) (+ 1 ct)) (list acc)))) (cons 1 1)))

(define (fibs_su)
    #|
    (let ((func (lambda (seed) (+ (car seed) (cdr seed)))))
        (unfold_strm car
            (lambda (seed) (cons (cdr seed)
                (+ (car seed) (cdr seed)))) (cons 0 1))))
    |#
    (stream-unfolds (lambda (seed) (let ((s0 (car seed)) (s1 (cdr seed)))
        (values (cons s1 (+ s0 s1)) (list s0)))) (cons 0 1)))

(define (pascalrows_su)
    ;(let ((func (lambda (row) (map + (cons 0 row) (append row '(0))))))
    ;    (unfold_strm values func '(1))))
    (stream-unfolds (lambda (row)
        (values (map + (cons 0 row) (append row '(0))) (list row))) '(1)))



(define (squares_g)
    (generate (lambda (yield) (let iter ((i 0))
        (yield (* i i)) (iter (+ i 1))))))

(define (expts_g b)
    (generate (lambda (yield) (let iter ((i 0) (acc 1))
        (yield acc) (iter (+ i 1) (* acc b))))))

(define (sums_g lo)
    (generate (lambda (yield) (let iter ((i (+ lo 1)) (acc lo))
        (yield acc) (iter (+ i 1) (+ acc i))))))

(define (facts_g)
    (generate (lambda (yield) (let iter ((i 1) (acc 1))
        (yield acc) (iter (+ i 1) (* acc i))))))

(define (fibs_g)
    (generate (lambda (yield) (let iter ((s0 0) (s1 1))
        (yield s0) (iter s1 (+ s0 s1))))))

(define (pascalrows_g)
    (generate (lambda (yield) (let iter ((row '(1)))
        (yield row)
        (iter (map + (cons 0 row) (append row '(0))))))))


(define (squares_gu)
    (let ((func (lambda (seed) (* (cdr seed) (cdr seed)))))
        (gunfold (lambda (seed) #f) func
            (lambda (seed) (cons (func seed) (+ (cdr seed) 1))) (cons 0 0))))

(define (expts_gu b)
    (let ((func (lambda (seed) (* (car seed) b))))
        (gunfold (lambda (seed) #f) car
            (lambda (seed) (cons (func seed) (+ (cdr seed) 1))) (cons 1 0))))

(define (sums_gu lo)
    (let ((func (lambda (seed) (+ (car seed) (cdr seed)))))
        (gunfold (lambda (seed) #f) func
            (lambda (seed) (cons (func seed) (+ (cdr seed) 1))) (cons 0 lo))))

(define (facts_gu)
    (let ((func (lambda (seed) (* (car seed) (cdr seed)))))
        (gunfold (lambda (seed) #f) car
            (lambda (seed) (cons (func seed) (+ (cdr seed) 1))) (cons 1 1))))

(define (fibs_gu)
    (let ((func (lambda (seed) (+ (car seed) (cdr seed)))))
        (gunfold (lambda (seed) #f) car
            (lambda (seed) (cons (cdr seed) (func seed))) (cons 0 1))))

(define (pascalrows_gu)
    (let ((func (lambda (row) (map + (cons 0 row) (append row '(0))))))
        (gunfold (lambda (row) #f) values func '(1))))
