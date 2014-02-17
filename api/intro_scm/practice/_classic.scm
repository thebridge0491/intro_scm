; tag::apidocs[]
;;; (iteratively) Compute b to the nth exponent
(define (expt_i b n)
; end::apidocs[]
    (let iter ((ct n) (acc 1))
        (if (> 1 ct)
            acc
            (iter (- ct 1) (* acc b)))))

(define (square_i n) (expt_i n 2))

; tag::apidocs[]
;;; (recursively) Compute b to the nth exponent
(define (expt_r b n)
; end::apidocs[]
    (if (> 1 n)
        1
        (* (expt_r b (- n 1)) b)))

(define (square_r n) (expt_r n 2))

; tag::apidocs[]
;;; (do loop) Compute b to the nth exponent
(define (expt_do b n)
; end::apidocs[]
    (do ([ct n (- ct 1)] [acc 1 (* acc b)]) ((> 1 ct) acc)))

(define (square_do n) (expt_do n 2))

(define (fast-expt_i b n)
    (let iter ((ct n) (acc 1))
        (cond ((> 1 ct) acc)
            ((even? ct) (iter (- ct 2) (* acc (square b))))
            (else (iter (- ct 1) (* acc b))))))

(define (fast-expt_r b n)
    (cond ((> 1 n) 1)
        ((even? n) (square (fast-expt_r b (/ n 2))))
        (else (* (fast-expt_r b (- n 1)) b))))

(define (fast-expt_do b n)
    (do ([ct n (if (even? ct) (- ct 2) (- ct 1))]
        [acc 1 (if (even? ct) (* acc b b) (* acc b))]) ((> 1 ct) acc)))

(define (numseq_math_i op hi lo)
    (let ((init (if (or (equal? + op) (equal? - op)) 0 1)))
        (let iter ((cur hi) (acc init))
            (if (> lo cur)
                acc
                (iter (- cur 1) (op acc cur))))))

(define (sum_to_i hi lo) (numseq_math_i + hi lo))

; tag::apidocs[]
;;; (iteratively) Compute factorial of n
(define (fact_i n)
; end::apidocs[]
	(Util:log_drains fact_i "entering" (list log_out log_prac))
	(numseq_math_i * n 1))

(define (numseq_math_r op hi lo)
    (let ((init (if (or (equal? + op) (equal? - op)) 0 1)))
        (define (helper hi lo)
            (if (> lo hi)
                init
                (op (helper (- hi 1) lo) hi)))
        (helper hi lo)))

(define (sum_to_r hi lo) (numseq_math_r + hi lo))

; tag::apidocs[]
;;; (recursively) Compute factorial of n
(define (fact_r n)
; end::apidocs[]
	(numseq_math_r * n 1))

(define (numseq_math_do op hi lo)
    (let ((init (if (or (equal? + op) (equal? - op)) 0 1)))
        (do ([cur hi (- cur 1)] [acc init (op acc cur)]) ((> lo cur) acc))))

(define (sum_to_do hi lo) (numseq_math_do + hi lo))

; tag::apidocs[]
;;; (do loop) Compute factorial of n
(define (fact_do n)
; end::apidocs[]
	(numseq_math_do * n 1))

(define (fib_i n)
    (let iter ((ct n) (s0 0) (s1 1))
        (if (> 1 ct)
            s0
            (iter (- ct 1) s1 (+ s1 s0)))))

(define (fib_r n)
    (if (or (= 0 n) (= 1 n))
        n
        (+ (fib_r (- n 2)) (fib_r (- n 1)))))

(define (fib_do n)
    (do ([ct n (- ct 1)] [s0 0 s1] [s1 1 (+ s1 s0)]) ((> 1 ct) s0)))

(define (pascaltri_r row col)
    (cond ((> col row) #\newline)
        ((or (= 0 col) (= row col)) 1)
        (else (+ (pascaltri_r (- row 1) (- col 1)) 
            (pascaltri_r (- row 1) col)))))

(define (pascaltri_add rows)
    (define (next-row xs)
        (map + (cons 0 xs) (append xs '(0))))
    (define (triangle xs rows)
        (if (> 1 rows)
            '()
            (cons xs (triangle (next-row xs) (- rows 1)))))
    (triangle '(1) (+ rows 1)))

(define (pascaltri_mult rows)
    (define (pascalrow r)
        (let iter ((col 1) (xs '(1)))
            (if (= r col)
                xs
                (iter (+ 1 col) (cons (* (car xs) (- r col) (/ col)) xs)))))
    (map pascalrow (iota (+ rows 1) 1)))

(define (pascaltri_do rows)
    (define (next-row xs)
        (map + (cons 0 xs) (append xs '(0))))
    (do ([i rows (- i 1)] [acc '((1)) (cons (next-row (car acc)) acc)])
        ((> 1 i) (reverse acc))))

(define (quot_rem a b)
    (if (and (integer? a) (integer? b))
        (let ((q (truncate (/ a b))))
            (values q (- a (* q b))))
        (error "not both integers" a b)))

(define (quot_m a b) (receive (q r) (quot_rem a b) q))

(define (rem_m a b) (receive (q r) (quot_rem a b) r))

(define (div_mod a b)
    (let ((q_floor (floor (/ a b))) (q_ceil (ceiling (/ a b))))
        (if (> b 0)
            (values q_floor (- a (* q_floor b)))
            (values q_ceil (- a (* q_ceil b))))))

(define (div_m a b) (receive (d m) (div_mod a b) d))

(define (mod_m a b) (receive (d m) (div_mod a b) m))

(define (euclid_i m n)
    (let iter ((a m) (b n))
        (if (= 0 b)
            a
            (iter b (remainder a b)))))

(define (gcd_i m . args)
    (let iter ((acc m) (rst args))
        (if (null? rst)
            acc
            (iter (euclid_i acc (car rst)) (cdr rst)))))

(define (lcm_i m . args)
    (let iter ((acc m) (rst args))
        (if (null? rst)
            acc
            (iter (/ (* acc (car rst)) (euclid_i acc (car rst))) (cdr rst)))))

(define (euclid_r m n)
    (if (= 0 n)
        m
        (euclid_r n (remainder m n))))

(define (gcd_r m . args)
    (if (null? args)
        m
        (apply gcd_r (euclid_r m (car args)) (cdr args))))

(define (lcm_r m . args)
    (if (null? args)
        m
        (apply lcm_r (/ (* m (car args)) (euclid_r m (car args)))
            (cdr args))))

(define (euclid_do m n)
    (do ([a m b] [b n (remainder a b)]) ((= b 0) a)))

(define (gcd_do m . args)
    (if (null? args)
        m
        (apply gcd_do (euclid_do m (car args)) (cdr args))))

(define (lcm_do m . args)
    (if (null? args)
        m
        (apply lcm_do (/ (* m (car args)) (euclid_do m (car args))) (cdr args))))

(define (base_expand_i b n)
    (let iter ((q n) (acc '()))
        (if (> 1 q)
            acc
            (iter (quotient q b) (cons (remainder q b) acc)))))

(define (base_expand_r b n)
    (if (> 1 n)
        '()
        (append (base_expand_r b (quotient n b)) (list (remainder n b)))))

(define (base_expand_do b n)
    (do ([q n (quotient q b)] [acc '() (cons (remainder q b) acc)])
        ((> 1 q) acc)))

(define (base_to10_i b lst)
    (let iter ((rst (reverse lst)) (ct 0) (acc 0))
        (if (null? rst)
            acc
            (iter (cdr rst) (+ ct 1) (+ acc (* (car rst) (expt b ct)))))))

(define (base_to10_r b lst)
    (if (null? lst)
        0
        (+ (* (car lst) (expt b (length (cdr lst))))
            (base_to10_r b (cdr lst)))))

(define (base_to10_do b lst)
    (do ([rst (reverse lst) (cdr rst)] [ct 0 (+ ct 1)]
        [acc 0 (+ acc (* (car rst) (expt b ct)))]) ((null? rst) acc)))

(define (range_step_i step start stop)
    (reverse (let iter ((cur start) (acc '()))
        (if ((if (> step 0) >= <=) cur stop)
            acc
            (iter (+ cur step) (cons cur acc))))))

;(define (range_i start stop) (range_step_i 1 start stop))
(define range_i (cute range_step_i 1 <> <>))

(define (range_step_r step start stop)
    (if ((if (> step 0) >= <=) start stop)
        '()
        (cons start (range_step_r step (+ start step) stop))))

(define (range_r start stop) (range_step_r 1 start stop))

(define (range_step_do step start stop)
    (do ([cur start (+ cur 1)] [acc '() (cons cur acc)])
        (((if (> step 0) >= <=) cur stop) (reverse acc))))

(define (range_do start stop) (range_step_do 1 start stop))

(define (compose_i func . funcs)
    (let iter ((acc func) (rst funcs))
        (if (null? rst)
            acc
            (iter (lambda (x) (acc ((car rst) x))) (cdr rst)))))

(define (compose_r . funcs)
    (if (null? funcs)
        (lambda (e) e)
        (lambda (x) ((car funcs) ((apply compose_r (cdr funcs)) x)))))

(define (compose_do func . funcs)
    (do ([rst funcs (cdr rst)] [acc func (lambda (x) (acc ((car rst) x)))])
        ((null? rst) acc)))
