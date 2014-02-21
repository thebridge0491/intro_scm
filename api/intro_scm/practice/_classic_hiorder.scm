(define (expt_f b n) (fold (lambda (e a) (* a b)) 1 (iota n 1)))

(define (square_f n) (expt_f n 2))

(define (numseq_math_f op hi lo)
    (let ((init (if (or (equal? + op) (equal? - op)) 0 1)))
        (fold (lambda (e a) (op a e)) init (iota (- (+ hi 1) lo) lo))))

(define (sum_to_f hi lo) (numseq_math_f + hi lo))

(define (fact_f n) (numseq_math_f * n 1))

(define (fib_f n)
    (car (fold (lambda (e s0_s1) (cons (+ (car s0_s1) (cdr s0_s1))
        (car s0_s1))) (cons 0 1) (iota n))))

(define (pascaltri_f rows)
    (reverse (fold (lambda (e a) (cons (map + (cons 0 (car a))
        (append (car a) '(0))) a)) '((1)) (iota rows))))

(define (gcd_f m . args) (fold (lambda (e a) (gcd a e)) m args))

(define (lcm_f m . args)
    (fold (lambda (e a) (* a (floor (/ e (gcd a e))))) m args))

(define (base_expand_f b n)
    (car (fold (lambda (e a)
        (cons (cons (modulo (cdr a) b) (car a)) (quotient (cdr a) b))) (cons '() n)
        (iota (+ (round (log n b)) 1)))))

(define (base_to10_f b lst)
    (let ((proc (lambda (el acc)
            (cons (+ 1 (car acc)) (+ (cdr acc) (* el (expt b (car acc))))))))
        (cdr (fold-right proc '(0 . 0) lst))))

(define (range_step_f step start stop)
    (let* ((cmp_op (if (> step 0) >= <=))
            (cnt (if (equal? >= cmp_op) (abs (- stop start))
                (abs (- start stop)))))
        (reverse (fold (lambda (e a)
            (if (not (cmp_op e stop))
                (cons e a)
                a))
            (list) (iota cnt start step)))))

(define (range_f start stop) (range_step_f 1 start stop))

(define (compose_f func . funcs)
    (fold (lambda (e a) (lambda (x) (a (e x)))) func funcs))


(define (expt_u b n)
    (car (unfold-right (lambda (tup) (> 0 (cdr tup))) car
        (lambda (tup) (cons (* (car tup) b) (- (cdr tup) 1)))
        (cons 1 n))))

(define (square_u n) (expt_u n 2))

(define (numseq_math_u op hi lo)
    (let ((init (if (or (equal? + op) (equal? - op)) 0 1)))
        (car (unfold-right (lambda (tup) (< (+ hi 1) (cdr tup))) car
            (lambda (tup) (cons (op (car tup) (cdr tup)) (+ (cdr tup) 1)))
            (cons init lo)))))

(define (sum_to_u hi lo) (numseq_math_u + hi lo))

(define (fact_u n) (numseq_math_u * n 1))

(define (fib_u n)
    (car (append (unfold-right (lambda (trip) (> 0 (cddr trip))) car
        (lambda (trip) (cons (cadr trip) (cons (+ (car trip) (cadr trip)) 
            (- (cddr trip) 1)))) (cons 0 (cons 1 n))) (cons 0 (cons 1 n)))))

(define (pascaltri_u rows)
    (reverse (unfold-right (lambda (tup) (> 0 (cdr tup))) car
        (lambda (tup) (cons (map + (cons 0 (car tup)) (append (car tup) '(0)))
            (- (cdr tup) 1))) (cons '(1) rows))))

#|
(define (euclid_u m n)
    (car (unfold-right (lambda (tup) (zero? (cdr tup))) cdr
        (lambda (tup) (cons (cdr tup) (remainder (car tup) (cdr tup))))
        (cons m n))))
|#
(define (gcd_u m . args)
    (let ((func (lambda (tup) (gcd (car tup) (cadr tup)))))
        (car (unfold-right (lambda (tup) (null? (cdr tup))) func
            (lambda (tup) (cons (func tup) (cddr tup))) (cons m args)))))

(define (lcm_u m . args)
    (let ((func (lambda (tup) (let ((m (car tup)) (n (cadr tup)))
            (/ (* m n) (gcd m n))))))
        (car (unfold-right (lambda (tup) (null? (cdr tup))) func
            (lambda (tup) (cons (func tup) (cddr tup))) (cons m args)))))

(define (base_expand_u b n)
    (unfold-right (lambda (x) (= 0 x)) (lambda (x) (remainder x b))
        (lambda (x) (quotient x b)) n))

(define (base_to10_u b lst)
    (let ((func (lambda (tup)
            (+ (car tup) (* (cadr tup) (expt b (length (cddr tup))))))))
        (car (unfold-right (lambda (tup) (null? (cdr tup))) func
            (lambda (tup) (cons (func tup) (cddr tup))) (cons 0 lst)))))

(define (range_step_u step start stop)
    (let ((pred_op (if (> step 0) >= <=)))
        ;(unfold (lambda (x) (pred_op x stop)) (lambda (e) e)
        ;    (lambda (x) (+ x 1)) start)))
        (reverse (unfold-right (lambda (x) (pred_op x stop)) (lambda (e) e)
            (lambda (x) (+ x 1)) start))))

(define (range_u start stop) (range_step_u 1 start stop))

(define (compose_u func . funcs)
    (let ((acc (lambda (tup) (lambda (x) ((cdr tup) ((caar tup) x))))))
        (car (unfold-right (lambda (tup) (null? (car tup))) acc
            (lambda (tup) (cons (cdar tup) (acc tup))) (cons funcs func)))))


            ;(list-ref (reverse (list-ec (: x (iota (+ n 1))) (expt b x))) 0)
(define (expt_lc b n) (fold-ec 1 (: x n) x (lambda (e a) (* a b))))

(define (square_lc n) (expt_lc n 2))

(define (numseq_math_lc op hi lo)
    (let ((init (if (or (equal? + op) (equal? - op)) 0 1)))
        (fold-ec init (: x lo (+ hi 1)) x (lambda (e a) (op e a)))))

            ;(sum-ec (: x lo (+ hi 1)) x)
(define (sum_to_lc hi lo) (numseq_math_lc + hi lo))

            ;(product-ec (: x 2 (+ n 1)) x)
(define (fact_lc n) (numseq_math_lc * n 1))

(define (fib_lc n)
    (car (fold-ec '(0 . 1) (: i (iota n)) i (lambda (e s0_s1)
        (cons (+ (car s0_s1) (cdr s0_s1)) (car s0_s1))))))

(define (pascaltri_lc rows)
    (reverse (fold-ec '((1)) (: i rows) i (lambda (e a)
        (cons (map + (cons 0 (car a)) (append (car a) '(0))) a)))))

(define (gcd_lc m . args) (fold-ec m (: n args) n (lambda (e a) (gcd a e))))

(define (lcm_lc m . args)
    (fold-ec m (: n args) n (lambda (e a) (* a (floor (/ e (gcd a e)))))))

(define (base_expand_lc b n)
    (car (fold-ec (cons '() n) (: i (iota (+ (round (log n b)) 1))) i
        (lambda (e a) (cons (cons (modulo (cdr a) b) (car a)) (quotient (cdr a) b))))))

(define (base_to10_lc b lst)
    ;(sum-ec (: i_n (zip (iota (length lst)) (reverse lst)))
    ;    (* (expt b (car i_n)) (cadr i_n)))
    (fold-ec 0 (: i_n (zip (iota (length lst)) (reverse lst)))
        (* (expt b (car i_n)) (cadr i_n)) (lambda (e a) (+ e a))))

(define (range_step_lc step start stop)
    (let* ((cmp_op (if (> step 0) >= <=)) (diff (- stop start))
            (cnt (if (> step 0) (if (> diff 0) diff 0)
                (if (< diff 0) diff 0))))
        (reverse (fold-ec (list) (: i (iota cnt start step)) i
            (lambda (e a) (cons e a))))))

(define (range_lc start stop) (range_step_lc 1 start stop))

(define (compose_lc func . funcs)
    (fold-ec func (: f funcs) f (lambda (e a) (lambda (x) (a (e x))))))
