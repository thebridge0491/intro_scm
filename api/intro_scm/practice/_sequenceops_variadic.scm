(define (any_every_iv pred lst . lsts)
    (let iter ((rst (cons lst lsts)) (acc (cons #f #t)))
        (if (any null? rst)
            acc
            (iter (map cdr rst) (cons (or (car acc) (apply pred (map car rst))) (and (cdr acc) (apply pred (map car rst))))))))

(define (any_iv pred lst . lsts) (car (apply any_every_iv pred lst lsts)))

(define (every_iv pred lst . lsts) (cdr (apply any_every_iv pred lst lsts)))

(define (any_every_rv pred lst . lsts)
    (let ((rst (cons lst lsts)))
        (if (any null? rst)
            (cons #f #t)
            (cons (or (apply pred (map car rst)) (car (apply any_every_rv pred (map cdr rst)))) (and (apply pred (map car rst)) (cdr (apply any_every_rv pred (map cdr rst))))))))

(define (any_rv pred lst . lsts) (car (apply any_every_rv pred lst lsts)))

(define (every_rv pred lst . lsts) (cdr (apply any_every_rv pred lst lsts)))

(define (any_every_dov pred lst . lsts)
    (do ([rst (cons lst lsts) (map cdr rst)] [acc (cons #f #t)
            (cons (or (apply pred (map car rst)) (car (apply any_every_dov pred (map cdr rst)))) (and (apply pred (map car rst)) (cdr (apply any_every_dov pred (map cdr rst)))))])
        ((any null? rst) acc)))

(define (any_dov pred lst . lsts) (car (apply any_every_dov pred lst lsts)))

(define (every_dov pred lst . lsts) (cdr (apply any_every_dov pred lst lsts)))

(define (map_iv proc lst . lsts)
    (let iter ((rst (cons lst lsts)) (acc '()))
        (if (any null? rst)
            (reverse acc)
            (iter (map cdr rst) (cons (apply proc (map car rst)) acc)))))

(define (map_rv proc lst . lsts)
    (let ((rst (cons lst lsts)))
        (if (any null? rst)
            '()
            (cons (apply proc (map car rst))
                (apply map_rv proc (map cdr rst))))))

(define (map_dov proc lst . lsts)
    (do ([rst (cons lst lsts) (map cdr rst)]
        [acc '() (cons (apply proc (map car rst)) acc)])
        ((any null? rst) (reverse acc))))
    
(define (for-each_iv proc lst . lsts)
    (let iter ((rst (cons lst lsts)))
        (if (any null? (map cdr rst))
            (apply proc (map car rst))
            (begin
                (apply proc (map car rst))
                (iter (map cdr rst))))))

(define (for-each_rv proc lst . lsts)
    (let ((rst (cons lst lsts)))
        (if (any null? rst)
            '()
            (begin
                (apply proc (map car rst))
                (apply for-each_rv proc (map cdr rst))))))

(define (for-each_dov proc lst . lsts)
    (do ([rst (cons lst lsts) (map cdr rst)]
        [acc '() (apply proc (map car rst))]) ((any null? rst) acc)))

(define (fold-left_iv corp init lst . lsts)
    (let iter ((rst (cons lst lsts)) (acc init))
        (if (any null? rst)
            acc
            (iter (map cdr rst) (apply corp (append (map car rst) (list acc)))))))

(define (fold-left_rv corp init lst . lsts)
    (let ((rst (cons lst lsts)))
        (if (any null? rst)
            init
            (apply fold-left_rv corp (apply corp (append (map car rst) (list init)))
                (map cdr rst)))))

(define (fold-left_dov corp init lst . lsts)
    (do ([rst (cons lst lsts) (map cdr rst)]
        [acc init (apply corp (append (map car rst) (list acc)))]) ((any null? rst) acc)))

(define (fold-right_iv proc init lst . lsts)
    (let iter ((rst (reverse (apply zip lst lsts))) (acc init))
        (if (null? rst)
            acc
            (iter (cdr rst) (apply proc (append (car rst) (list acc)))))))

(define (fold-right_rv proc init lst . lsts)
    (let ((rst (cons lst lsts)))
        (if (any null? rst)
            init
            (apply proc (append (map car rst)
                (list (apply fold-right_rv proc init (map cdr rst))))))))

(define (fold-right_dov proc init lst . lsts)
    (do ([rst (reverse (apply zip lst lsts)) (cdr rst)]
        [acc init (apply proc (append (car rst) (list acc)))])
        ((null? rst) acc)))

(define (append_iv lst . lsts)
    (let iter ((rst (reverse (cons lst lsts))) (acc '()))
        (if (null? rst)
            acc
            (iter (cdr rst) (append (car rst) acc)))))

(define (append_rv lst . lsts)
    (cond ((null? lsts) lst)
        ((and (list? lsts) (= 2 (length (apply list lst lsts))))
            (apply append lst lsts))
        ((null? lst) (apply append_rv (car lsts) (cdr lsts)))
        (else (cons (car lst) (apply append_rv (cdr lst) lsts)))))

(define (append_dov lst . lsts)
    (do ([rst (reverse (cons lst lsts)) (cdr rst)]
        [acc '() (append (car rst) acc)]) ((null? rst) acc)))

(define (zip_iv lst . lsts) (apply map_iv list lst lsts))

(define (zip_rv lst . lsts) (apply map_rv list lst lsts))

(define (zip_dov lst . lsts) (apply map_dov list lst lsts))


(define (any_every_fv pred lst . lsts)
    (fold (lambda (e a)
        (cons (or (car a) (apply pred e)) (and (cdr a) (apply pred e))))
        (cons #f #t) (apply zip lst lsts)))

(define (any_fv pred lst . lsts) (car (apply any_every_fv pred lst lsts)))

(define (every_fv pred lst . lsts) (cdr (apply any_every_fv pred lst lsts)))

(define (map_fv proc lst . lsts)
    (fold-right (lambda (e a) (cons (apply proc e) a)) '()
        (apply zip lst lsts)))

(define (for-each_fv proc lst . lsts)
    (fold (lambda (e a) (apply proc e)) '()
        (apply zip lst lsts)))


(define (append_fv lst . lsts)
    (fold-right (lambda (e a) (append e a)) '() (cons lst lsts)))

(define (zip_fv lst . lsts) (apply map_fv list lst lsts))


(define (any_every_uv pred lst . lsts)
    (let ((func (lambda (tup) 
            (cons (or (caar tup) (apply pred (map car (cdr tup)))) 
            (and (cdar tup) (apply pred (map car (cdr tup))))))))
        (car (unfold-right (lambda (tup) (any null? (cdr tup)))
            func (lambda (tup) (cons (func tup) (map cdr (cdr tup))))
            (cons (cons #f #t) (cons lst lsts))))))

(define (any_uv pred lst . lsts) (car (apply any_every_uv pred lst lsts)))

(define (every_uv pred lst . lsts) (cdr (apply any_every_uv pred lst lsts)))

(define (map_uv proc lst . lsts)
    (let ((func (lambda (tup) (cons (apply proc (map car (cdr tup)))
            (car tup)))))
        (reverse (car (unfold-right
            (lambda (tup) (any null? (cdr tup))) func
            (lambda (tup) (cons (func tup) (map cdr (cdr tup))))
            (cons '() (cons lst lsts)))))))

(define (for-each_uv proc lst . lsts)
    (let ((func (lambda (tup) (apply proc (map car (cdr tup))))))
        (car (unfold-right
            (lambda (tup) (any null? (cdr tup))) func
            (lambda (tup) (cons (func tup) (map cdr (cdr tup))))
            (cons '() (cons lst lsts))))))


(define (append_uv lst . lsts)
    (let ((func (lambda (tup) (append (cadr tup) (car tup)))))
        (car (unfold-right
            (lambda (tup) (or (null? (cdr tup)) (any null? (cdr tup)))) func
            (lambda (tup) (cons (func tup) (cddr tup)))
            (cons '() (reverse (cons lst lsts)))))))

(define (zip_uv lst . lsts) (apply map_uv list lst lsts))


(define (any_every_lcv pred lst . lsts)
    (fold-ec (cons #f #t) (: heads (apply zip lst lsts)) heads
            (lambda (els a) (cons (or (car a) (apply pred els)) (and (cdr a) (apply pred els))))))

(define (any_lcv pred lst . lsts) (car (apply any_every_lcv pred lst lsts)))

(define (every_lcv pred lst . lsts) (cdr (apply any_every_lcv pred lst lsts)))

(define (map_lcv proc lst . lsts)
    (fold-ec '() (: heads (reverse (apply zip lst lsts))) heads
            (lambda (e a) (cons (apply proc e) a))))

(define (for-each_lcv proc lst . lsts)
    (fold-ec '() (: heads (apply zip lst lsts)) heads
            (lambda (e a) (apply proc e))))


(define (append_lcv lst . lsts)
    (fold-ec '() (: xs (reverse (cons lst lsts))) xs
            (lambda (e a) (append e a))))

(define (zip_lcv lst . lsts) (apply map_lcv list lst lsts))


#|
(define (fold-left_fv corp init lst . lsts)
    (apply fold corp (append lst lsts (list init))))

(define (fold-right_fv proc init lst . lsts)
    (apply fold-right proc init lst lsts))


(define (fold-left_uv corp init lst . lsts)
    (let ((func (lambda (tup) (apply corp (car tup) (map car (cdr tup))))))
        (car (unfold-right
            (lambda (tup) (any null? (cdr tup))) func
            (lambda (tup) (cons (func tup) (map cdr (cdr tup))))
            (cons init (cons lst lsts))))))

(define (fold-right_uv proc init lst . lsts)
    (let ((func (lambda (tup) (apply proc (append (cadr tup)
            (list (car tup)))))))
        (car (unfold-right
            (lambda (tup) (or (null? (cdr tup)) (any null? (cdr tup)))) func
            (lambda (tup) (cons (func tup) (cddr tup)))
            (cons init (reverse (apply zip lst lsts)))))))


(define (fold-left_lcv corp init lst . lsts)
    (fold-ec init (: heads (reverse (apply zip lst lsts))) heads
        (lambda (e a) (apply corp a e))))

(define (fold-right_lcv proc init lst . lsts)
    (fold-ec init (: heads (reverse (apply zip lst lsts))) heads
        (lambda (e a) (apply proc (append e (list a))))))
|#
