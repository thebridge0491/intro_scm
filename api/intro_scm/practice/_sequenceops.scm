(define (tabulate_i func cnt)
    (let iter ((idx cnt) (acc '()))
        (if (> 1 idx)
            acc
            (iter (- idx 1) (cons (func (- idx 1)) acc)))))

(define (tabulate_r func cnt)
    (if (> 1 cnt)
        '()
        (append (tabulate_r func (- cnt 1)) (list (func (- cnt 1))))))

(define (tabulate_do func cnt)
    (do ([idx cnt (- idx 1)] [acc '() (cons (func (- idx 1)) acc)])
        ((> 1 idx) acc)))

(define (length_i lst)
    (let iter ((rst lst) (acc 0))
        (if (null? rst)
            acc
            (iter (cdr rst) (+ acc 1)))))

(define (length_r lst)
    (if (null? lst)
        0
        (+ (length_r (cdr lst)) 1)))

(define (length_do lst)
    (do ([rst lst (cdr rst)] [acc 0 (+ acc 1)]) ((null? rst) acc)))

(define (nth_i idx lst)
    (let iter ((n idx) (rst lst))
        (cond ((null? rst) (error "argument out of range:" n))
            ((> 1 n) (car rst))
            (else (iter (- n 1) (cdr rst))))))

(define (nth_r idx lst)
    (cond ((null? lst) (error "argument out of range:" n))
        ((> 1 idx) (car lst))
        (else (nth_r (- idx 1) (cdr lst)))))

(define (nth_do idx lst)
    (if (<= (length lst) idx)
        (error "argument out of range:" idx)
        (do ([i idx (- i 1)] [rst lst (cdr rst)]) ((> 1 i) (car rst)))))

(define (index_find_i pred lst)
    (let iter ((rst lst) (idx 0))
        (cond ((null? rst) (cons #f #f))
            ((pred (car rst)) (cons idx (car rst)))
            (else (iter (cdr rst) (+ idx 1))))))

; tag::apidocs[]
;;; (iteratively) List index of element satisfying predicate
(define (index_i pred lst)
; end::apidocs[]
	(car (index_find_i pred lst)))

; tag::apidocs[]
;;; (iteratively) Find element satisfying predicate
(define (find_i pred lst)
; end::apidocs[]
	(cdr (index_find_i pred lst)))

(define (index_find_r pred lst)
    (define (helper idx rst)
        (cond ((null? rst) (cons #f #f))
            ((pred (car rst)) (cons idx (car rst)))
            (else (helper (+ idx 1) (cdr rst)))))
    (helper 0 lst))

(define (index_r pred lst) (car (index_find_r pred lst)))

(define (find_r pred lst) (cdr (index_find_r pred lst)))

(define (index_find_do pred lst)
    (do ([rst lst (cdr rst)] [idx 0 (+ idx 1)]
        [acc (cons #f #f) (if (pred (car rst)) (cons idx (car rst)) acc)])
        ((or (null? rst) (cdr acc)) acc)))

; tag::apidocs[]
;;; (do loop) List index of element satisfying predicate
(define (index_do pred lst)
; end::apidocs[]
	(car (index_find_do pred lst)))

; tag::apidocs[]
;;; (do loop) Find element satisfying predicate
(define (find_do pred lst)
; end::apidocs[]
	(cdr (index_find_do pred lst)))

(define (minmax_i x . args)
    (let iter ((rst args) (lo x) (hi x))
        (cond ((null? rst) (values lo hi))
            ((< (car rst) lo) (iter (cdr rst) (car rst) hi))
            ((> (car rst) hi) (iter (cdr rst) lo (car rst)))
            (else (iter (cdr rst) lo hi)))))

(define (min_i x . args) (receive (lo hi) (apply minmax_i x args) lo))

(define (max_i x . args) (receive (lo hi) (apply minmax_i x args) hi))

(define (minmax_r x . args)
    (define (helper lo hi rst)
        (cond ((null? rst) (list lo hi))
            ((< (car rst) lo) (helper (car rst) hi (cdr rst)))
            ((> (car rst) hi) (helper lo (car rst) (cdr rst)))
            (else (helper lo hi (cdr rst)))))
    (apply values (helper x x args)))

(define (min_r x . args) (receive (lo hi) (apply minmax_r x args) lo))

(define (max_r x . args) (receive (lo hi) (apply minmax_r x args) hi))

(define (minmax_do x . args)
    (do ([rst args (cdr rst)] [lo x (if (< (car rst) lo) (car rst) lo)]
        [hi x (if (> (car rst) hi) (car rst) hi)])
        ((null? rst) (values lo hi))))

(define (min_do x . args) (receive (lo hi) (apply minmax_do x args) lo))

(define (max_do x . args) (receive (lo hi) (apply minmax_do x args) hi))

; tag::apidocs[]
;;; (iteratively) Reverse list elements
(define (reverse_i lst)
; end::apidocs[]
	(Util:log_drains reverse_i "entering" (list log_out log_prac))
	(let iter ((rst lst) (acc '()))
        (if (null? rst)
            acc
            (iter (cdr rst) (cons (car rst) acc)))))

; tag::apidocs[]
;;; (recursively) Reverse list elements
(define (reverse_r lst)
; end::apidocs[]
	(if (null? (cdr lst))
        lst
        (append (reverse_r (cdr lst)) (list (car lst)))))

; tag::apidocs[]
;;; (do loop) Reverse list elements
(define (reverse_do lst)
; end::apidocs[]
	(do ([rst lst (cdr rst)] [acc '() (cons (car rst) acc)])
		((null? rst) acc)))

(define (copy_i lst)
    (let iter ((rst (reverse lst)) (acc '()))
        (if (null? rst)
            acc
            (iter (cdr rst) (cons (car rst) acc)))))

(define (copy_r lst)
    (if (null? lst)
        '()
        (cons (car lst) (copy_r (cdr lst)))))

(define (copy_do lst)
    (do ([rst lst (cdr rst)] [acc '() (cons (car rst) acc)])
        ((null? rst) (reverse acc))))

(define (split-at_i n lst)
    (let iter ((idx n) (rst lst) (acc '()))
        (cond ((> 1 idx) (values (reverse acc) rst))
            ((pair? rst)
                (iter (- idx 1) (cdr rst) (cons (car rst) acc)))
            (else (error "argument out of range:" n)))))

(define (take_i n lst) (receive (l1 l2) (split-at_i n lst) l1))

(define (drop_i n lst) (receive (l1 l2) (split-at_i n lst) l2))

(define (split-at_do n lst)
    (if (< (length lst) n)
        (error "argument out of range:" n)
        (do ([idx n (- idx 1)] [rst lst (cdr rst)]
            [acc '() (cons (car rst) acc)])
            ((> 1 idx) (values (reverse acc) rst)))))

(define (take_do n lst) (receive (l1 l2) (split-at_do n lst) l1))

(define (drop_do n lst) (receive (l1 l2) (split-at_do n lst) l2))

(define (any_every_i pred lst)
    (let iter ((rst lst) (acc (cons #f #t)))
        (if (null? rst)
            acc
            (iter (cdr rst) (cons (or (car acc) (pred (car rst)))
                (and (cdr acc) (pred (car rst))))))))

(define (any_i pred lst) (car (any_every_i pred lst)))

(define (every_i pred lst) (cdr (any_every_i pred lst)))

(define (any_every_r pred lst)
    (if (null? lst)
        (cons #f #t)
        (cons (or (pred (car lst)) (car (any_every_r pred (cdr lst))))
            (and (pred (car lst)) (cdr (any_every_r pred (cdr lst)))))))

(define (any_r pred lst) (car (any_every_r pred lst)))

(define (every_r pred lst) (cdr (any_every_r pred lst)))

(define (any_every_do pred lst)
    (do ([rst lst (cdr rst)] [acc (cons #f #t)
            (cons (or (car acc) (pred (car rst)))
                (and (cdr acc) (pred (car rst))))])
        ((null? rst) acc)))

(define (any_do pred lst) (car (any_every_do pred lst)))

(define (every_do pred lst) (cdr (any_every_do pred lst)))

(define (map_i proc lst)
    (let iter ((rst lst) (acc '()))
        (if (null? rst)
            (reverse acc)
            (iter (cdr rst) (cons (proc (car rst)) acc)))))

(define (map_r proc lst)
    (if (null? lst)
        '()
        (cons (proc (car lst)) (map_r proc (cdr lst)))))

(define (map_do proc lst)
    (do ([rst lst (cdr rst)] [acc '() (cons (proc (car rst)) acc)])
        ((null? rst) (reverse acc))))

(define (for-each_i proc lst)
    (let iter ((rst lst))
        (if (null? (cdr rst))
            (proc (car rst))
            (begin
                (proc (car rst))
                (iter (cdr rst))))))

(define (for-each_r proc lst)
    (if (null? (cdr lst))
        (proc (car lst))
        (begin
            (proc (car lst))
            (for-each_r proc (cdr lst)))))

(define (for-each_do proc lst)
    (do ([rst lst (cdr rst)] [acc '() (proc (car rst))]) ((null? rst) acc)))

(define (partition_i pred lst)
    (let iter ((rst lst) (acc '(() . ())))
        (cond ((null? rst) (values (reverse (car acc)) (reverse (cdr acc))))
            ((pred (car rst))
                (iter (cdr rst) (cons (cons (car rst) (car acc)) (cdr acc))))
            (else (iter (cdr rst) 
                (cons (car acc) (cons (car rst) (cdr acc))))))))

(define (filter_i pred lst) (receive (l1 l2) (partition_i pred lst) l1))

(define (remove_i pred lst) (receive (l1 l2) (partition_i pred lst) l2))

(define (partition_r pred lst)
    (define (helper norm pred lst)
        (cond ((null? lst) '())
            ((norm (pred (car lst))) 
                (cons (car lst) (helper norm pred (cdr lst))))
            (else (helper norm pred (cdr lst)))))
    (values (helper (lambda (e) e) pred lst) (helper not pred lst)))

(define (filter_r pred lst) (receive (l1 l2) (partition_r pred lst) l1))

(define (remove_r pred lst) (receive (l1 l2) (partition_r pred lst) l2))

(define (partition_do pred lst)
    (do ([rst lst (cdr rst)] [acc '(() . ()) (if (pred (car rst))
        (cons (cons (car rst) (car acc)) (cdr acc))
        (cons (car acc) (cons (car rst) (cdr acc))))])
        ((null? rst) (values (reverse (car acc)) (reverse (cdr acc))))))

(define (filter_do pred lst) (receive (l1 l2) (partition_do pred lst) l1))

(define (remove_do pred lst) (receive (l1 l2) (partition_do pred lst) l2))

(define (fold-left_i corp init lst)
    (let iter ((rst lst) (acc init))
        (if (null? rst)
            acc
            (iter (cdr rst) (corp (car rst) acc)))))

(define (fold-left_r corp init lst)
    (if (null? lst)
        init
        (fold-left_r corp (corp (car lst) init) (cdr lst))))

(define (fold-left_do corp init lst)
    (do ([rst lst (cdr rst)] [acc init (corp (car rst) acc)])
        ((null? rst) acc)))

(define (fold-right_i proc init lst)
    (let iter ((rst lst) (acc init))
        (if (null? rst)
            acc
            (iter (cdr rst) (proc (car rst) acc)))))

(define (fold-right_r proc init lst)
    (if (null? lst)
        init
        (proc (car lst) (fold-right_r proc init (cdr lst)))))

(define (fold-right_do proc init lst)
    (do ([rst lst (cdr rst)] [acc init (proc (car rst) acc)])
        ((null? rst) acc)))

(define (unfold-right_i pred func gen seed)
    (let iter ((cur seed) (acc '()))
        (if (pred cur)
            acc
            (iter (gen cur) (cons (func cur) acc)))))

(define (unfold-right_do pred func gen seed)
    (do ([cur seed (gen cur)] [acc '() (cons (func cur) acc)])
        ((pred cur) acc)))

(define (unfold-left_r pred func gen seed)
    (if (pred seed)
        '()
        (cons (func seed) (unfold-left_r pred func gen (gen seed)))))

(define (unfold-left_do pred func gen seed)
    (do ([cur seed (gen cur)] [acc '() (cons (func cur) acc)])
        ((pred cur) (reverse acc))))

(define (is-ordered_i? xs cmpfn . opt-keyfn)
    (let ((keyfn (if (null? opt-keyfn) (lambda (e) e) (car opt-keyfn))))
        (if (> 2 (length xs))
            #t
            (let iter ((rst (cdr xs)) (oldval (car xs)) (acc #t))
                (if (null? rst)
                    acc
                    (iter (cdr rst) (car rst) (and (cmpfn (keyfn oldval)
                        (keyfn (car rst))) acc)))))))

(define (is-ordered_r? xs cmpfn . opt-keyfn)
    (let ((keyfn (if (null? opt-keyfn) (lambda (e) e) (car opt-keyfn))))
        (if (> 2 (length xs))
            #t
            (and (cmpfn (keyfn (car xs)) (keyfn (cadr xs)))
                (is-ordered_r? (cdr xs) cmpfn keyfn)))))

(define (is-ordered_do? xs cmpfn . opt-keyfn)
    (let ((keyfn (if (null? opt-keyfn) (lambda (e) e) (car opt-keyfn))))
        (if (> 2 (length xs))
            #t
            (do ([rst (cdr xs) (cdr rst)] [oldval (car xs) (car rst)]
                [acc #t (and (cmpfn (keyfn oldval) (keyfn (car rst))) acc)])
                ((null? rst) acc)))))

(define (append_i lst1 lst2)
    (let iter ((rst (reverse lst1)) (acc lst2))
        (if (null? rst)
            acc
            (iter (cdr rst) (cons (car rst) acc)))))

(define (append_r lst1 lst2)
    (if (null? lst1)
        lst2
        (cons (car lst1) (append_r (cdr lst1) lst2))))

(define (append_do lst lst2)
    (do ([rst (reverse lst) (cdr rst)] [acc lst2 (cons (car rst) acc)])
        ((null? rst) acc)))

(define (interleave_i lst1 lst2)
    (let iter ((rst1 lst1) (rst2 lst2) (acc '()))
        (if (null? rst1)
            (append (reverse acc) rst2)
            (iter rst2 (cdr rst1) (cons (car rst1) acc)))))

(define (interleave_r lst1 lst2)
    (if (null? lst1)
        lst2
        (cons (car lst1) (interleave_r lst2 (cdr lst1)))))

(define (interleave_do lst1 lst2)
    (do ([ws lst1 zs] [zs lst2 (cdr ws)] [acc '() (cons (car ws) acc)])
        ((null? ws) (append (reverse acc) zs))))

(define (map2_i proc xs ys)
    (let iter ((ws xs) (zs ys) (acc '()))
        (if (or (null? ws) (null? zs))
            (reverse acc)
            (iter (cdr ws) (cdr zs) (cons (proc (car ws) (car zs)) acc)))))

(define (map2_r proc xs ys)
    (if (or (null? xs) (null? ys))
        '()
        (cons (proc (car xs) (car ys)) (map2_r proc (cdr xs) (cdr ys)))))

(define (map2_do proc xs ys)
    (do ([ws xs (cdr ws)] [zs ys (cdr zs)]
        [acc '() (cons (proc (car ws) (car zs)) acc)])
        ((or (null? ws) (null? zs)) (reverse acc))))

(define (zip_i lst1 lst2) (map2_i list lst1 lst2))

(define (zip_r lst1 lst2) (map2_r list lst1 lst2))

(define (zip_do lst1 lst2) (map2_do list lst1 lst2))

(define (zip_m lst1 lst2) (map list lst1 lst2))

(define (unzip_i lst)
    (let iter ((rst (reverse lst)) (acc '(() ())))
        (if (null? rst)
            (apply values acc)
            (iter (cdr rst) (list (cons (caar rst) (car acc))
                (cons (cadar rst) (cadr acc)) )))))

(define (unzip_do nlst)
    (do ([rst (reverse nlst) (cdr rst)] [acc1 '() (cons (caar rst) acc1)]
        [acc2 '() (cons (cadar rst) acc2)]) ((null? rst) (values acc1 acc2))))

(define (unzip_m lst) (values (map car lst) (map cadr lst)))

(define (concat_i nlsts)
    (let iter ((rst nlsts) (acc '()))
        (if (null? rst)
            acc
            (iter (cdr rst) (append acc (car rst))))))

(define (concat_r nlsts)
    (if (null? nlsts)
        '()
        (append (car nlsts) (concat_r (cdr nlsts)))))

(define (concat_do nlsts)
    (do ([rst nlsts (cdr rst)] [acc '() (append acc (car rst))])
        ((null? rst) acc)))

(define (concat_a nlsts) (apply append nlsts))


(define (flatten_r nlsts)
    (cond ((null? nlsts) '())
        ((not (pair? nlsts)) (list nlsts))
        (else (append (flatten_r (car nlsts)) (flatten_r (cdr nlsts))))))
