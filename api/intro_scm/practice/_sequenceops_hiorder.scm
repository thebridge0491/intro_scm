(define (tabulate_f func cnt)
    ;(reverse (fold (lambda (e a) (cons (func e) a)) '() (iota cnt)))
    (fold (lambda (e a) (cons (func e) a)) '() (iota cnt (- cnt 1) -1)))

(define (length_f lst) (fold (lambda (e a) (+ a 1)) 0 lst))

(define (nth_f idx lst)
    (if (>= idx (length lst))
        (error "argument out of range:" idx)
        (cdr (fold (lambda (e a) (if (= 0 (car a)) (cons (- (car a) 1) e)
            (cons (- (car a) 1) (cdr a)))) (cons idx '()) lst))))

(define (index_find_f pred lst)
    (fold (lambda (i_e a) (if (and (not (cdr a)) (pred (cdr i_e))) i_e a))
        (cons #f #f) (map cons (iota (length lst)) lst)))

(define (index_f pred lst) (car (index_find_f pred lst)))

(define (find_f pred lst) (cdr (index_find_f pred lst)))

(define (minmax_f x . args)
    (apply values (fold (lambda (e lo_hi)
        (cond ((< e (car lo_hi)) (list e (cadr lo_hi)))
            ((> e (cadr lo_hi)) (list (car lo_hi) e))
            (else lo_hi))) (list x x) args)))

(define (min_f x . args) (receive (lo hi) (apply minmax_f x args) lo))

(define (max_f x . args) (receive (lo hi) (apply minmax_f x args) hi))

(define (reverse_f lst) (fold (lambda (e a) (cons e a)) '() lst))

(define (copy_f lst) (fold-right cons '() lst))

(define (split-at_f n lst)
    (let ((res (fold (lambda (i t_d)
            (list (cons (caadr t_d) (car t_d)) (cdadr t_d)))
            (list '() lst) (iota n))))
        (values (reverse (car res)) (cadr res))))

;(define (take_f n lst)
;    ;(fold-right (lambda (e1 e2 a) (cons e2 a)) '() (iota n) lst)
;    (reverse (fold (lambda (e1 e2 a) (cons e2 a)) '() (iota n) lst)))
(define (take_f n lst) (receive (l1 l2) (split-at_f n lst) l1))

;(define (drop_f n lst) (fold (lambda (e a) (cdr a)) lst (iota n)))
(define (drop_f n lst) (receive (l1 l2) (split-at_f n lst) l2))

(define (any_every_f pred lst)
    (fold (lambda (e a)
        (cons (or (car a) (pred e)) (and (cdr a) (pred e))))
        (cons #f #t) lst))

(define (any_f pred lst) (car (any_every_f pred lst)))

(define (every_f pred lst) (cdr (any_every_f pred lst)))

(define (map_f proc lst)
    (fold-right (lambda (e a) (cons (proc e) a)) '() lst))

(define (for-each_f proc lst)
    (fold (lambda (e a) (proc e)) '() lst))

(define (partition_f pred lst)
    (let ((res (fold (lambda (e f_r)
            (if (pred e)
                (list (cons e (car f_r)) (cadr f_r))
                (list (car f_r) (cons e (cadr f_r)))))
            (list '() '()) lst)))
        (values (reverse (car res)) (reverse (cadr res)))))

;(define (filter_f pred lst)
;    ;(fold-right (lambda (e a) (if (pred e) (cons e a) a)) '() lst)
;    (reverse (fold (lambda (e a) (if (pred e) (cons e a) a)) '() lst)))
(define (filter_f pred lst) (receive (l1 l2) (partition_f pred lst) l1))

;(define (remove_f pred lst) (filter_f (lambda (e) (not (pred e))) lst))
(define (remove_f pred lst) (receive (l1 l2) (partition_f pred lst) l2))


(define (is-ordered_f? xs cmpfn . opt-keyfn)
    (let ((keyfn (if (null? opt-keyfn) (lambda (e) e) (car opt-keyfn))))
        (if (> 2 (length xs))
            #t
            (cdr (fold (lambda (e old_acc) (cons e
                (and (cdr old_acc) (cmpfn (keyfn (car old_acc)) (keyfn e)))))
                (cons (car xs) #t) (cdr xs))))))


(define (append_f xs ys) (fold-right cons ys xs))

(define (interleave_f xs ys)
    (let ((extra (if (> (length xs) (length ys)) (drop xs (length ys))
            (drop ys (length xs)))))
        ;(fold-right (lambda (e1 e2 a) (append (list e1 e2) a)) extra xs ys)
        (append (reverse
            (fold (lambda (e1 e2 a) (cons e2 (cons e1 a))) '() xs ys)
            ) extra)))

(define (map2_f proc xs ys)
    ;(fold-right (lambda (e1 e2 a) (cons (proc e1 e2) a)) '() xs ys)
    (reverse (fold (lambda (e1 e2 a) (cons (proc e1 e2) a)) '() xs ys)))

(define (zip_f xs ys) (map2_f list xs ys))

(define (unzip_f lst)
    (apply values (fold-right (lambda (e a)
            (list (cons (car e) (car a)) (cons (cadr e) (cadr a))))
        '(() ()) lst)))

(define (concat_f nlsts)
    (fold (lambda (e a) (append e a)) '() (reverse nlsts)))

(define (flatten_f nlsts)
    (fold (lambda (e a)
        (append a (if (list? e) (flatten_f e) (list e)))) '() nlsts))



(define (tabulate_u func cnt)
    ;(reverse (unfold-right (lambda (idx) (<= cnt idx))
    ;    (lambda (idx) (func idx)) (lambda (idx) (+ idx 1)) 0))
    (unfold-right (lambda (idx) (> 0 idx))
        (lambda (idx) (func idx)) (lambda (idx) (- idx 1)) (- cnt 1)))

(define (length_u lst)
    (let ((func (lambda (tup) (+ (car tup) 1))))
        (car (append (unfold-right (lambda (tup) (null? (cdr tup))) func
            (lambda (tup) (cons (func tup) (cddr tup))) (cons 0 lst)) '(0)))))

(define (nth_u idx lst)
    (let ((func (lambda (trip) (if (= idx (car trip))
            (caadr trip) (caddr trip)))))
        (car (unfold-right (lambda (trip) (< idx (car trip))) func
            (lambda (trip) (list (+ (car trip) 1) (cdadr trip) (func trip)))
            (list 0 lst '())))))

(define (index_find_u pred lst)
    (let ((xs (map cons (iota (length lst)) lst))
            (func (lambda (tup) (if (and (not (cdar tup)) (pred (cdadr tup)))
                (cadr tup) (car tup)))))
        (car (unfold-right (lambda (tup) (or (null? (cdr tup)) (cdar tup)))
            func (lambda (tup) (cons (func tup) (cddr tup)))
            (cons (cons #f #f) xs)))))

(define (index_u pred lst) (car (index_find_u pred lst)))

(define (find_u pred lst) (cdr (index_find_u pred lst)))

(define (minmax_u x . args)
    (let* ((func (lambda (trip)
                (cond ((< (caar trip) (cadr trip))
                        (cons (caar trip) (cddr trip)))
                    ((> (caar trip) (cddr trip))
                        (cons (cadr trip) (caar trip)))
                    (else (cons (cadr trip) (cddr trip))))))
            (res (car (unfold-right (lambda (trip) (null? (car trip))) func
                (lambda (trip) (cons (cdar trip) (func trip)))
                (cons args (cons x x))))))
        (values (car res) (cdr res))))

(define (min_u x . args) (receive (lo hi) (apply minmax_u x args) lo))

(define (max_u x . args) (receive (lo hi) (apply minmax_u x args) hi))

(define (reverse_u lst) (unfold-right null? car cdr lst))

(define (copy_u lst) (unfold null? car cdr lst))

(define (split-at_u n lst)
    (let* ((func (lambda (trip) (cons (cons (caddr trip) (cadr trip))
                (cdddr trip))))
            (res (car (unfold-right
                (lambda (trip) (or (null? (caddr trip)) (= n (car trip))))
                func
                (lambda (trip) (cons (+ (car trip) 1) (func trip)))
                (cons 0 (cons '() lst))))))
        (values (reverse (car res)) (cdr res))))

;(define (take_u n lst)
;    (reverse (unfold-right
;        (lambda (tup) (or (null? (cdr tup)) (> 1 (car tup))))
;        (lambda (tup) (cadr tup))
;        (lambda (tup) (cons (- (car tup) 1) (cddr tup))) (cons n lst))))
(define (take_u n lst) (receive (l1 l2) (split-at_u n lst) l1))

;(define (drop_u n lst)
;    (car (unfold-right
;        (lambda (tup) (or (null? (cdr tup)) (> 1 (car tup))))
;        (lambda (tup) (cddr tup))
;        (lambda (tup) (cons (- (car tup) 1) (cddr tup))) (cons n lst))))
(define (drop_u n lst) (receive (l1 l2) (split-at_u n lst) l2))

(define (any_every_u pred lst)
    (let ((func (lambda (tup) (cons (or (caar tup) (pred (cadr tup)))
                (and (cdar tup) (pred (cadr tup)))))))
        (car (unfold-right (lambda (tup) (null? (cdr tup)))
            (lambda (tup) (func tup))
            (lambda (tup) (cons (func tup) (cddr tup)))
            (cons (cons #f #t) lst)))))

(define (any_u pred lst) (car (any_every_u pred lst)))

(define (every_u pred lst) (cdr (any_every_u pred lst)))

(define (map_u proc lst) (unfold null? (lambda (xs) (proc (car xs))) cdr lst))

(define (for-each_u proc lst)
    (car (unfold-right null? (lambda (el) (proc (car el))) cdr lst)))

(define (partition_u pred lst)
    (let* ((func (lambda (trip) (if (pred (caar trip))
                (cons (cons (caar trip) (cadr trip)) (cddr trip))
                (cons (cadr trip) (cons (caar trip) (cddr trip))))))
            (res (car (unfold-right (lambda (trip) (null? (car trip))) func
                (lambda (trip) (cons (cdar trip) (func trip)))
                (cons lst (cons '() '()))))))
        (values (reverse (car res)) (reverse (cdr res)))))

;(define (filter_u pred lst)
;    (let ((func (lambda (tup) (if (pred (caar tup))
;            (cons (caar tup) (cdr tup)) (cdr tup)))))
;        (reverse (car (unfold-right (lambda (tup) (null? (car tup))) func
;            (lambda (tup) (cons (cdar tup) (func tup))) (cons lst '()))))))
(define (filter_u pred lst) (receive (l1 l2) (partition_u pred lst) l1))

;(define (remove_u pred lst) (filter_u (lambda (e) (not (pred e))) lst))
(define (remove_u pred lst) (receive (l1 l2) (partition_u pred lst) l2))


(define (is-ordered_u? xs cmpfn . opt-keyfn)
    (let* ((keyfn (if (null? opt-keyfn) (lambda (e) e) (car opt-keyfn)))
            (func (lambda (trip) (and (caddr trip)
                (cmpfn (keyfn (car trip)) (keyfn (caadr trip)))))))
        (if (> 2 (length xs))
            #t
            (car (unfold-right (lambda (trip) (null? (cadr trip))) func
                (lambda (trip) (list (caadr trip) (cdadr trip) (func trip)))
                (list (car xs) (cdr xs) #t))))))


(define (append_u xs ys)
    (let ((func (lambda (tup) (cons (cadr tup) (car tup)))))
        (car (unfold-right (lambda (tup) (null? (cdr tup))) func
            (lambda (tup) (cons (func tup) (cddr tup)))
            (cons ys (reverse xs))))))

(define (interleave_u xs ys)
    (let ((extra (if (> (length xs) (length ys)) (drop xs (length ys))
                (drop ys (length xs))))
            (func (lambda (tup) (append (list (caadr tup) (cadadr tup))
                (car tup)))))
        (car (unfold-right (lambda (tup) (null? (cdr tup))) func
            (lambda (tup) (cons (func tup) (cddr tup)))
            (cons extra (reverse (zip xs ys)))))))

(define (map2_u proc xs ys)
    (unfold (lambda (tup) (any null? tup))
        (lambda (tup) (proc (caar tup) (caadr tup)))
        (lambda (tup) (list (cdar tup) (cdadr tup))) (list xs ys)))

(define (zip_u xs ys) (map2_u list xs ys))

(define (unzip_u lst)
    (values (unfold null? (lambda (el) (caar el)) cdr lst)
        (unfold null? (lambda (el) (cadar el)) cdr lst)))

(define (concat_u nlsts)
    (let ((func (lambda (tup) (append (cadr tup) (car tup)))))
        (car (unfold-right (lambda (tup) (null? (cdr tup))) func
            (lambda (tup) (cons (func tup) (cddr tup)))
            (cons '() (reverse nlsts))))))



            ;(list-ec (: i (iota cnt)) (func i))
(define (tabulate_lc func cnt)
    (fold-ec '() (: i (iota cnt (- cnt 1) -1)) i
        (lambda (e a) (cons (func e) a))))

            ;(sum-ec (: i lst) 1)
(define (length_lc lst) (fold-ec 0 (: i lst) i (lambda (e a) (+ a 1))))

(define (nth_lc idx lst)
    (cdr (fold-ec (cons idx '()) (: e lst) e
        (lambda (e i_a) (if (= 0 (car i_a)) (cons (- (car i_a) 1) e)
            (cons (- (car i_a) 1) (cdr i_a)))))))

(define (index_find_lc pred lst)
    (fold-ec (cons #f #f) (: i_e (map cons (iota (length lst)) lst)) i_e
        (lambda (i_e a) (if (and (not (cdr a)) (pred (cdr i_e))) i_e a))))

(define (index_lc pred lst) (car (index_find_lc pred lst)))

(define (find_lc pred lst) (cdr (index_find_lc pred lst)))

(define (minmax_lc x . args)
    (apply values (fold-ec (list x x) (: e args) e (lambda (e lo_hi)
        (cond ((< e (car lo_hi)) (list e (cadr lo_hi)))
            ((> e (cadr lo_hi)) (list (car lo_hi) e))
            (else lo_hi))))))

(define (min_lc x . args) (receive (lo hi) (apply minmax_lc x args) lo))

(define (max_lc x . args) (receive (lo hi) (apply minmax_lc x args) hi))

(define (reverse_lc lst) (fold-ec '() (: e lst) e cons))

(define (copy_lc lst) (list-ec (: e lst) e))

(define (split-at_lc n lst)
    (let ((res (fold-ec (list '() lst) (: i (iota n)) i (lambda (e t_d)
            (list (cons (caadr t_d) (car t_d)) (cdadr t_d))))))
        (values (reverse (car res)) (cadr res))))

;(define (take_lc n lst)
;    ;(list-ec (:parallel (: i (iota n)) (: e lst)) e)
;    (reverse (fold-ec '() (:parallel (: e lst) (: i (iota n))) e
;        (lambda (e a) (cons e a)))))
(define (take_lc n lst) (receive (l1 l2) (split-at_lc n lst) l1))

;(define (drop_lc n lst) (fold-ec lst (: i (iota n)) i (lambda (e a) (cdr a))))
(define (drop_lc n lst) (receive (l1 l2) (split-at_lc n lst) l2))

(define (any_every_lc pred lst)
    (fold-ec (cons #f #t) (: e lst) e (lambda (e tup)
        (cons (or (car tup) (pred e)) (and (cdr tup) (pred e))))))

(define (any_lc pred lst) (car (any_every_lc pred lst)))

(define (every_lc pred lst) (cdr (any_every_lc pred lst)))

            ;(list-ec (: e lst) (proc e))
(define (map_lc proc lst)
    (reverse (fold-ec '() (: e lst) e (lambda (e a) (cons (proc e) a)))))

            ;(do-ec (: e lst) (lambda (e a) (proc e)))
(define (for-each_lc proc lst)
    (fold-ec '() (: e lst) e (lambda (e a) (proc e))))

(define (partition_lc pred lst)
    (let ((res (fold-ec (list '() '()) (: e lst) e (lambda (e f_r)
            (if (pred e)
                (list (cons e (car f_r)) (cadr f_r))
                (list (car f_r) (cons e (cadr f_r))))))))
        (values (reverse (car res)) (reverse (cadr res)))))

;(define (filter_lc pred lst)
;    (fold-ec '() (: e (reverse lst)) e (lambda (e a)
;        (if (pred e) (cons e a) a))))
(define (filter_lc pred lst) (receive (l1 l2) (partition_lc pred lst) l1))

;(define (remove_lc pred lst) (filter_lc (lambda (e) (not (pred e))) lst))
(define (remove_lc pred lst) (receive (l1 l2) (partition_lc pred lst) l2))


(define (is-ordered_lc? xs cmpfn . opt-keyfn)
    (let ((keyfn (if (null? opt-keyfn) (lambda (e) e) (car opt-keyfn))))
        (if (> 2 (length xs))
            #t
            (cdr (fold-ec (cons (car xs) #t) (: e (cdr xs)) e
                (lambda (e old_acc) (cons e (and (cdr old_acc)
                    (cmpfn (keyfn (car old_acc)) (keyfn e))))))))))


(define (append_lc xs ys)
    (fold-ec ys (: e (reverse xs)) e (lambda (e a) (cons e a))))

(define (interleave_lc xs ys)
    (let ((extra (if (> (length xs) (length ys)) (drop xs (length ys))
            (drop ys (length xs)))))
        (append (reverse
            (fold-ec '() (:parallel (: x xs) (: y ys)) (cons x y)
                (lambda (e1_e2 a) (cons (cdr e1_e2) (cons (car e1_e2) a))))
            ) extra)))

            ;(list-ec (:parallel (: x xs) (: y ys)) (proc x y))
(define (map2_lc proc xs ys)
    (reverse (fold-ec '() (:parallel (: x xs) (: y ys)) (cons x y)
        (lambda (e1_e2 a) (cons (proc (car e1_e2) (cdr e1_e2)) a)))))

(define (zip_lc xs ys) (map2_lc list xs ys))

(define (unzip_lc lst)
    (let ((res (fold-ec '(() ()) (: z (reverse lst)) z
            (lambda (e a) (list (cons (car e) (car a))
                (cons (cadr e) (cadr a)))))))
    (values (car res) (cadr res))))

(define (concat_lc nlsts)
    (fold-ec '() (: lst (reverse nlsts)) lst (lambda (e a) (append e a))))
