; tag::apidocs[]
;;; (iteratively) List index of element satisfying predicate
(define (index_i pred lst)
; end::apidocs[]
	(let iter ((rst lst) (idx 0))
        (cond ((null? rst) #f)
            ((pred (car rst)) idx)
            (else (iter (cdr rst) (+ idx 1))))))

; tag::apidocs[]
;;; (do loop) List index of element satisfying predicate
(define (index_do pred lst)
; end::apidocs[]
	(do ([rst lst (cdr rst)] [idx 0 (+ idx 1)]) ((pred (car rst)) idx)))

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
