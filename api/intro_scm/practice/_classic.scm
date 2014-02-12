; tag::apidocs[]
;;; (iteratively) Compute factorial of n
(define (fact_i n)
; end::apidocs[]
	(Util:log_drains fact_i "entering" (list log_out log_prac))
	(let iter ((ct n) (acc 1))
        (if (>= 0 ct)
            acc
            (iter (- ct 1) (* acc ct)))))

; tag::apidocs[]
;;; (recursively) Compute factorial of n
(define (fact_r n)
; end::apidocs[]
	(if (>= 0 n)
        1
        (* n (fact_r (- n 1)))))

; tag::apidocs[]
;;; (do loop) Compute factorial of n
(define (fact_do n)
; end::apidocs[]
	(do ([ct n (- ct 1)] [acc 1 (* acc ct)]) ((>= 0 ct) acc)))

; tag::apidocs[]
;;; (iteratively) Compute b to the nth exponent
(define (expt_i b n)
; end::apidocs[]
	(let iter ((ct n) (acc 1))
        (if (>= 0 ct)
            acc
            (iter (- ct 1) (* acc b)))))

; tag::apidocs[]
;;; (recursively) Compute b to the nth exponent
(define (expt_r b n)
; end::apidocs[]
	(if (>= 0 n)
        1
        (* (expt_r b (- n 1)) b)))

; tag::apidocs[]
;;; (do loop) Compute b to the nth exponent
(define (expt_do b n)
; end::apidocs[]
	(do ([ct n (- ct 1)] [acc 1 (* acc b)]) ((>= 0 ct) acc)))
