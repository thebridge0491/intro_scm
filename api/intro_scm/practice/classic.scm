; Scheme - comments style convention
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

; tag::apidocs[]
;;; Intro_scm.Practice classic library module
(define-library (intro_scm practice classic)
; end::apidocs[]
	(export square_i square_r square_do expt_i expt_r expt_do fast-expt_i
        fast-expt_r fast-expt_do sum_to_i sum_to_r sum_to_do fact_i fact_r
        fact_do fib_i fib_r fib_do pascaltri_add pascaltri_mult pascaltri_do
        quot_m rem_m div_m mod_m gcd_i gcd_r gcd_do lcm_i lcm_r lcm_do
        base_expand_i base_expand_r base_expand_do base_to10_i base_to10_r
        base_to10_do range_step_i range_step_r range_step_do range_i
        range_r range_do compose_i compose_r compose_do
        
        hanoi hanoi_moves nqueens nqueens_grid
        )
	
	(import (scheme base) (scheme process-context) (scheme write))
	
	(cond-expand
		(gauche (import (gauche base) (gauche logger)))
		(sagittarius (import (sagittarius regex) (util logging)))
		(else))
	
	(import (srfi 1) (srfi 8) (srfi 26) (srfi 25))
	(cond-expand
		((library (srfi 29)) (import (srfi 29)))
		(gauche (import (only (gauche base) format)))
		(sagittarius (import (only (sagittarius) format)))
		(else))
	(cond-expand
		((library (srfi 95)) (import (srfi 95)))
		(else))
	
	(import (prefix (intro_scm util) Util:))
	
	(begin
		(define mod-sym 'intro_scm.practice.classic)

		(define log_out
			(cond-expand
				(gauche (make <log-drain> :path #t :prefix 
					(format "~a ~a:" "(~Y ~T)"mod-sym))
					;(slot-set! log_out 'prefix (format "~a ~a:" "(~Y ~T)" mod-sym))
					)
				(sagittarius (make-logger +info-level+ (make-appender 
					(format "~a ~a:~a" "[~w5] ~l" mod-sym "~m"))))
				(else '()))
			)
		
		(define log_prac
			(cond-expand
				(gauche (make <log-drain> :path "prac.log" :prefix 
					(format "~a ~a:" "(~Y ~T)" mod-sym)))
				(sagittarius (make-logger +debug-level+ (make-file-appender 
					(format "~a ~a:~a" "[~w5] ~l" mod-sym "~m") "prac.log")))
				(else '()))
			)
			
		(include "_classic.scm")
		(include "_classic_puzzles.scm")
		
		; tag::apidocs[]
		;;; Main function for library example(s)
		(define (lib_main argv)
		; end::apidocs[]
			(display (format "(fact 5): ~a~%" (fact_i 5)))
			0)
		
		;; guard against main function execution within library
		;(if (string=? "intro_scm/practice/classic.scm" (car (command-line)))
		(if
			(cond-expand
				(gauche
					(rxmatch (string->regexp "intro_scm/practice/classic.scm") (car (command-line)))
					)
				(sagittarius
					(looking-at (regex "intro_scm/practice/classic.scm") (car (command-line)))
					)
				(else #f))
			(lib_main (command-line))
			0)
		)
	)
