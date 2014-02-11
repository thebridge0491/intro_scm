; Scheme - comments style convention
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

; tag::apidocs[]
;;; Intro_scm.Util util library module
(define-library (intro_scm util)
; end::apidocs[]
	(export log_drains echo_invoke mkstring_nested in_epsilon? cartesian_prod
		bound_values robust_failures worst_values worst_failures)
	
	(import (scheme base) (scheme process-context) (scheme write))
	
	(cond-expand
		(gauche (import (gauche base) (gauche logger)))
		(sagittarius (import (sagittarius regex) (util logging)))
		(else))
	
	(import (srfi 1) (srfi 42) (srfi 8))
	(cond-expand
		((library (srfi 29)) (import (srfi 29)))
		(gauche (import (only (gauche base) format)))
		(sagittarius (import (only (sagittarius) format)))
		(else))
	
	(begin
		(define mod-sym 'intro_scm.util)

		(define log_out
			(cond-expand
				(gauche (make <log-drain> :path #t :prefix 
					(format "~a ~a:" "(~Y ~T)"mod-sym))
					;(slot-set! log_out 'prefix (format "~a ~a:" "(~Y ~T)"mod-sym))
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
		
		; tag::apidocs[]
		;;; Log messages to log drains
		(define (log_drains func msg drains)
		; end::apidocs[]
			(if (null? drains)
				'()
				(begin
					(cond-expand
						(gauche (log-format (car drains) "~a -- ~a" func msg))
						(sagittarius (info-log (car drains)
							(format "~a -- ~a" func msg)))
						(else))
					(log_drains func msg (cdr drains))))
			)
			
		(include "_util.scm")
		
		; tag::apidocs[]
		;;; Main function for library example(s)
		(define (lib_main argv)
		; end::apidocs[]
			(display (format "~a~%" (echo_invoke cartesian_prod '(0 1 2) 
				'(10 20 30))))
			(log_drains cartesian_prod "cartesian_prod worked!" 
				(list log_out log_prac))
			0)
		
		;; guard against main function execution within library
		;(if (string=? "intro_scm/util.scm" (car (command-line)))
		(if
			(cond-expand
				(gauche
					(rxmatch (string->regexp "intro_scm/util.scm") (car (command-line)))
					;(#/intro_scm\/util.scm/ (car (command-line)))
					)
				(sagittarius
					(looking-at (regex "intro_scm/util.scm") (car (command-line)))
					;; requires at top of file: #!read-macro=sagittarius/regex
					;(#/intro_scm\/.scm/ (car (command-line)))
					)
				(else #f))
			(lib_main (command-line))
			0)
		)
	)
