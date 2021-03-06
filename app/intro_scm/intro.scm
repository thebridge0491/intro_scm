; Scheme - comments style convention
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

; tag::apidocs[]
;;; Intro_scm.Intro intro library module
(define-library (intro_scm intro)
; end::apidocs[]
	(export greeting delay_char_r 
        <user> make-user user? user-name user-num user-time_in user-name!
        user-num! user-time_in!)
	
	(import (scheme base) (scheme process-context) (scheme write)
		(scheme file))
	
	(cond-expand
		(gauche (import (gauche base) (gauche logger)))
		(sagittarius (import (sagittarius regex) (util logging)))
		(else))
	
	(import (srfi 1) (srfi 9))
	(cond-expand
		((library (srfi 29)) (import (srfi 29)))
		(gauche (import (only (gauche base) format)))
		(sagittarius (import (only (sagittarius) format)))
		(else))
	
	(import (prefix (intro_scm util) Util:))
	
	(begin
		(define mod-sym 'intro_scm.intro)

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
			
		(include "_intro.scm")
		
		; tag::apidocs[]
		;;; Main function for library example(s)
		(define (lib_main argv)
		; end::apidocs[]
			(display (format "~a~%" (echo_invoke delay_char_r 3)))
			0)
		
		;; guard against main function execution within library
		;(if (string=? "intro_scm/intro.scm" (car (command-line)))
		(if
			(cond-expand
				(gauche
					(rxmatch (string->regexp "intro_scm/intro.scm") (car (command-line)))
					;(#/intro_scm\/intro.scm/ (car (command-line)))
					)
				(sagittarius
					(looking-at (regex "intro_scm/intro.scm") (car (command-line)))
					;; requires at top of file: #!read-macro=sagittarius/regex
					;(#/intro_scm\/intro.scm/ (car (command-line)))
					)
				(else #f))
			(lib_main (command-line))
			0)
		)
	)
