; Scheme - comments style convention
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

; tag::apidocs[]
;;; Intro_scm.Foreignc FFI Classic module
(define-library (intro_scm classic)
; end::apidocs[]
	(export fact_i fact_lp expt_i expt_lp
		)
	
	 (import (scheme base) (scheme process-context) (scheme write))
	
	(cond-expand
		(gauche (import (only (gauche base) make string->regexp rxmatch 
			dynamic-load) (gauche logger)))
		(sagittarius (import (sagittarius regex)
			(only (sagittarius) load-dynamic-library) (util logging)))
		(else))
	
	(import (srfi 1) (srfi 8))
	(cond-expand
		((library (srfi 29)) (import (srfi 29)))
		(gauche (import (only (gauche base) format)))
		(sagittarius (import (only (sagittarius) format)))
		(else))
	
	(begin
		(define mod-sym 'intro_scm.classic)
		
		;; Loads extension
		;(dynamic-load "classic_stubs" :init-function "Scm_Init_classic_stubs")
		(dynamic-load "classic_stubs")
		
		#|
		(define log_prac
			(cond-expand
				(gauche (make <log-drain> :path "prac.log" :prefix 
					(format "~a ~a:" "(~Y ~T)" mod-sym)))
				(sagittarius (make-logger +debug-level+ (make-file-appender 
					(format "~a ~a:~a" "[~w5] ~l" mod-sym "~m") "prac.log")))
				(else '()))
			)
		|#
		
		;(include "_classic.scm")
		
		; tag::apidocs[]
		;;; Main function for library example(s)
		(define (lib_main argv)
		; end::apidocs[]
			(display (format "~a~%" (fact_i 5)))
			0)
		
		;; guard against main function execution within library
		;(if (string=? "intro_scm/classic.scm" (car (command-line)))
		(if
			(cond-expand
				(gauche
					(rxmatch (string->regexp "intro_scm/classic.scm") (car (command-line)))
					)
				(sagittarius
					(looking-at (regex "intro_scm/classic.scm") (car (command-line)))
					)
				(else #f))
			(lib_main (command-line))
			0)
		)
	)

;; to generate stub files for <packagename> classic_stubs <modulename> intro_scm.classic
; cd <path> ; gauche-package generate classic_stubs intro_scm.classic
