; Scheme - comments style convention
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

; tag::apidocs[]
;;; Intro_scm.Practice sequenceops library module
(define-library (intro_scm practice sequenceops)
; end::apidocs[]
	(export tabulate_i tabulate_r tabulate_do length_i length_r length_do
        nth_i nth_r nth_do index_i index_do find_i find_r find_do min_i max_i
        min_r max_r min_do max_do reverse_i reverse_r reverse_do copy_i copy_r
        copy_do split-at_i split-at_r split-at_do take_i take_r take_do
        drop_i drop_r drop_do any_i any_r any_do every_i every_r every_do
        map_i map_r map_do for-each_i for-each_r for-each_do partition_i
        partition_r partition_do filter_i filter_r filter_do remove_i
        remove_r remove_do fold-left_i fold-left_r fold-left_do fold-right_i
        fold-right_r fold-right_do unfold-right_i unfold-right_do
        unfold-left_r unfold-left_do

        is-ordered_i? is-ordered_r? is-ordered_do?

        append_i append_r append_do interleave_i interleave_r interleave_do
        map2_i map2_r map2_do zip_i zip_r zip_do zip_m unzip_i unzip_do
        unzip_m concat_i concat_r concat_do concat_a
        )
	
	(import (scheme base) (scheme process-context) (scheme write)
		(scheme cxr))
	
	(cond-expand
		(gauche (import (gauche base) (gauche logger)))
		(sagittarius (import (sagittarius regex) (util logging)))
		(else))
	
	(import (srfi 1) (srfi 8))
	(cond-expand
		((library (srfi 29)) (import (srfi 29)))
		(gauche (import (only (gauche base) format)))
		(sagittarius (import (only (sagittarius) format)))
		(else))
	
	(import (prefix (intro_scm util) Util:))
	
	(begin
		(define mod-sym 'intro_scm.practice.sequenceops)

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
			
		(include "_sequenceops.scm")
		
		; tag::apidocs[]
		;;; Main function for library example(s)
		(define (lib_main argv)
		; end::apidocs[]
			(display (format "(reverse '(0 1 2): ~a~%" (reverse_i '(0 1 2))))
			0)
		
		;; guard against main function execution within library
		;(if (string=? "intro_scm/practice/sequenceops.scm" (car (command-line)))
		(if
			(cond-expand
				(gauche
					(rxmatch (string->regexp "intro_scm/practice/sequenceops.scm") (car (command-line)))
					)
				(sagittarius
					(looking-at (regex "intro_scm/practice/sequenceops.scm") (car (command-line)))
					)
				(else #f))
			(lib_main (command-line))
			0)
		)
	)
