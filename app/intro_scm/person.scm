; Scheme - comments style convention
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

; tag::apidocs[]
;;; Intro_scm.Intro person library module
(define-library (intro_scm person)
; end::apidocs[]
	(export <person> person-name person-age person? peek_person size)
	
	(import (scheme base) (scheme process-context) (scheme write))
    
    (cond-expand
		(gauche (import (gauche base) (gauche logger) (only (gauche base) make make-keyword
			string->regexp rxmatch define-class define-method <object> is-a? 
			class-slots class-of slot-exists? slot-ref slot-set!)))
		(sagittarius (import (sagittarius regex) (util logging) (clos user)
			(only (clos core) class-slots class-of slot-exists?) 
			(only (sagittarius) make-keyword)))
		(else))
    
    ;(import (srfi 1) (srfi 9))
    (cond-expand
		((library (srfi 29)) (import (srfi 29)))
		(gauche (import (only (gauche base) format)))
		(sagittarius (import (only (sagittarius) format)))
		(else))
	
	(import (prefix (intro_scm intro) Intro:))
	
	(begin
		(define mod-sym 'intro_scm.person)
		
		(define log_root
			(cond-expand
				(gauche (make <log-drain> :path "root.log" :prefix 
					(format "~a ~a:" "(~Y ~T)" mod-sym))
					)
				(sagittarius (make-logger +debug-level+ (make-file-appender 
					(format "~a ~a:~a" "[~w5] ~l" mod-sym "~m") "root.log")))
				(else '()))
			)
		
		;(include "_person.scm")
		
		(cond-expand
			(gauche
				; tag::apidocs[]
				;;; Person class
				(define-class <person> ()
				; end::apidocs[]
					((name :init-keyword :name :init-value "John")
					(age :init-keyword :age :init-value 18)))
				)
			(sagittarius
				;; (sagittarius) keyword notation UNAVAILABLE R[6|7]RS mode
				(define-class <person> ()
					((name (make-keyword 'init-keyword) (make-keyword 'name) 
						(make-keyword 'init-value) "John")
					(age (make-keyword 'init-keyword) (make-keyword 'age) 
						(make-keyword 'init-value) 18)))
				)
			(else)
			)
        
		; tag::apidocs[]
		;;; Init person object
		(define-method initialize ((self <person>) initargs)
		; end::apidocs[]
  			(cond-expand
				(gauche (next-method))
				(sagittarius (call-next-method))
				(else))
  			(Util:log_drains initialize "entering" (list log_root)))
		
		; tag::apidocs[]
		;;; Peek person object state
		(define-method peek_person ((pers1 <object>))
		; end::apidocs[]
            (cond ((is-a? pers1 <person>)
                    (format "~a((name . \"~a\") (age . ~a))" pers1
                    	(slot-ref pers1 'name) (slot-ref pers1 'age)))
                (else (format "~a" pers1))))
		
		; tag::apidocs[]
		;;; Check object is a person object
		(define-method person? ((pers1 <object>))
		; end::apidocs[]
            (cond ((is-a? pers1 <person>) #t)
                (else #f)))
		
		; tag::apidocs[]
		;;; Return size value of person object
		(define-method size ((pers1 <object>))
		; end::apidocs[]
            (cond ((slot-exists? pers1 'size) (size pers1))
                (else 0)))
        
		; tag::apidocs[]
		;;; Main function for library example(s)
		(define (lib_main argv)
		; end::apidocs[]
			(let ((person1 (make <person> (make-keyword 'name)
					"imcomputer" (make-keyword 'age) 32)))
				(display (Util:echo_invoke make <person> (make-keyword 'name)
					"imcomputer" (make-keyword 'age) 32))
				(Util:log_drains <person> "<person> worked!" (list log_root))
				(display (Util:echo_invoke class-slots <person>))
				(display (Util:echo_invoke class-of person1))
				(display (Util:echo_invoke slot-set! person1 'age 33))
				(display (Util:echo_invoke peek_person person1))
				)
			0)
        
        ;; guard against main function execution within library
        ;(if (string=? "intro_scm/person.scm" (car (command-line)))
        (if
			(cond-expand
				(gauche
					(rxmatch (string->regexp "intro_scm/person.scm") (car (command-line)))
					)
				(sagittarius
					(looking-at (regex "intro_scm/person.scm") (car (command-line)))
					)
				(else #f))
			(lib_main (command-line))
			0)
        )
	)
