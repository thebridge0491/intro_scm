; tag::apidocs[]
;;; Run intro
(define (run_intro user)
; end::apidocs[]
    #|
	(with-exception-handler
		(lambda (exc)
			(display (format "---Condition: ~a---~%" exc))
			(display (format "Does not match: ~a to ~a~%" user 
				"\"(?i:quit)\"")))
		(lambda ()
	|#
	(guard
		(exc
			((file-error? exc)
				(display (format "---File Error Condition: ~a---~%" exc)))
			((read-error? exc)
				(display (format "---Read Error Condition: ~a---~%" exc)))
			((error-object? exc)
				(display (format "---Error Condition: ~a---~%" exc)))
			(else (display (format "---Condition: ~a---~%" exc))
				(display (format "Does not match: ~a to ~a~%" user 
					"\"(?i:quit)\"!"))))
	
		;(if (string-ci=? "quit" user)
		(if 
			(cond-expand
				(gauche
					;(rxmatch (string->regexp "quit" :case-fold #t) user)
					(rxmatch (string->regexp "(?i:quit)") user))
				(sagittarius
					;(looking-at (regex "quit" CASE-INSENSITIVE) user)
					(looking-at (regex "(?i:quit)") user))
				(else #f))
			(display (format "Good match: ~a to ~a~%" user "\"(?i:quit)\""))
			(raise-continuable "Failure: regex search"))
			;) ; end with-exception-handler lambda
		)
	)

; tag::apidocs[]
;;; Usage help
(define (show-help progname)
; end::apidocs[]
    (display (format "Usage: ~a [-h][-u USER]~%" progname)))

(define (_parse_cmdopts_srfi37 argv)
	;; Parse command-line options using srfi-37 args-fold
    (args-fold (cdr argv)
        (list		; options
            (option '(#\h "help") #f #f
                (lambda (option name arg . seeds)
                    (show-help (car (command-line))) (exit)))
            (option '(#\u "user") #t #f
                (lambda (option name arg user)
                    (values (or arg "World"))))
			)
        (lambda (option name arg . seeds)
            (display (format "error: Unknown option -~a~%" name))
            (show-help (car (command-line))) (exit))
        (lambda (operand user)
            (values user))
        "World" ; seed value for user
        )
	)
