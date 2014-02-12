(define-record-type <user>
    (make-user name num time_in)
    user?
    (name user-name user-name!)
    (num user-num user-num!)
    (time_in user-time_in user-time_in!))

; tag::apidocs[]
;;; Run intro
(define (run_intro user num is_expt2 rsrc_path)
; end::apidocs[]
    (let ((time_in (current-time))
            (ch #\null) (greet_path (string-append rsrc_path "/greet.txt"))
            (user1 (make-user user num 0.0))
            (person1 (make Person:<person> :name "I. M. Computer" :age 32))
            (time_dur 0.0) (num_val 0) (lst '(0 1 2 3 4))
            (num_vec (vector #b1011 #o13 #xb 11)) ; (bin oct hex dec)
            (delay_secs (ceiling (/ 25 10)))
            )
	
	(random-source-randomize! default-random-source)
	
	(set! num_val (vector-fold (lambda (idx el acc) (+ acc el)) 0 
		num_vec))
	
	(if (not (= num_val 
			(* (vector-length num_vec) (vector-ref num_vec 0))))
		(error "Assert fails: len * 1st elem == num_val->" num_val))
	
	(set! ch (Intro:delay_char_r (lambda () (thread-sleep! delay_secs))))
	
	(display 
		(date->string (time-utc->date (current-time))
			"~a ~b ~d ~T~z ~Y~n")  ; "~c~n"
		)
	
	;(user-name! user1 user))
	(if (= 0 (user-num user1))
		(user-num! user1 (+ (random-integer 18) 2)))
	(user-time_in! user1 time_in)
    
	(with-exception-handler
		(lambda (exc)
			(display (format "---Condition: ~a---~%" exc))
			(display (format "Does not match: ~a to ~a~%" 
				(user-name user1) "\"(?i:quit)\"")))
		(lambda ()
			;(if (string-ci=? "quit" (user-name user1))
			(if 
				(cond-expand
					(gauche
						;(rxmatch (string->regexp "quit" :case-fold #t) (user-name user1))
						(rxmatch (string->regexp "(?i:quit)") 
							(user-name user1)))
					(sagittarius
						;(looking-at (regex "quit" CASE-INSENSITIVE) (user-name user1))
						(looking-at (regex "(?i:quit)") 
							(user-name user1)))
					(else #f))
				(display (format "Good match: ~a to ~a~%" 
					(user-name user1) "\"(?i:quit)\""))
				(raise-continuable "Failure: regex search"))
				) ; end with-exception-handler lambda
			)
	
	(guard
		(exc
			((file-error? exc)
				(display (format "---File Error Condition: ~a---~%" exc)))
			((read-error? exc)
				(display (format "---Read Error Condition: ~a---~%" exc)))
			((error-object? exc)
				(display (format "---Error Condition: ~a---~%" exc)))
			(else (display (format "---Unknown Condition: ~a---~%" exc)))
			)
		
		;(fprintf (current-output-port) "%a"
        (display 
			;"Hello, World!\n" (current-output-port))
            (Intro:greeting greet_path (user-name user1)))
		)
	
	(set! time_dur (time-second (time-difference (current-time) 
		(user-time_in user1))))
	;(display (format "(program ~a) Took ~,2f seconds.~%" 
	(display (format "(program ~a) Took ~a seconds.~%"
		(car (command-line)) time_dur))
	
	(display (format "~a~%" (make-string 40 #\#)))
	(if is_expt2
		(begin
			(display (Util:echo_invoke Classic:expt_i 2 (user-num user1)))
			(display (Util:echo_invoke Seqops:reverse_i
				(iota (user-num user1) 0)))
			(cond-expand
				((library (srfi 95))
					(display (Util:echo_invoke sort (append '(9 9 9 9) lst) <)))
				(else))
			)
		(begin
			(display (Util:echo_invoke Classic:fact_i (user-num user1)))
			(display (Util:echo_invoke Seqops:index_i (lambda (el) 
				(= 3 el)) (iota (user-num user1) 0)))
			(display (Util:echo_invoke append '(9 9 9 9) lst))
			)
		)
		
	(display (format "~a~%" (make-string 40 #\#)))
	
	(display (Util:echo_invoke class-slots Person:<person>))
	(display (Util:echo_invoke slot-ref person1 'age))
	(display (Util:echo_invoke slot-set! person1 'age 33))
	(display (Util:echo_invoke class-of person1))
	(display (Util:echo_invoke Person:person? person1))
	(display (Util:echo_invoke Person:size person1))
	(display (Util:echo_invoke Person:peek_person person1))
	(display (format "~a~%" (make-string 40 #\#)))
	)
	)

; tag::apidocs[]
;;; Usage help
(define (show-help progname)
; end::apidocs[]
    (display (format "Usage: ~a [-h][-u USER][-n NUM][-2]~%" progname)))

(define (_parse_cmdopts_srfi37 argv)
	;; Parse command-line options using srfi-37 args-fold
    (args-fold (cdr argv)
        ;options
        (list 
            (option '(#\h "help") #f #f
                (lambda (option name arg . seeds)
                    (show-help (car (command-line))) (exit)))
            (option '(#\u "user") #t #f
                (lambda (option name arg user num is_expt2)
                    (values (or arg "World") num is_expt2)))
            (option '(#\n "num") #t #f
                (lambda (option name arg user num is_expt2)
                    (values user (or arg "0") is_expt2)))
            (option '(#\2 "is_expt2") #f #f
                (lambda (option name arg user num is_expt2)
                    (values user num #t)))
        )
        (lambda (option name arg . seeds)
            ;(error "Unknown option:" name))
            (display (format "error: Unknown option -~a~%" name))
            (show-help (car (command-line))) (exit))
        (lambda (operand user num is_expt2)
            (values user num is_expt2))
        "World" ; seed value for user
        "0"     ; seed value for num
        #f      ; seed value for is_expt2
        )
	)
