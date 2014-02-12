(define (greeting greet_path name)
    (Util:log_drains greeting "entering" (list log_out))
    (with-input-from-file greet_path
        ;(lambda () (string-append "\n" (read-line) name "!\n"))))
        (lambda () (format "~%~a~a!~%" (read-line) name))))

(define (delay_char_r delay_func)
    (delay_func)
    (display "Type any character when ready.")
    (flush-output-port)
    (if (not (or (char=? (peek-char) #\newline) (char=? (peek-char) #\null)))
        (string-ref (read-line) 0)
        (begin
            (read-line)
            (delay_char_r delay_func))))
