#!/bin/sh
":"; set -ex
":"; exec gosh -r7 -A. -Atests $0 $@
":"; exit

;#!/usr/bin/env gosh -r7 -A. -Atests $0 $@

; Scheme - comments style convention
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

; tag::apidocs[]
;;; Intro_scm.Intro main script
; end::apidocs[]

(import (scheme base) (scheme load) (scheme process-context) (scheme write)
    (scheme file) (scheme time))

(cond-expand
	(gauche
		(import (gauche base) (only (gauche base) make string->regexp rxmatch slot-ref 
			slot-set! class-slots class-of) (gauche logger) (rfc json)))
	(sagittarius
		(import (sagittarius regex) (util logging) (text json) (clos user)
			(only (clos core) class-slots class-of)))
	(else))

;(add-load-path "." :relative)

(import (srfi 1) (srfi 8) (srfi 37) (srfi 19) (srfi 27) (srfi 43) (srfi 18))
(cond-expand
	((library (srfi 29)) (import (srfi 29)))
	(gauche (import (only (gauche base) format)))
	(sagittarius (import (only (sagittarius) format)))
	(else))
(cond-expand
	((library (srfi 95)) (import (srfi 95)))
	(else))

;(load "./intro_scm/util.scm")
(import (prefix (intro_scm util) Util:) (prefix (intro_scm intro) Intro:)
    (prefix (intro_scm person) Person:)
    (prefix (intro_scm practice classic) Classic:)
    (prefix (intro_scm practice sequenceops) Seqops:)
    )

(define *argv* (cdr (command-line)))

(include "_main.scm")

; single-line comment
#| multi-line comment
    --- run w/out compile ---
    [sh | gosh -r7 | sash -d -r7] main.scm arg1 argN
    
    --- run REPL, load script, & run ---
    [gosh -r7|sash -d -r7] -A. -l main.scm arg1 argN ; > (main '("arg1" "argN"))
    
    --- help/info tools in REPL ---
    ;; (import (gauche interactive))
    (apropos 'PATTERN ['MODULE]) ; (d OBJ) ; (info 'SYMBOL) ; (features)
    ;; (import (sagittarius) (apropos))
    (apropos 'PATTERN) ; (features)
|#

(define mod-sym 'intro_scm.main)

(define log_root
	(cond-expand
		(gauche (begin 
			;(log-open #t)
			;(slot-set! (log-default-drain) 'prefix (format "~a" "~T ~P[~$]: "))
			(make <log-drain> :path "root.log" :prefix 
				(format "~a ~a:" "(~Y ~T)" mod-sym))
			;(slot-set! log_root 'prefix (format "~a ~a:" "(~Y ~T)" mod-sym))
			))
		(sagittarius (make-logger +debug-level+ (make-file-appender 
			(format "~a ~a:~a" "[~w5] ~l" mod-sym "~m") "root.log")))
		(else '()))
	)

; tag::apidocs[]
;;; Parse command-line options
(define (parse_cmdopts argv)
; end::apidocs[]
    (Util:log_drains parse_cmdopts "entering" (list log_root))
    
    (_parse_cmdopts_srfi37 argv))

; tag::apidocs[]
;;; Main entry point
(define (main argv)
; end::apidocs[]
	(let* ((rsrc_path (or (get-environment-variable "RSRC_PATH") "resources"))
		(blank-json "{\"domain\":\"???\",\"user1\":{\"name\":\"???\"}}")
		(port-in
			(guard
				(exc
					(else (display (format "---Condition: ~a---~%" exc))
						(open-input-string blank-json)))
				(open-input-file (string-append rsrc_path "/prac.json")))))
		(let* ((blank-cfg '(("domain" . "???") ("user1" . ("name" . "???"))))
				(alst-cfg 
					(cond-expand
						(gauche	(parse-json port-in))
						(sagittarius (make-parameter 'alist *json-map-type*)
							(json-read port-in))
						(else blank-cfg)))
				(tup-vec (vector (cons alst-cfg
					(cons (cdr (assoc "domain" alst-cfg)) (cdr (assoc
						"name" (cdr (assoc "user1" alst-cfg)))))))))
			(vector-for-each (lambda (i tup3)
				(begin
					(display (format "config: ~a~%" (car tup3)))
					(display (format "domain: ~a~%" (cadr tup3)))
					(display (format "user1Name: ~a~%" (cddr tup3)))
					)) tup-vec)
			
			(receive (user num is_expt2)
				(parse_cmdopts argv)
				
				(run_intro user (or (string->number num) 5) is_expt2 rsrc_path))
			(close-port port-in))))

;; guard against main function execution within library
;(if (string=? "intro_scm/main.scm" (car (command-line)))
(if
	(cond-expand
		(gauche
			(rxmatch (string->regexp "intro_scm/main.scm") (car (command-line)))
			;(#/intro_scm\/main.scm/ (car (command-line)))
			(main (command-line))
			)
		(sagittarius
			(looking-at (regex "intro_scm/main.scm") (car (command-line)))
			;; requires at top of file: #!read-macro=sagittarius/regex
			;(#/intro_scm\/main.scm/ (car (command-line)))
			)
		(else #f))
	0
	0)
