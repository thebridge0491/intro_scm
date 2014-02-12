#!/bin/sh
":"; set -ex
":"; exec gosh -r7 -Atests $0 $@
":"; exit

;#!/usr/bin/env gosh -r7 -Atests $0 $@

(import (scheme base) (scheme load) (scheme process-context) (scheme write))

(cond-expand
	(gauche (import (only (gauche base) string->regexp rxmatch)))
	(sagittarius (import (sagittarius regex)))
	(else))

;(add-load-path "." :relative)

(import (srfi 78))
(cond-expand
	((library (srfi 29)) (import (srfi 29)))
	(gauche (import (only (gauche base) format)))
	(sagittarius (import (only (sagittarius) format)))
	(else))

(import (prefix (tc_classic) Tc_Classic:) (prefix (tc_sequenceops) Tc_Sequenceops:))

(check-set-mode! 'report-failed) ; 'off, 'summary, 'report-failed, 'report

(define suites
    (for-each (lambda (func)
            (func) 
            ;(check-report)
            ;(check-reset!)
            (display (format "~a~%" (make-string 40 #\#))))
        (list Tc_Classic:suite Tc_Sequenceops:suite)))

;(if (string=? "tests/ts_main.scm" (car (command-line)))
(if
	(cond-expand
		(gauche
			(rxmatch (string->regexp "tests/ts_main.scm") (car (command-line)))
			)
		(sagittarius
			(looking-at (regex "tests/ts_main.scm") (car (command-line)))
			)
		(else #f))
	(begin
		suites
		0)
	0)
