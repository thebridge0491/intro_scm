(define-library (tc_classic)
    (export suite)
    
    (import (scheme base) (scheme write) (scheme inexact))
    
    #|
    (cond-expand
		(gauche (import (only (gauche base) make string->regexp rxmatch)))
		(sagittarius (import (sagittarius regex)))
		(else))
    |#
    
    (import (srfi 1) (srfi 42) (srfi 78))
	(cond-expand
		((library (srfi 29)) (import (srfi 29)))
		(gauche (import (only (gauche base) format)))
		(sagittarius (import (only (sagittarius) format)))
		(else))
    
    (import (prefix (intro_scm util) Util:) (intro_scm practice classic))
    
    (begin
		(define mod-sym 'tc_classic)
		
        (include "_tc_classic.scm")
        )
	)
