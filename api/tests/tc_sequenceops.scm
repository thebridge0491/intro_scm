(define-library (tc_sequenceops)
    (export suite)
    
    (import (scheme base) (scheme write) (scheme inexact))
    
    #|
    (cond-expand
		(gauche (import (only (gauche base) make string->regexp rxmatch)))
		(sagittarius (import (sagittarius regex)))
		(else))
    |#
    
    (import (srfi 1) (srfi 8) (srfi 42) (srfi 78))
	(cond-expand
		((library (srfi 29)) (import (srfi 29)))
		(gauche (import (only (gauche base) format)))
		(sagittarius (import (only (sagittarius) format)))
		(else))
    (cond-expand
		((library (srfi 95)) (import (srfi 95)))
		(else))
	
    (import (prefix (intro_scm util) Util:) (intro_scm practice sequenceops))
    
    (begin
        (define mod-sym 'tc_sequenceops)
        
        (include "_tc_sequenceops.scm")
        )
	)
