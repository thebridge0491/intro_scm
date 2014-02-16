(define-library (tc_collections)
    (export suite)
    
    (import (scheme base) (scheme write) (scheme inexact))
    
    (cond-expand
		(gauche (import (gauche base)))
		(sagittarius (import (sagittarius)))
		(else))
    #|
    (cond-expand
		(gauche (import (only (gauche base) make string->regexp rxmatch)))
		(sagittarius (import (sagittarius regex)))
		(else))
    |#
    
    (import (srfi 42) (srfi 78) (srfi 1) (srfi 25) (srfi 43) (srfi 69))
	(cond-expand
		((library (srfi 29)) (import (srfi 29)))
		(gauche (import (only (gauche base) format)))
		(sagittarius (import (only (sagittarius) format)))
		(else))
	(cond-expand
		(gauche (import (data queue) (data heap)))
		(sagittarius (import (util queue) (util heap)))
		(else))
    (cond-expand
		((library (srfi 95)) (import (srfi 95)))
		(else))
    
    (import (prefix (intro_scm util) Util:))
    
    (begin
		(define mod-sym 'tc_collections)
		
        (include "_tc_collections.scm")
        )
	)
