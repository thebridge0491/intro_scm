(define (hanoi src dest spare num_disks)
    (if (>= 0 num_disks)
        '()
	    (append (hanoi src spare dest (- num_disks 1))
	        (list (cons src dest))
	        (hanoi spare dest src (- num_disks 1)))))

(define (hanoi_moves src dest spare num_disks)
#|
    (define (hanoi_pegs res)    ; mutable version using vector-set!
        (reverse (let iter ((vec_pegs (vector (iota num_disks 1) '() '()))
            (lst res) (acc '()))
        (let ((el1 (- (caar lst) 1)) (el2 (- (cdar lst) 1)))
            (vector-set! vec_pegs el2 (cons (car (vector-ref vec_pegs el1))
	            (vector-ref vec_pegs el2)))
	        (vector-set! vec_pegs el1 (cdr (vector-ref vec_pegs el1)))
	        (if (pair? (cdr lst))
	            (iter vec_pegs (cdr lst) (cons (vector-copy vec_pegs) acc))
	            (cons (vector-copy vec_pegs) acc))))))
|#
    (define (hanoi_pegs res)    ; immutable version using alists
        (reverse (let iter ((lst_pegs (list (cons 0 (iota num_disks 1))
            (cons 1 '()) (cons 2 '()))) (lst res) (acc '()))
        (let* ((el1 (- (caar lst) 1)) (el2 (- (cdar lst) 1))
                (peg_uplst (alist-cons el1 (cddr (assoc el1 lst_pegs))
                    (alist-delete el1 lst_pegs)))
                (peg_dnlst (alist-cons el2 (cons (cadr (assoc el1 lst_pegs))
                    (cdr (assoc el2 peg_uplst)))
                    (alist-delete el2 peg_uplst))))
	        (if (pair? (cdr lst))
	            (iter peg_dnlst (cdr lst) (cons (vector (cdr
	                (assoc 0 peg_dnlst)) (cdr (assoc 1 peg_dnlst))
	                (cdr (assoc 2 peg_dnlst))) acc))
	            (cons (vector (cdr (assoc 0 peg_dnlst)) (cdr 
	                (assoc 1 peg_dnlst)) (cdr (assoc 2 peg_dnlst))) acc))))))
    (define (stat_txt res_len)
        (let ((calc_len (- (expt 2 num_disks) 1)))
            (format "((n = ~a) 2^n - 1 = ~a) ~a (length(result) = ~a)~%"
                num_disks calc_len
                (if (= calc_len res_len) "==" "!=") res_len)))
    (let* ((txt_fmt "'move from ~a to ~a'")
            (res (hanoi src dest spare num_disks))
            (proc (lambda (el) (format txt_fmt (car el) (cdr el)))))
        (values res (list (stat_txt (length res)) (make-string 40 #\-))
            (map list (map proc res) (hanoi_pegs res)))))

(define (nqueens n)
    (define (threat? q1 q2)
	    (or (= (car q1) (car q2))
		    (= (cdr q1) (cdr q2))
          	(= (abs (- (car q1) (car q2))) (abs (- (cdr q1) (cdr q2))))))
    (define (safe? pos placed_set)
	    (cond ((null? placed_set) #t)
        	((threat? pos (car placed_set)) #f)
            (else (safe? pos (cdr placed_set)))))
  	(let iter ((col 0) (row 0) (placed_set '()) (board '()))
    	(cond ((< (- n 1) col) (cons (reverse placed_set) board))
          	((< (- n 1) row) board)
          	((safe? (cons col row) placed_set)
          		(iter col (+ 1 row) placed_set
          		    (iter (+ 1 col) 0 (cons (cons col row) placed_set) board)))
          	(else (iter col (+ 1 row) placed_set board)))))
#|
(define (nqueens_grid num_queens answer)    ; mutable version using array-set!
    (let ((calc_grid (lambda (el) (abs (- (+ 1 el) num_queens))))
            (arr2d (make-array (shape 0 (+ 1 num_queens) 0 (+ 1 num_queens)) " ")))
        (for-each (lambda (el) (array-set! arr2d
            (calc_grid el) 0 (number->string el))) (iota num_queens))
        (for-each (lambda (el) (array-set! arr2d
            num_queens (+ 1 el) (integer->char (+ el (char->integer #\a))))) (iota num_queens))
        (for-each (lambda (el) (array-set! arr2d (calc_grid (cdr el))
			(+ 1 (car el)) "Q")) answer)
        (fold-right (lambda (j acc0) 
			(cons (fold-right (lambda (i acc1) (cons (array-ref arr2d j i) acc1))
				'() (iota (+ 1 num_queens))) acc0))
			'() (iota (+ 1 num_queens)))
        ))
|#
(define (nqueens_grid num_queens answer)    ; immutable version
    (define (mk_row tup acc)
        (let ((lst_blank (list-tabulate num_queens (lambda (i) (cons i " ")))))
            (cons (cons (cdr tup)
                (map (lambda (pr) (if (= (car tup) (car pr)) "Q" (cdr pr)))
                    lst_blank)) acc)))
    (let ((lst_ltrs (cons " " (list-tabulate num_queens (lambda (i)
                (integer->char (+ i (char->integer #\a))))))))
        (fold mk_row (list lst_ltrs)
			(cond-expand
				((library (srfi 95))
					(sort answer (lambda (t1 t2) (< (cdr t1) (cdr t2)))))
				;(else answer)
				(else
					(fold-right (lambda (i acc) (cons (find (lambda (e) (= i (cdr e))) answer) acc)) '() (iota num_queens)))
				))))
