
(define len (lambda (xs)
	      (cond
	       ((null? xs) 0)
	       (#t (+ 1 (len (cdr xs)))))))

(len '(1 2 3))
(len '(a b c d e ))
