

#|

1
 2 4 6 8 10 ...
  3  6  9   12   15 ...
   4   8    12    16   20   24 
at each number another elf is created


|#

(import (chicken format))
(import (math number-theory))

(define house
  (lambda (n)
    (let ((sum 0))
      (letrec ((foo (lambda (r)
		      ;; (when (zero? (modulo r 10000))
		      ;; 	(format #t " r = ~a ~%" r))
		      (when (zero? (modulo n r))
			(set! sum (+ sum (* 10 r))))
		      (cond
		       ((< r n)
			(foo (+ r 1)))))))
	(foo 1)
	sum))))

(define *target* 896472900)
;; (define *max-presents* 0)
;; 
;; (define search
;;   (lambda (n)
;;     (when (modulo n 10000000)
;;       (format #t "search at house ~a : *max-presents* ~a ~%" n *max-presents*))
;;     (let ((c (house n)))
;;       (when (> c *max-presents*)
;; 	(set! *max-presents* c)
;; 	;;(format #t " max present = ~a ~%" *max-presents*)
;; 	)
;;       (cond
;;        ((>= c *target*)
;; 	(format #t "house number ~a gets ~a presents~%" n c )
;; 	(format #t " , more than house no 34000000 who gets 896472900 presents~%")
;; 	n)
;;        (#t (search (+ n 1)))))))
;; 
;; 
;; (define target
;;   (lambda ()
;;     (let ((start 34000000))
;;       (house start))))
;; 
;; ;; a pre-compiled target ...  896472900
;; ;; looking for a house number that gets >= that number of presents 
;; 
;; (define run
;;   (lambda ()
;;     (search (* 2 3 5 7 11 13 17 19))))
  
(define search
  (lambda (n prod)
    (let ((presents (house prod)))
      (format #t "nth-prime ~a : product ~a : presents ~a : target ~a ~%"
	      n prod presents *target*)
      (cond
       ((>= presents *target*)
	(format #t "house number ~a yields ~a presents ~%" prod presents)
	(format #t " ..............................~a ~%" *target*)
	prod)
       (#t (let ((next (nth-prime (+ n 1))))
	     (search (+ n 1) (* prod next))))))))



;; nth-prime 0 yields 2
(define (run)
  (search 0 2))
    

;;(run)






;;(run)


#|
terry@debian:~/code/advent-of-code/advent-of-code-2015/day20/guile$ csc -O3 fun.scm
terry@debian:~/code/advent-of-code/advent-of-code-2015/day20/guile$ time ./fun
nth-prime 0 : product 2 : presents 30 : target 896472900 
nth-prime 1 : product 6 : presents 120 : target 896472900 
nth-prime 2 : product 30 : presents 720 : target 896472900 
nth-prime 3 : product 210 : presents 5760 : target 896472900 
nth-prime 4 : product 2310 : presents 69120 : target 896472900 
nth-prime 5 : product 30030 : presents 967680 : target 896472900 
nth-prime 6 : product 510510 : presents 17418240 : target 896472900 
nth-prime 7 : product 9699690 : presents 348364800 : target 896472900 
nth-prime 8 : product 223092870 : presents 8360755200 : target 896472900 
house number 223092870 yields 8360755200 presents 
 ..............................896472900 

real    0m6.653s
user    0m6.642s
sys     0m0.009s

|#



