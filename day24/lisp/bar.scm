
(import (chicken format))
(import (chicken sort))
(import (chicken pretty-print))

#|

now given set puz input break into groups of FOUR
instead of groups of THREE

sa = sb = sc = sd
3 * sa = (sb + sc + sd)

|#

(define (puz)
  '(1   3   5   11   13   17   19   23   29   31   41   43 
       47   53   59   61   67   71   73   79   83   89   97   101 
       103   107   109   113 ))


;; xs : set of values do not want in resulting set
;; ys : set of misc values may or may not have elements of xs in there , we wish to filter out
(define without (lambda (xs ys)
		  (define without-h (lambda (xs ys zs)
				      (cond
				       ((null? ys) zs)
				       ((member (car ys) xs) (without-h xs (cdr ys) zs))
				       (#t (without-h xs (cdr ys) (cons (car ys) zs))))))
		  (without-h xs ys '())))


;; xs : choices can make
;; ys : choice have made
;; n  ; number choices to pick
;; yn ; number chosen so far
;; f  ; what to do with those choices
;; without ys xs ;  from those chosen - filter any not chosen from xs -i.e this is without ys xs
(define pick (lambda (xs ys n yn f orig-xs)
	       (cond		
		((= n yn) (f ys (without ys orig-xs)))
		((null? xs) #f)		
		(#t
		 ;; pick it 
		 (pick (cdr xs) (cons (car xs) ys) n (+ yn 1) f orig-xs)
		 ;; dont pick it
		 (pick (cdr xs) ys n yn f orig-xs)))))
		
;; if procedure returns to where we have control of lexical scope
;; we can introduce our own counters
;; this means do not need global variables
;; also means variable is initialised correctly for every run automatically

;; 
;; sa  : sum of picked values
;; sbc : sum of values left over from original puzzle list given at start - called initial
;; pa : product of picked values - the quantum entanglement in the puzzle 
(define feed (lambda (m)
	       (letrec ((empty-list '())
			(initial (puz))
			(n m)
			(yn 0)
			(puz-count 0)
			(res '())
			(f (lambda (xs ys)
			     (let ((sa (apply + xs))
				   (sbc (apply + ys))
				   (pa (apply * xs))
				   )
			       (when (= (* 3 sa) sbc)
				 (set! puz-count (+ 1 puz-count))
				 (set! res (cons (list pa xs puz-count) res))
				 (format #t "picked ~a : ~a : ~a : left ~a  ~%" pa puz-count xs ys))))))
		 (pick initial
		       empty-list
		       n
		       yn
		       f
		       initial)
		 (sort res (lambda (x y) (> (car x)(car y)))))))


;; here is a wonderful example of changing one thing and getting the new solution
;; (feed 1) ; no solutions 
;; (feed 2); no solutions 
;; (feed 3); no solutions 
;; (feed 4); no solutions 
;; (feed 5) ; -- aha lots solutions , lets see them tidied up for visual inspection

(pp (feed 5))


#|
> (pp (feed 5))

...
...
(84361967 (109 101 97 79 1) 7)
 (84307831 (113 101 89 83 1) 9)
 (82776671 (109 103 101 73 1) 6)
 (82585703 (109 107 97 73 1) 5)
 (82415759 (113 103 97 73 1) 4)
 (80486363 (109 107 103 67 1) 3)
 (80048183 (113 109 97 67 1) 2)
 (77387711 (113 109 103 61 1) 1))

*** 77387711  ....... ACCEPTED solution 

|#

