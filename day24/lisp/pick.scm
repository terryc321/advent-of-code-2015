
(import (chicken format))
(import (chicken sort))
(import (chicken pretty-print))

#|

what is the set of 6 numbers from a list that is the lowest possible
given remaining numbers is it possible to

setA
setB
setC

three sets of numbers have to add to same value
prodA  a1 a2 a3
prodB  b1 b2 b3 ... bN
prodC  c1 c2 c3 ....       cN

set with fewest elements 

inp = [ 1,2,3,4,5,7,8,9,10,11 ] 

puz =  [1 , 3 , 5  ,11 , 13,  17,  19,  23 , 29 , 31 , 41 , 43,
       47  ,53,  59,  61,  67,  71,  73 , 79  ,83,  89 , 97 , 101,
       103 , 107,  109,  113 ]

suppose i take A numbers from a given starting list , their sum is target T for other numbers
now two sets B C also sum to target T
so sum sets B union C is 2 T
if 

pick takes 1 or does not take 1 and moves onto rest of list

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
			       (when (= (* 2 sa) sbc)
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


;; fire up solver as soon as load file
(pp (feed 6))

#|
weak constraint - that set values picked must be equal to other 2 sets ,
set A = set B = set C
        know sum (union set B & C ) = sbc : (apply + ys)
        so sa - sum of set A  must be one half of sbc
        therefore if 2 * sa = sbc 
        we can say atleast possible this COULD be a possible solution
        if not , 
(feed 1) ... (feed 5) all yield #f meaning not possible to find a solution for 5 values

(feed 6)

------------------------------------------------

csc -O5 pick.scm

> time ./pick

...
...
 (33583973691 (113 107 103 101 89 3) 5)
 (33144344931 (113 109 107 101 83 3) 4)
 (11403903839 (113 109 103 101 89 1) 3)
 (11377594727 (113 109 107 97 89 1) 2)
 (11266889531 (113 109 107 103 83 1) 1))

real	0m0.302s
user	0m0.298s
sys	0m0.004s


*  11266889531  is proposed solution  *
the weak constraint was absolutely all we needed to solve this problem
- stunning work


|#
