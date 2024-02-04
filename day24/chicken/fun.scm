#|
Aoc 2015 - Day 24: 

given a set of integers , find a number called then sum such that
the integers can be split into 3 groups add up to sum

one group containing least number of elements should add to sum

list of integers are these 
(1 3 5 11 13 17 19 23 29 31 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113)

all integers from list must be used
sum >= 113

thought was need to be balanced
if one group has 113 then other groups need to have two sets numbers that add to 113





|#

(import scheme)
(import simple-exceptions)
(import (chicken repl))
(import (chicken string))
(import (chicken pretty-print))
(import (chicken io))
(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day17")
;; (get-current-directory)
(import procedural-macros)
(import regex)
(import simple-md5)
(import simple-loops)
(import srfi-69)
;; hash-table-ref  hash key thunk
;; hash-table-set! hash key val
;; sudo chicken-install srfi-178
(import srfi-178)
;; srfi-178 provides bit-vectors
;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))
(import sequences)
(import srfi-1)
(import matchable)
(define pp pretty-print)

;;--------------------------------------
#|

given a set of positive integers
split set into 3 groups , each group adds up to the same value

algorithm to do this ?

how efficient is algorithm ?
how rate it or compare it against another ?
how coded ?


take 113 as example
if 113 is in group 1
group2 has to make 113
group3 has to make 113 


|#

(define output #t)


(define input '(1 3 5 11 13 17 19 23 29 31 41 43 47 53 59 61 67 71 73
79 83 89 97 101 103 107 109 113 ))

;; 1 thru 5
;; 7 thru 11
;; really 1 thru 11 except 6
(define example  '(1 2 3 4 5 7 8 9 10 11))


#|
split packages into 3 equal groups

1st group needs least number of packages

smallest quantum entanglement for group 1
qe = product of all elements of group 1

each item of list goes into group 1 , 2 or 3 

|#

(define sol #f)
(define best-qe #f)
(define best #f)

(define (answer? a b c)
  ;;(format #t "answer? ...~%")
  (let ((qe (apply * a))
	(sa (apply + a))
	(sb (apply + b))
	(sc (apply + c))
	(la (length a))
	(lb (length b))
	(lc (length c)))
    (let ((least (and (<= la lb) (<= la lc))))
      (cond
       ((and least (= sa sb sc))

	;; update Q.E. quantum entangleement
	(cond
	 ((not best-qe)
	  (set! best-qe qe)
	  (set! best (list a b c sa least qe)))
	 ((< qe best-qe)
	  (set! best-qe qe)
	  (set! best (list a b c sa least qe))))
	  
	(format #t "[~a] [~a] [~a] : sum (~a) : least (~a) : quantum (~a) ~%" a b c sa least qe))))))

   

;; brute force search fails on larger input 
(define (brute xs a b c)  
  (cond
   ((null? xs) (answer? a b c))
   (#t (let ((p (car xs))
	     (r (cdr xs)))
	 (brute r (cons p a) b c)
	 (brute r a (cons p b) c)
	 (brute r a b (cons p c))))))

	 
(define (run xs)
  (let ((nil '()))
    (set! sol (make-hash-table))
    (brute xs nil nil nil)
    (format #t "~%")
    (format #t "~a~%" best-qe)
    (format #t "~a ~%" best)))

    

;; without procedure that returns a list with some numbers removed - ie without 'them'
#|

BUG - 001 in description of problem 

Of these, although 10 9 1 has the smallest quantum entanglement (90),
                                                              ^^^^^^^^^ FALSE
*** (11 8 1) has quantum entanglement of 88 .
the configuration with only two packages, 11 9, in the passenger
compartment gives Santa the most legroom and wins. In this situation,
the quantum entanglement for the ideal configuration is therefore
99. Had there been two configurations with only two packages in the
first group, the one with the smaller quantum entanglement would be
chosen.

What is the quantum entanglement of the first group of packages in the
ideal configuration?

|#

#|

brute force recursion and putting one item into three boxes too slow for larger inputs

if think of a total number T
can we make T three ways using different numbers of set numbers given


(define (make-t xs t ys zs)
  (cond
   ((null? xs)
    (cond
     ((null? ys) #f)
     ((= t (apply + ys))
      (make-t2 zs t '() '() ys))))
   (


what group of numbers add up to 113  ?

|#

(define input '(1 3 5 11 13 17 19 23 29 31 41 43 47 53 59 61 67 71 73
79 83 89 97 101 103 107 109 113 ))

(define counter 0)

(define (sums-to n xs)
  ;; ---------------------------------
  (define (sums-to2 n tot xs ys)
    (cond
     ((null? xs)
      (cond
       ((null? ys) #f)
       ((> tot n) #f)
       ((= n tot)
	(set! counter (+ 1 counter))
	(format output "~a = ~a ~%" counter ys))
       (#t #f)))
     (#t (let ((num (car xs))
	       (tail (cdr xs)))
	   ;; use num in tot
	   (sums-to2 n (+ tot num) tail (cons num ys))
	   ;; do not use num
	   (sums-to2 n tot tail ys)))))
  ;;---------------------------------------
  ;; entry sums-to
  (sums-to2 n 0 xs '()))




(define (hack xs)  
  (do-list (num xs)
	   (format output "searching for solutions for num ~a ~%" num)
	   (set! counter 0)
	   (sums-to num xs)))



#|
(with-output-to-file "results.txt"
  (lambda (port)
    (set! output port)
    (hack)))
|#

#|

(call-with-output-file "results.txt"
  (lambda (port)
    (format port "hello world~%")))

|#

(define (pass-1)
  (call-with-output-file "results-example.txt"
    (lambda (port)
      (set! output port)
      (hack example))))

(define (pass-2)
  (call-with-output-file "results.txt"
    (lambda (port)
      (set! output port)
      (hack input))))

;; 
;; not sure on documentation on how it supposed to work
;; almost like after thought , seems broken to be honest
;;(pass-1)

























