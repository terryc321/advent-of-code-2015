#|
Aoc 2015 - Day 20: Science for Hungry People

Day 20: Infinite Elves and Infinite Houses ---

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

(define input 34000000)

#|

some sample data

House 1 got 10 presents.
House 2 got 30 presents.
House 3 got 40 presents.
House 4 got 70 presents.
House 5 got 60 presents.
House 6 got 120 presents.
House 7 got 80 presents.
House 8 got 150 presents.
House 9 got 130 presents.

1 2 3 4 5 6 7 8 9 10-11-12
1 1 1 1 1 1 1 1 1 1 1 1
0 2 0 2 0 2 0 2 0 2 0 2 
0 0 3 0 0 3 0 0 3 0 0 3 
0 0 0 4 0 0 0 4 0 0 0 4
0 0 0 0 5 0 0 0 0 5 0 0
0 0 0 0 0 6 0 0 0 0 0 6
0 0 0 0 0 0 7 0 0 0 7 0
0 0 0 0 0 0 0 8 0 0 0 0
0 0 0 0 0 0 0 0 9 0 0 0
0 0 0 0 0 0 0 0 0 10 0 0
0 0 0 0 0 0 0 0 0 0 11 0
0 0 0 0 0 0 0 0 0 0 0 12

1  2  3  4   5  6   7  8
10 30 40 70 60 120 80 

|#

;; (list
;; (modulo 6 1)
;; (modulo 6 2)
;; (modulo 6 3)
;; (modulo 6 4)
;; (modulo 6 5)
;; (modulo 6 6)
;; (modulo 6 7)
;; (modulo 6 8)
;; (modulo 6 9))

(define (pr n)
  (let ((sum 0))
    (do-for i (1 (+ n 1) 1)
	    (cond
	     ((= 0 (modulo n i))
	      (set! sum (+ sum i)))))
    (* sum 10)))

;; (map pr '(1 2 3 4 5 6 7 8 9 10))
;;  (10 30 40 70 60 120 80 150 130 180)

#|
House 1 got 10 presents.
House 2 got 30 presents.
House 3 got 40 presents.
House 4 got 70 presents.
House 5 got 60 presents.
House 6 got 120 presents.
House 7 got 80 presents.
House 8 got 150 presents.
House 9 got 130 presents.
|#

(define (seek)
  (let ((most 0)
	(most-i 0))
    (call/cc (lambda (done)
	       (let ((i 20))
		 (do-while #t
			   (let ((val (pr i)))
			     (cond
			      ((> val most)
			       (set! most-i i)
			       (set! most val)
			       (format #t "highest so far ~a at house number ~a ~%" most most-i)))
			     (cond
			      ((>= val input) (format #t "solution : house number ~a ~%" i)
			       (done #t))))
			   (set! i (+ i 20))))))))


#|
highest so far 33611760 at house number 776160 
highest so far 34137600 at house number 786240 
solution : house number 786240 

real	6m23.159s
user	6m22.001s
sys	0m1.149s

observed highest so far fell on multiple of 20 houses
so started at 20 and counted up in 20

accepted !
 good guessing ...



|#









