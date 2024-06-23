#lang racket
(provide (all-defined-out))

(require math/number-theory)

#|

documentation - static ... how about an executable document ... 

34000000

elf 1 delivers to every house
1 2 3 4 5 6 7 8 9 10 ..

elf 2 delivers to every other house
2 4 6 8 10 12 ...

elf 3 delivers to every third house
3 6 9 12 15 ...

elf N delivers to every N house
N 2N 3N 4N 5N ...



|#


(define *input* 34000000)

(define house
  (lambda (n)
    (apply + (map (lambda (x) (* x 10))
		  (divisors n)))))


(define brute
  (lambda (h)
    (let ((count (house h)))
      (cond
       ((>= count *input*)
	(printf (format "house ~a is the answer with [ ~a ] presents ~%" h count))
	h)
       (#t (brute (+ h 1)))))))

(define run
  (lambda ()
    (time (brute 1))))



#|

terry@debian:~/code/advent-of-code/advent-of-code-2015/day20/racket$ racket
Welcome to Racket v8.13 [cs].
> ,r "review.rkt"
> (run)
house 786240 is the answer with [ 34137600 ] presents 
cpu time: 2563 real time: 2563 gc time: 41
786240
> 

 correct target ...

 target is ............    34137600  ..........


* REVIEW *

what was the mistake that led to the incorrect target ?



|#



