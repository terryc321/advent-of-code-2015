#lang racket
(provide (all-defined-out))


(define *target* 34137600)
;; buggy old target ...
;;(define *target* 896472900)

(define improved-house
  (lambda ( H )
    (let ((count 0))
      (for/list [(i (range 1 51))]
	(let ((h (/ H i)))
	  (cond
	   ((and (integer? h) (zero? (modulo H h)))
            (printf (format "elf ~a delivers ~a ~%" h (* h 11)))
	    (set! count (+ count (* h 11)))))))
      count)))


(define brute
  (lambda (i)
    (cond
     ((zero? (modulo i 1000000))
      (printf (format "at house ~a ~%" i))))
    (let ((p (improved-house i)))
      (cond
       ((>= p *target*)
	(printf (format "solution found at house ~a with ~a presents delivered ~%" i p)))
       (#t
	;;(printf (format "house got ~a presents ~%" p))
	(brute (+ i 1)))))))


(time (brute 1))

#|
elf 18480 delivers 203280 
elf 17325 delivers 190575 
elf 16632 delivers 182952 

solution found at house 831600 with 35780206 presents delivered 
cpu time: 10379 real time: 10874 gc time: 74

       831600 

|#

