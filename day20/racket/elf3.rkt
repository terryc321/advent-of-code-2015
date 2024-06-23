#lang racket
(provide (all-defined-out))



(define *target* 896472900)

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

;; shows bug in house routine
;; as elf 1 should deliver 11 presents
;;    elf 2 should deliver 22 presents
;;    house 2 gets 33 presents in total

;; (improved-house 2)


#|

896472900 is the target number of presents 

|#

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


;;(time (brute 1))


#|
> ,r "elf3.rkt"
at house 1000000 
at house 2000000 
at house 3000000 
at house 4000000 
at house 5000000 
at house 6000000 
at house 7000000 
at house 8000000 
at house 9000000 
at house 10000000 
at house 11000000 
at house 12000000 
at house 13000000 
at house 14000000 
at house 15000000 
at house 16000000 
at house 17000000 
at house 18000000 
at house 19000000 
at house 20000000
                                      896472900
solution found at house 20540520 with 901087495 presents delivered 
cpu time: 32036 real time: 32037 gc time: 117
>

........ REJECTED .... 20540520



|#

