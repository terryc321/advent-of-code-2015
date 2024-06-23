#lang racket
(provide (all-defined-out))


#|
--- Part Two ---

The Elves decide they don't want to visit an infinite number of
houses.

Instead, each Elf will stop after delivering presents to 50 houses.

To make up for it, they decide to deliver presents equal to eleven times their number at each house.

With these changes, what is the new lowest house number of the house
to get at least as many presents as the number in your puzzle input?

|#

(define elf
  (lambda (n hos fun)
    (letrec ((foo (lambda (i h)
		    (cond
		     ((> i 50) #f) ;; stop after 50 houses
		     ((> h hos) #f) ;; gone past the house 
		     (#t
		      (let ((p (* n 11))) 
			;;(printf (format "elf ~a at house ~a " n h))
		       	(cond
			 ((= h hos)
			  ;;(printf (format " ... delivers ~a presents ~%" p))
			  (fun h p))
			 (#t
			  ;;(printf (format " ...~%"))
                          #t
                          ))
			(foo (+ i 1) (+ h n))))))))
      (foo 1 n))))


;; given a house number
;; look for elfs from 1 to n , until i > n 
;; tell elf if he reaches house < n > to tell me
(define house
  (lambda (n)
    (let ((count 0))
      (letrec ((foo (lambda (i)
		      (cond
		       ((> i n) #f)
		       (#t (elf i n (lambda (h p)
				      (set! count (+ count p))))
			   (foo (+ i 1)))))))
	(foo 1)
	count))))

;; small verification improved-house 
(define improved-house
  (lambda ( H )
    (let ((count 0))
      (for/list [(i (range 1 51))]
	(let ((h (/ H i)))
	  (cond
	   ((and (integer? h) (zero? (modulo H h)))
	    (set! count (+ count (* h 11)))))))
      count)))


;; shows bug in house routine
;; as elf 1 should deliver 11 presents
;;    elf 2 should deliver 22 presents
;;    house 2 gets 33 presents in total

(define (check)
  (call/cc (lambda (exit)
             (for ([h (range 1 101 )])
               (let* ((a (house h))
                      (b (improved-house h))
                      (c (= a b)))
                 (printf (format "house ~a : ~a vs ~a : agree? ~a ~%" h a b c))
                 (when (not c)
                   (exit #f))))
             #t)))

  
(define (check-forever)
  (call/cc (lambda (exit)
             (letrec ((foo (lambda (h)
                             (let* ((a (house h))
                                    (b (improved-house h))
                                    (c (= a b)))
                               ;;(printf (format "house ~a : ~a vs ~a : agree? ~a ~%" h a b c))
                               (when (zero? (modulo h 1000))
                                 (printf (format "at house ~a ...~%" h)))
                               (when (not c)
                                 (exit #f)))
                             (foo (+ h 1)))))
               (foo 1))
             #t)))





896472900

