

#|
--- Part Two ---

The Elves decide they don't want to visit an infinite number of
houses.

Instead, each Elf will stop after delivering presents to 50 houses.

To make up for it, they decide to deliver presents equal to eleven times their number at each house.

With these changes, what is the new lowest house number of the house
to get at least as many presents as the number in your puzzle input?

|#

(import (chicken format))

(define elf
  (lambda (n hos fun)
    (letrec ((foo (lambda (i h)
		    (cond
		     ((> i 50) #f) ;; stop after 50 houses
		     ((> h hos) #f) ;; gone past the house 
		     (#t
		      (let ((p (* h 11))) 
			(format #t "elf ~a at house ~a " n h)
		       	(cond
			 ((= h hos)
			  (format #t " ... delivers ~a presents ~%" p)
			  (fun h p))
			 (#t
			  (format #t " ...~%")))
			(foo (+ i 1) (+ n n))))))))
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

	


