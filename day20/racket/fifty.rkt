#lang racket
(provide (all-defined-out))


#|

given a house number
given a elf starting house ,

if elf delivers to 50 houses
h , h + h , h + h + h , h + h + h + h ....
h , 2 h , 3 h , 4 h , 5 h , 6 h ... 50 h

elf 1 will deliver to
1 2 3 4 5 ... 50

elf2 will deliver to
2 4 6 8 10 12 ... ??

elf3 will deliver to
3 6 9 12 15 ... ??

pattern is ELF < n >  will deliver to
     (map (lambda (h) (* h elf-n)) (range 1 51 ))


elf 5 will deliver to 5 .. 50 * 5  every 5 th house

5 10 15 20 25 ... 250

given a house number H
given a elf number   E
does elf E deliver to house H 

H modulo E == 0  and  E <= H <= 50 * E

says
House H is visited by elf E when
H mod E is zero and that
house number H is greater or equal to E  :  H >= E
and that house H is less than or equal to 50 * E  : H <= 50 * E

Q . E . D 
-----------------

|#

(define elf-visit (h e)
  (cond
   ((and (zero? (modulo h e))
	 (>= h e)
	 (<= h (* 50 e)))
    (* 11 h))
   (#t #f)))


(define elf
  (lambda (n)
    (map (lambda (h) (* h n))
	 (range 1 51))))


(define improved-house
  (lambda ( H )
    (let ((count 0))
      (for/list [(i (range 1 51))]
	(let ((h (/ H i)))
	  (cond
	   ((and (integer? h) (zero? (modulo H h)))
	    (set! count (+ count (* h 11)))))))
      count)))


#|

if elf delivers to 50 houses
h , h + h , h + h + h , h + h + h + h ....
h , 2 h , 3 h , 4 h , 5 h , 6 h ... 50 h

last house elf delivers to is 50 * h
where h is elf number of first house delivers to

target house H

lowest elf need consider is  50 h = H 
or h = H / 50

next elf need consider is 49 h = H

...

last elf need consider is h = H

                                                    last elf
                                                       h = H
                                                       H
 first elf
  h = H / 50
           second elf
             h = H / 49
                   third elf 
                   h = H / 48
                        ....
                                         next to last elf
                                             h = H / 2

at each house visit
they deliver  11 * h  presents

where
h : elf number first house that elf visits
H : target house


|#

