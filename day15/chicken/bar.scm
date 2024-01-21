#|
Aoc 2015 - Day 15: Science for Hungry People
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
Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3

44 butterscotch + 56 cinamon
|#
(define ing `((but (cap -1) (dur -2)(fla 6)(tex 3)(cal 8))
	      (cin (cap 2)(dur 3)(fla -2)(tex -1)(cal 3))))

(define (neg-zero a)
  (cond
   ((< a 0) 0)
   (#t a)))


(define (exf a b)
  (let ((but-cap -1)(but-dur -2)(but-fla 6)(but-tex 3)(but-cal 8)
	(cin-cap 2)(cin-dur 3)(cin-fla -2)(cin-tex -1)(cin-cal 3)
	)
    (let (
	  (cap (neg-zero (+ (* a but-cap) (* b cin-cap))))
	  (dur (neg-zero (+ (* a but-dur) (* b cin-dur))))
	  (fla (neg-zero (+ (* a but-fla) (* b cin-fla))))
	  (tex (neg-zero (+ (* a but-tex) (* b cin-tex))))
	  (cal (neg-zero (+ (* a but-cal) (* b cin-cal))))
	  )
      ;;(let ((val (* cap dur fla tex cal)))

      ;; everything except calories 
      (let ((val (* cap dur fla tex)))
	(cond
	 ((= cal 500) val)
	 (#t 0))))))

(define (ex)
  (let ((best 0)
	(best-sol '())
	)	
    (do-for a (0 101 1)
	    (let ((b (- 100 a)))
	      (let ((val (exf a b)))
		(format #t "a ~a : b ~a : val ~a ~%" a b val)
		(cond
		 ((> val best)
		  (set! best val)
		  (set! best-sol (list 'but a 'cin b))
		  (format #t "new best ~a : ~a ~%" best best-sol))))))))



;; -----------------------------

#|
Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5
PeanutButter: capacity -1, durability 3, flavor 0, texture 0, calories 1
Frosting: capacity 0, durability -1, flavor 4, texture 0, calories 6
Sugar: capacity -1, durability 0, flavor 0, texture 2, calories 8
|#

(define (inf a b c d)
  (let ((spr-cap 5)(spr-dur -1)(spr-fla 0)(spr-tex 0)(spr-cal 5)
	(peb-cap -1)(peb-dur 3)(peb-fla 0)(peb-tex 0)(peb-cal 1)
	(fro-cap 0)(fro-dur -1)(fro-fla 4)(fro-tex 0)(fro-cal 6)
	(sug-cap -1)(sug-dur 0)(sug-fla 0)(sug-tex 2)(sug-cal 8)
	)
    (let (
	  (cap (neg-zero (+ (* a spr-cap) (* b peb-cap) (* c fro-cap) (* d sug-cap))))
	  (dur (neg-zero (+ (* a spr-dur) (* b peb-dur) (* c fro-dur) (* d sug-dur))))
	  (fla (neg-zero (+ (* a spr-fla) (* b peb-fla) (* c fro-fla) (* d sug-fla))))
	  (tex (neg-zero (+ (* a spr-tex) (* b peb-tex) (* c fro-tex) (* d sug-tex))))
	  (cal (neg-zero (+ (* a spr-cal) (* b peb-cal) (* c fro-cal) (* d sug-cal))))	  
	  )
      ;; everything except calories 
      (let ((val (* cap dur fla tex)))
	(cond
	 ((= cal 500) val)
	 (#t 0))))))

 

(define (in)
  (let ((best 0)
	(best-sol '())
	)	
    (do-for a (0 101 1)
	    (do-for b (0 101 1)
		    (do-for c (0 101 1)
			    (do-for d (0 101 1)
				    (when (= (+ a b c d) 100)
				      ;;(format #t "~a ~a ~a ~a : ~a ~%" a b c d (+ a b c d ))
				      (let ((val (inf a b c d)))
					(cond
					 ((> val best)
					  (set! best val)
					  (set! best-sol (list 'spr a 'peb b 'fro c 'sug d))
					  (format #t "new best ~a : ~a ~%" best best-sol)))))))))))

(in)










		    
		    


