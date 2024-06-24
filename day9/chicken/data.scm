
#|

advent of code 2015 day 9

so if want to build table find distance c1 c2

c1 c2 c3 c4 c5 ... number of cities 
c2 c3 c4 c5 ...
c3 c4 c5 ...
c4 c5 ..
c5 ..

flat grid array
if we translate symbol to int
then assign Tristram as 0 , Snowdin as 1 , Tambi as 2
if want to know distance Tristram -> Tambi  lookup (Tris..0, Tam .. 2)
two d grid 

or we could just have a hash table lookup on list (from to)

c1 + c2 c3 c4 c5 ...
c2 +

|#

(import (chicken pretty-print)) ;; pp pretty-print
(import (chicken format))  ;; format printf 
(import procedural-macros) ;; 
(import expand-full) ;; macro expansion - can use ,x at repl also
(import srfi-69)  ;; hash tables 
(import srfi-1)   ;; first second third ..
(import bindings) ;; provides bind 

;; re-implement dolist , for loops etc.. each time really tyring tye-ring
;; yak a de yak yak yak shaving
;; procedural macros

(import-for-syntax (only bindings bind))
(import-for-syntax (only srfi-1 second))


;;(bind (_ x y) form ...)
;; common lisp pattern matching

;; form = (set-a! v)
(define-syntax set-a!
  (er-macro-transformer
   (lambda (form rename compare?)
     (let ((v (cadr form)))
       `(,(rename 'set!) a ,v)))))

(define-syntax set-b!
  (er-macro-transformer
   (lambda (form rename compare?)
     (let ((v (second form)))
       `(,(rename 'set!) b ,v)))))

(define-syntax set-c!
  (er-macro-transformer
   (lambda (form rename compare?)
     (bind (_ v) form
	   `(,(rename 'set!) c ,v)))))

;; expand macros at repl using comma x
;; >,x (set-a! 10)
;; (##core#set! a 10)
(expand* '(set-a! 10))

(pp (expand '(set-a! 10)))

(let ((a 5))
  (list a (set-a! 10) a))

(define-syntax dolist
  (er-macro-transformer
   (lambda (form rename compare?)
     (bind (_ (v ys) . body) form
	   (let ((%tmp (rename 'tmp))
		 (%set (rename 'set!))
		 (%fun (rename 'fun))
		 (%lambda (rename 'lambda))
		 (%let (rename 'let))
		 (%letrec (rename 'letrec))
		 (%xs (rename 'xs))
		 (%cond (rename 'cond))
		 (%null? (rename 'null?))
		 (%cdr (rename 'cdr))
		 (%car (rename 'car))
		 )
	     `(,%letrec
	       ((,%fun
		 (,%lambda (,%xs)
			  (,%cond
			   ((,%null? ,%xs) #f)
			   (#t (,%let ((,v (,%car ,%xs)))
				     ,@body
				     (,%fun (,%cdr ,%xs))))))))
	       (,%fun ,ys)))))))

(pp (expand* 
     '(dolist (x '(1 2 3))
	      (format #t "hello world ~a~%" x))))



;; implicit renaming introduction
(define-syntax swap!
  (ir-macro-transformer
    (lambda (form inject compare?)
      (let ((x (cadr form)) (y (caddr form)))
        `(let ((tmp ,x))
            (set! ,x ,y)
            (set! ,y tmp))))))


;; with implicit renaming - slightly different
;; rename has become inject

(define-syntax dolist2
  (ir-macro-transformer
   (lambda (form inject compare?)
     (bind (_ (v ys) . body) form
	   `(letrec
	       ((fun
		 (lambda (xs)
			  (cond
			   ((null? xs) #f)
			   (#t (let ((,v (car xs)))
				     ,@body
				     (fun (cdr xs))))))))
	       (fun ,ys))))))

(pp (expand* 
     '(dolist2 (x '(1 2 3))
	       (format #t "hello world ~a~%" x))))
					 

(define table '((Tristram AlphaCentauri 34)
		(Tristram Snowdin 100)
		(Tristram Tambi 63)
		(Tristram Faerun 108)
		(Tristram Norrath 111)
		(Tristram Straylight 89)
		(Tristram Arbre 132)
		(AlphaCentauri Snowdin 4)
		(AlphaCentauri Tambi 79)
		(AlphaCentauri Faerun 44)
		(AlphaCentauri Norrath 147)
		(AlphaCentauri Straylight 133)
		(AlphaCentauri Arbre 74)
		(Snowdin Tambi 105)
		(Snowdin Faerun 95)
		(Snowdin Norrath 48)
		(Snowdin Straylight 88)
		(Snowdin Arbre 7)
		(Tambi Faerun 68)
		(Tambi Norrath 134)
		(Tambi Straylight 107)
		(Tambi Arbre 40)
		(Faerun Norrath 11)
		(Faerun Straylight 66)
		(Faerun Arbre 144)
		(Norrath Straylight 115)
		(Norrath Arbre 135)
		(Straylight Arbre 127)))


(define cities (let ((known '()))
		 (dolist (t table)
			 (bind (c1 c2 _) t
			       (when (not (member c1 known))
				 (set! known (cons c1 known)))
			       (when (not (member c2 known))
				 (set! known (cons c2 known)))))
		 known))

(define distance 
  (let ((hash (make-hash-table)))
    (dolist (t table)
	    (bind (c1 c2 d1) t
		  (hash-table-set! hash (list c1 c2) d1)
		  (hash-table-set! hash (list c2 c1) d1)))
    (lambda (c1 c2)
      (hash-table-ref hash (list c1 c2)))))


(define without
  (lambda (x ys)
    (without2 x ys '())))

(define without2
  (lambda (x ys zs)
    (cond
     ((null? ys) zs)
     ((eq? x (car ys)) (without2 x (cdr ys) zs))
     (#t (without2 x (cdr ys) (cons (car ys) zs))))))

(without 1 '())
(without 1 '(1))
(without 1 '(1 1 ))
(without 1 '(1 2 1 3 ))


(define best #f)
(define best-longest #f)
(define best-shortest #f)

(define end-game-shortest (lambda (n)
			    (cond
			     (best-shortest (cond
				    ((< n best-shortest)
				     (set! best-shortest n)
				     (format #t "new best ~a ~%" best-shortest))))
			     (#t (set! best-shortest n)))))


(define end-game-longest (lambda (n)
		   (cond
		    (best-longest (cond
			   ((> n best-longest)
			    (format #t "new best ~a .. cf ~a ~%" n best-longest)
			    (set! best-longest n))))
		    (#t (set! best-longest n)))))


(define search-shortest
      (lambda (cities visited dist)
	(cond
	 ((null? cities)
	  (format #t "dist = ~a : visited ~a ~%" dist visited)
	  (end-game-shortest dist))
	 (#t
	  (dolist (city cities)
		  (cond
		   ((null? visited) ;;bootstrap first city picked - no distance to add here
		    (search-shortest (without city cities) (cons city visited) 0))
		   ((member city visited)
		    (error "violation ")))
		  ;; free to choose this city
		  ;; free not to choose this city
		  (when (not (null? visited))
		    (search-shortest (without city cities)
			    (cons city visited)
			    (+ dist (distance city (first visited)))))
		  )))))



(define search-longest
      (lambda (cities visited dist)
	(cond
	 ((null? cities)
	  (format #t "dist = ~a : visited ~a ~%" dist visited)
	  (end-game-longest dist))
	 (#t
	  (dolist (city cities)
		  (cond
		   ((null? visited) ;;bootstrap first city picked - no distance to add here
		    (search-longest (without city cities) (cons city visited) 0))
		   ((member city visited)
		    (error "violation ")))
		  ;; free to choose this city
		  ;; free not to choose this city
		  (when (not (null? visited))
		    (search-longest (without city cities)
			    (cons city visited)
			    (+ dist (distance city (first visited)))))
		  ;; choose not to take 1st city
		  
		  )))))




(define run-shortest
  (lambda ()
    (set! best-shortest #f)
    (search-shortest cities '() 0)))

(define run-longest
  (lambda ()
    (set! best-longest #f)
    (search-longest cities '() 0)))


(run-shortest)
(run-longest)

(format #t "~%~%")
(format #t "best shortest ~a ~%" best-shortest)
(format #t "best longest ~a ~%" best-longest)
(format #t "~%~%")

#|

...
dist = 667 : visited (Tambi Norrath Faerun Snowdin Straylight AlphaCentauri Arbre Tristram) 
dist = 724 : visited (Norrath Tambi Faerun Snowdin Straylight AlphaCentauri Arbre Tristram) 
dist = 554 : visited (Tambi Faerun Norrath Snowdin Straylight AlphaCentauri Arbre Tristram) 
dist = 677 : visited (Faerun Tambi Norrath Snowdin Straylight AlphaCentauri Arbre Tristram) 
...

best shortest 251 
best longest 898 

real	0m0.290s
user	0m0.179s
sys	0m0.077s
|#











