
;; -------- preamble ---------------
(import (chicken format))
(import (chicken sort))


;; (import scheme)
;; (import expand-full)
;; (import simple-exceptions)
;; (import (chicken repl))
;; (import (chicken string))
;; (import (chicken pretty-print))
;; (import (chicken io))
;; (import (chicken sort))
;; (import (chicken file))
;; (import (chicken process-context))
;; ;; (change-directory "day17")
;; ;; (get-current-directory)
;; (import procedural-macros)
;; (import regex)
;; (import simple-md5)
;; (import simple-loops)
;; ;; hash-table-ref  hash key thunk
;; ;; hash-table-set! hash key val
;; ;; sudo chicken-install srfi-178
;; (import srfi-178)
;; ;; srfi-178 provides bit-vectors
;; ;; (import-for-syntax
;; ;;   (only checks <<)
;; ;;   (only bindings bind bind-case)
;; ;;   (only procedural-macros macro-rules with-renamed-symbols once-only))
;; (import sequences)
;; (import srfi-1)
;; (import matchable)
;; (define pp pretty-print)
;; (import srfi-69) ;; hash tables

;; ;; ------------ macros ---------------------------------------
;; ;; dolist
;; (define-macro (dolist varlist . body)
;;   (let ((var (car varlist))
;; 	(ls (cadr varlist))
;; 	(fn (gensym "fn")))	
;;     `(begin
;;        (letrec
;; 	   ((,fn (lambda (xs)
;; 		   (cond
;; 		    ((null? xs) #f)
;; 		    (#t (let ((,var (car xs)))
;; 			  ,@body
;; 			  (,fn (cdr xs))))))))
;; 	 (,fn ,ls)))))
;; 
;; ;; dofor
;; ;; cannot handle decreasing steps ?
;; (define-macro (for v . body)
;;   (let ((var (car v))
;; 	(init (cadr v))
;; 	(lim (caddr v))
;; 	(step (cadddr v))	      
;; 	(foo (gensym "foo"))
;; 	(v-i (gensym "i"))
;; 	(v-step (gensym "step"))
;; 	(v-lim (gensym "lim")))
;;     `(begin
;;        (letrec ;; want to capture var
;; 	   ((,foo (lambda (,var ,v-step ,v-lim)
;; 		    (cond
;; 		     ((> ,var ,v-lim) #f)
;; 		     (#t
;; 		      ,@body
;; 		      (,foo (+ ,var ,v-step) ,v-step ,v-lim))))))
;; 	 (,foo ,init ,step ,lim)))))
;; 
;; ;;(pp (expand* '(for (i 1 10 1) (format #t "i = ~A ~%" i))))
;; ;; (for (i 1 10 1) (format #t "i = ~A ~%" i))
;; ;; (for (i 10 1 -1) (format #t "i = ~A ~%" i))

#|

1 through 5 , 7 through 11
1 2 3 4 5 and 7 8 9 10 11

split into three groups all add up to same value

xs are choice of values
as go into a's
bs go into b's
cs go into c's
a - sums 
b
c

|#

(define *qe* 154614303323) ;; was #f , but now we have some data
(define *qe-n* #f)
(define *qe-min* #f)

(define *min-count* #f)


;; iterate split2 - find a quantum entanglement min value
;; then search upwards if there is a larger quantum entanglement value that yields smaller number
;; values in as list
(define split2 (lambda (xs
			as a na
			bs b nb
			cs c nc)
		 (let ((min-count (min na nb nc))
		       (qea (apply * as))
		       (qeb (apply * bs))
		       (qec (apply * cs)))
		   (cond
		    ((and *qe* (> (min qea qeb qec) *qe*)) #f)
		    ((and *min-count* (> min-count *min-count*)) #f)
		    ((> min-count 6) #f)
		    ;; null --- no more integers to choose where they go
		    ((null? xs)
		     (cond
		      ((= a b c)
		       (when (not *min-count*)
			 (set! *min-count* min-count))
		       (let* ((sorted (sort (list as bs cs)
					    (lambda (x y) (and (< (length x) (length y))
							       (< (apply * x) (apply * y))))))
			      (qe (apply * (car sorted))))
			 (when (not *qe*)
			   (set! *qe* qe))
			 (when (<= qe *qe*)
			   (set! *qe* qe)
			   (format #t "solution ~a : ~a ~%" qe (map (lambda (x) (list x (apply * x))) sorted)))))))
		    ;; not null - still can choose where to put integer into as bs or cs
		    (#t (let ((v (car xs))
			      (r (cdr xs)))
			  (split2 r
				  (cons v as) (+ a v) (+ na 1)
				  bs b nb
				  cs c nc)		       
			  (split2 r
				  as a na
				  (cons v bs) (+ b v) (+ nb 1)
				  cs c nc)
			  (split2 r
				  as a na
				  bs b nb
				  (cons v cs) (+ c v) (+ nc 1))))))))



(define split (lambda (xs)
		(let ((as '())
		      (bs '())
		      (cs '())
		      (a 0)
		      (b 0)
		      (c 0)
		      (an 0) ;; number parcels next to santa
		      (bn 0) 
		      (cn 0)
		      )
		  (split2 xs
			  as a an
			  bs b bn
			  cs c cn))))


(define run (lambda ()
	      (set! *qe* #f)
	      (set! *qe-n* #f)	      
	      (split '(1 2 3 4 5
			 7 8 9 10 11))))


(define run2 (lambda ()
	       (set! *qe* 154614303323)
	       (set! *qe-n* #f)	      
	       (split '(
			1 3 5 11 13 17 19 23 29 31 41 43 47 53 59 61
			  67 71 73 79 83 89 97 101 103 107 109 113
			))))




#|
;; for puzzle 28 numbers all odd
;;
|#
(define puz '(
			1 3 5 11 13 17 19 23 29 31 41 43 47 53 59 61
			  67 71 73 79 83 89 97 101 103 107 109 113
			  ))


		     
(run2)

#|
solution qe[2004372679702965] qe-n[12] as[(113 107 103 101 23 19 17 13 11 5 3 1)] bs[(109 97 89 83 79 59)] cs[(73 71 67 61 53 47 43 41 31 29)] 
solution qe[2004372679702965] qe-n[12] as[(113 107 103 101 23 19 17 13 11 5 3 1)] bs[(109 97 89 83 71 67)] cs[(79 73 61 59 53 47 43 41 31 29)] 
(* 109 97 89 83 71 67)
371534786507
109003762571

2004372679702965 tried ... REJECTED .. huh ...

(* 113 109 107 103 73 11)   109003762571
(* 109  97  89  83 79 59)   364036922411

(* 113 107 103 101 23 19 17 13 11 5 3 1) 2004372679702965
(* 109 97 89 83 71 67)                       371534786507
(* 79 73 61 59 53 47 43 41 31 29)       81944069798388011


;; ways cut search space down ? 
((> qe-n 12) #f)
		  ;; cut search if number parcels by santas legs is too large
		  ;; ?? does not cut search bounces around 14 and 16
		  ((and *qe-n* (> qe-n *qe-n*)) #f )
		  ;; stop search if a has greater qe?
		  ;;((and *qe* (> a *qe*)) #f)

solution 317793762227 : (((113 109 107 67 61 59) 317793762227) ((103 97 89 83 73 71) 382523628611) ((101 79 53 47 43 41 31 29 23 19 17 13 11 5 3 1) 501986284728763256565)) 
solution 317730649919 : (((113 109 101 79 61 53) 317730649919) ((103 97 89 83 73 71) 382523628611) ((107 67 59 47 43 41 31 29 23 19 17 13 11 5 3 1) 502085996585399228145)) 
solution 311040745271 : (((113 109 107 73 61 53) 311040745271) ((101 97 89 83 79 67) 383056733627) ((103 71 59 47 43 41 31 29 23 19 17 13 11 5 3 1) 512171138656580353665)) 
solution 309475353107 : (((113 109 101 79 67 47) 309475353107) ((103 97 89 83 73 71) 382523628611) ((107 61 59 53 43 41 31 29 23 19 17 13 11 5 3 1) 515479208307588346965)) 
solution 309044652251 : (((113 109 103 73 71 47) 309044652251) ((101 97 89 83 79 67) 383056733627) ((107 61 59 53 43 41 31 29 23 19 17 13 11 5 3 1) 515479208307588346965)) 
solution 298499428067 : (((113 109 107 79 61 47) 298499428067) ((103 97 89 83 73 71) 382523628611) ((101 67 59 53 43 41 31 29 23 19 17 13 11 5 3 1) 534433553334985497765)) 
solution 290263857203 : (((109 107 103 97 53 47) 290263857203) ((113 101 83 79 73 67) 366018185531) ((89 71 61 59 43 41 31 29 23 19 17 13 11 5 3 1) 574380729553103643885)) 
solution 286922827571 : (((113 109 107 83 61 43) 286922827571) ((103 101 89 79 73 71) 379102724219) ((97 67 59 53 47 41 31 29 23 19 17 13 11 5 3 1) 561013716245498246445)) 
solution 286452515243 : (((113 109 103 89 59 43) 286452515243) ((107 101 97 73 71 67) 364026413819) ((83 79 61 53 47 41 31 29 23 19 17 13 11 5 3 1) 585207589127729304165)) 
solution 275006130071 : (((113 109 101 97 53 43) 275006130071) ((107 103 83 79 73 71) 374547924551) ((89 67 61 59 47 41 31 29 23 19 17 13 11 5 3 1) 592441833397551056205)) 
solution 258360887003 : (((113 109 107 97 47 43) 258360887003) ((103 101 89 79 73 71) 379102724219) ((83 67 61 59 53 41 31 29 23 19 17 13 11 5 3 1) 623034096370027993365)) 
solution 239319593291 : (((113 109 107 103 43 41) 239319593291) ((101 97 89 83 79 67) 383056733627) ((73 71 61 59 53 47 31 29 23 19 17 13 11 5 3 1) 665662558102092487365)) 
solution 237855034643 : (((113 107 103 101 61 31) 237855034643) ((109 97 89 83 71 67) 371534786507) ((79 73 59 53 47 43 41 29 23 19 17 13 11 5 3 1) 690531756092872355805)) 
solution 233815963547 : (((113 109 107 97 59 31) 233815963547) ((103 101 89 79 73 71) 379102724219) ((83 67 61 53 47 43 41 29 23 19 17 13 11 5 3 1) 688437347601873468885)) 
solution 223030114451 : (((113 109 107 103 53 31) 223030114451) ((101 97 89 83 79 67) 383056733627) ((73 71 61 59 47 43 41 29 23 19 17 13 11 5 3 1) 714280639034685973965)) 
solution 196998578363 : (((113 109 107 97 67 23) 196998578363) ((103 101 89 79 73 71) 379102724219) ((83 61 59 53 47 43 41 31 29 19 17 13 11 5 3 1) 817100524830516920565)) 
solution 190451156771 : (((113 109 107 103 61 23) 190451156771) ((101 97 89 83 79 67) 383056733627) ((73 71 59 53 47 43 41 31 29 19 17 13 11 5 3 1) 836466920836770531165)) 
solution 169448799587 : (((113 109 107 101 67 19) 169448799587) ((103 97 89 83 73 71) 382523628611) ((79 61 59 53 47 43 41 31 29 23 17 13 11 5 3 1) 941453172870671683365)) 
solution 158647135463 : (((113 109 107 97 73 17) 158647135463) ((103 101 89 83 79 61) 370325505059) ((71 67 59 53 47 43 41 31 29 23 19 13 11 5 3 1) 1038674879137359700665)) 
solution 154614303323 : (((113 109 107 103 67 17) 154614303323) ((83 79 73 71 61 59 47 43) 247192080438049) ((101 97 89 53 41 31 29 23 19 13 11 5 3 1) 1596655665056524215)) 
solution 125293241411 : (((113 109 107 103 71 13) 125293241411) ((101 97 89 83 79 67) 383056733627) ((73 61 59 53 47 43 41 31 29 23 19 17 11 5 3 1) 1271465969592620861565)) 
solution 109003762571 : (((113 109 107 103 73 11) 109003762571) ((101 97 89 83 79 67) 383056733627) ((71 61 59 53 47 43 41 31 29 23 19 17 13 5 3 1) 1461473337402227964165)) 

nope any ideas ......


|#
