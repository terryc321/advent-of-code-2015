

(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...
(use-modules (srfi srfi-2)) ;; first second third ...


(use-modules (rnrs)) ;; assert

;; regular expression
(use-modules (ice-9 regex)) 

;; pattern matcher ?
(use-modules (ice-9 match))

;; binary io -- dont know if this helped any as read-u8 is for reading ints no??
(use-modules (ice-9 binary-ports))

;; r7rs 
(use-modules (scheme base))

;; --------------------- macros --------------------------
(define-macro (dolist varls . body)
  (let* ((fn (gensym "fn"))
	 (xs (gensym "xs"))
	 (var (car varls))
	 (ls  (car (cdr varls))))
    `(letrec ((,fn (lambda (,xs)
		     (cond
		      ((null? ,xs) #f)
		      (#t (let ((,var (car ,xs)))
			    ,@body
			    (,fn (cdr ,xs))))))))
       (,fn ,ls))))

;; --------------------- while ----------------------------

(defmacro while (condition . body)
  (let ((lup (gensym "loop")))
    `(letrec ((,lup (lambda ()
		      (when ,condition
			,@body
			(,lup)))))
       (,lup))))

;; (let ((i 0))
;;   (while (< i 10)
;;     (format #t " i = ~a ~%" i )
;;     (set!  i (+ i 1))))

;; --------------------- macros --------------------------

(define *debug* #f)

(define input #f)

(define (get-input filename)  
  (call-with-port (open-input-file filename)
    (lambda (port)
      (read port))))


;; for example
;;(define input #f)

;;(set! input (get-input "input2"))
(set! input (get-input "input"))


(define (part2)
  (set! input (get-input "input"))
  (set! input (append input
		      `((8 0 0)
			(8 0 1)
			(8 0 2)
			(8 0 3)
			(8 0 4)
			(8 0 5)
			(8 0 6)
			(8 0 7)
			(0 0 8)
			(1 0 8)
			(2 0 8)
			(3 0 8)
			(4 0 8)
			(5 0 8)
			(6 0 8)
			(7 0 8)))))
  




;; speed up using 2 d vector possibly .......
(define (cost a b)
  (call/cc (lambda (exit)
	     (dolist (p input)
		     (when (and (= a (car p))
				(= b (caddr p)))
		       (exit (second p))))
	     (error "cost not found" (list a b)))))

(define (bifrost a b)
  (+ (cost a b)
     (cost b a)))
	   

#|
0 1 2 3 4 5 6 7
start with person 0 sat 

|#

(define best-joy 0)

(define best-table '())


(define (bar ys joy)
  ;;(format #t "ys = ~a : joy = ~a ~%" ys joy)
  (when (>= joy best-joy)
    (set! best-joy joy)
    (set! best-table ys)
    (format #t "*** another best joy ~a : table ~a ~%" best-joy best-table)))

(define (fiz xs ys joy 1st)
  (cond
   ((null? xs)
    ;;(format #t "xs = ~a : ys = ~a : joy = ~a : 1st = ~a  last = ~a~%" xs ys joy 1st (car ys))
    (bar ys (+ joy
	       (cost 1st (car ys))
	       (cost (car ys) 1st)))
	       
    )
   (#t 		    
    (dolist (x xs)
	    (fiz (filter (lambda (y) (not (= x y))) xs)
		 (cons x ys)
		 (+ joy (cost x (car ys)) (cost (car ys) x))
		 1st)))))

		 
(define (foo)
  (fiz '(1 2 3 4 5 6 7) '(0) 0 0))

#|
scheme@(guile-user) [2]> (foo)
*** another best joy 283 : table (6 7 5 4 3 2 1 0) 
*** another best joy 372 : table (5 7 6 4 3 2 1 0) 
*** another best joy 403 : table (4 6 7 5 3 2 1 0) 
*** another best joy 412 : table (5 7 4 6 3 2 1 0) 
*** another best joy 509 : table (4 5 7 6 3 2 1 0) 
*** another best joy 555 : table (4 5 7 6 3 1 2 0) 
*** another best joy 618 : table (5 7 6 3 2 1 4 0) 
*** another best joy 618 : table (4 1 2 3 6 7 5 0) 
$29 = #f

suggests 618 is maximum joy party can feel

(0 4 1 2 3 6 7 5 0)


(bifrost 0 4)  94
(bifrost 4 1)  17  <------ sit between 4 and 1 then change of 17
(bifrost 1 2)  93
(bifrost 2 3)  123
(bifrost 3 6)  90
(bifrost 6 7)  41
(bifrost 7 5)  105
(bifrost 5 0)  55
|#


(define (fuz)
  (part2)
  (fiz '(1 2 3 4 5 6 7 8) '(0) 0 0))

#|

scheme@(guile-user) [2]> (fuz)
*** another best joy 102 : table (8 7 6 5 4 3 2 1 0) 
*** another best joy 369 : table (8 6 7 5 4 3 2 1 0) 
*** another best joy 390 : table (5 7 6 8 4 3 2 1 0) 
*** another best joy 421 : table (4 8 6 7 5 3 2 1 0) 
*** another best joy 475 : table (4 8 5 7 6 3 2 1 0) 
*** another best joy 521 : table (4 8 5 7 6 3 1 2 0) 
*** another best joy 563 : table (8 5 7 6 3 2 1 4 0) 
*** another best joy 577 : table (5 7 8 6 3 2 1 4 0) 
*** another best joy 580 : table (8 2 1 3 6 7 5 4 0) 
*** another best joy 580 : table (8 1 2 3 6 7 5 4 0) 
*** another best joy 601 : table (5 7 6 3 2 1 8 4 0) 
*** another best joy 601 : table (5 7 6 3 1 2 8 4 0) 
*** another best joy 601 : table (4 8 2 1 3 6 7 5 0) 
*** another best joy 601 : table (4 8 1 2 3 6 7 5 0) 
$31 = #f
scheme@(guile-user) [2]>

suggests 601 is new happiness

a change of 17

entered 17 but answer rejected
try 601 then

...... answer accepted


|#






  
