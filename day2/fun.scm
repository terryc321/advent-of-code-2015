

(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...
(use-modules (srfi srfi-2)) ;; first second third ...

;; regular expression
(use-modules (ice-9 regex)) 

;; pattern matcher ?
(use-modules (ice-9 match))

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
  (let ((port (open-input-file filename)))
    (set! input (read port))
    (close-input-port port)))

;; for example
(define input #f)

(get-input "input")
;;(get-input "input2")

(define wrap 0)

;; destructuring-bind ?
;; using match from ice-9 
(define (foo)
  (set! wrap 0)
  (dolist (in input)
	  (match in           ;; <- the input object
	    ((len wid hgt)  ;; <- the pattern
	     (set! wrap (+ wrap (gift len wid hgt)))
	     (format #t "wrap = ~a ~%" wrap)
	     (format #t "vals ~a ~a ~a ~%" len wid hgt)))))


;; (define (bar)
;;   (set! wrap 0)
;;   (dolist (in input)
;; 	  (let ((l (first in))
;; 		(w (second in))
;; 		(h (third in)))	    
;; 	    (format #t "vals ~a ~a ~a ~%" l w h))))


(define (gift l w h)
  (let ((m (min (* l w) (* w h) (* h l)))
	(s (+ (* 2 l w) (* 2 w h) (* 2 h l))))
    (format #t " smallest = ~a : wrap = ~a : tot ~a ~%" m s (+ m s))
    (+ m s)))


#|
A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet of wrapping paper plus 6 square feet of slack, for a total of 58 square feet.

A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet of wrapping paper plus 1 square foot of slack, for a total of 43 square feet.


1606483 

|#

(define ribbon 0)

(define (baz)
  (set! ribbon 0)
  (dolist (in input)
	  (match (sort in <)           ;; <- the input object
	    ((a b c)  ;; <- the pattern
	     (set! ribbon (+ ribbon (+ (* a b c) (* 2 a) (* 2 b))))
	     (format #t "ribbon = ~a ~%" ribbon)))))


#|

completed day 2 aoc 2015

3842356
|#















