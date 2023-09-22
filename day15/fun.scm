

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

(set! input (get-input "input2"))
;;(set! input (get-input "input"))

#|
100 teasponns exactly
a sprinkles
b peanut
c frosting
d sugar
|#
(define tot 0)
(define best-sum 0)
(define best-sum-xs '())

(define (foo)
  (let ((0-100 (iota 101)))
    (dolist (a 0-100)
	    (dolist (b 0-100)
		    (dolist (c 0-100)
			    (dolist (d 0-100)
				    (when (= (+ a b c d) 100)
				      (see a b c d input))))))))


(define (see a b c d xs)
  (let ((abcd (list a b c d)))
    (let ((sum    (apply * (map (lambda (x) (apply + x))
				(list
				 (map * abcd (map second xs))
				 (map * abcd (map third xs))
				 (map * abcd (map fourth xs))
				 (map * abcd (map fifth xs)))))))
      ;;(format #t "sum = ~a ~%" sum)
      (when (> sum best-sum)
	(set! best-sum sum)
	(set! best-sum-xs abcd)
	(format #t "new best sum ~a ~%" sum))      
      (set! tot (+ tot 1)))))


(define example '((butter -1 -2 6 3 8)
		  (cinamon 2 3 -2 -1 3)))


(define (see-example a b xs)
  (let ((ab (list a b)))
    (let ((sum (apply * (map (lambda (x) (apply + x))
			     (list 
			      (map * ab (map second xs))
			      (map * ab (map third xs))
			      (map * ab (map fourth xs))
			      (map * ab (map fifth xs)))))))
      (format #t "sum = ~a ~%" sum)
      sum)))



(define (demo)
  (see-example 44 56 '((butter -1 -2 6 3 8)
		       (cinamon 2 3 -2 -1 3))))


;;(map * (list 44 56) (map second example))

#|
new best sum 49840128 
new best sum 49960008 
new best sum 50000000  <----- 5 then 7 zeros  5 x 10^7
$37 = #f

answer rejected ... next!

|#





;; (foo)
;; (format #t "tot cases = ~a ~%" tot)
