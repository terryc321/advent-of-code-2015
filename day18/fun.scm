

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
(define input2 #f)

(define (get-input filename)  
  (call-with-port (open-input-file filename)
    (lambda (port)
      (read port))))


;; for example
;;(define input #f)

;;(set! input2 (get-input "input2"))
(set! input (get-input "input"))

#|
(define arr #f)

(define (create-array)
  (set! arr (make-array #f 103))
  (let loop ((i 0))
    (format #t "loop = ~a ~%" i )
    (array-set! arr (make-array #f 102) i)
    (when (< i 102)
      (loop (+ i 1)))))
;;(make-array "." 100)
|#

(define (foo)
  (let ((x 0)
	(y 0)
	(hash (make-hash-table)))
    (dolist (str input)	    
	    (set! y (+ y 1))
	    (letrec ((lup (lambda (i)
			    (cond
			     ((>= i (string-length str)) #f)
			     (#t
			      (cond
			       ((char=? (string-ref str i) #\#)
				(let ((x (+ i 1)))
				  (hash-set! hash (list x y) #t)))
			       (#t #f))
			      (lup (+ i 1)))))))
	      (lup 0)))
    hash))


(define s1 (foo))

(define (check h)
  (format #t "~%")
  (letrec ((ck (lambda (x y)
		 (cond
		  ((> x 100) (ck 1 (+ y 1)))
		  ((> y 100) #f)
		  (#t
		   (when (= x 1) (format #t "~%"))
		   (cond
		    ((hash-ref h (list x y)) (format #t "#"))
		    (#t (format #t ".")))
		   (ck (+ x 1) y))))))
    (ck 1 1)
    (format #t "~%")
    ))


#|
(when (and (on-board (- x 1)(- y 1))
  (hash-ref hash (list (- x 1)(- y 1))))
  (set! neighbour (1+ neighbour)))		      
|#
(defmacro inc-neighbour (x y)
  `(when (and (on-board ,x ,y)
	      (hash-ref hash (list ,x ,y)))
     (set! neighbour (1+ neighbour))))
		      

(define (step hash)
  (let ((x 0)
	(y 0)
	(res (make-hash-table)))
    (letrec ((on-board (lambda (x y) (and (>= x 1)(<= x 100)  (>= y 1)(<= y 100))))
	     (foo (lambda (x y)
		    (let ((neighbour 0))
		      ;; top
		      (inc-neighbour (- x 1)(+ y 1))
		      (inc-neighbour x(+ y 1))
		      (inc-neighbour (+ x 1)(+ y 1))		      
		      ;; middle
		      (inc-neighbour (- x 1) y)
		      ;;(inc-neighbour x y) ;; exclude itself
		      (inc-neighbour (+ x 1) y)
		      ;; bottom
		      (inc-neighbour (- x 1)(- y 1))
		      (inc-neighbour x (- y 1))
		      (inc-neighbour (+ x 1)(- y 1))
		      ;; 
		      
		      "A light which is on stays on when 2 or 3 neighbors are on, and turns off otherwise."
		      "A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise."
                      (let ((on (hash-ref hash (list x y))))
			(cond
			 ((and on (or (= neighbour 3) (= neighbour 2)))
			  (hash-set! res (list x y) #t))
			 ((and (not on) (= neighbour 3))
			  (hash-set! res (list x y) #t))
			 (#t #f))))))
	     (ck (lambda (x y)
		   (cond
		    ((> x 100) (ck 1 (+ y 1)))
		    ((> y 100) #f)
		    (#t
		     (foo x y)
		     (ck (+ x 1) y))))))
      (ck 1 1)
      res)))





(define (bar)
  (letrec ((lup (lambda (h k)
		  (cond
		   ((> k 100) #f)
		   (#t 
		    (let ((h2 (step h))
			  (i (+ k 1)))
		      (format #t "after step ~a : ~a ~%" i h2)
		      (lup h2 i)))))))
    (lup s1 0)))

#|
after step 99 : #<hash-table 7f9d1d010720 852/1759> 
after step 100 : #<hash-table 7f9d1d010600 821/1759>   <------ 821 lights on 
after step 101 : #<hash-table 7f9d1d0104e0 857/1759> 

|#



