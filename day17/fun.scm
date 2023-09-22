

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

(set! input2 (get-input "input2"))
(set! input (get-input "input"))

#|
example 
set of numbers , want all ways to get to 25
ys choice values
- xs - my picked values
either pick value or i dont
if total too high stop - as exceeded
if total exactly required sum - solution
otherwise rest ys
|#

(define (foo xs required)
  (let ((nsol 0))
    (letrec ((fiz (lambda (ys xs t)
		    ;;(format #t "fiz ys ~a xs ~a t ~a ~%" ys xs t)
		    (cond
		     ((= t required)
		      (format #t "solution ~a : ~a ~%" xs t)
		      (set! nsol (1+ nsol)))
		     ((null? ys) #f)		     
		     ((> t required) #f)
		     (#t (fiz (cdr ys) (cons (car ys) xs) (+ t (car ys)))
			 (fiz (cdr ys) xs t))))))
		     
		  
      (let ((tot 0))
	(fiz xs '() tot)
	nsol))))


(define (example)
  (foo input2 25))

(define (part-1)
  (foo input 150))


#|
solution (6 13 1 45 20 18 14 33) : 150 
solution (6 13 1 45 20 18 14 33) : 150 
solution (13 1 16 35 20 18 14 33) : 150 
solution (13 1 16 35 20 18 14 33) : 150 
solution (24 6 35 20 18 14 33) : 150 
solution (30 35 20 18 14 33) : 150 
solution (13 1 35 16 20 18 14 33) : 150 
solution (13 1 35 16 20 18 14 33) : 150 
solution (24 6 18 1 16 20 18 14 33) : 150 
solution (30 18 1 16 20 18 14 33) : 150 
solution (48 1 16 20 18 14 33) : 150 
solution (42 6 1 16 20 18 14 33) : 150 
solution (30 6 13 16 20 18 14 33) : 150 
solution (30 6 13 16 20 18 14 33) : 150 
solution (24 6 35 20 18 14 33) : 150 
solution (30 35 20 18 14 33) : 150 
solution (41 6 18 20 18 14 33) : 150 
solution (41 24 20 18 14 33) : 150 
solution (6 18 16 45 18 14 33) : 150 
solution (24 16 45 18 14 33) : 150 
solution (30 6 13 1 35 18 14 33) : 150 
solution (30 6 13 1 35 18 14 33) : 150 
solution (6 13 18 13 35 18 14 33) : 150 
solution (24 13 13 35 18 14 33) : 150 
solution (50 35 18 14 33) : 150 
solution (6 44 35 18 14 33) : 150 
solution (24 13 18 13 1 16 18 14 33) : 150 
solution (42 13 13 1 16 18 14 33) : 150 
solution (50 18 1 16 18 14 33) : 150 
solution (6 44 18 1 16 18 14 33) : 150 
solution (24 44 1 16 18 14 33) : 150 
solution (6 50 13 16 18 14 33) : 150 
solution (6 50 13 16 18 14 33) : 150 
solution (30 6 13 1 35 18 14 33) : 150 
solution (30 6 13 1 35 18 14 33) : 150 
solution (6 13 18 13 35 18 14 33) : 150 
solution (24 13 13 35 18 14 33) : 150 
solution (50 35 18 14 33) : 150 
solution (6 44 35 18 14 33) : 150 
solution (41 24 6 13 1 18 14 33) : 150 
solution (30 41 13 1 18 14 33) : 150 
solution (42 24 18 1 18 14 33) : 150 
solution (41 24 6 13 1 18 14 33) : 150 
solution (30 41 13 1 18 14 33) : 150 
solution (30 6 48 1 18 14 33) : 150 
solution (41 13 18 13 18 14 33) : 150 
solution (6 48 18 13 18 14 33) : 150 
solution (30 24 18 13 18 14 33) : 150 
solution (24 48 13 18 14 33) : 150 
solution (42 24 6 13 18 14 33) : 150 
solution (42 30 13 18 14 33) : 150 
solution (6 48 13 18 18 14 33) : 150 
solution (30 24 13 18 18 14 33) : 150 
solution (24 48 13 18 14 33) : 150 
solution (42 24 6 13 18 14 33) : 150 
solution (42 30 13 18 14 33) : 150 
solution (41 44 18 14 33) : 150 
solution (6 18 13 1 45 20 14 33) : 150 
solution (24 13 1 45 20 14 33) : 150 
solution (6 13 18 1 45 20 14 33) : 150 
solution (24 13 1 45 20 14 33) : 150 
solution (18 13 1 16 35 20 14 33) : 150 
solution (13 18 1 16 35 20 14 33) : 150 
solution (6 13 13 16 35 20 14 33) : 150 
solution (13 35 35 20 14 33) : 150 
solution (13 35 35 20 14 33) : 150 
solution (41 6 1 35 20 14 33) : 150 
solution (24 6 18 35 20 14 33) : 150 
solution (30 18 35 20 14 33) : 150 
solution (48 35 20 14 33) : 150 
solution (42 6 35 20 14 33) : 150 
solution (18 13 1 35 16 20 14 33) : 150 
solution (13 18 1 35 16 20 14 33) : 150 
solution (6 13 13 35 16 20 14 33) : 150 
solution (48 18 1 16 20 14 33) : 150 
solution (42 6 18 1 16 20 14 33) : 150 
solution (42 24 1 16 20 14 33) : 150 
solution (30 6 18 13 16 20 14 33) : 150 
solution (41 13 13 16 20 14 33) : 150 
solution (6 48 13 16 20 14 33) : 150 
solution (30 24 13 16 20 14 33) : 150 
solution (30 6 13 18 16 20 14 33) : 150 
solution (6 48 13 16 20 14 33) : 150 
solution (30 24 13 16 20 14 33) : 150 
solution (41 6 1 35 20 14 33) : 150 
solution (24 6 18 35 20 14 33) : 150 
solution (30 18 35 20 14 33) : 150 
solution (48 35 20 14 33) : 150 
solution (42 6 35 20 14 33) : 150 
solution (6 50 13 13 1 20 14 33) : 150 
solution (41 24 18 20 14 33) : 150 
solution (42 41 20 14 33) : 150 
solution (6 1 16 35 45 14 33) : 150 
solution (6 1 35 16 45 14 33) : 150 
solution (41 1 16 45 14 33) : 150 
solution (24 18 16 45 14 33) : 150 
solution (42 16 45 14 33) : 150 
solution (44 13 1 45 14 33) : 150 
solution (44 13 1 45 14 33) : 150 
solution (6 13 13 1 35 35 14 33) : 150 
solution (30 6 18 13 1 35 14 33) : 150 
solution (41 13 13 1 35 14 33) : 150 
solution (6 48 13 1 35 14 33) : 150 
solution (30 24 13 1 35 14 33) : 150 
solution (30 6 13 18 1 35 14 33) : 150 
solution (6 48 13 1 35 14 33) : 150 
solution (30 24 13 1 35 14 33) : 150 
solution (24 13 18 13 35 14 33) : 150 
solution (42 13 13 35 14 33) : 150 
solution (50 18 35 14 33) : 150 
solution (6 44 18 35 14 33) : 150 
solution (24 44 35 14 33) : 150 
solution (42 13 18 13 1 16 14 33) : 150 
solution (30 24 6 13 13 1 16 14 33) : 150 
solution (24 44 18 1 16 14 33) : 150 
solution (30 6 50 1 16 14 33) : 150 
solution (42 44 1 16 14 33) : 150 
solution (6 50 18 13 16 14 33) : 150 
solution (24 50 13 16 14 33) : 150 
solution (24 6 44 13 16 14 33) : 150 
solution (30 44 13 16 14 33) : 150 
solution (6 50 13 18 16 14 33) : 150 
solution (24 50 13 16 14 33) : 150 
solution (24 6 44 13 16 14 33) : 150 
solution (30 44 13 16 14 33) : 150 
solution (30 6 18 13 1 35 14 33) : 150 
solution (41 13 13 1 35 14 33) : 150 
solution (6 48 13 1 35 14 33) : 150 
solution (30 24 13 1 35 14 33) : 150 
solution (30 6 13 18 1 35 14 33) : 150 
solution (6 48 13 1 35 14 33) : 150 
solution (30 24 13 1 35 14 33) : 150 
solution (24 13 18 13 35 14 33) : 150 
solution (42 13 13 35 14 33) : 150 
solution (50 18 35 14 33) : 150 
solution (6 44 18 35 14 33) : 150 
solution (24 44 35 14 33) : 150 
solution (41 24 6 18 13 1 14 33) : 150 
solution (30 41 18 13 1 14 33) : 150 
solution (41 48 13 1 14 33) : 150 
solution (42 41 6 13 1 14 33) : 150 
solution (41 24 6 13 18 1 14 33) : 150 
solution (30 41 13 18 1 14 33) : 150 
solution (30 6 48 18 1 14 33) : 150 
solution (41 48 13 1 14 33) : 150 
solution (42 41 6 13 1 14 33) : 150 
solution (30 24 48 1 14 33) : 150 
solution (42 30 24 6 1 14 33) : 150 
solution (24 48 18 13 14 33) : 150 
solution (42 24 6 18 13 14 33) : 150 
solution (42 30 18 13 14 33) : 150 
solution (30 41 6 13 13 14 33) : 150 
solution (42 48 13 14 33) : 150 
solution (24 48 13 18 14 33) : 150 
solution (42 24 6 13 18 14 33) : 150 
solution (42 30 13 18 14 33) : 150 
solution (41 44 18 14 33) : 150 
solution (42 48 13 14 33) : 150 
solution (18 16 45 20 18 33) : 150 
solution (24 6 13 1 35 20 18 33) : 150 
solution (30 13 1 35 20 18 33) : 150 
solution (24 6 13 1 35 20 18 33) : 150 
solution (30 13 1 35 20 18 33) : 150 
solution (13 18 13 35 20 18 33) : 150 
solution (44 35 20 18 33) : 150 
solution (30 6 13 13 1 16 20 18 33) : 150 
solution (44 18 1 16 20 18 33) : 150 
solution (50 13 16 20 18 33) : 150 
solution (6 44 13 16 20 18 33) : 150 
solution (50 13 16 20 18 33) : 150 
solution (6 44 13 16 20 18 33) : 150 
solution (24 6 13 1 35 20 18 33) : 150 
solution (30 13 1 35 20 18 33) : 150 
solution (24 6 13 1 35 20 18 33) : 150 
solution (30 13 1 35 20 18 33) : 150 
solution (13 18 13 35 20 18 33) : 150 
solution (44 35 20 18 33) : 150 
solution (41 6 18 13 1 20 18 33) : 150 
solution (41 24 13 1 20 18 33) : 150 
solution (41 6 13 18 1 20 18 33) : 150 
solution (30 24 6 18 1 20 18 33) : 150 
solution (41 24 13 1 20 18 33) : 150 
solution (24 6 48 1 20 18 33) : 150 
solution (30 48 1 20 18 33) : 150 
solution (42 30 6 1 20 18 33) : 150 
solution (48 18 13 20 18 33) : 150 
solution (42 6 18 13 20 18 33) : 150 
solution (42 24 13 20 18 33) : 150 
solution (48 13 18 20 18 33) : 150 
solution (42 6 13 18 20 18 33) : 150 
solution (42 24 13 20 18 33) : 150 
solution (18 1 35 45 18 33) : 150 
solution (6 13 35 45 18 33) : 150 
solution (6 13 35 45 18 33) : 150 
solution (6 18 13 1 16 45 18 33) : 150 
solution (24 13 1 16 45 18 33) : 150 
solution (6 13 18 1 16 45 18 33) : 150 
solution (24 13 1 16 45 18 33) : 150 
solution (18 1 35 45 18 33) : 150 
solution (6 13 35 45 18 33) : 150 
solution (6 13 35 45 18 33) : 150 
solution (41 13 45 18 33) : 150 
solution (30 6 18 45 18 33) : 150 
solution (41 13 45 18 33) : 150 
solution (6 48 45 18 33) : 150 
solution (30 24 45 18 33) : 150 
solution (13 35 16 35 18 33) : 150 
solution (13 35 16 35 18 33) : 150 
solution (41 6 1 16 35 18 33) : 150 
solution (24 6 18 16 35 18 33) : 150 
solution (30 18 16 35 18 33) : 150 
solution (48 16 35 18 33) : 150 
solution (42 6 16 35 18 33) : 150 
solution (50 13 1 35 18 33) : 150 
solution (6 44 13 1 35 18 33) : 150 
solution (50 13 1 35 18 33) : 150 
solution (6 44 13 1 35 18 33) : 150 
solution (41 6 1 35 16 18 33) : 150 
solution (24 6 18 35 16 18 33) : 150 
solution (30 18 35 16 18 33) : 150 
solution (48 35 16 18 33) : 150 
solution (42 6 35 16 18 33) : 150 
solution (6 50 13 13 1 16 18 33) : 150 
solution (41 24 18 16 18 33) : 150 
solution (42 41 16 18 33) : 150 
solution (50 13 1 35 18 33) : 150 
solution (6 44 13 1 35 18 33) : 150 
solution (50 13 1 35 18 33) : 150 
solution (6 44 13 1 35 18 33) : 150 
solution (6 48 13 18 13 1 18 33) : 150 
solution (30 24 13 18 13 1 18 33) : 150 
solution (24 48 13 13 1 18 33) : 150 
solution (42 24 6 13 13 1 18 33) : 150 
solution (42 30 13 13 1 18 33) : 150 
solution (41 44 13 1 18 33) : 150 
solution (24 6 50 18 1 18 33) : 150 
solution (30 50 18 1 18 33) : 150 
solution (30 6 44 18 1 18 33) : 150 
solution (41 44 13 1 18 33) : 150 
solution (48 50 1 18 33) : 150 
solution (42 6 50 1 18 33) : 150 
solution (6 48 44 1 18 33) : 150 
solution (30 24 44 1 18 33) : 150 
solution (24 44 18 13 18 33) : 150 
solution (30 6 50 13 18 33) : 150 
solution (42 44 13 18 33) : 150 
solution (24 44 13 18 18 33) : 150 
solution (30 6 50 13 18 33) : 150 
solution (42 44 13 18 33) : 150 
solution (1 16 35 45 20 33) : 150 
solution (1 35 16 45 20 33) : 150 
solution (30 6 16 45 20 33) : 150 
solution (13 13 1 35 35 20 33) : 150 
solution (24 6 18 13 1 35 20 33) : 150 
solution (30 18 13 1 35 20 33) : 150 
solution (48 13 1 35 20 33) : 150 
solution (42 6 13 1 35 20 33) : 150 
solution (24 6 13 18 1 35 20 33) : 150 
solution (30 13 18 1 35 20 33) : 150 
solution (48 13 1 35 20 33) : 150 
solution (42 6 13 1 35 20 33) : 150 
solution (30 6 13 13 35 20 33) : 150 
solution (44 18 35 20 33) : 150 
solution (30 6 13 18 13 1 16 20 33) : 150 
solution (6 48 13 13 1 16 20 33) : 150 
solution (30 24 13 13 1 16 20 33) : 150 
solution (24 6 50 1 16 20 33) : 150 
solution (30 50 1 16 20 33) : 150 
solution (30 6 44 1 16 20 33) : 150 
solution (50 18 13 16 20 33) : 150 
solution (6 44 18 13 16 20 33) : 150 
solution (24 44 13 16 20 33) : 150 
solution (50 13 18 16 20 33) : 150 
solution (6 44 13 18 16 20 33) : 150 
solution (24 44 13 16 20 33) : 150 
solution (24 6 18 13 1 35 20 33) : 150 
solution (30 18 13 1 35 20 33) : 150 
solution (48 13 1 35 20 33) : 150 
solution (42 6 13 1 35 20 33) : 150 
solution (24 6 13 18 1 35 20 33) : 150 
solution (30 13 18 1 35 20 33) : 150 
solution (48 13 1 35 20 33) : 150 
solution (42 6 13 1 35 20 33) : 150 
solution (30 6 13 13 35 20 33) : 150 
solution (44 18 35 20 33) : 150 
solution (41 24 18 13 1 20 33) : 150 
solution (42 41 13 1 20 33) : 150 
solution (41 24 13 18 1 20 33) : 150 
solution (24 6 48 18 1 20 33) : 150 
solution (30 48 18 1 20 33) : 150 
solution (42 30 6 18 1 20 33) : 150 
solution (42 41 13 1 20 33) : 150 
solution (42 6 48 1 20 33) : 150 
solution (42 30 24 1 20 33) : 150 
solution (42 24 18 13 20 33) : 150 
solution (41 24 6 13 13 20 33) : 150 
solution (30 41 13 13 20 33) : 150 
solution (30 6 48 13 20 33) : 150 
solution (42 24 13 18 20 33) : 150 
solution (30 6 48 13 20 33) : 150 
solution (41 6 50 20 33) : 150 
solution (30 6 1 35 45 33) : 150 
solution (6 18 13 35 45 33) : 150 
solution (24 13 35 45 33) : 150 
solution (6 13 18 35 45 33) : 150 
solution (24 13 35 45 33) : 150 
solution (24 18 13 1 16 45 33) : 150 
solution (42 13 1 16 45 33) : 150 
solution (24 13 18 1 16 45 33) : 150 
solution (42 13 1 16 45 33) : 150 
solution (24 6 13 13 16 45 33) : 150 
solution (30 13 13 16 45 33) : 150 
solution (6 50 16 45 33) : 150 
solution (30 6 1 35 45 33) : 150 
solution (6 18 13 35 45 33) : 150 
solution (24 13 35 45 33) : 150 
solution (6 13 18 35 45 33) : 150 
solution (24 13 35 45 33) : 150 
solution (41 24 6 1 45 33) : 150 
solution (30 41 1 45 33) : 150 
solution (41 18 13 45 33) : 150 
solution (41 13 18 45 33) : 150 
solution (6 48 18 45 33) : 150 
solution (30 24 18 45 33) : 150 
solution (24 48 45 33) : 150 
solution (42 24 6 45 33) : 150 
solution (42 30 45 33) : 150 
solution (24 6 1 35 16 35 33) : 150 
solution (30 1 35 16 35 33) : 150 
solution (18 13 35 16 35 33) : 150 
solution (13 18 35 16 35 33) : 150 
solution (41 6 18 1 16 35 33) : 150 
solution (41 24 1 16 35 33) : 150 
solution (48 18 16 35 33) : 150 
solution (42 6 18 16 35 33) : 150 
solution (42 24 16 35 33) : 150 
solution (41 6 35 35 33) : 150 
solution (50 18 13 1 35 33) : 150 
solution (6 44 18 13 1 35 33) : 150 
solution (24 44 13 1 35 33) : 150 
solution (50 13 18 1 35 33) : 150 
solution (6 44 13 18 1 35 33) : 150 
solution (24 44 13 1 35 33) : 150 
solution (6 50 13 13 35 33) : 150 
solution (41 6 18 1 35 16 33) : 150 
solution (41 24 1 35 16 33) : 150 
solution (48 18 35 16 33) : 150 
solution (42 6 18 35 16 33) : 150 
solution (42 24 35 16 33) : 150 
solution (6 50 13 18 13 1 16 33) : 150 
solution (24 50 13 13 1 16 33) : 150 
solution (24 6 44 13 13 1 16 33) : 150 
solution (30 44 13 13 1 16 33) : 150 
solution (6 44 50 1 16 33) : 150 
solution (42 41 18 16 33) : 150 
solution (30 41 24 6 16 33) : 150 
solution (50 18 13 1 35 33) : 150 
solution (6 44 18 13 1 35 33) : 150 
solution (24 44 13 1 35 33) : 150 
solution (50 13 18 1 35 33) : 150 
solution (6 44 13 18 1 35 33) : 150 
solution (24 44 13 1 35 33) : 150 
solution (6 50 13 13 35 33) : 150 
solution (24 48 13 18 13 1 33) : 150 
solution (42 24 6 13 18 13 1 33) : 150 
solution (42 30 13 18 13 1 33) : 150 
solution (41 44 18 13 1 33) : 150 
solution (42 48 13 13 1 33) : 150 
solution (41 44 13 18 1 33) : 150 
solution (48 50 18 1 33) : 150 
solution (42 6 50 18 1 33) : 150 
solution (6 48 44 18 1 33) : 150 
solution (30 24 44 18 1 33) : 150 
solution (42 24 50 1 33) : 150 
solution (24 48 44 1 33) : 150 
solution (42 24 6 44 1 33) : 150 
solution (42 30 44 1 33) : 150 
solution (30 6 50 18 13 33) : 150 
solution (42 44 18 13 33) : 150 
solution (41 50 13 13 33) : 150 
solution (41 6 44 13 13 33) : 150 
solution (6 48 50 13 33) : 150 
solution (30 24 50 13 33) : 150 
solution (30 24 6 44 13 33) : 150 
solution (30 6 50 13 18 33) : 150 
solution (42 44 13 18 33) : 150 
solution (6 48 50 13 33) : 150 
solution (30 24 50 13 33) : 150 
solution (30 24 6 44 13 33) : 150 
solution (18 35 45 20 18 14) : 150 
solution (30 6 1 16 45 20 18 14) : 150 
solution (6 18 13 16 45 20 18 14) : 150 
solution (24 13 16 45 20 18 14) : 150 
solution (6 13 18 16 45 20 18 14) : 150 
solution (24 13 16 45 20 18 14) : 150 
solution (18 35 45 20 18 14) : 150 
solution (41 6 16 35 20 18 14) : 150 
solution (30 6 13 13 1 35 20 18 14) : 150 
solution (44 18 1 35 20 18 14) : 150 
solution (50 13 35 20 18 14) : 150 
solution (6 44 13 35 20 18 14) : 150 
solution (50 13 35 20 18 14) : 150 
solution (6 44 13 35 20 18 14) : 150 
solution (41 6 35 16 20 18 14) : 150 
solution (50 18 13 1 16 20 18 14) : 150 
solution (6 44 18 13 1 16 20 18 14) : 150 
solution (24 44 13 1 16 20 18 14) : 150 
solution (50 13 18 1 16 20 18 14) : 150 
solution (6 44 13 18 1 16 20 18 14) : 150 
solution (24 44 13 1 16 20 18 14) : 150 
solution (6 50 13 13 16 20 18 14) : 150 
solution (30 6 13 13 1 35 20 18 14) : 150 
solution (44 18 1 35 20 18 14) : 150 
solution (50 13 35 20 18 14) : 150 
solution (6 44 13 35 20 18 14) : 150 
solution (50 13 35 20 18 14) : 150 
solution (6 44 13 35 20 18 14) : 150 
solution (42 24 18 13 1 20 18 14) : 150 
solution (41 24 6 13 13 1 20 18 14) : 150 
solution (30 41 13 13 1 20 18 14) : 150 
solution (30 6 48 13 1 20 18 14) : 150 
solution (42 24 13 18 1 20 18 14) : 150 
solution (30 6 48 13 1 20 18 14) : 150 
solution (41 6 50 1 20 18 14) : 150 
solution (6 48 13 18 13 20 18 14) : 150 
solution (30 24 13 18 13 20 18 14) : 150 
solution (24 48 13 13 20 18 14) : 150 
solution (42 24 6 13 13 20 18 14) : 150 
solution (42 30 13 13 20 18 14) : 150 
solution (41 44 13 20 18 14) : 150 
solution (24 6 50 18 20 18 14) : 150 
solution (30 50 18 20 18 14) : 150 
solution (30 6 44 18 20 18 14) : 150 
solution (41 44 13 20 18 14) : 150 
solution (48 50 20 18 14) : 150 
solution (42 6 50 20 18 14) : 150 
solution (6 48 44 20 18 14) : 150 
solution (30 24 44 20 18 14) : 150 
solution (6 18 13 1 35 45 18 14) : 150 
solution (24 13 1 35 45 18 14) : 150 
solution (6 13 18 1 35 45 18 14) : 150 
solution (24 13 1 35 45 18 14) : 150 
solution (24 6 13 13 1 16 45 18 14) : 150 
solution (30 13 13 1 16 45 18 14) : 150 
solution (6 50 1 16 45 18 14) : 150 
solution (44 13 16 45 18 14) : 150 
solution (44 13 16 45 18 14) : 150 
solution (6 18 13 1 35 45 18 14) : 150 
solution (24 13 1 35 45 18 14) : 150 
solution (6 13 18 1 35 45 18 14) : 150 
solution (24 13 1 35 45 18 14) : 150 
solution (41 18 13 1 45 18 14) : 150 
solution (41 13 18 1 45 18 14) : 150 
solution (6 48 18 1 45 18 14) : 150 
solution (30 24 18 1 45 18 14) : 150 
solution (24 48 1 45 18 14) : 150 
solution (42 24 6 1 45 18 14) : 150 
solution (42 30 1 45 18 14) : 150 
solution (42 18 13 45 18 14) : 150 
solution (41 6 13 13 45 18 14) : 150 
solution (30 24 6 13 45 18 14) : 150 
solution (42 13 18 45 18 14) : 150 
solution (30 24 6 13 45 18 14) : 150 
solution (18 13 1 35 16 35 18 14) : 150 
solution (13 18 1 35 16 35 18 14) : 150 
solution (6 13 13 35 16 35 18 14) : 150 
solution (48 18 1 16 35 18 14) : 150 
solution (42 6 18 1 16 35 18 14) : 150 
solution (42 24 1 16 35 18 14) : 150 
solution (30 6 18 13 16 35 18 14) : 150 
solution (41 13 13 16 35 18 14) : 150 
solution (6 48 13 16 35 18 14) : 150 
solution (30 24 13 16 35 18 14) : 150 
solution (30 6 13 18 16 35 18 14) : 150 
solution (6 48 13 16 35 18 14) : 150 
solution (30 24 13 16 35 18 14) : 150 
solution (41 6 1 35 35 18 14) : 150 
solution (24 6 18 35 35 18 14) : 150 
solution (30 18 35 35 18 14) : 150 
solution (48 35 35 18 14) : 150 
solution (42 6 35 35 18 14) : 150 
solution (6 50 13 13 1 35 18 14) : 150 
solution (41 24 18 35 18 14) : 150 
solution (42 41 35 18 14) : 150 
solution (48 18 1 35 16 18 14) : 150 
solution (42 6 18 1 35 16 18 14) : 150 
solution (42 24 1 35 16 18 14) : 150 
solution (30 6 18 13 35 16 18 14) : 150 
solution (41 13 13 35 16 18 14) : 150 
solution (6 48 13 35 16 18 14) : 150 
solution (30 24 13 35 16 18 14) : 150 
solution (30 6 13 18 35 16 18 14) : 150 
solution (6 48 13 35 16 18 14) : 150 
solution (30 24 13 35 16 18 14) : 150 
solution (42 41 18 1 16 18 14) : 150 
solution (30 41 24 6 1 16 18 14) : 150 
solution (41 24 6 18 13 16 18 14) : 150 
solution (30 41 18 13 16 18 14) : 150 
solution (41 48 13 16 18 14) : 150 
solution (42 41 6 13 16 18 14) : 150 
solution (41 24 6 13 18 16 18 14) : 150 
solution (30 41 13 18 16 18 14) : 150 
solution (30 6 48 18 16 18 14) : 150 
solution (41 48 13 16 18 14) : 150 
solution (42 41 6 13 16 18 14) : 150 
solution (30 24 48 16 18 14) : 150 
solution (42 30 24 6 16 18 14) : 150 
solution (6 50 13 13 1 35 18 14) : 150 
solution (41 24 18 35 18 14) : 150 
solution (42 41 35 18 14) : 150 
solution (30 6 50 18 13 1 18 14) : 150 
solution (42 44 18 13 1 18 14) : 150 
solution (41 50 13 13 1 18 14) : 150 
solution (41 6 44 13 13 1 18 14) : 150 
solution (6 48 50 13 1 18 14) : 150 
solution (30 24 50 13 1 18 14) : 150 
solution (30 24 6 44 13 1 18 14) : 150 
solution (30 6 50 13 18 1 18 14) : 150 
solution (42 44 13 18 1 18 14) : 150 
solution (6 48 50 13 1 18 14) : 150 
solution (30 24 50 13 1 18 14) : 150 
solution (30 24 6 44 13 1 18 14) : 150 
solution (24 50 13 18 13 18 14) : 150 
solution (24 6 44 13 18 13 18 14) : 150 
solution (30 44 13 18 13 18 14) : 150 
solution (42 50 13 13 18 14) : 150 
solution (48 44 13 13 18 14) : 150 
solution (42 6 44 13 13 18 14) : 150 
solution (6 44 50 18 18 14) : 150 
solution (24 44 50 18 14) : 150 
solution (6 13 1 16 35 45 20 14) : 150 
solution (6 13 1 16 35 45 20 14) : 150 
solution (1 35 35 45 20 14) : 150 
solution (30 6 35 45 20 14) : 150 
solution (6 13 1 35 16 45 20 14) : 150 
solution (6 13 1 35 16 45 20 14) : 150 
solution (41 13 1 16 45 20 14) : 150 
solution (30 6 18 1 16 45 20 14) : 150 
solution (41 13 1 16 45 20 14) : 150 
solution (6 48 1 16 45 20 14) : 150 
solution (30 24 1 16 45 20 14) : 150 
solution (24 18 13 16 45 20 14) : 150 
solution (42 13 16 45 20 14) : 150 
solution (24 13 18 16 45 20 14) : 150 
solution (42 13 16 45 20 14) : 150 
solution (30 6 35 45 20 14) : 150 
solution (44 13 13 1 45 20 14) : 150 
solution (41 24 6 45 20 14) : 150 
solution (30 41 45 20 14) : 150 
solution (24 6 35 16 35 20 14) : 150 
solution (30 35 16 35 20 14) : 150 
solution (41 6 18 16 35 20 14) : 150 
solution (41 24 16 35 20 14) : 150 
solution (30 6 13 18 13 1 35 20 14) : 150 
solution (6 48 13 13 1 35 20 14) : 150 
solution (30 24 13 13 1 35 20 14) : 150 
solution (24 6 50 1 35 20 14) : 150 
solution (30 50 1 35 20 14) : 150 
solution (30 6 44 1 35 20 14) : 150 
solution (50 18 13 35 20 14) : 150 
solution (6 44 18 13 35 20 14) : 150 
solution (24 44 13 35 20 14) : 150 
solution (50 13 18 35 20 14) : 150 
solution (6 44 13 18 35 20 14) : 150 
solution (24 44 13 35 20 14) : 150 
solution (41 6 18 35 16 20 14) : 150 
solution (41 24 35 16 20 14) : 150 
solution (24 44 18 13 1 16 20 14) : 150 
solution (30 6 50 13 1 16 20 14) : 150 
solution (42 44 13 1 16 20 14) : 150 
solution (24 44 13 18 1 16 20 14) : 150 
solution (30 6 50 13 1 16 20 14) : 150 
solution (42 44 13 1 16 20 14) : 150 
solution (6 50 13 18 13 16 20 14) : 150 
solution (24 50 13 13 16 20 14) : 150 
solution (24 6 44 13 13 16 20 14) : 150 
solution (30 44 13 13 16 20 14) : 150 
solution (6 44 50 16 20 14) : 150 
solution (30 6 13 18 13 1 35 20 14) : 150 
solution (6 48 13 13 1 35 20 14) : 150 
solution (30 24 13 13 1 35 20 14) : 150 
solution (24 6 50 1 35 20 14) : 150 
solution (30 50 1 35 20 14) : 150 
solution (30 6 44 1 35 20 14) : 150 
solution (50 18 13 35 20 14) : 150 
solution (6 44 18 13 35 20 14) : 150 
solution (24 44 13 35 20 14) : 150 
solution (50 13 18 35 20 14) : 150 
solution (6 44 13 18 35 20 14) : 150 
solution (24 44 13 35 20 14) : 150 
solution (41 24 6 13 18 13 1 20 14) : 150 
solution (30 41 13 18 13 1 20 14) : 150 
solution (30 6 48 18 13 1 20 14) : 150 
solution (41 48 13 13 1 20 14) : 150 
solution (42 41 6 13 13 1 20 14) : 150 
solution (30 24 48 13 1 20 14) : 150 
solution (42 30 24 6 13 1 20 14) : 150 
solution (30 6 48 13 18 1 20 14) : 150 
solution (41 6 50 18 1 20 14) : 150 
solution (30 24 48 13 1 20 14) : 150 
solution (42 30 24 6 13 1 20 14) : 150 
solution (41 24 50 1 20 14) : 150 
solution (41 24 6 44 1 20 14) : 150 
solution (30 41 44 1 20 14) : 150 
solution (24 48 13 18 13 20 14) : 150 
solution (42 24 6 13 18 13 20 14) : 150 
solution (42 30 13 18 13 20 14) : 150 
solution (41 44 18 13 20 14) : 150 
solution (42 48 13 13 20 14) : 150 
solution (41 44 13 18 20 14) : 150 
solution (48 50 18 20 14) : 150 
solution (42 6 50 18 20 14) : 150 
solution (6 48 44 18 20 14) : 150 
solution (30 24 44 18 20 14) : 150 
solution (42 24 50 20 14) : 150 
solution (24 48 44 20 14) : 150 
solution (42 24 6 44 20 14) : 150 
solution (42 30 44 20 14) : 150 
solution (24 18 13 1 35 45 14) : 150 
solution (42 13 1 35 45 14) : 150 
solution (24 13 18 1 35 45 14) : 150 
solution (42 13 1 35 45 14) : 150 
solution (24 6 13 13 35 45 14) : 150 
solution (30 13 13 35 45 14) : 150 
solution (6 50 35 45 14) : 150 
solution (24 6 13 18 13 1 16 45 14) : 150 
solution (30 13 18 13 1 16 45 14) : 150 
solution (48 13 13 1 16 45 14) : 150 
solution (42 6 13 13 1 16 45 14) : 150 
solution (6 50 18 1 16 45 14) : 150 
solution (24 50 1 16 45 14) : 150 
solution (24 6 44 1 16 45 14) : 150 
solution (30 44 1 16 45 14) : 150 
solution (44 18 13 16 45 14) : 150 
solution (44 13 18 16 45 14) : 150 
solution (24 18 13 1 35 45 14) : 150 
solution (42 13 1 35 45 14) : 150 
solution (24 13 18 1 35 45 14) : 150 
solution (42 13 1 35 45 14) : 150 
solution (24 6 13 13 35 45 14) : 150 
solution (30 13 13 35 45 14) : 150 
solution (6 50 35 45 14) : 150 
solution (30 41 6 13 1 45 14) : 150 
solution (24 48 18 1 45 14) : 150 
solution (42 24 6 18 1 45 14) : 150 
solution (42 30 18 1 45 14) : 150 
solution (30 41 6 13 1 45 14) : 150 
solution (42 48 1 45 14) : 150 
solution (41 6 13 18 13 45 14) : 150 
solution (30 24 6 18 13 45 14) : 150 
solution (41 24 13 13 45 14) : 150 
solution (24 6 48 13 45 14) : 150 
solution (30 48 13 45 14) : 150 
solution (42 30 6 13 45 14) : 150 
solution (30 24 6 13 18 45 14) : 150 
solution (24 6 48 13 45 14) : 150 
solution (30 48 13 45 14) : 150 
solution (42 30 6 13 45 14) : 150 
solution (41 50 45 14) : 150 
solution (41 6 44 45 14) : 150 
solution (30 6 13 1 35 16 35 14) : 150 
solution (30 6 13 1 35 16 35 14) : 150 
solution (6 13 18 13 35 16 35 14) : 150 
solution (24 13 13 35 16 35 14) : 150 
solution (50 35 16 35 14) : 150 
solution (6 44 35 16 35 14) : 150 
solution (41 24 6 13 1 16 35 14) : 150 
solution (30 41 13 1 16 35 14) : 150 
solution (42 24 18 1 16 35 14) : 150 
solution (41 24 6 13 1 16 35 14) : 150 
solution (30 41 13 1 16 35 14) : 150 
solution (30 6 48 1 16 35 14) : 150 
solution (41 13 18 13 16 35 14) : 150 
solution (6 48 18 13 16 35 14) : 150 
solution (30 24 18 13 16 35 14) : 150 
solution (24 48 13 16 35 14) : 150 
solution (42 24 6 13 16 35 14) : 150 
solution (42 30 13 16 35 14) : 150 
solution (6 48 13 18 16 35 14) : 150 
solution (30 24 13 18 16 35 14) : 150 
solution (24 48 13 16 35 14) : 150 
solution (42 24 6 13 16 35 14) : 150 
solution (42 30 13 16 35 14) : 150 
solution (41 44 16 35 14) : 150 
solution (41 6 18 1 35 35 14) : 150 
solution (41 24 1 35 35 14) : 150 
solution (48 18 35 35 14) : 150 
solution (42 6 18 35 35 14) : 150 
solution (42 24 35 35 14) : 150 
solution (6 50 13 18 13 1 35 14) : 150 
solution (24 50 13 13 1 35 14) : 150 
solution (24 6 44 13 13 1 35 14) : 150 
solution (30 44 13 13 1 35 14) : 150 
solution (6 44 50 1 35 14) : 150 
solution (42 41 18 35 14) : 150 
solution (30 41 24 6 35 14) : 150 
solution (41 24 6 13 1 35 16 14) : 150 
solution (30 41 13 1 35 16 14) : 150 
solution (42 24 18 1 35 16 14) : 150 
solution (41 24 6 13 1 35 16 14) : 150 
solution (30 41 13 1 35 16 14) : 150 
solution (30 6 48 1 35 16 14) : 150 
solution (41 13 18 13 35 16 14) : 150 
solution (6 48 18 13 35 16 14) : 150 
solution (30 24 18 13 35 16 14) : 150 
solution (24 48 13 35 16 14) : 150 
solution (42 24 6 13 35 16 14) : 150 
solution (42 30 13 35 16 14) : 150 
solution (6 48 13 18 35 16 14) : 150 
solution (30 24 13 18 35 16 14) : 150 
solution (24 48 13 35 16 14) : 150 
solution (42 24 6 13 35 16 14) : 150 
solution (42 30 13 35 16 14) : 150 
solution (41 44 35 16 14) : 150 
solution (30 41 24 6 18 1 16 14) : 150 
solution (41 24 6 48 1 16 14) : 150 
solution (30 41 48 1 16 14) : 150 
solution (42 30 41 6 1 16 14) : 150 
solution (41 48 18 13 16 14) : 150 
solution (42 41 6 18 13 16 14) : 150 
solution (44 50 13 13 16 14) : 150 
solution (42 41 24 13 16 14) : 150 
solution (41 48 13 18 16 14) : 150 
solution (42 41 6 13 18 16 14) : 150 
solution (30 24 48 18 16 14) : 150 
solution (42 30 24 6 18 16 14) : 150 
solution (42 41 24 13 16 14) : 150 
solution (42 24 6 48 16 14) : 150 
solution (42 30 48 16 14) : 150 
solution (6 50 13 18 13 1 35 14) : 150 
solution (24 50 13 13 1 35 14) : 150 
solution (24 6 44 13 13 1 35 14) : 150 
solution (30 44 13 13 1 35 14) : 150 
solution (6 44 50 1 35 14) : 150 
solution (42 41 18 35 14) : 150 
solution (30 41 24 6 35 14) : 150 
solution (41 50 13 18 13 1 14) : 150 
solution (41 6 44 13 18 13 1 14) : 150 
solution (6 48 50 18 13 1 14) : 150 
solution (30 24 50 18 13 1 14) : 150 
solution (30 24 6 44 18 13 1 14) : 150 
solution (41 24 44 13 13 1 14) : 150 
solution (24 48 50 13 1 14) : 150 
solution (42 24 6 50 13 1 14) : 150 
solution (42 30 50 13 1 14) : 150 
solution (24 6 48 44 13 1 14) : 150 
solution (30 48 44 13 1 14) : 150 
solution (42 30 6 44 13 1 14) : 150 
solution (6 48 50 13 18 1 14) : 150 
solution (30 24 50 13 18 1 14) : 150 
solution (30 24 6 44 13 18 1 14) : 150 
solution (24 48 50 13 1 14) : 150 
solution (42 24 6 50 13 1 14) : 150 
solution (42 30 50 13 1 14) : 150 
solution (24 6 48 44 13 1 14) : 150 
solution (30 48 44 13 1 14) : 150 
solution (42 30 6 44 13 1 14) : 150 
solution (41 44 50 1 14) : 150 
solution (42 50 13 18 13 14) : 150 
solution (48 44 13 18 13 14) : 150 
solution (42 6 44 13 18 13 14) : 150 
solution (30 24 6 50 13 13 14) : 150 
solution (42 24 44 13 13 14) : 150 
solution (24 44 50 18 14) : 150 
solution (42 44 50 14) : 150 
solution (18 13 1 35 45 20 18) : 150 
solution (13 18 1 35 45 20 18) : 150 
solution (6 13 13 35 45 20 18) : 150 
solution (6 13 18 13 1 16 45 20 18) : 150 
solution (24 13 13 1 16 45 20 18) : 150 
solution (50 1 16 45 20 18) : 150 
solution (6 44 1 16 45 20 18) : 150 
solution (18 13 1 35 45 20 18) : 150 
solution (13 18 1 35 45 20 18) : 150 
solution (6 13 13 35 45 20 18) : 150 
solution (48 18 1 45 20 18) : 150 
solution (42 6 18 1 45 20 18) : 150 
solution (42 24 1 45 20 18) : 150 
solution (30 6 18 13 45 20 18) : 150 
solution (41 13 13 45 20 18) : 150 
solution (6 48 13 45 20 18) : 150 
solution (30 24 13 45 20 18) : 150 
solution (30 6 13 18 45 20 18) : 150 
solution (6 48 13 45 20 18) : 150 
solution (30 24 13 45 20 18) : 150 
solution (13 13 35 16 35 20 18) : 150 
solution (41 6 13 1 16 35 20 18) : 150 
solution (42 18 1 16 35 20 18) : 150 
solution (41 6 13 1 16 35 20 18) : 150 
solution (30 24 6 1 16 35 20 18) : 150 
solution (24 6 18 13 16 35 20 18) : 150 
solution (30 18 13 16 35 20 18) : 150 
solution (48 13 16 35 20 18) : 150 
solution (42 6 13 16 35 20 18) : 150 
solution (24 6 13 18 16 35 20 18) : 150 
solution (30 13 18 16 35 20 18) : 150 
solution (48 13 16 35 20 18) : 150 
solution (42 6 13 16 35 20 18) : 150 
solution (41 1 35 35 20 18) : 150 
solution (24 18 35 35 20 18) : 150 
solution (42 35 35 20 18) : 150 
solution (50 13 13 1 35 20 18) : 150 
solution (6 44 13 13 1 35 20 18) : 150 
solution (30 41 6 35 20 18) : 150 
solution (41 6 13 1 35 16 20 18) : 150 
solution (42 18 1 35 16 20 18) : 150 
solution (41 6 13 1 35 16 20 18) : 150 
solution (30 24 6 1 35 16 20 18) : 150 
solution (24 6 18 13 35 16 20 18) : 150 
solution (30 18 13 35 16 20 18) : 150 
solution (48 13 35 16 20 18) : 150 
solution (42 6 13 35 16 20 18) : 150 
solution (24 6 13 18 35 16 20 18) : 150 
solution (30 13 18 35 16 20 18) : 150 
solution (48 13 35 16 20 18) : 150 
solution (42 6 13 35 16 20 18) : 150 
solution (30 41 6 18 1 16 20 18) : 150 
solution (41 6 48 1 16 20 18) : 150 
solution (30 41 24 1 16 20 18) : 150 
solution (41 24 18 13 16 20 18) : 150 
solution (42 41 13 16 20 18) : 150 
solution (41 24 13 18 16 20 18) : 150 
solution (24 6 48 18 16 20 18) : 150 
solution (30 48 18 16 20 18) : 150 
solution (42 30 6 18 16 20 18) : 150 
solution (42 41 13 16 20 18) : 150 
solution (42 6 48 16 20 18) : 150 
solution (42 30 24 16 20 18) : 150 
solution (50 13 13 1 35 20 18) : 150 
solution (6 44 13 13 1 35 20 18) : 150 
solution (30 41 6 35 20 18) : 150 
solution (24 6 50 18 13 1 20 18) : 150 
solution (30 50 18 13 1 20 18) : 150 
solution (30 6 44 18 13 1 20 18) : 150 
solution (41 44 13 13 1 20 18) : 150 
solution (48 50 13 1 20 18) : 150 
solution (42 6 50 13 1 20 18) : 150 
solution (6 48 44 13 1 20 18) : 150 
solution (30 24 44 13 1 20 18) : 150 
solution (24 6 50 13 18 1 20 18) : 150 
solution (30 50 13 18 1 20 18) : 150 
solution (30 6 44 13 18 1 20 18) : 150 
solution (48 50 13 1 20 18) : 150 
solution (42 6 50 13 1 20 18) : 150 
solution (6 48 44 13 1 20 18) : 150 
solution (30 24 44 13 1 20 18) : 150 
solution (24 44 13 18 13 20 18) : 150 
solution (30 6 50 13 13 20 18) : 150 
solution (42 44 13 13 20 18) : 150 
solution (44 50 18 20 18) : 150 
solution (1 35 16 35 45 18) : 150 
solution (30 6 16 35 45 18) : 150 
solution (30 6 35 16 45 18) : 150 
solution (44 13 13 1 16 45 18) : 150 
solution (41 24 6 16 45 18) : 150 
solution (30 41 16 45 18) : 150 
solution (42 13 18 13 1 45 18) : 150 
solution (30 24 6 13 13 1 45 18) : 150 
solution (24 44 18 1 45 18) : 150 
solution (30 6 50 1 45 18) : 150 
solution (42 44 1 45 18) : 150 
solution (6 50 18 13 45 18) : 150 
solution (24 50 13 45 18) : 150 
solution (24 6 44 13 45 18) : 150 
solution (30 44 13 45 18) : 150 
solution (6 50 13 18 45 18) : 150 
solution (24 50 13 45 18) : 150 
solution (24 6 44 13 45 18) : 150 
solution (30 44 13 45 18) : 150 
solution (30 6 13 18 13 1 16 35 18) : 150 
solution (6 48 13 13 1 16 35 18) : 150 
solution (30 24 13 13 1 16 35 18) : 150 
solution (24 6 50 1 16 35 18) : 150 
solution (30 50 1 16 35 18) : 150 
solution (30 6 44 1 16 35 18) : 150 
solution (50 18 13 16 35 18) : 150 
solution (6 44 18 13 16 35 18) : 150 
solution (24 44 13 16 35 18) : 150 
solution (50 13 18 16 35 18) : 150 
solution (6 44 13 18 16 35 18) : 150 
solution (24 44 13 16 35 18) : 150 
solution (24 6 18 13 1 35 35 18) : 150 
solution (30 18 13 1 35 35 18) : 150 
solution (48 13 1 35 35 18) : 150 
solution (42 6 13 1 35 35 18) : 150 
solution (24 6 13 18 1 35 35 18) : 150 
solution (30 13 18 1 35 35 18) : 150 
solution (48 13 1 35 35 18) : 150 
solution (42 6 13 1 35 35 18) : 150 
solution (30 6 13 13 35 35 18) : 150 
solution (44 18 35 35 18) : 150 
solution (41 24 18 13 1 35 18) : 150 
solution (42 41 13 1 35 18) : 150 
solution (41 24 13 18 1 35 18) : 150 
solution (24 6 48 18 1 35 18) : 150 
solution (30 48 18 1 35 18) : 150 
solution (42 30 6 18 1 35 18) : 150 
solution (42 41 13 1 35 18) : 150 
solution (42 6 48 1 35 18) : 150 
solution (42 30 24 1 35 18) : 150 
solution (42 24 18 13 35 18) : 150 
solution (41 24 6 13 13 35 18) : 150 
solution (30 41 13 13 35 18) : 150 
solution (30 6 48 13 35 18) : 150 
solution (42 24 13 18 35 18) : 150 
solution (30 6 48 13 35 18) : 150 
solution (41 6 50 35 18) : 150 
solution (30 6 13 18 13 1 35 16 18) : 150 
solution (6 48 13 13 1 35 16 18) : 150 
solution (30 24 13 13 1 35 16 18) : 150 
solution (24 6 50 1 35 16 18) : 150 
solution (30 50 1 35 16 18) : 150 
solution (30 6 44 1 35 16 18) : 150 
solution (50 18 13 35 16 18) : 150 
solution (6 44 18 13 35 16 18) : 150 
solution (24 44 13 35 16 18) : 150 
solution (50 13 18 35 16 18) : 150 
solution (6 44 13 18 35 16 18) : 150 
solution (24 44 13 35 16 18) : 150 
solution (41 24 6 13 18 13 1 16 18) : 150 
solution (30 41 13 18 13 1 16 18) : 150 
solution (30 6 48 18 13 1 16 18) : 150 
solution (41 48 13 13 1 16 18) : 150 
solution (42 41 6 13 13 1 16 18) : 150 
solution (30 24 48 13 1 16 18) : 150 
solution (42 30 24 6 13 1 16 18) : 150 
solution (30 6 48 13 18 1 16 18) : 150 
solution (41 6 50 18 1 16 18) : 150 
solution (30 24 48 13 1 16 18) : 150 
solution (42 30 24 6 13 1 16 18) : 150 
solution (41 24 50 1 16 18) : 150 
solution (41 24 6 44 1 16 18) : 150 
solution (30 41 44 1 16 18) : 150 
solution (24 48 13 18 13 16 18) : 150 
solution (42 24 6 13 18 13 16 18) : 150 
solution (42 30 13 18 13 16 18) : 150 
solution (41 44 18 13 16 18) : 150 
solution (42 48 13 13 16 18) : 150 
solution (41 44 13 18 16 18) : 150 
solution (48 50 18 16 18) : 150 
solution (42 6 50 18 16 18) : 150 
solution (6 48 44 18 16 18) : 150 
solution (30 24 44 18 16 18) : 150 
solution (42 24 50 16 18) : 150 
solution (24 48 44 16 18) : 150 
solution (42 24 6 44 16 18) : 150 
solution (42 30 44 16 18) : 150 
solution (41 24 18 13 1 35 18) : 150 
solution (42 41 13 1 35 18) : 150 
solution (41 24 13 18 1 35 18) : 150 
solution (24 6 48 18 1 35 18) : 150 
solution (30 48 18 1 35 18) : 150 
solution (42 30 6 18 1 35 18) : 150 
solution (42 41 13 1 35 18) : 150 
solution (42 6 48 1 35 18) : 150 
solution (42 30 24 1 35 18) : 150 
solution (42 24 18 13 35 18) : 150 
solution (41 24 6 13 13 35 18) : 150 
solution (30 41 13 13 35 18) : 150 
solution (30 6 48 13 35 18) : 150 
solution (42 24 13 18 35 18) : 150 
solution (30 6 48 13 35 18) : 150 
solution (41 6 50 35 18) : 150 
solution (6 44 50 18 13 1 18) : 150 
solution (24 44 50 13 1 18) : 150 
solution (6 44 50 13 18 1 18) : 150 
solution (41 24 48 18 1 18) : 150 
solution (42 41 24 6 18 1 18) : 150 
solution (42 30 41 18 1 18) : 150 
solution (24 44 50 13 1 18) : 150 
solution (42 41 48 1 18) : 150 
solution (30 41 24 6 18 13 18) : 150 
solution (41 24 6 48 13 18) : 150 
solution (30 41 48 13 18) : 150 
solution (42 30 41 6 13 18) : 150 
solution (30 41 24 6 13 18 18) : 150 
solution (42 24 48 18 18) : 150 
solution (41 24 6 48 13 18) : 150 
solution (30 41 48 13 18) : 150 
solution (42 30 41 6 13 18) : 150 
solution (30 6 13 1 35 45 20) : 150 
solution (30 6 13 1 35 45 20) : 150 
solution (6 13 18 13 35 45 20) : 150 
solution (24 13 13 35 45 20) : 150 
solution (50 35 45 20) : 150 
solution (6 44 35 45 20) : 150 
solution (24 13 18 13 1 16 45 20) : 150 
solution (42 13 13 1 16 45 20) : 150 
solution (50 18 1 16 45 20) : 150 
solution (6 44 18 1 16 45 20) : 150 
solution (24 44 1 16 45 20) : 150 
solution (6 50 13 16 45 20) : 150 
solution (6 50 13 16 45 20) : 150 
solution (30 6 13 1 35 45 20) : 150 
solution (30 6 13 1 35 45 20) : 150 
solution (6 13 18 13 35 45 20) : 150 
solution (24 13 13 35 45 20) : 150 
solution (50 35 45 20) : 150 
solution (6 44 35 45 20) : 150 
solution (41 24 6 13 1 45 20) : 150 
solution (30 41 13 1 45 20) : 150 
solution (42 24 18 1 45 20) : 150 
solution (41 24 6 13 1 45 20) : 150 
solution (30 41 13 1 45 20) : 150 
solution (30 6 48 1 45 20) : 150 
solution (41 13 18 13 45 20) : 150 
solution (6 48 18 13 45 20) : 150 
solution (30 24 18 13 45 20) : 150 
solution (24 48 13 45 20) : 150 
solution (42 24 6 13 45 20) : 150 
solution (42 30 13 45 20) : 150 
solution (6 48 13 18 45 20) : 150 
solution (30 24 13 18 45 20) : 150 
solution (24 48 13 45 20) : 150 
solution (42 24 6 13 45 20) : 150 
solution (42 30 13 45 20) : 150 
solution (41 44 45 20) : 150 
solution (24 6 13 1 35 16 35 20) : 150 
solution (30 13 1 35 16 35 20) : 150 
solution (24 6 13 1 35 16 35 20) : 150 
solution (30 13 1 35 16 35 20) : 150 
solution (13 18 13 35 16 35 20) : 150 
solution (44 35 16 35 20) : 150 
solution (41 6 18 13 1 16 35 20) : 150 
solution (41 24 13 1 16 35 20) : 150 
solution (41 6 13 18 1 16 35 20) : 150 
solution (30 24 6 18 1 16 35 20) : 150 
solution (41 24 13 1 16 35 20) : 150 
solution (24 6 48 1 16 35 20) : 150 
solution (30 48 1 16 35 20) : 150 
solution (42 30 6 1 16 35 20) : 150 
solution (48 18 13 16 35 20) : 150 
solution (42 6 18 13 16 35 20) : 150 
solution (42 24 13 16 35 20) : 150 
solution (48 13 18 16 35 20) : 150 
solution (42 6 13 18 16 35 20) : 150 
solution (42 24 13 16 35 20) : 150 
solution (41 18 1 35 35 20) : 150 
solution (41 6 13 35 35 20) : 150 
solution (42 18 35 35 20) : 150 
solution (41 6 13 35 35 20) : 150 
solution (30 24 6 35 35 20) : 150 
solution (50 13 18 13 1 35 20) : 150 
solution (6 44 13 18 13 1 35 20) : 150 
solution (24 44 13 13 1 35 20) : 150 
solution (44 50 1 35 20) : 150 
solution (30 41 6 18 35 20) : 150 
solution (41 6 48 35 20) : 150 
solution (30 41 24 35 20) : 150 
solution (41 6 18 13 1 35 16 20) : 150 
solution (41 24 13 1 35 16 20) : 150 
solution (41 6 13 18 1 35 16 20) : 150 
solution (30 24 6 18 1 35 16 20) : 150 
solution (41 24 13 1 35 16 20) : 150 
solution (24 6 48 1 35 16 20) : 150 
solution (30 48 1 35 16 20) : 150 
solution (42 30 6 1 35 16 20) : 150 
solution (48 18 13 35 16 20) : 150 
solution (42 6 18 13 35 16 20) : 150 
solution (42 24 13 35 16 20) : 150 
solution (48 13 18 35 16 20) : 150 
solution (42 6 13 18 35 16 20) : 150 
solution (42 24 13 35 16 20) : 150 
solution (6 44 50 13 1 16 20) : 150 
solution (41 6 48 18 1 16 20) : 150 
solution (30 41 24 18 1 16 20) : 150 
solution (6 44 50 13 1 16 20) : 150 
solution (41 24 48 1 16 20) : 150 
solution (42 41 24 6 1 16 20) : 150 
solution (42 30 41 1 16 20) : 150 
solution (42 41 18 13 16 20) : 150 
solution (30 41 24 6 13 16 20) : 150 
solution (42 41 13 18 16 20) : 150 
solution (42 6 48 18 16 20) : 150 
solution (42 30 24 18 16 20) : 150 
solution (30 41 24 6 13 16 20) : 150 
solution (42 24 48 16 20) : 150 
solution (50 13 18 13 1 35 20) : 150 
solution (6 44 13 18 13 1 35 20) : 150 
solution (24 44 13 13 1 35 20) : 150 
solution (44 50 1 35 20) : 150 
solution (30 41 6 18 35 20) : 150 
solution (41 6 48 35 20) : 150 
solution (30 41 24 35 20) : 150 
solution (41 44 13 18 13 1 20) : 150 
solution (48 50 18 13 1 20) : 150 
solution (42 6 50 18 13 1 20) : 150 
solution (6 48 44 18 13 1 20) : 150 
solution (30 24 44 18 13 1 20) : 150 
solution (42 24 50 13 1 20) : 150 
solution (24 48 44 13 1 20) : 150 
solution (42 24 6 44 13 1 20) : 150 
solution (42 30 44 13 1 20) : 150 
solution (48 50 13 18 1 20) : 150 
solution (42 6 50 13 18 1 20) : 150 
solution (6 48 44 13 18 1 20) : 150 
solution (30 24 44 13 18 1 20) : 150 
solution (42 24 50 13 1 20) : 150 
solution (24 48 44 13 1 20) : 150 
solution (42 24 6 44 13 1 20) : 150 
solution (42 30 44 13 1 20) : 150 
solution (30 6 50 13 18 13 20) : 150 
solution (42 44 13 18 13 20) : 150 
solution (6 48 50 13 13 20) : 150 
solution (30 24 50 13 13 20) : 150 
solution (30 24 6 44 13 13 20) : 150 
solution (30 6 44 50 20) : 150 
solution (18 1 35 16 35 45) : 150 
solution (6 13 35 16 35 45) : 150 
solution (6 13 35 16 35 45) : 150 
solution (41 13 16 35 45) : 150 
solution (30 6 18 16 35 45) : 150 
solution (41 13 16 35 45) : 150 
solution (6 48 16 35 45) : 150 
solution (30 24 16 35 45) : 150 
solution (6 50 13 1 35 45) : 150 
solution (6 50 13 1 35 45) : 150 
solution (44 13 13 35 45) : 150 
solution (41 13 35 16 45) : 150 
solution (30 6 18 35 16 45) : 150 
solution (41 13 35 16 45) : 150 
solution (6 48 35 16 45) : 150 
solution (30 24 35 16 45) : 150 
solution (44 13 18 13 1 16 45) : 150 
solution (41 24 6 18 16 45) : 150 
solution (30 41 18 16 45) : 150 
solution (41 48 16 45) : 150 
solution (42 41 6 16 45) : 150 
solution (6 50 13 1 35 45) : 150 
solution (6 50 13 1 35 45) : 150 
solution (44 13 13 35 45) : 150 
solution (30 24 6 13 18 13 1 45) : 150 
solution (24 6 48 13 13 1 45) : 150 
solution (30 48 13 13 1 45) : 150 
solution (42 30 6 13 13 1 45) : 150 
solution (41 50 13 1 45) : 150 
solution (41 6 44 13 1 45) : 150 
solution (30 6 50 18 1 45) : 150 
solution (42 44 18 1 45) : 150 
solution (41 50 13 1 45) : 150 
solution (41 6 44 13 1 45) : 150 
solution (6 48 50 1 45) : 150 
solution (30 24 50 1 45) : 150 
solution (30 24 6 44 1 45) : 150 
solution (24 50 18 13 45) : 150 
solution (24 6 44 18 13 45) : 150 
solution (30 44 18 13 45) : 150 
solution (42 50 13 45) : 150 
solution (48 44 13 45) : 150 
solution (42 6 44 13 45) : 150 
solution (24 50 13 18 45) : 150 
solution (24 6 44 13 18 45) : 150 
solution (30 44 13 18 45) : 150 
solution (42 50 13 45) : 150 
solution (48 44 13 45) : 150 
solution (42 6 44 13 45) : 150 
solution (50 13 1 35 16 35) : 150 
solution (6 44 13 1 35 16 35) : 150 
solution (50 13 1 35 16 35) : 150 
solution (6 44 13 1 35 16 35) : 150 
solution (6 48 13 18 13 1 16 35) : 150 
solution (30 24 13 18 13 1 16 35) : 150 
solution (24 48 13 13 1 16 35) : 150 
solution (42 24 6 13 13 1 16 35) : 150 
solution (42 30 13 13 1 16 35) : 150 
solution (41 44 13 1 16 35) : 150 
solution (24 6 50 18 1 16 35) : 150 
solution (30 50 18 1 16 35) : 150 
solution (30 6 44 18 1 16 35) : 150 
solution (41 44 13 1 16 35) : 150 
solution (48 50 1 16 35) : 150 
solution (42 6 50 1 16 35) : 150 
solution (6 48 44 1 16 35) : 150 
solution (30 24 44 1 16 35) : 150 
solution (24 44 18 13 16 35) : 150 
solution (30 6 50 13 16 35) : 150 
solution (42 44 13 16 35) : 150 
solution (24 44 13 18 16 35) : 150 
solution (30 6 50 13 16 35) : 150 
solution (42 44 13 16 35) : 150 
solution (48 18 13 1 35 35) : 150 
solution (42 6 18 13 1 35 35) : 150 
solution (42 24 13 1 35 35) : 150 
solution (48 13 18 1 35 35) : 150 
solution (42 6 13 18 1 35 35) : 150 
solution (42 24 13 1 35 35) : 150 
solution (30 6 13 18 13 35 35) : 150 
solution (6 48 13 13 35 35) : 150 
solution (30 24 13 13 35 35) : 150 
solution (24 6 50 35 35) : 150 
solution (30 50 35 35) : 150 
solution (30 6 44 35 35) : 150 
solution (42 41 18 13 1 35) : 150 
solution (30 41 24 6 13 1 35) : 150 
solution (42 41 13 18 1 35) : 150 
solution (42 6 48 18 1 35) : 150 
solution (42 30 24 18 1 35) : 150 
solution (30 41 24 6 13 1 35) : 150 
solution (42 24 48 1 35) : 150 
solution (41 24 6 13 18 13 35) : 150 
solution (30 41 13 18 13 35) : 150 
solution (30 6 48 18 13 35) : 150 
solution (41 48 13 13 35) : 150 
solution (42 41 6 13 13 35) : 150 
solution (30 24 48 13 35) : 150 
solution (42 30 24 6 13 35) : 150 
solution (30 6 48 13 18 35) : 150 
solution (41 6 50 18 35) : 150 
solution (30 24 48 13 35) : 150 
solution (42 30 24 6 13 35) : 150 
solution (41 24 50 35) : 150 
solution (41 24 6 44 35) : 150 
solution (30 41 44 35) : 150 
solution (6 48 13 18 13 1 35 16) : 150 
solution (30 24 13 18 13 1 35 16) : 150 
solution (24 48 13 13 1 35 16) : 150 
solution (42 24 6 13 13 1 35 16) : 150 
solution (42 30 13 13 1 35 16) : 150 
solution (41 44 13 1 35 16) : 150 
solution (24 6 50 18 1 35 16) : 150 
solution (30 50 18 1 35 16) : 150 
solution (30 6 44 18 1 35 16) : 150 
solution (41 44 13 1 35 16) : 150 
solution (48 50 1 35 16) : 150 
solution (42 6 50 1 35 16) : 150 
solution (6 48 44 1 35 16) : 150 
solution (30 24 44 1 35 16) : 150 
solution (24 44 18 13 35 16) : 150 
solution (30 6 50 13 35 16) : 150 
solution (42 44 13 35 16) : 150 
solution (24 44 13 18 35 16) : 150 
solution (30 6 50 13 35 16) : 150 
solution (42 44 13 35 16) : 150 
solution (41 48 13 18 13 1 16) : 150 
solution (42 41 6 13 18 13 1 16) : 150 
solution (30 24 48 18 13 1 16) : 150 
solution (42 30 24 6 18 13 1 16) : 150 
solution (42 41 24 13 13 1 16) : 150 
solution (42 24 6 48 13 1 16) : 150 
solution (42 30 48 13 1 16) : 150 
solution (30 24 48 13 18 1 16) : 150 
solution (42 30 24 6 13 18 1 16) : 150 
solution (41 24 50 18 1 16) : 150 
solution (41 24 6 44 18 1 16) : 150 
solution (30 41 44 18 1 16) : 150 
solution (42 24 6 48 13 1 16) : 150 
solution (42 30 48 13 1 16) : 150 
solution (42 41 50 1 16) : 150 
solution (41 48 44 1 16) : 150 
solution (42 41 6 44 1 16) : 150 
solution (42 48 13 18 13 16) : 150 
solution (30 24 6 48 13 13 16) : 150 
solution (41 24 6 50 13 16) : 150 
solution (30 41 50 13 16) : 150 
solution (30 41 6 44 13 16) : 150 
solution (42 24 50 18 16) : 150 
solution (24 48 44 18 16) : 150 
solution (42 24 6 44 18 16) : 150 
solution (42 30 44 18 16) : 150 
solution (41 24 6 50 13 16) : 150 
solution (30 41 50 13 16) : 150 
solution (30 41 6 44 13 16) : 150 
solution (30 6 48 50 16) : 150 
solution (42 48 44 16) : 150 
solution (42 41 18 13 1 35) : 150 
solution (30 41 24 6 13 1 35) : 150 
solution (42 41 13 18 1 35) : 150 
solution (42 6 48 18 1 35) : 150 
solution (42 30 24 18 1 35) : 150 
solution (30 41 24 6 13 1 35) : 150 
solution (42 24 48 1 35) : 150 
solution (41 24 6 13 18 13 35) : 150 
solution (30 41 13 18 13 35) : 150 
solution (30 6 48 18 13 35) : 150 
solution (41 48 13 13 35) : 150 
solution (42 41 6 13 13 35) : 150 
solution (30 24 48 13 35) : 150 
solution (42 30 24 6 13 35) : 150 
solution (30 6 48 13 18 35) : 150 
solution (41 6 50 18 35) : 150 
solution (30 24 48 13 35) : 150 
solution (42 30 24 6 13 35) : 150 
solution (41 24 50 35) : 150 
solution (41 24 6 44 35) : 150 
solution (30 41 44 35) : 150 
solution (24 44 50 18 13 1) : 150 
solution (42 44 50 13 1) : 150 
solution (24 44 50 13 18 1) : 150 
solution (42 41 48 18 1) : 150 
solution (42 44 50 13 1) : 150 
solution (30 41 24 6 48 1) : 150 
solution (41 24 6 48 18 13) : 150 
solution (30 41 48 18 13) : 150 
solution (42 30 41 6 18 13) : 150 
solution (24 6 44 50 13 13) : 150 
solution (30 44 50 13 13) : 150 
solution (42 41 6 48 13) : 150 
solution (42 30 41 24 13) : 150 
solution (41 24 6 48 13 18) : 150 
solution (30 41 48 13 18) : 150 
solution (42 30 41 6 13 18) : 150 
solution (42 41 6 48 13) : 150 
solution (42 30 41 24 13) : 150 
solution (42 30 24 6 48) : 150 
$16 = 1304

1304 solutions of egg nog



|#

(define (foo2 xs required)
  (let ((nsol 0)
	(min-length 99999999999999))
    (letrec ((fiz (lambda (ys xs t)
		    ;;(format #t "fiz ys ~a xs ~a t ~a ~%" ys xs t)
		    (cond
		     ((= t required)
		      (format #t "solution ~a : ~a ~%" xs t)
		      (let ((len (length xs)))
			(when (< len min-length)
			  (set! min-length len)))
		      (set! nsol (1+ nsol)))
		     ((null? ys) #f)		     
		     ((> t required) #f)
		     (#t (fiz (cdr ys) (cons (car ys) xs) (+ t (car ys)))
			 (fiz (cdr ys) xs t))))))
		     
		  
      (let ((tot 0))
	(fiz xs '() tot)
	(values nsol min-length)))))

(define (foo2b xs required require-len)
  (let ((nsol 0))
    (letrec ((fiz (lambda (ys xs t)
		    ;;(format #t "fiz ys ~a xs ~a t ~a ~%" ys xs t)
		    (cond
		     ((= t required)
		      (let ((len (length xs)))
			(when (= len require-len)
			  (format #t "solution ~a : ~a ~%" xs t)		      
			  (set! nsol (1+ nsol)))))
		     ((null? ys) #f)		     
		     ((> t required) #f)
		     (#t (fiz (cdr ys) (cons (car ys) xs) (+ t (car ys)))
			 (fiz (cdr ys) xs t))))))
      (let ((tot 0))
	(fiz xs '() tot)
	nsol))))



(define (example2)
  (call-with-values (lambda () (foo2 input2 25))
    (lambda (nsol min-len)
      (format #t "example2 : nsol = ~a : min-len ~a ~%" nsol min-len)
      (foo2b input2 25 min-len)
      )))




;;  (foo2 input 150))

(define (part-2)
  (call-with-values (lambda () (foo2 input 150))
    (lambda (nsol min-len)
      (format #t "example2 : nsol = ~a : min-len ~a ~%" nsol min-len)
      (foo2b input 150 min-len)
      )))


#|

example2 : nsol = 1304 : min-len 4 
solution (24 48 45 33) : 150 
solution (42 30 45 33) : 150 
solution (41 50 45 14) : 150 
solution (42 44 50 14) : 150 
solution (50 35 45 20) : 150 
solution (50 35 45 20) : 150 
solution (41 44 45 20) : 150 
solution (41 48 16 45) : 150 
solution (42 50 13 45) : 150 
solution (48 44 13 45) : 150 
solution (42 50 13 45) : 150 
solution (48 44 13 45) : 150 
solution (30 50 35 35) : 150 
solution (41 24 50 35) : 150 
solution (30 41 44 35) : 150 
solution (42 48 44 16) : 150 
solution (41 24 50 35) : 150 
solution (30 41 44 35) : 150 
$31 = 18

scheme@(guile-user) [7]>

18 solutions of length 4 

|#












