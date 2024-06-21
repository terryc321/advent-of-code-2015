

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
;;(define input #f)

(get-input "input")
;;(get-input "input2")


(define (foo)
  (let ((f 1))
  (dolist (x input)
	  (match x
	    ((op x y x2 y2)
	     ;;(format #t "matched OP[~a] ~a ~a -> ~a ~a ~%" op x y x2 y2)
	     (gen op x y x2 y2 f)
	     (set! f (+ f 1))
	     )
	    (#t (error "match fail" (list (format #f "could not match ~a " x))))))))


(define (gen op x y x2 y2 n)
  (cond
   ((eq? op 'on) (gen-on x y x2 y2 n))
   ((eq? op 'off) (gen-off x y x2 y2 n))
   ((eq? op 'toggle) (gen-toggle x y x2 y2 n))
   (#t (error "gen" (list op x y x2 y2)))))


(define (gen-on x y x2 y2 n)
  (format #t "(define (f~a a b s) " n)
  (format #t "(if (and (>= a ~a) (<= a ~a) (>= b ~a)(<= b ~a)) " x x2 y y2)
  (format #t "(f~a a b 1) (f~a a b s))) ~%" (+ n 1) (+ n 1)))


(define (gen-off x y x2 y2 n)
  (format #t "(define (f~a a b s) " n)
  (format #t "(if (and (>= a ~a) (<= a ~a) (>= b ~a)(<= b ~a)) " x x2 y y2)
  (format #t "(f~a a b 0) (f~a a b s))) ~%" (+ n 1) (+ n 1)))

(define (gen-toggle x y x2 y2 n)
  (format #t "(define (f~a a b s) " n)
  (format #t "(if (and (>= a ~a) (<= a ~a) (>= b ~a)(<= b ~a)) " x x2 y y2)
  (format #t "(f~a a b (- 1 s)) (f~a a b s))) ~%" (+ n 1) (+ n 1)))







	    



