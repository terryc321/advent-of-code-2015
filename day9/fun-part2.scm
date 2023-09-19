

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
(set! input (get-input "input"))

(define towns '())
(define distances #f)

(define (get-towns)
  (set! towns '())
  (set! distances (make-hash-table))
  (letrec ((fiz (lambda (xs rs)
		  (cond
		   ((null? xs) rs)
		   (#t (match (car xs)
			 ((src dest dist)
			  ;;(format #t "matched ~a ~a ~a ~%" src dest dist)
			  ;; distances via these routes
			  (hash-set! distances (list src dest) dist)
			  (hash-set! distances (list dest src) dist)
			  
			  (when (not (member src towns))
			    (set! towns (cons src towns)))
			  (when (not (member dest towns))
			    (set! towns (cons dest towns)))			  
			  (fiz (cdr xs) rs))
			 (#t (error "get-towns" (list "mis-match ")))))))))
    (fiz input '())
    towns))

(define (get-dist a b)
  (let ((dist (hash-ref distances (list a b))))
    (assert (integer? dist))
    dist))


(define (-town here avail-towns)
  (filter (lambda (town) (not (eq? town here)))
	  avail-towns))


(define longest 0)

(define (foo)
  (get-towns)
  (set! longest -999999999999999999999)
  (letrec ((fuz (lambda (here dests dist)
		  ;;(format #t "      here ~a  : dests = ~a : dist = ~a ~%" here dests dist)
		  (when (null? dests)
		    ;;(format #t " ~a ~%" dist)
		    (when (>= dist longest)
		      (format #t "new or equal longest ~a ~%" dist)
		      (set! longest dist))
		    )
		  (dolist (s dests)
			  (let ((d (hash-ref distances (list here s))))			    
			    (when d
			      ;;(format #t "   : ~a -> ~a " here s)
			      (fuz s (-town s dests) (+ dist d))))))))
    (dolist (t towns)
	    ;;(format #t "start = ~a ~%" t)
	    (fuz t (-town t towns) 0))
    longest))


#|

scheme@(guile-user) [4]> ,time (foo)
new or equal longest 464 
new or equal longest 560 
new or equal longest 566 
new or equal longest 590 
new or equal longest 645 
new or equal longest 673 
new or equal longest 718 
new or equal longest 733 
new or equal longest 746 
new or equal longest 776 
new or equal longest 781 
new or equal longest 787 
new or equal longest 800 
new or equal longest 804 
new or equal longest 817 
new or equal longest 849 
new or equal longest 854 
new or equal longest 860 
new or equal longest 871 
new or equal longest 872 
new or equal longest 890 
new or equal longest 895 
new or equal longest 895 
new or equal longest 898 
new or equal longest 898 
$57 = 898

898 <<<<<<<<<<<<<<  accepted longest route 

;; 0.040134s real time, 0.053591s run time.  0.016774s spent in GC.
scheme@(guile-user) [4]>

|#



    









	 
