

(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...
(use-modules (srfi srfi-2)) ;; first second third ...

;; regular expression
(use-modules (ice-9 regex)) 

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

(define ilen (string-length input))

(define floor 0)


#|

i index into input string
ilen length of input string
floor is floor santa is on

|#
(define (foo i)
  (call/cc (lambda (exit)
	     (cond
	      ((>= i ilen)
	       (format #t "done. floor = ~a ~%" floor)
	       floor)
	      (#t
	       (let ((ch (string-ref input i)))
		 (cond
		  ((char=? ch #\( ) (set! floor (+ floor 1))
		   (foo (+ i 1)))
		  ((char=? ch #\) ) (set! floor (- floor 1))
		   (when (< floor 0)
		     (exit (+ i 1)))
		   (foo (+ i 1)))
		  (#t (error (list (format #f "unrecognised character at index ~a" i)))))))))))

(define (fuz)
  (set! floor 0)
  (foo 0))

(define (bar)
  (set! floor 0)
  (let ((index (foo 0)))
    (format #t "index santa in basement = ~a ~%" index)))




       
    
  
	      

