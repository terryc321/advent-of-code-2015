
(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...
(use-modules (srfi srfi-2)) ;; first second third ...


;; (use-modules (rnrs)) ;; assert

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

;; (define *debug* #f)
;; 
;; (define input #f)
;; 
;; ;; in this puzzle we want to read the input file 
;; (define (get-input filename)
;;   (let ((port (open-input-file filename)))
;;     (set! input (read port))
;;     (close-input-port port)))
;; 
;; ;; for example
;; ;;(define input #f)
;; 
;; (get-input "input")
;; ;;(get-input "input2")
;; 
;; input
;; -----------------------------------------------------------
;; (format #t "hello world!~%")

#|
(getcwd)

|#
(use-modules (ice-9 binary-ports))

;; (define (read-bytes333 port)
;;   (format #t "reading bytes ... on port ~a ~%" port)
;;   (let ((w '()))	
;;     (letrec ((foo (lambda ()
;; 		    (let ((p (get-u8 port)))
;; 		      (format #t "read byte ~a ~%" p)
;; 		      (cond
;; 		       ((eof-object? p) w)
;; 		       ((= p 10)
;; 			(format #t "found a newline !~%")
;; 			w)
;; 		       (#t (set! w (cons p w))
;; 			   (foo)))))))
;;       (foo)
;;       (reverse w))))
;; 
;; 
;; (define (reaction)
;;   (call-with-input-file "input"
;;     (lambda (port)
;;       (let ((known '()))
;; 	(letrec ((foo (lambda ()
;; 			(let ((w (read-bytes333 port)))
;; 			  (cond
;; 			   ((null? w) known)
;; 			   (#t (set! known (cons w known))
;; 			       (foo)))))))
;; 	  (set! known (reverse known))
;; 	  known)))))

(define (sanity)
  (define port (open-input-file "input" #:binary #t))
  (letrec ((foo (lambda ()
		  (let ((s (get-u8 port)))
		    (cond
		     ((eof-object? s) #f)
		     (#t
		      (format #t "~a " s)
		      (foo)))))))
    (foo)
    (close-input-port port)))


(define (strip)
  (define port (open-input-file "input" #:binary #t))
  (let ((word '())
	(words '()))
    (letrec ((foo (lambda ()
		    (let ((s (get-u8 port)))
		      (cond
		       ((eof-object? s)
			(cond ((null? word) #f)
			      (#t (set! words (cons (reverse word) words))))
			#f)
		       ((= s 10)
			(format #t "~%")
			(set! words (cons (reverse word) words))
			(set! word '())
			(foo))
		       (#t
			(format #t "~a " s)
			(set! word (cons s word))
			(foo)))))))
      (foo)
      (close-input-port port)
      (set! words (reverse words))
      (with-output-to-file "stripped.out"
	(lambda ()
	  (dolist (ws words)
		  (dolist (w ws)
			  (format #t "~a " w))
		  (format #t "~%"))))
      (process words))))

#|
\    -> \\
"    -> \"
\x27 -> \\
|#
(define (process words)
  (let ((byte 0)
	(char 0)
	(backslash-char (char->integer #\\))
	(quote-char (char->integer #\")))
    (dolist (ws words)
	    ;; two quotes either side of word
	    (set! byte (+ byte 2)) 
	    (dolist (w ws)
		    (cond
		     ((= w backslash-char)
		      (set! byte (+ byte 2))
		      (set! char (+ char 1)))
		     ((= w quote-char)
		      (set! byte (+ byte 2))
		      (set! char (+ char 1)))
		     (#t
		      (set! byte (+ byte 1))
		      (set! char (+ char 1))))))
    (format #t "~%~%")
    (format #t " byte ~a : char ~a : diff = ~a ~%~%" byte char (- byte char))))


(define (run)
  (strip))


#|


 byte 8356 : char 6310 : diff = 2046 

$14 = #t

 .......... ACCEPTED ........ 2046 .........tfkft


|#
		      





;;(define port (open-input-file "input" #:binary #t))




;; ;;(read-bytes)
;; (reaction)
;; 
;; (first (reaction))




