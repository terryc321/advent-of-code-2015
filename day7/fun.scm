

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


;; here are reg ex matching rules 

(define m #f)

(define (m-not s)
  (set! m (string-match "NOT ([a-z0-9]+) -> ([a-z]+)" s))
  m)

(define (m-and s)
  (set! m (string-match "([a-z0-9]+) AND ([a-z0-9]+) -> ([a-z]+)" s))
  m)

(define (m-r-shift s)
  (set! m (string-match "([a-z0-9]+) RSHIFT ([a-z0-9]+) -> ([a-z]+)" s))
  m)

(define (m-l-shift s)
  (set! m (string-match "([a-z0-9]+) LSHIFT ([a-z0-9]+) -> ([a-z]+)" s))
  m)
 

(define (m-or s)
  (set! m (string-match "([a-z0-9]+) OR ([a-z0-9]+) -> ([a-z]+)" s))
  m)


(define (m-dir s)
  (set! m (string-match "([a-z0-9]+) -> ([a-z]+)" s))
  m)


;; some tests 
(m-and "lf AND lq -> ls")
(m-r-shift "iu RSHIFT 1 -> jn")
(m-or "bo OR bu -> bv")
(m-l-shift "ip LSHIFT 15 -> it")
(match:substring (m-not "NOT el -> em") 2)
(m-dir  "19138 -> b") 


(define (all-chars s)
  (call/cc (lambda (exit)
  (let ((slen (string-length s))
	(i 0))
    (while (< i slen)
      (let ((ch (string-ref s i)))
	(cond
	 ((member ch '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u
		       #\v #\w #\x #\y #\z)) #t)
	 (#t (exit #f)))
	(set! i (+ i 1))))
    #t))))


(define (all-digits s)
  (call/cc (lambda (exit)
  (let ((slen (string-length s))
	(i 0))
    (while (< i slen)
      (let ((ch (string-ref s i)))
	(cond
	 ((member ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) #t)
	 (#t (exit #f)))
	(set! i (+ i 1))))
    #t))))







#|
16 bit

 Left shift
 Right shift
 AND
 oR 
 NOT

binary to decimal conversion 

increase exponent
multiplying by 2 each time 
2 ^ 0 * a +
2 ^ 1 * b +

binary

decimal to binary conversion

13 = 1101 b

|#

;; extended 16 bit
(define (ex16b xs n)
  (cond
   ((<= n 0) xs)
   (#t (ex16b (cons 0 xs) (- n 1)))))

(define (ex16 xs)  
  (let ((len (length xs)))
    (ex16b xs (- 16 len))))


;; MSB first ... LSB 
(define (dec->bin2 d rs)
  (cond
   ((zero? d) rs)
   (#t (dec->bin2 (floor (/ d 2)) (cons (remainder d 2) rs)))))

(define (dec->bin d)
  (ex16 (dec->bin2 d '())))

;; LSB first ... MSB
(define (bin->dec2 xs p pwr)
  (cond
   ((null? xs) p)
   (#t (bin->dec2 (cdr xs) (+ p (* (car xs) pwr)) (* pwr 2)))))

(define (bin->dec xs)
  (bin->dec2 (reverse xs) 0 1))

(define (or16 xs ys)
  (cond
   ((null? xs) '())
   (#t (let ((a (car xs))
	     (b (car ys)))
	 (cond
	  ((= a 1) (cons 1 (or16 (cdr xs) (cdr ys))))
	  ((= b 1) (cons 1 (or16 (cdr xs) (cdr ys))))
	  (#t (cons 0 (or16 (cdr xs) (cdr ys)))))))))

(define (not16 xs)
  (cond
   ((null? xs) '())
   (#t (let ((a (car xs)))
	 (cond
	  ((= a 1) (cons 0 (not16 (cdr xs))))
	  (#t (cons 1 (not16 (cdr xs)))))))))

(define (and16 xs ys)
  (cond
   ((null? xs) '())
   (#t (let ((a (car xs))
	     (b (car ys)))
	 (cond
	  ((and (= a 1) (= b 1)) (cons 1 (and16 (cdr xs) (cdr ys))))
	  (#t (cons 0 (and16 (cdr xs) (cdr ys)))))))))

(define (shift16-l xs n)
  (cond
   ((<= n 0) xs)
   (#t (shift16-l (append (cdr xs) (list 0)) (- n 1)))))

(define (shift16-r xs n)
  (cond
   ((<= n 0) xs)
   (#t
    (shift16-r (cons 0 (take xs 15)) (- n 1)))))

;; argument checking

(define symbols '())
(define constants '())
(define store #f)

(define (maybe-symbol s)
  (cond
   ((all-chars s) (cond
		   ((member (string->symbol s) symbols) #f)
		   (#t (set! symbols (cons (string->symbol s) symbols)))))
   ((all-digits s) (cond
		    ((member (string->number s) constants) #f)
		    (#t (set! constants (cons (string->number s) constants)))))
   (#t #f)))



;; pre-processing stage
;; collect all wire symbols
;; 
(define (pre)
  (set! symbols '())
  (let ((f 1))
    (dolist (x input)
	    ;;(format #t "processing : ~a ~%" x)
	    (cond
	     ((m-not x) (let ((s1 (match:substring m 1))
			      (s2 (match:substring m 2)))
			  ;;(format #t "NOT check s1 = [~a] : s2 = [~a] ~%" s1 s2)
			  (maybe-symbol s1)
			  (maybe-symbol s2)			  
			  ))
	     ((m-and x) (let ((s1 (match:substring m 1))
			      (s2 (match:substring m 2))
			      (s3 (match:substring m 3)))
			  ;;(format #t "AND check s1 = [~a] : s2 = [~a] : s3 = [~a] ~%" s1 s2 s3)
			  (maybe-symbol s1)
			  (maybe-symbol s2)
			  (maybe-symbol s3)
			  ))
	     ((m-r-shift x)
	      (let ((s1 (match:substring m 1))
		    (s2 (match:substring m 2))
		    (s3 (match:substring m 3)))
		;;(format #t "RSHIFT check s1 = [~a] : s2 = [~a] : s3 = [~a]~%" s1 s2 s3)
		;; (all-chars s1)
		;; (all-digits s2)
		;; (all-chars s3)
		(maybe-symbol s1)
		(maybe-symbol s2)
		(maybe-symbol s3)
			  
		))	      
	     ((m-l-shift x)
	      (let ((s1 (match:substring m 1))
		    (s2 (match:substring m 2))
		    (s3 (match:substring m 3)))
		;;(format #t "LSHIFT check s1 = [~a] : s2 = [~a] : s3 = [~a]~%" s1 s2 s3)
		;; (all-chars s1)
		;; (all-digits s2)
		;; (all-chars s3)
		(maybe-symbol s1)
		(maybe-symbol s2)
		(maybe-symbol s3)
		
		
		))
	     ((m-or x)
	      (let ((s1 (match:substring m 1))
		    (s2 (match:substring m 2))
		    (s3 (match:substring m 3)))
		;;(format #t "OR check s1 = [~a] : s2 = [~a] : s3 = [~a]~%" s1 s2 s3)
		;; (all-chars s1)
		;; (all-chars s2)
		;; (all-chars s3)
		(maybe-symbol s1)
		(maybe-symbol s2)
		(maybe-symbol s3)
		))
	     ((m-dir x)
	      (let ((s1 (match:substring m 1))
		    (s2 (match:substring m 2)))
		;;(format #t "DIRECT check s1 = [~a] : s2 = [~a] ~%" s1 s2)
		;; (all-digits s1)
		;; (all-chars s2)
		(maybe-symbol s1)
		(maybe-symbol s2)
		
		))
	     (#t (error pre "no match"))
	     ))))

(define (lookup-symbol2 s xs n)
  (cond
   ((null? xs) (error "lookup-symbol2" (list "symbol not found " s xs n)))
   ((eq? s (car xs)) n)
   (#t (lookup-symbol2 s (cdr xs) (+ n 1)))))

(define (sym-index s)
  (lookup-symbol2 (string->symbol s) symbols 0))


(define (pre2)
  (set! store (make-array #f (length symbols)))
  (let ((f 1))
    (dolist (x input)
	    ;;(format #t "processing : ~a ~%" x)
	    (cond
	     ((m-not x) (let ((s1 (match:substring m 1))
			      (s2 (match:substring m 2)))
			  ;;(format #t "NOT check s1 = [~a] : s2 = [~a] ~%" s1 s2)

			  (cond
			   ((and (all-chars s1)(all-chars s2))
			    (format #t "(not16 (sym ~a ~a) (sym ~a ~a))~%"
				    (sym-index s1) s1
				    (sym-index s2) s2))
			   (#t (error "pre2 m-not" (list "s1 s2 not symbols"))))
			  
			  ))
	     ((m-and x) (let ((s1 (match:substring m 1))
			      (s2 (match:substring m 2))
			      (s3 (match:substring m 3)))
			  ;;(format #t "AND check s1 = [~a] : s2 = [~a] : s3 = [~a] ~%" s1 s2 s3)
			  (cond
			   ((and (all-chars s1)(all-chars s2)(all-chars s3))
			    (format #t "(and16 (sym ~a ~a) (sym ~a ~a) (sym ~a ~a))~%"
				    (sym-index s1) s1
				    (sym-index s2) s2
			            (sym-index s3) s3
				    ))
			   ((and (all-digits s1)(all-chars s2)(all-chars s3))
			    (format #t "(and16 (const ~a) (sym ~a ~a) (sym ~a ~a))~%"
				    (string->number s1)
				    (sym-index s2) s2
			            (sym-index s3) s3
				    ))
			   (#t (error "pre2 m-and" (list "s1 s2 s3 not symbols" s1 s2 s3))))
			  
			  
			  ))
	     ((m-r-shift x)
	      (let ((s1 (match:substring m 1))
		    (s2 (match:substring m 2))
		    (s3 (match:substring m 3)))
		;;(format #t "RSHIFT check s1 = [~a] : s2 = [~a] : s3 = [~a]~%" s1 s2 s3)

			  (cond
			   ((and (all-chars s1)(all-digits s2)(all-chars s3))
			    (format #t "(shift-r (sym ~a ~a) (const ~a) (sym ~a ~a))~%"
				    (sym-index s1) s1
				    (string->number s2)
			            (sym-index s3) s3
				    ))			   
			   (#t (error "pre2 m-r-shift" (list "s1 s2 s3 not symbols" s1 s2 s3))))
		
		
		))	      
	     ((m-l-shift x)
	      (let ((s1 (match:substring m 1))
		    (s2 (match:substring m 2))
		    (s3 (match:substring m 3)))
		;;(format #t "LSHIFT check s1 = [~a] : s2 = [~a] : s3 = [~a]~%" s1 s2 s3)

			  (cond
			   ((and (all-chars s1)(all-digits s2)(all-chars s3))
			    (format #t "(shift-l (sym ~a ~a) (const ~a) (sym ~a ~a))~%"
				    (sym-index s1) s1
				    (string->number s2)
			            (sym-index s3) s3
				    ))			   
			   (#t (error "pre2 m-l-shift" (list "s1 s2 s3 not symbols" s1 s2 s3))))
		
		
		))
	     ((m-or x)
	      (let ((s1 (match:substring m 1))
		    (s2 (match:substring m 2))
		    (s3 (match:substring m 3)))
		;;(format #t "OR check s1 = [~a] : s2 = [~a] : s3 = [~a]~%" s1 s2 s3)

			  (cond
			   ((and (all-chars s1)(all-chars s2)(all-chars s3))
			    (format #t "(or16 (sym ~a ~a) (sym ~a ~a) (sym ~a ~a))~%"
				    (sym-index s1) s1
				    (sym-index s2) s2
			            (sym-index s3) s3
				    ))
			   ;; ((and (all-digits s1)(all-chars s2)(all-chars s3))
			   ;;  (format #t "(or16 (const ~a) (sym ~a ~a) (sym ~a ~a))~%"
			   ;; 	    (string->number s1)
			   ;; 	    (sym-index s2) s2
			   ;;          (sym-index s3) s3
			   ;; 	    ))
			   (#t (error "pre2 m-or" (list "s1 s2 s3 not symbols" s1 s2 s3))))
		
		
		))
	     ((m-dir x)
	      (let ((s1 (match:substring m 1))
		    (s2 (match:substring m 2)))
		;;(format #t "DIRECT check s1 = [~a] : s2 = [~a] ~%" s1 s2)

		(cond
		 ((and (all-chars s1)(all-chars s2))
		  (format #t "(assign (sym ~a ~a) (sym ~a ~a))~%"
			  (sym-index s1) s1
			  (sym-index s2) s2
			  ))
		 ((and (all-digits s1)(all-chars s2))
		  (format #t "(assign (const ~a) (sym ~a ~a))~%"
			  (string->number s1)
			  (sym-index s2) s2
			  ))
		 ;; ((and (all-digits s1)(all-chars s2)(all-chars s3))
		 ;;  (format #t "(or16 (const ~a) (sym ~a ~a) (sym ~a ~a))~%"
		 ;; 	    (string->number s1)
		 ;; 	    (sym-index s2) s2
		 ;;          (sym-index s3) s3
		 ;; 	    ))
		 (#t (error "pre2 m-dir" (list "s1 not digits s2 not symbol" s1 s2))))
		
		
		))
	     (#t (error pre "no match"))
	     ))))







;; (define (pre3)
;;   (set! store (make-array #f (length symbols)))
;;   (let ((f 1))
;;     (dolist (x input)
;; 	    ;;(format #t "processing : ~a ~%" x)
;; 	    (cond
;; 	     ((m-not x) (let ((s1 (match:substring m 1))
;; 			      (s2 (match:substring m 2)))
;; 			  ;;(format #t "NOT check s1 = [~a] : s2 = [~a] ~%" s1 s2)
;; 			  (maybe-symbol s1)
;; 			  (maybe-symbol s2)			  
;; 			  ))
;; 	     ((m-and x) (let ((s1 (match:substring m 1))
;; 			      (s2 (match:substring m 2))
;; 			      (s3 (match:substring m 3)))
;; 			  ;;(format #t "AND check s1 = [~a] : s2 = [~a] : s3 = [~a] ~%" s1 s2 s3)
;; 			  (maybe-symbol s1)
;; 			  (maybe-symbol s2)
;; 			  (maybe-symbol s3)
;; 			  ))
;; 	     ((m-r-shift x)
;; 	      (let ((s1 (match:substring m 1))
;; 		    (s2 (match:substring m 2))
;; 		    (s3 (match:substring m 3)))
;; 		;;(format #t "RSHIFT check s1 = [~a] : s2 = [~a] : s3 = [~a]~%" s1 s2 s3)
;; 		;; (all-chars s1)
;; 		;; (all-digits s2)
;; 		;; (all-chars s3)
;; 		(maybe-symbol s1)
;; 		(maybe-symbol s2)
;; 		(maybe-symbol s3)
			  
;; 		))	      
;; 	     ((m-l-shift x)
;; 	      (let ((s1 (match:substring m 1))
;; 		    (s2 (match:substring m 2))
;; 		    (s3 (match:substring m 3)))
;; 		;;(format #t "LSHIFT check s1 = [~a] : s2 = [~a] : s3 = [~a]~%" s1 s2 s3)
;; 		;; (all-chars s1)
;; 		;; (all-digits s2)
;; 		;; (all-chars s3)
;; 		(maybe-symbol s1)
;; 		(maybe-symbol s2)
;; 		(maybe-symbol s3)
		
		
;; 		))
;; 	     ((m-or x)
;; 	      (let ((s1 (match:substring m 1))
;; 		    (s2 (match:substring m 2))
;; 		    (s3 (match:substring m 3)))
;; 		;;(format #t "OR check s1 = [~a] : s2 = [~a] : s3 = [~a]~%" s1 s2 s3)
;; 		;; (all-chars s1)
;; 		;; (all-chars s2)
;; 		;; (all-chars s3)
;; 		(maybe-symbol s1)
;; 		(maybe-symbol s2)
;; 		(maybe-symbol s3)
;; 		))
;; 	     ((m-dir x)
;; 	      (let ((s1 (match:substring m 1))
;; 		    (s2 (match:substring m 2)))
;; 		;;(format #t "DIRECT check s1 = [~a] : s2 = [~a] ~%" s1 s2)
;; 		;; (all-digits s1)
;; 		;; (all-chars s2)
;; 		(maybe-symbol s1)
;; 		(maybe-symbol s2)
		
;; 		))
;; 	     (#t (error pre "no match"))
;; 	     ))))


(define (run)
  (pre)
  (pre2)
  ;;(pre3)
  )


    





















