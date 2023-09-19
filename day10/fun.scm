

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
;;(set! input (get-input "input"))

(define input "1113222113")

(define (string->list s)  
  (let ((len (string-length s))
	(i 0))
    (letrec ((fiz (lambda (i rs)
		    (cond
		     ((>= i len) (reverse rs))
		     (#t (fiz (+ i 1) (cons (string->number (format #f "~a" (string-ref s i)))
					    rs)))))))
      (fiz i '()))))

(string->list "112233")
;; (1 1 2 2 3 3)
;; works on list representation of string
;; initially set up as counting 1st item count

(define (foo->string xs)
  (cond
   ((null? xs) "")
   (#t (match (car xs)
	 ((ct n)
	  (string-append (format #f "~a~a" ct n) (foo->string (cdr xs))))
	 (#t (error "list->string" (list "mismatch")))))))

;;(equal? input (list->string (string->list input)))



(define (fiz xs)
  (letrec ((fuz (lambda (n ct ys rs)
		  (cond
		   ((null? ys) (reverse (cons (list ct n) rs)))
		   ((not (= n (car ys)))
		    (fuz (car ys) 1 (cdr ys) (cons (list ct n) rs)))
		   (#t
		    (fuz n (+ ct 1) (cdr ys) rs))))))
    (fuz (car xs) 1 (cdr xs) '())))



(define (foo s)
  (let ((xs (string->list s)))
    (fiz xs)))
    
(define (test s)
  (fiz (string->list s)))

 ;; example:
 ;;    1 becomes 11 (1 copy of digit 1).
 ;;    11 becomes 21 (2 copies of digit 1).
 ;;    21 becomes 1211 (one 2 followed by one 1).
 ;;    1211 becomes 111221 (one 1, one 2, and two 1s).
 ;;    111221 becomes 312211 (three 1s, two 2s, and one 1).
(test "1")
(test "11")
(test "21")
(test "1211")
(test "111221")

(define (fun s)
  (foo->string (foo s)))



;; apply foo 40 times 
(define (repeat f n in)
  ;;(format #t "~a : ~a  : ~% ~a : ~a ~%" n in n (string-length in))
  (format #t "~a : ~a  ~%" n (string-length in))
  (cond   
   ((>= n 50) in)
   (#t (repeat f (+ n 1) (f in)))))


(define (solve)
  (repeat fun 0 input)
  #t)


#|

0 : 10  
1 : 10  
2 : 12  
3 : 18  
4 : 22  
5 : 28  
6 : 38  
7 : 48  
8 : 60  
9 : 76  
10 : 100  
11 : 118  
12 : 146  
13 : 192  
14 : 246  
15 : 328  
16 : 428  
17 : 562  
18 : 732  
19 : 970  
20 : 1250  
21 : 1622  
22 : 2128  
23 : 2754  
24 : 3592  
25 : 4728  
26 : 6162  
27 : 8072  
28 : 10504  
29 : 13736  
30 : 17874  
31 : 23248  
32 : 30322  
33 : 39392  
34 : 51438  
35 : 67070  
36 : 87402  
37 : 114056  
38 : 148740  
39 : 193828  
40 : 252594    <<<<<<<----------- 252,594
41 : 329356  

|#


#|

Enter `,help' for help.
scheme@(guile-user)> (load "fun.scm")
scheme@(guile-user)> ,time (solve)
0 : 10  
1 : 10  
2 : 12  
3 : 18  
4 : 22  
5 : 28  
6 : 38  
7 : 48  
8 : 60  
9 : 76  
10 : 100  
11 : 118  
12 : 146  
13 : 192  
14 : 246  
15 : 328  
16 : 428  
17 : 562  
18 : 732  
19 : 970  
20 : 1250  
21 : 1622  
22 : 2128  
23 : 2754  
24 : 3592  
25 : 4728  
26 : 6162  
27 : 8072  
28 : 10504  
29 : 13736  
30 : 17874  
31 : 23248  
32 : 30322  
33 : 39392  
34 : 51438  
35 : 67070  
36 : 87402  
37 : 114056  
38 : 148740  
39 : 193828  
40 : 252594  
41 : 329356  
42 : 429204  
43 : 559092  
44 : 729412  
45 : 950310  
46 : 1239364  
47 : 1615860  
48 : 2106324  
49 : 2746068
........... oo one shy ..........


|#






	 
