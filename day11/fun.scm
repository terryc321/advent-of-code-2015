

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

(define code (make-array 0 8))

(define alphabet (iota 26 97))

#|
scheme@(guile-user)> (char->integer #\o)
$25 = 111
scheme@(guile-user)> (char->integer #\i)
$26 = 105
scheme@(guile-user)> (char->integer #\l)
$27 = 108

i o u
|#



;; 0 to 7
(define index (iota 8))

(define count 0)

(define (reset)
  (map (lambda (ch i)
	 (array-set! code ch i)
	 )
       (map char->integer '(#\c #\q #\j #\x #\j #\n #\d #\s))
       (iota 8))
  code)


(reset)

#|
scheme@(guile-user)> (char->integer #\c)
$31 = 99
scheme@(guile-user)> (char->integer #\q)
$32 = 113
scheme@(guile-user)> (char->integer #\j)
$33 = 106
scheme@(guile-user)> (char->integer #\x)
$34 = 120
scheme@(guile-user)> (char->integer #\j)
$35 = 106
scheme@(guile-user)> (char->integer #\n)
$36 = 110
scheme@(guile-user)> (char->integer #\d)
$37 = 100
scheme@(guile-user)> (char->integer #\s)
$38 = 115
scheme@(guile-user)> code
$39 = #(99 113 106 120 106 110 100 115)
scheme@(guile-user)> 
|#

;; this is not allowed
(define (contain-iol)
  (call/cc (lambda (exit)
	     (dolist (i index)
		     (let ((n (array-ref code i)))
		       (when ;; i o l
			   (or (= n 105)
			       (= n 111)
			       (= n 108))
			 (exit #t))))
	     #f)))

#|
scheme@(guile-user)> (array-set! code (char->integer #\i) 2)
scheme@(guile-user)> code
$75 = #(99 100 105 120 106 110 100 115)
scheme@(guile-user)> (contain-iol)
$76 = #t
|#

(define (contain-two-pair)
  (or (and (= (array-ref code 0) (array-ref code 1))
           (= (array-ref code 2) (array-ref code 3)))
      (and (= (array-ref code 0) (array-ref code 1))
           (= (array-ref code 3) (array-ref code 4)))
      (and (= (array-ref code 0) (array-ref code 1))
           (= (array-ref code 4) (array-ref code 5)))
      (and (= (array-ref code 0) (array-ref code 1))
           (= (array-ref code 5) (array-ref code 6)))
      (and (= (array-ref code 0) (array-ref code 1))
           (= (array-ref code 6) (array-ref code 7)))
      (and (= (array-ref code 1) (array-ref code 2))
           (= (array-ref code 3) (array-ref code 4)))
      (and (= (array-ref code 1) (array-ref code 2))
           (= (array-ref code 4) (array-ref code 5)))
      (and (= (array-ref code 1) (array-ref code 2))
           (= (array-ref code 5) (array-ref code 6)))
      (and (= (array-ref code 1) (array-ref code 2))
           (= (array-ref code 6) (array-ref code 7)))
      (and (= (array-ref code 2) (array-ref code 3))
           (= (array-ref code 4) (array-ref code 5)))
      (and (= (array-ref code 2) (array-ref code 3))
           (= (array-ref code 5) (array-ref code 6)))
      (and (= (array-ref code 2) (array-ref code 3))
           (= (array-ref code 6) (array-ref code 7)))
      (and (= (array-ref code 3) (array-ref code 4))
           (= (array-ref code 5) (array-ref code 6)))
      (and (= (array-ref code 3) (array-ref code 4))
           (= (array-ref code 6) (array-ref code 7)))
      (and (= (array-ref code 4) (array-ref code 5))
           (= (array-ref code 6) (array-ref code 7)))))

#|
scheme@(guile-user)> code
$57 = #(99 113 106 120 106 110 100 115)
scheme@(guile-user)> (array-set! code 99 1)
scheme@(guile-user)> code
$58 = #(99 99 106 120 106 110 100 115)
scheme@(guile-user)> (array-set! code 106 3)
scheme@(guile-user)> code
$59 = #(99 99 106 106 106 110 100 115)
scheme@(guile-user)> (contain-two-pair)
$60 = #t

|#


(define (contain-triple)
(or (let ((a (array-ref code 0))
          (b (array-ref code 1))
          (c (array-ref code 2)))
      (and (>= a 0)
           (> b a)
           (> c b)
           (= a (- b 1) (- c 2))))
    (let ((a (array-ref code 1))
          (b (array-ref code 2))
          (c (array-ref code 3)))
      (and (>= a 0)
           (> b a)
           (> c b)
           (= a (- b 1) (- c 2))))
    (let ((a (array-ref code 2))
          (b (array-ref code 3))
          (c (array-ref code 4)))
      (and (>= a 0)
           (> b a)
           (> c b)
           (= a (- b 1) (- c 2))))
    (let ((a (array-ref code 3))
          (b (array-ref code 4))
          (c (array-ref code 5)))
      (and (>= a 0)
           (> b a)
           (> c b)
           (= a (- b 1) (- c 2))))
    (let ((a (array-ref code 4))
          (b (array-ref code 5))
          (c (array-ref code 6)))
      (and (>= a 0)
           (> b a)
           (> c b)
           (= a (- b 1) (- c 2))))
    (let ((a (array-ref code 5))
          (b (array-ref code 6))
          (c (array-ref code 7)))
      (and (>= a 0)
           (> b a)
           (> c b)
           (= a (- b 1) (- c 2))))))

#|
scheme@(guile-user)> (array-set! code 99 1)
scheme@(guile-user)> code
$65 = #(99 99 106 120 106 110 100 115)
scheme@(guile-user)> (array-set! code 99 2)
scheme@(guile-user)> code
$66 = #(99 99 99 120 106 110 100 115)
scheme@(guile-user)> (contain-triple)
$67 = #f
scheme@(guile-user)> (contain-triple)
$68 = #f
scheme@(guile-user)> code
$69 = #(99 99 99 120 106 110 100 115)
scheme@(guile-user)> (contain-triple)
$70 = #f
scheme@(guile-user)> (array-set! code 100 1)
scheme@(guile-user)> (array-set! code 101 2)
scheme@(guile-user)> (contain-triple)
$71 = #t
scheme@(guile-user)> code
$72 = #(99 100 101 120 106 110 100 115)

|#


;; looking at code
(define (is-password?)
  (and (not (contain-iol))
       (contain-two-pair)
       (contain-triple)))


#|

p p q q
0 1 2 3 4 5 6 7

|#
(define (foo)
  (let ((ex '(or )))
    (dolist (p index)
	    (when (<= p 4)
	      (dolist (q index)
		      (when (and (<= (+ q 1) 7) (>= q (+ p 2)))
			(format #t "~a ~a : ~a ~a ~%" p (+ p 1) q (+ q 1))
			(set! ex (cons
				  `(and (= (array-ref code ,p) (array-ref code ,(+ p 1)))
					(= (array-ref code ,q) (array-ref code ,(+ q 1))))
				  ex ))
			))))
    (reverse ex)))


#|
;; triple

a b c
......
          a b c
0 1 2 3 4 5 6 7

|#
(define (fuz)
  (let ((ex '(or)))
    (dolist (p index)
	    (when (<= p 5)
	      (set! ex (cons
			`(let ((a (array-ref code ,p))
				(b (array-ref code ,(+ p 1)))
				(c (array-ref code ,(+ p 2))))
			    (and (>= a 0)(> b a)(> c b)(= a (- b 1)(- c 2))))
			ex))))
    (reverse ex)))


#|
counter
|#


(define (next)
  (let ((lo 97)
	(hi 122))
    (array-set! code (+ 1 (array-ref code 7)) 7)
    (when (> (array-ref code 7) hi)
      (array-set! code lo 7)
      (array-set! code (1+ (array-ref code 6)) 6)) 
    (when (> (array-ref code 6) hi)
      (array-set! code lo 6)
      (array-set! code (1+ (array-ref code 5)) 5)) 
    (when (> (array-ref code 5) hi)
      (array-set! code lo 5)
      (array-set! code (1+ (array-ref code 4)) 4)) 
    (when (> (array-ref code 4) hi)
      (array-set! code lo 4)
      (array-set! code (1+ (array-ref code 3)) 3)) 
    (when (> (array-ref code 3) hi)
      (array-set! code lo 3)
      (array-set! code (1+ (array-ref code 2)) 2)) 
    (when (> (array-ref code 2) hi)
      (array-set! code lo 2)
      (array-set! code (1+ (array-ref code 1)) 1)) 
    (when (> (array-ref code 1) hi)
      (array-set! code lo 1)
      (array-set! code (1+ (array-ref code 0)) 0)) ))

  
  

(define (baz)
  (dolist (i '(7 6 5 4 3 2 1))
	  (format #t "(when (> (array-ref code ~a) hi)~%" i)
	  (format #t "  (array-set! code lo ~a)~%" i)
	  (format #t "  (array-set! code (1+ (array-ref code ~a)) ~a)) ~%" (- i 1) (- i 1))))

	  
	  
	  


(define (solve)
  (let ((t 0)
	(n 0)
	(sol 0))
  (call/cc (lambda (exit)
	   (letrec ((loop (lambda ()
			    (when (is-password?)
			      (format #t "~% *** solution *** ~a *** ~%" code)
			      (set! sol (1+ sol))
			      (when (> sol 2)
				(exit code)))
			    (next)
			    (set! t (1+ t))
			    (set! n (1+ n))
			    (when (> t 10000)
			      (format #t "~a : ~a ~%" n code)
			      (set! t 0))
			    (loop))))
	     (loop))))))

#|
scheme@(guile-user) [1]> (reset)
$99 = #(99 113 106 120 106 110 100 115)
scheme@(guile-user) [1]> (solve)
10001 : #(99 113 106 120 107 98 121 106) 
20002 : #(99 113 106 120 107 113 116 97) 
30003 : #(99 113 106 120 108 102 110 114) 
40004 : #(99 113 106 120 108 117 105 105) 
50005 : #(99 113 106 120 109 106 99 122) 
60006 : #(99 113 106 120 109 120 120 113) 
70007 : #(99 113 106 120 110 109 115 104) 
80008 : #(99 113 106 120 111 98 109 121) 
90009 : #(99 113 106 120 111 113 104 112) 
100010 : #(99 113 106 120 112 102 99 103) 
110011 : #(99 113 106 120 112 116 119 120) 
120012 : #(99 113 106 120 113 105 114 111) 
130013 : #(99 113 106 120 113 120 109 102) 
140014 : #(99 113 106 120 114 109 103 119) 
150015 : #(99 113 106 120 115 98 98 110) 
160016 : #(99 113 106 120 115 112 119 101) 
170017 : #(99 113 106 120 116 101 113 118) 
180018 : #(99 113 106 120 116 116 108 109) 
190019 : #(99 113 106 120 117 105 103 100) 
200020 : #(99 113 106 120 117 120 97 117) 
210021 : #(99 113 106 120 118 108 118 108) 
220022 : #(99 113 106 120 119 97 113 99) 
230023 : #(99 113 106 120 119 112 107 116) 
240024 : #(99 113 106 120 120 101 102 107) 
250025 : #(99 113 106 120 120 116 97 98) 

 *** solution *** #(99 113 106 120 120 121 122 122) *** 
$100 = #(99 113 106 120 120 121 122 122)
scheme@(guile-user) [1]> (map integer->char '(99 113 106 120 120 121 122 122))
$101 = (#\c #\q #\j #\x #\x #\y #\z #\z)
scheme@(guile-user) [1]> "cqjxxyzz"

$102 = "cqjxxyzz"  <<<<<<<<<<<<<<<<<<<<------------- solution accepted 
scheme@(guile-user) [1]>


|#



#|

2nd solution ......

1180118 : #(99 113 106 122 121 113 119 119) 
1190119 : #(99 113 106 122 122 102 114 110) 
1200120 : #(99 113 106 122 122 117 109 101) 

 *** solution *** #(99 113 107 97 97 98 99 99) *** 
1210121 : #(99 113 107 97 97 106 103 118) 
1220122 : #(99 113 107 97 97 121 98 109) 
1230123 : #(99 113 107 97 98 109 119 100) 
1240124 : #(99 113 107 97 99 98 113 117) 
1250125 : #(99 113 107 97 99 113 108 108) 
1260126 : #(99 113 107 97 100 102 103 99) 
1270127 : #(99 113 107 97 100 117 97 116) 
1280128 : #(99 113 107 97 101 105 118 107) 
1290129 : #(99 113 107 97 101 120 113 98) 
1300130 : #(99 113 107 97 102 109 107 115) 
1310131 : #(99 113 107 97 103 98 102 106) 
1320132 : #(99 113 107 97 103 113 97 97) 
1330133 : #(99 113 107 97 104 101 117 114) 
1340134 : #(99 113 107 97 104 116 112 105) 
1350135 : #(99 113 107 97 105 105 106 122) 
1360136 : #(99 113 107 97 105 120 101 113) 
1370137 : #(99 113 107 97 106 108 122 104) 
1380138 : #(99 113 107 97 107 97 116 121) 
1390139 : #(99 113 107 97 107 112 111 112) 
1400140 : #(99 113 107 97 108 101 106 103) 
1410141 : #(99 113 107 97 108 116 100 120) 
1420142 : #(99 113 107 97 109 104 121 111) 
1430143 : #(99 113 107 97 109 119 116 102) 
1440144 : #(99 113 107 97 110 108 110 119) 
1450145 : #(99 113 107 97 111 97 105 110) 
ice-9/boot-9.scm:1685:16: In procedure raise-exception:
User interrupt

Entering a new prompt.  Type `,bt' for a backtrace or `,q' to continue.
scheme@(guile-user) [2]> #|
1180118 : #(99 113 106 122 121 113 119 119) 
1190119 : #(99 113 106 122 122 102 114 110) 
1200120 : #(99 113 106 122 122 117 109 101) 

 *** solution *** #(99 113 107 97 97 98 99 99) *** 
1210121 : #(99 113 107 97 97 106 103 118) 
1220122 : #(99 113 107 97 97 121 98 109) 

|#

While compiling expression:
Syntax error:
unknown file:2130:0: unquote: expression not valid outside of quasiquote in form (unquote geiser-no-values)
scheme@(guile-user) [2]> (map integer->char '(99 113 107 97 97 98 99 99))
$104 = (#\c #\q #\k #\a #\a #\b #\c #\c)
scheme@(guile-user) [2]> "cqkaabcc"
$105 = "cqkaabcc"  <<<<<<<<<<<<<<<-----------------------------------

scheme@(guile-user) [2]>


|#
  
  
	    



#|
Passwords must include one increasing straight of at least three letters,
like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't count.

Passwords may not contain the letters i, o, or l, as these letters can be
mistaken for other characters and are therefore confusing.

Passwords must contain at least two different,
non-overlapping pairs of letters, like aa, bb, or zz.

|#



#|

only 8


char->integer  ascii value
integer->char  ascii letter from ascii value

$5 = #(0 0 0 0 0 0 0 0)
scheme@(guile-user)> (char->integer #\a)
$6 = 97
scheme@(guile-user)> (char->integer #\z)
$7 = 122
scheme@(guile-user)> (char->integer #\a)
$8 = 97
scheme@(guile-user)> (char->integer #\z)
$9 = 122
scheme@(guile-user)> (iota 10 1)
$10 = (1 2 3 4 5 6 7 8 9 10)
scheme@(guile-user)> (iota 10 97)
$11 = (97 98 99 100 101 102 103 104 105 106)
scheme@(guile-user)> (iota 10 26)
$12 = (26 27 28 29 30 31 32 33 34 35)
scheme@(guile-user)> (iota 26 97)
$13 = (97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122)
scheme@(guile-user)> (char->integer #\a)
$14 = 97
scheme@(guile-user)> (char->integer #\z)
$15 = 122
scheme@(guile-user)> (iota 26 97)
$16 = (97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122)
scheme@(guile-user)> alphabet
$17 = (97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122)
scheme@(guile-user)> (map integer->char alphabet)
$18 = (#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)
scheme@(guile-user)> (char->integer #\i)
$19 = 105
scheme@(guile-user)> (char->integer #\o)
$20 = 111
scheme@(guile-user)> (char->integer #\u)
$21 = 117
scheme@(guile-user)> 
|#



