

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
;;(set! input (get-input "input"))
#|
;; substitutions 
(define subs
  '(
Al   ThF
Al   ThRnFAr
B   BCa
B   TiB
B   TiRnFAr
Ca   CaCa
Ca   PB
Ca   PRnFAr
Ca   SiRnFYFAr
Ca   SiRnMgAr
Ca   SiTh
F   CaF
F   PMg
F   SiAl
H   CRnAlAr
H   CRnFYFYFAr
H   CRnFYMgAr
H   CRnMgYFAr
H   HCa
H   NRnFYFAr
H   NRnMgAr
H   NTh
H   OB
H   ORnFAr
Mg   BF
Mg   TiMg
N   CRnFAr
N   HSi
O   CRnFYFAr
O   CRnMgAr
O   HP
O   NRnFAr
O   OTi
P   CaP
P   PTi
P   SiRnFAr
Si   CaSi
Th   ThCa
Ti   BP
Ti   TiTi
e   HF
e   NAl
e   OMg
))
|#
(define input "CRnCaCaCaSiRnBPTiMgArSiRnSiRnMgArSiRnCaFArTiTiBSiThFYCaFArCaCaSiThCaPBSiThSiThCaCaPTiRnPBSiThRnFArArCaCaSiThCaSiThSiRnMgArCaPTiBPRnFArSiThCaSiRnFArBCaSiRnCaPRnFArPMgYCaFArCaPTiTiTiBPBSiThCaPTiBPBSiRnFArBPBSiRnCaFArBPRnSiRnFArRnSiRnBFArCaFArCaCaCaSiThSiThCaCaPBPTiTiRnFArCaPTiBSiAlArPBCaCaCaCaCaSiRnMgArCaSiThFArThCaSiThCaSiRnCaFYCaSiRnFYFArFArCaSiRnFYFArCaSiRnBPMgArSiThPRnFArCaSiRnFArTiRnSiRnFYFArCaSiRnBFArCaSiRnTiMgArSiThCaSiThCaFArPRnFArSiRnFArTiTiTiTiBCaCaSiRnCaCaFYFArSiThCaPTiBPTiBCaSiThSiRnMgArCaF")


#|
(define (pairwise xs)
  (cond
   ((null? xs) xs)
   (#t (cons (list (car xs) (cadr xs))
	     (pairwise (cdr (cdr xs)))))))

(define subs (pairwise (map symbol->string subs)))
|#

(define subs '(("Al" "ThF") ("Al" "ThRnFAr") ("B" "BCa") ("B" "TiB") ("B" "TiRnFAr") ("Ca" "CaCa") ("Ca" "PB") ("Ca" "PRnFAr") ("Ca" "SiRnFYFAr") ("Ca" "SiRnMgAr") ("Ca" "SiTh") ("F" "CaF") ("F" "PMg") ("F" "SiAl") ("H" "CRnAlAr") ("H" "CRnFYFYFAr") ("H" "CRnFYMgAr") ("H" "CRnMgYFAr") ("H" "HCa") ("H" "NRnFYFAr") ("H" "NRnMgAr") ("H" "NTh") ("H" "OB") ("H" "ORnFAr") ("Mg" "BF") ("Mg" "TiMg") ("N" "CRnFAr") ("N" "HSi") ("O" "CRnFYFAr") ("O" "CRnMgAr") ("O" "HP") ("O" "NRnFAr") ("O" "OTi") ("P" "CaP") ("P" "PTi") ("P" "SiRnFAr") ("Si" "CaSi") ("Th" "ThCa") ("Ti" "BP") ("Ti" "TiTi") ("e" "HF") ("e" "NAl") ("e" "OMg")))

(define hash (make-hash-table))


(substring "asdf" 0 1)
(substring "asdf" 1 2)
(substring "asdf" 1 3)
(substring "asdf" 2 3)
(substring "asdf" 3 4)
;;(substring "asdf" 3 5)

(substring "thisisasubstring" 0 4)

(define (sstr s a b)
  (substring s (- a 1) b))

(sstr "thisisasubstring" 1 4)

(let ((s "thisisalongstringiwantitall"))
  (sstr s 1 (string-length s)))

;; scheme@(guile-user)> (map (lambda (x) (string-length (car x))) subs)
;; $8 = (2 2 1 1 1 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 1 1 1 1 1 1 1 1 1 1 2 2 2 2 1 1 1)

(define (sub-1 a b str)
  (let ((str-len (string-length str))
	(ch (string-ref a 0)))
    (letrec ((foo (lambda (i)
		    (cond
		     ((>= i str-len) #f)
		     (#t (when (char=? (string-ref str i) ch)

			   (let ((gen
				  (format #f "~a~a~a"
					  (substring str 0 i)
					  b
					  (substring str (+ i 1) str-len))))
			     (hash-set! hash gen gen))
			     
			   ;; (format #t "~a" (substring str 0 i))
			   ;; (format #t "~a" b)
			   ;; (format #t "~a~%" (substring str (+ i 1) str-len))

			   )
			 (foo (+ i 1)))))))
      (foo 0))))

(sub-1 "a" "bbb" "aaaa")

(define (sub-2 a b str)
  (let ((str-len (string-length str))
	(ch (string-ref a 0))
	(ch2 (string-ref a 1)))
    (letrec ((foo (lambda (i)
		    (cond
		     ((>= i (- str-len 1)) #f)
		     (#t (when (and (char=? (string-ref str i) ch)
				    (char=? (string-ref str (+ i 1)) ch2))

			   (let ((gen
				  (format #f "~a~a~a"
					  (substring str 0 i)
					  b
					  (substring str (+ i 2) str-len))))
			     (hash-set! hash gen gen))
			   
			   ;; (format #t "~a" (substring str 0 i))
			   ;; (format #t "~a" b)
			   ;; (format #t "~a~%" (substring str (+ i 2) str-len))
			   
			   )
			 (foo (+ i 1)))))))
      (foo 0))))

;;(sub-2 "ab" "california" "ababab")

(define (foo)
  (set! hash (make-hash-table))
  (dolist (s subs)
	  (let ((a (car s))
		(b (cadr s)))
	    (cond
	     ((= (string-length a) 1) (sub-1 a b input))
	     ((= (string-length a) 2) (sub-2 a b input))
	     (#t (error "foo" (list "string-length on substituion not 1 or 2 "))))))
  hash)


#|
scheme@(guile-user)> (foo)
$18 = #<hash-table 7f84e4b98f40 535/883>

535 <-------- accepted answer

|#




		  






