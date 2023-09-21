

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
(set! input (get-input "input"))



#|

B km/s for C seconds and rest for D seconds

((Vixen 19 7 124)
 (Rudolph 3 15 28)
 (Donner 19 9 164)
 (Blitzen 19 9 158)
 (Comet 13 7 82)
 (Cupid 25 6 145)
 (Dasher 14 3 38)
 (Dancer 3 16 37)
 (Prancer 25 6 143))

Vixen dist 19 * 7 or 133 km and rests of 124  total time of 131

 time 2503 after seconds ... where is each deer ?
|#

;; coment 14 10 127 1000
;; dancer 16 11 162 1000
(define (model-deer name a b c time-limit)
  (let ((time 0)
	(dist 0)
	(i 0))
    (letrec ((tick (lambda ()
		     (set! time (+ time 1))
		     ;;(format #t "debug ~a : ~a ~%" time dist)
		     (when (= time time-limit)
		       (format #t "~a -> ~a : ~a ~%" name time dist))))
	     (lup (lambda ()
		    (let ((i b))
		      (while (> i 0)
			(set! dist (+ dist a))			
			(set! i (- i 1))
			(tick)))
		    (let ((i c))
		      (while (> i 0)
			(tick)
			(set! i (- i 1))))
		    (when (< time (+ time-limit 100))
		      (lup)))))
      (lup))))

(define (model-deer-helper name a b c)
  (model-deer name a b c 2503))


(define (solve)
  (dolist (deer input)
	  (apply model-deer-helper deer)))


;;(model-deer 'debug 10 3 5)

#|

|#

(model-deer 'coment 14 10 127 1)

(model-deer 'dancer 16 11 162 1)

(model-deer 'coment 14 10 127 10)

(model-deer 'dancer 16 11 162 10)

(model-deer 'coment 14 10 127 11)

(model-deer 'dancer 16 11 162 11)

(model-deer 'coment 14 10 127 12)

(model-deer 'dancer 16 11 162 12)

(model-deer 'coment 14 10 127 138)

(model-deer 'dancer 16 11 162 138)


(model-deer 'coment 14 10 127 173)

(model-deer 'dancer 16 11 162 170)
(model-deer 'dancer 16 11 162 171)
(model-deer 'dancer 16 11 162 172)
(model-deer 'dancer 16 11 162 173)
(model-deer 'dancer 16 11 162 174)



(model-deer 'coment 14 10 127 174)

(model-deer 'dancer 16 11 162 174)


(model-deer 'coment 14 10 127 1000)

(model-deer 'dancer 16 11 162 1000)


#|

Vixen -> 2503 : 2660 
Rudolph -> 2503 : 2637 
Donner -> 2503 : 2565 
Blitzen -> 2503 : 2565 
Comet -> 2503 : 2639   <------ 2639 ..... 
Cupid -> 2503 : 2550 
Dasher -> 2503 : 2590 
Dancer -> 2503 : 2292 
Prancer -> 2503 : 2550 


|#

