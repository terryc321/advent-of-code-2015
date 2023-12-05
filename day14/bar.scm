;;(getcwd)
;;(chdir "day14")

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

;; reference implementation - but gives wrong final answer......??
(define (model-deer name a b c time-limit)
  (let ((time 0)
	(dist 0)
	(i 0))
    (call/cc (lambda (escape)
    (letrec ((tick (lambda ()
		     (set! time (+ time 1))
		     ;;(format #t "debug ~a : ~a ~%" time dist)
		     (when (>= time time-limit)
		       ;;(format #t "~a -> ~a : ~a ~%" name time dist)
		       (escape dist)
		       )))
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
      (lup)
      )))))




;; attempt to rewrite model-deer
(define (model-deer2 name speed duration sleep time-limit debug)
  (when debug
    (format #t "~%~%deer ~a :: over-all time limit ~a : speed ~a : duration ~a : sleep ~a ~%"
	    name time-limit speed duration sleep))
  (letrec ((run (lambda (dist time dur)
		  (when debug (format #t "running : time ~a  :  dist ~a ~%" time dist))
		  (cond
		   ((>= time time-limit) 
		    (when debug
		      (format #t "time up ! ::: time limit ~a : dist ~a ~%" 
			      time-limit
			      dist))
		    dist)
		   ((= dur 0) 
		    (when debug (format #t "instant sleep~%"))
		    (not-run dist time sleep))
		   (#t 		    
		    (run (+ dist speed) (+ time 1) (- dur 1))))))
	   (not-run (lambda (dist time sle)
		      (when debug (format #t "not-run : time ~a  :  dist ~a ~%" time dist))
		      (cond
		       ((>= time time-limit) 
			(when debug (format #t "time up ! ::: time limit ~a : dist ~a ~%" 
					    time-limit
					    dist))
			dist)
		       ((= sle 0) 
			(when debug (format #t "instant run~%"))
			(run dist time duration))
		       (#t (not-run dist (+ time 1) (- sle 1))))))
	   )
    ;; try kick things off , so 
    ;;(run speed 1 (- duration 1) sleep)
    (run 0 0 duration)))



    




;; (define (model-deer-helper name a b c)
;;   (model-deer name a b c 2503))

;; (define (solve)
;;   (dolist (deer input)
;; 	  (apply model-deer-helper deer)))


;;(model-deer 'debug 10 3 5)

#|

|#

;; comet flys 14km/s for 10 seconds then must rest 127 seconds
(define comet
  (lambda (after-n-seconds)
    (model-deer 'comet 14 10 127 after-n-seconds)))

#|
after 1 second comet has gone 14km
after 10 seconds comet has gone 140km
after 11 seconds comet at rest 140km
after 138 seconds comet flies for another 10 seconds
after 1000 seconds comet in lead at 1120 km
|#
(let ((com (equal? (map comet '(1 10 11 138 1000))
	   '(14 140 140 154 1120))))
  (cond
   ((not com)
    (format #t "FAIL - disagrees with example given for comet ~%")
    (error "comet test-case-001"))
   (#t #t)))


(map (lambda (n)(model-deer2 'comet 14 10 127 n #f)) '(1 10 11 138 1000))





;; dancer flys 16km/s for 11 seconds then must rest 162 seconds
(define dancer
  (lambda (after-n-seconds)
    (model-deer 'dancer 16 11 162 after-n-seconds)))

#|
after 1 second dancer gone 16 km
after 10 seonds dancer gone 160 km
after 12 seconds dancer 176km
on 174th second dancer flies for another 11 seconds
after 1000 seconds dancer gone 1056 km
|#
(let ((com (equal? (map dancer '(1 10 12 138 174 1000))
	   ' (16 160 176 176 192 1056))))
  (cond
   ((not com)
    (format #t "FAIL - disagrees with example given for dancer ~%")
    (error "dancer test-case-001"))
   (#t #t)))

(map (lambda (n)(model-deer2 'dancer 16 11 162 n #f)) '(1 10 12 138 174 1000))

(map (lambda (n)(model-deer2 'dancer 1 1 1 n #t)) '(1 2 3 4 5 10))





#|
Vixen can fly 19 km/s for 7 seconds, but then must rest for 124 seconds.
Rudolph can fly 3 km/s for 15 seconds, but then must rest for 28 seconds.
Donner can fly 19 km/s for 9 seconds, but then must rest for 164 seconds.
Blitzen can fly 19 km/s for 9 seconds, but then must rest for 158 seconds.
Comet can fly 13 km/s for 7 seconds, but then must rest for 82 seconds.
Cupid can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.
Dasher can fly 14 km/s for 3 seconds, but then must rest for 38 seconds.
Dancer can fly 3 km/s for 16 seconds, but then must rest for 37 seconds.
Prancer can fly 25 km/s for 6 seconds, but then must rest for 143 seconds.
|#
(define (after-race)
  (let ((race-limit 2503)) ;; 2503 seconds race
  (define (foo x)
    (let ((name (car x))
	  (flys (second x))
	  (duration (third x))
	  (sleeps (fourth x)))
      (list name (model-deer name flys duration sleeps race-limit))))
  (sort (map foo input)
	(lambda (x y) (>= (second x)(second y))))))


(define (one-k)
  (let ((race-limit 1000)) ;; who wins the race at 1000km 
  (define (foo x)
    (let ((name (car x))
	  (flys (second x))
	  (duration (third x))
	  (sleeps (fourth x)))
      (list name (model-deer2 name flys duration sleeps race-limit #f))))
  (sort (map foo input)
	(lambda (x y) (>= (second x)(second y))))))


#|

scheme@(guile-user) [2]> (pp (part-1))

expect output to be sorted ??

((Vixen 2660)  ;; 2660 ....  ??? must be an example of number blindness
 (Comet 2639)  ;; ......... why thinking comet won race ??
 (Rudolph 2637)
 (Dasher 2590)
 (Blitzen 2565)
 (Donner 2565)
 (Prancer 2550)
 (Cupid 2550)
 (Dancer 2292))
scheme@(guile-user) [2]> 
|#

(define (points-race n)
  (let ((race-limit n)) ;; n seconds race
  (define (foo x)
    (let ((name (car x))
	  (flys (second x))
	  (duration (third x))
	  (sleeps (fourth x)))
      (list name (model-deer name flys duration sleeps race-limit))))
  (sort (map foo input)
	(lambda (x y) (>= (second x)(second y))))))

(define (vixen-in-lead? outcome n)
  (let* ((m (apply max (map second outcome)))
	 (q (= m (second (assoc 'Vixen outcome)))))
    q))

(define (deer-in-lead? deer outcome n)
  (let* ((m (apply max (map second outcome)))
	 (q (= m (second (assoc deer outcome)))))
    (cond
     (q 1)
     (#t 0))))


(define table (make-vector 10 0))

(define (inc deer n)
  (let* ((alist (map list (map first input) (iota (length input))))
	 (index (second (assoc deer alist))))
    (vector-set! table index (+ n (vector-ref table index)))))

(define (recur n)
  (cond
   ((>= n 2504) #f)
   (#t 
    (let ((outcome (points-race n)))
      ;;(format #t "~a ~%" outcome)
      (map (lambda (deer)
	     (inc deer (deer-in-lead? deer outcome n)))
	   '(Vixen Rudolph Donner Blitzen Comet Cupid Dasher Dancer Prancer))
      (recur (+ n 1))))))

#|
think problem reduces to is vixen in the lead at time stamp ? 
if so add 1
add up all times vixen is in the lead = vixens score = winning reindeer score.

|#


(define (brute)
  (set! table (make-vector 10 0))
  (recur 1)
  (format #t "~a ~%" table)
  (format #t "~a ~%" (map list '(Vixen Rudolph Donner Blitzen Comet Cupid Dasher Dancer Prancer)
			  (vector->list table)))
  )

#|

really puzzle should have said what reindeer has most points after 2503 seconds .

((Vixen 469) (Rudolph 188) (Donner 589) (Blitzen 1256) (Comet 158) (Cupid 307) (Dasher 9) (Dancer 0) (Prancer 504)) 

blitzen has 1256 points
accepted answer

|#


#|
(map list '(Vixen Rudolph Donner Blitzen Comet Cupid Dasher Dancer Prancer)
     (iota (length '(Vixen Rudolph Donner Blitzen Comet Cupid Dasher Dancer Prancer))))
;; ((Vixen 0) (Rudolph 1) (Donner 2) (Blitzen 3) (Comet 4) (Cupid 5) (Dasher 6) (Dancer 7) (Prancer 8))
|#
     
