

(use-modules (ice-9 format)) ;; format common lisp 
(use-modules (ice-9 pretty-print)) ;; pretty-print
(define pp pretty-print)
;;(use-modules (rnrs)) ;; assert 
(use-modules (srfi srfi-1)) ;; first second third ...
(use-modules (srfi srfi-2)) ;; first second third ...

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
(define input #f)

(get-input "../input")
;;(get-input "input2")

;; (define input "^v")
;; (define input "^>v<")
;; (define input "^v^v^v^v^v")


#|
    ^v delivers presents to 3 houses, because Santa goes north, and then Robo-Santa goes south.
    ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up back where they started.
    ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one direction and Robo-Santa going the other.
|#

(define ilen (string-length input))

(define my-hash 0)

;; only want to know where presents are ...
(define (put-present x y)
  (hash-set! my-hash (list x y) #t))

(define (foo)
  (set! my-hash (make-hash-table))
  (let ((i 0)
	(x 0)(y 0))
    (while (< i ilen)
      (put-present x y)
      (let ((ch (string-ref input i)))
	(cond
	 ((char=? ch #\^) (set! y (+ y 1)))
	 ((char=? ch #\v) (set! y (- y 1)))
	 ((char=? ch #\<) (set! x (- x 1)))
	 ((char=? ch #\>) (set! x (+ x 1)))
	 (#t (error (list (format #nil "dont know character action ch = ~a ~%" ch)))))
	(set! i (+ i 1))))))

(define (fiz)
  (foo)
  (hash-count (const #t) my-hash)
  )


(define (bar)
  (set! my-hash (make-hash-table))
  (let ((i 0)
	(x 0)(y 0)
	(x2 0)(y2 0))
    (while (< i ilen)
      (put-present x y)
      (put-present x2 y2)
      
      (let ((ch (string-ref input i)))
	(cond
	 ((char=? ch #\^) (set! y (+ y 1)))
	 ((char=? ch #\v) (set! y (- y 1)))
	 ((char=? ch #\<) (set! x (- x 1)))
	 ((char=? ch #\>) (set! x (+ x 1)))
	 (#t (error (list (format #nil "dont know character action ch = ~a ~%" ch)))))
	(set! i (+ i 1))
	(let ((ch (string-ref input i)))
	  (cond
	   ((char=? ch #\^) (set! y2 (+ y2 1)))
	   ((char=? ch #\v) (set! y2 (- y2 1)))
	   ((char=? ch #\<) (set! x2 (- x2 1)))
	   ((char=? ch #\>) (set! x2 (+ x2 1)))
	   (#t (error (list (format #nil "dont know character action ch = ~a ~%" ch)))))
	  (set! i (+ i 1))

	  (put-present x y)
	  (put-present x2 y2))))))


(define (baz)
  (bar)
  (hash-count (const #t) my-hash)
  )




#|

while loop exiting before allowed finish putting presents

2639

|#












