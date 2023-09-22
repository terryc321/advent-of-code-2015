

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

(set! input (get-input "input3"))
;;(set! input (get-input "input2"))
;;(set! input (get-input "input"))


(define (make-assoc-list xs)
  (cond
   ((null? xs) xs)
   (#t (cons (list (car xs)(cadr xs))
	     (make-assoc-list (cdr (cdr xs)))))))

#|
children: 3 if children is a key its value must match 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1


if assoc key exists it must match
if assoc key does not exist then non-zero , 

so akitas must be 0
so vizslas must be 0

|#


#|
this did not work ... ?

(define (big-condition ass)
  ;;(format #t "big-condition on ~a ~%" ass)
  (letrec ((has-assoc (lambda (sym n)
			(let ((kv (assoc sym ass)))
			  (cond
			   (kv
			    ;;(format #t "assoc for ~a found - value ~a : compared to : ~a ~%" sym (second kv) n)
			    (= n (second kv)))       
			   (#t
			    ;;(format #t "assoc for ~a not found - it must be > 0 : ~a ~%" sym n)
			       (> n 0)))))))
    (and (has-assoc 'children 3)
	 (has-assoc 'cats 7)
	 (has-assoc 'samoyeds 2)
	 (has-assoc 'pomeranians 3)
	 (has-assoc 'akitas 0)
	 (has-assoc 'vizslas 0)
	 (has-assoc 'goldfish 5)
	 (has-assoc 'trees 3)
	 (has-assoc 'cars 2)
	 (has-assoc 'perfumes 1))))


;; didnt yield any results
(define (foo)
  (let ((pos '()))
    (dolist (p input)
	    ;;(format #t "~%processing ~a " p)
	    (let ((ass (make-assoc-list p)))
	      (format #t "~a~%" ass)
	      (when (big-condition ass)
		;;(format #t "******** match found ~a ~%" p)
		(set! pos (cons p pos)))
	      )
	    )
    pos
    )
)

|#

;; --------- using input3 --------------------------
#|
;; filter out those mention children but not value 3
(define (foo-1 xs)
  (filter (lambda (x) (let ((c (assoc 'children x)))
			(cond
			 (c (= 3 (second c)))
			 (#t #t))))
	  xs))


;; vizslas zero
(define (foo-2 xs)
  (filter (lambda (x) (let ((c (assoc 'vizslas x)))
			(cond
			 (c (= 0 (second c)))
			 (#t #t))))
	  xs))

;; akitas 0
(define (foo-3 xs)
  (filter (lambda (x) (let ((c (assoc 'akitas x)))
			(cond
			 (c (= 0 (second c)))
			 (#t #t))))
	  xs))
  

;; cats 7
(define (foo-4 xs)
  (filter (lambda (x) (let ((c (assoc 'cats x)))
			(cond
			 (c (= 7 (second c)))
			 (#t #t))))
	  xs))


;; simplify this with a macro 
|#


(defmacro make-filter-condition (name sym n)
  `(define (,name xs)
     (filter (lambda (x) (let ((c (assoc ,sym x)))
			   (cond
			    (c (= ,n (second c)))
			    (#t #t))))
	     xs)))
#|
    (and (has-assoc 'children 3)
	 (has-assoc 'cats 7)
	 (has-assoc 'samoyeds 2)
	 (has-assoc 'pomeranians 3)
	 (has-assoc 'akitas 0)
	 (has-assoc 'vizslas 0)
	 (has-assoc 'goldfish 5)
	 (has-assoc 'trees 3)
	 (has-assoc 'cars 2)
	 (has-assoc 'perfumes 1))))
|#
(make-filter-condition foo-1 'children 3)
(make-filter-condition foo-2 'cats 7)
(make-filter-condition foo-3 'samoyeds 2)
(make-filter-condition foo-4 'pomeranians 3)
(make-filter-condition foo-5 'akitas 0)
(make-filter-condition foo-6 'vizslas 0)
(make-filter-condition foo-7 'goldfish 5)
(make-filter-condition foo-8 'trees 3)
(make-filter-condition foo-9 'cars 2)
(make-filter-condition foo-10 'perfumes 1)


(define (boo)
  (foo-1 (foo-2 (foo-3 (foo-4 (foo-5 (foo-6 (foo-7 (foo-8 (foo-9 (foo-10 input)))))))))))

#|

running boo gives us only one surviving aunt - sue 373

scheme@(guile-user) [1]> (boo)
$41 = (((sue 373) (pomeranians 3) (perfumes 1) (vizslas 0)))
scheme@(guile-user) [2]> 

|#

(defmacro make-greater-filter-condition (name sym n)
  `(define (,name xs)
     (filter (lambda (x) (let ((c (assoc ,sym x)))
			   (cond
			    (c (> (second c) ,n))
			    (#t #t))))
	     xs)))


(defmacro make-lesser-filter-condition (name sym n)
  `(define (,name xs)
     (filter (lambda (x) (let ((c (assoc ,sym x)))
			   (cond
			    (c (< (second c) ,n))
			    (#t #t))))
	     xs)))


(make-greater-filter-condition goo-2 'cats 7)
(make-greater-filter-condition goo-8 'trees 3)

(make-lesser-filter-condition loo-4 'pomeranians 3)
(make-lesser-filter-condition loo-7 'goldfish 5)

;; replaced foo-2 foo-8 with goo-2 goo-8
;; replaced foo-4 foo-7 with loo-4 loo-7

(define (boo2)
  (foo-1 (goo-2 (foo-3 (loo-4 (foo-5 (foo-6 (loo-7 (goo-8 (foo-9 (foo-10 input)))))))))))


#|
scheme@(guile-user) [2]> (boo2)
$42 = (((sue 260) (goldfish 0) (vizslas 0) (samoyeds 2)))

accepted answers

|#







