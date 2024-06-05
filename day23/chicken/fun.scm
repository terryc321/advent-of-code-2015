

(import scheme)
(import simple-exceptions)
(import (chicken repl))
(import (chicken string))
(import (chicken pretty-print))
(import (chicken io))
(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day17")
;; (get-current-directory)
(import procedural-macros)
(import regex)
(import simple-md5)
(import simple-loops)
(import srfi-69)
;; hash-table-ref  hash key thunk
;; hash-table-set! hash key val
;; sudo chicken-install srfi-178
(import srfi-178)
;; srfi-178 provides bit-vectors
;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))
(import sequences)
(import srfi-1)
(import matchable)
(define pp pretty-print)

;;--------------------------------------

#|

only two registers - a , b - non negative integers

begin with value of 0

(hlf r) 

|#


;; ;; increment register by 1 
;; (define-syntax inc
;;   (syntax-rules ()
;;     ((_ v) (set! v (+ v 1)))))
;; 
;; ;; jump if odd
;; (define-syntax jio
;;   (syntax-rules ()
;;     ((_ v) (set! v (+ v 1)))))
;; 


(define-syntax inc 
  (er-macro-transformer
   (lambda (form rename compare?)
     (let ((v (cadr form))
	   (%begin (rename 'begin))
	   (%set! (rename 'set!))
	   (%+ (rename '+)))
       `(lambda ()
	  (,%begin (,%set! ,v (,%+ ,v 1))
		   (,%set! ip (,%+ ip 1))))))))

;; half hlf 
(define-syntax hlf 
  (er-macro-transformer
   (lambda (form rename compare?)
     (let ((v (cadr form))
	   (%begin (rename 'begin))
	   (%set! (rename 'set!))
	   (%* (rename '*))
	   (%/ (rename '/))
	   (%floor (rename 'floor))	   
	   (%+ (rename '+)))
       `(lambda ()
	  (,%begin (,%set! ,v (,%floor (,%/ ,v 2)))
		   (,%set! ip (,%+ ip 1))))))))


;; triple tpl 
(define-syntax tpl
  (er-macro-transformer
   (lambda (form rename compare?)
     (let ((v (cadr form))
	   (%begin (rename 'begin))
	   (%set! (rename 'set!))
	   (%* (rename '*))
	   (%+ (rename '+)))
       `(lambda ()
	  (,%begin (,%set! ,v (,%* ,v 3))
		   (,%set! ip (,%+ ip 1))))))))

;; jmp unconditional 
(define-syntax jmp
  (er-macro-transformer
   (lambda (form rename compare?)
     (let ((z (cadr form))
	   (%set! (rename 'set!))
	   (%+ (rename '+)))
       `(lambda ()
	  (,%set! ip (,%+ ip ,z)))))))

;; jie jump if even
(define-syntax jie
  (er-macro-transformer
   (lambda (form rename compare?)
     (let ((v (cadr form))
	   (z (caddr form))
	   (%begin (rename 'begin))
	   (%set! (rename 'set!))
	   (%even? (rename 'even?))	   
	   (%+ (rename '+))
	   (%= (rename '=))
	   (%if (rename 'if)))
       `(lambda ()
	  (,%if (,%even? ,v) (,%set! ip (,%+ ip ,z))
		(,%set! ip (,%+ ip 1))
		))))))



;; jio jump if one - v variable to check - z distance to jump
(define-syntax jio
  (er-macro-transformer
   (lambda (form rename compare?)
     (let ((v (cadr form))
	   (z (caddr form))
	   (%begin (rename 'begin))
	   (%set! (rename 'set!))
	   (%+ (rename '+))
	   (%= (rename '=))
	   (%if (rename 'if)))
       `(lambda ()
	  (,%if (,%= ,v 1) (,%set! ip (,%+ ip ,z))
		(,%set! ip (,%+ ip 1))
		))))))



#|
(let ((a 0)
      (ip 0))
  (inc a)
  (list a ip))

(let ((a 0)
      (ip 0))
  (jio a 23)
  (list a ip))

(let ((a 1)
      (ip 0))
  (jio a 23)
  (list a ip))


|#

(define run-example
  (lambda ()
    (let* ((a 0)
	   (b 0)
	   (ip 0)
	   (prog (list 
		  (inc a)
		  (jio a +2)
		  (tpl a)
		  (inc a)))
	   (plen (length prog)))
      (letrec ((loop (lambda ()
		       (format #t "ip ~a : a = ~a : b = ~a ~%" ip a b)
		       (cond
			((< ip 0) 'ip<0)
			((>= ip plen) 'ip>=plen)
			(#t (let ((lam (list-ref prog ip)))
			      (lam)
			      (loop)))))))
	(loop)))))


(define run-input
  (lambda ()
    (let* ((a 1) ;; starts a instead --- part 1 put a 0 here --- part 2 put a 1 here --- 
	   (b 0) 
	   (ip 0)
	   (prog (list
		  (jio a  +16)
		(inc a)
		(inc a)
		(tpl a)
		(tpl a)
		(tpl a)
		(inc a)
		(inc a)
		(tpl a)
		(inc a)
		(inc a)
		(tpl a)
		(tpl a)
		(tpl a)
		(inc a)
		(jmp +23)
		(tpl a)
		(inc a)
		(inc a)
		(tpl a)
		(inc a)
		(inc a)
		(tpl a)
		(tpl a)
		(inc a)
		(inc a)
		(tpl a)
		(inc a)
		(tpl a)
		(inc a)
		(tpl a)
		(inc a)
		(inc a)
		(tpl a)
		(inc a)
		(tpl a)
		(tpl a)
		(inc a)
		(jio a  +8)
		(inc b)
		(jie a  +4)
		(tpl a)
		(inc a)
		(jmp +2)
		(hlf a)
		(jmp -7)
		  ))
	   (plen (length prog)))
	   (letrec ((loop (lambda ()
			    (format #t "ip ~a : a = ~a : b = ~a ~%" ip a b)
			    (cond
			     ((< ip 0) 'ip<0)
			     ((>= ip plen) 'ip>=plen)
			     (#t (let ((lam (list-ref prog ip)))
				   (lam)
				   (loop)))))))
	     (loop)))))

(define (run)
  (run-input))



#|

> (run)
...
...
...
ip 43 : a = 16 : b = 166 
ip 45 : a = 16 : b = 166 
ip 38 : a = 16 : b = 166 
ip 39 : a = 16 : b = 166 
ip 40 : a = 16 : b = 167 
ip 44 : a = 16 : b = 167 
ip 45 : a = 8 : b = 167 
ip 38 : a = 8 : b = 167 
ip 39 : a = 8 : b = 167 
ip 40 : a = 8 : b = 168 
ip 44 : a = 8 : b = 168 
ip 45 : a = 4 : b = 168 
ip 38 : a = 4 : b = 168 
ip 39 : a = 4 : b = 168 
ip 40 : a = 4 : b = 169 
ip 44 : a = 4 : b = 169 
ip 45 : a = 2 : b = 169 
ip 38 : a = 2 : b = 169 
ip 39 : a = 2 : b = 169 
ip 40 : a = 2 : b = 170 
ip 44 : a = 2 : b = 170 
ip 45 : a = 1 : b = 170 
ip 38 : a = 1 : b = 170 
ip 46 : a = 1 : b = 170 
ip>=plen

0.004s CPU time, 12465/46 mutations (total/tracked), 0/24 GCs (major/minor), maximum live heap: 1.14 MiB

170 value left in b register ........ ACCEPTED answer


|#










