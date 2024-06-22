#|

|#

(load-option 'format)

(define (read-bytes port)
  (format #t "reading bytes ... on port ~a ~%" port)
  (let ((w '()))	
    (letrec ((foo (lambda ()
		    (let ((p (read-u8 port)))
		      (format #t "read byte ~a ~%" p)
		      (cond
		       ((eof-object? p) w)
		       ((= p 10)
			(format #t "found a newline !~%")
			w)
		       (#t (set! w (cons p w))
			   (foo)))))))
      (foo)
      (reverse w))))


(define (reaction)
  (call-with-binary-input-file "../guile/input"
    (lambda (port)
      (let ((known '()))
	(letrec ((foo (lambda ()
			(let ((w (read-bytes port)))
			  (cond
			   ((null? w) known)
			   (#t (set! known (cons w known))
			       (foo)))))))
	  (set! known (reverse known))
	  known)))))

;; (+ 1 2)



;; ;;(read-bytes)

 ;; (reaction)
;; 
;; (first (reaction))

;;(define port (open-binary-input-file "../guile/input") 


(read-bytes port)


;; 
;; (close-port port)














































