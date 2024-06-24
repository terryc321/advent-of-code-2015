
(import (ice-9 pretty-print))
(import (ice-9 match))
(define pp pretty-print)


(define parse
  (lambda ()
    (with-input-from-file "output"
      (lambda ()
	(read)))))


(define decode
  (lambda (e)
    (map (lambda (ex)
	   (match ex
	     ((x from 'to dest '= dist)
	      (list from dest dist))))
	 (cdr e))))

;; (decode (parse))



