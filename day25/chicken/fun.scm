
(import (chicken format))
(import (chicken pretty-print))

#|

  1 2 3 4 5 6 7
1 1 3 6 10
2 2 5 9  
3 4 8
4 7 

The voice on the other end of the phone continues with how the codes are actually generated. The first code is 20151125. After that, each code is generated by taking the previous one, multiplying it by 252533, and then keeping the remainder from dividing that value by 33554393.

Y , X coords - >  [ row first , column second ] 
1,1
2,1 1,2
3,1 2,2 1,3   y decreases from some n , while x increases
|#
(define code 20151125)

(define next
  (lambda (x y prev)
    (cond
     ((and (= x 1)(= y 1)) 20151125)
     (#t (remainder (* prev 252533) 33554393)))))

(define (coord n x y prev exit)
  (cond
   ((> y 1)
    (let ((code (next x y prev)))
      ;; (format #t "~a ~a => ~a ~%" x y code)
      (solution? x y code exit)
      (coord n (+ x 1) (- y 1) code exit)))
   ((= y 1)
    (let ((code (next x y prev)))
      (format #t "~a ~a => ~a ~%" x y code)
      (solution? x y code exit)
      (coord (+ n 1) 1 (+ n 1) code exit)))
   (#t
    (error "coord"))))


(define (solution? x y code exit)
  (cond
   ((and (= x 3019) (= y 3010)) (format #t "solution is ~a ~%" code)
    (exit code))
   (#t #f)))

;;
(define (run)
  (let ((n 1)
	(x 1)
	(y 1)
	(code 20151125))
    (call/cc (lambda (exit)
	       (coord n x y code exit)
	       #f ))))



#|
To continue, please consult the code grid in the manual.  Enter the code at
row 3010, column 3019
.

>(run)
...
...
6024 1 => 32193111 
6025 1 => 4654698 
6026 1 => 12231971 
6027 1 => 9099303 
solution is 8997277 
11.025s CPU time, 0.058s GC time (major), 221790208/927007 mutations (total/tracked), 105/281257 GCs (major/minor), maximum live heap: 715.04 KiB
8997277
|#

