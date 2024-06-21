

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
;;(define input #f)

(get-input "../input")
;;(get-input "input2")

(define (nice s)
  (cond
   ((string-match "ab" s) #f)
   ((string-match "cd" s) #f)
   ((string-match "pq" s) #f)
   ((string-match "xy" s) #f)
   ((or (string-match "aa" s) 
	(string-match "bb" s) 
	(string-match "cc" s) 
	(string-match "dd" s) 
	(string-match "ee" s) 
	(string-match "ff" s) 
	(string-match "gg" s) 
	(string-match "hh" s) 
	(string-match "ii" s) 
	(string-match "jj" s) 
	(string-match "kk" s) 
	(string-match "ll" s) 
	(string-match "mm" s) 
	(string-match "nn" s) 
	(string-match "oo" s) 
	(string-match "pp" s) 
	(string-match "qq" s) 
	(string-match "rr" s) 
	(string-match "ss" s) 
	(string-match "tt" s) 
	(string-match "uu" s) 
	(string-match "vv" s) 
	(string-match "ww" s) 
	(string-match "xx" s) 
	(string-match "yy" s) 
	(string-match "zz" s))
    (let ((vowels (+ (length (list-matches "a" s))
		     (length (list-matches "e" s))
		     (length (list-matches "i" s))
		     (length (list-matches "o" s))
		     (length (list-matches "u" s)))))
      (>= vowels 3)))
   (#t #f)))




(define (bar)
  (let* ((s "abcdefghijklmnopqrstuvwxyz")
	 (slen (string-length s)))
    (letrec ((baz (lambda (i)
		    (cond
		     ((< i slen)
		      (let ((ch (string-ref s i)))
			(format #t "(string-match \"~a~a\" s) ~%" ch ch)
			(baz (+ i 1))))
		     (#t #f)))))
      (baz 0))))


	
	    #|

    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
    It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.

	    |#

(define (foo)
  (let ((score 0))
    (dolist (s input)
	    (assert (string? s))
	    (format #t "str = ~a ~%" s)
	    (when (nice s)
	      (set! score (1+ score))))
    score))

(define (test s)
  (format #t "test : ~a is ~a~%" s (if (not (nice s)) "not nice" "nice")))




(define (test-suite )
  (test "ugknbfddgicrmopn")

;;  is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
  (test "aaa")
  ;;is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
  (test "jchzalrnumimnmhp")
  ;;is naughty because it has no double letter.
  (test "haegwjzuvuyypxyu")
  ;;is naughty because it contains the string xy.
  (test "dvszwmarrgswjxmb")
  ;;is naughty because it contains only one vowel.

  )



#|

str = cqfikbgxvjmnfncy 
$27 = 238

238 nice strings

yay !

PART I I


Realizing the error of his ways, Santa has switched to a better model of determining whether a
string is naughty or nice. None of the old rules apply, as they are all clearly ridiculous.

Now, a nice string is one with all of the following properties:

It contains a pair of any two letters that appears at least twice in the string without overlapping,
like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).

It contains at least one letter which repeats with exactly one letter between them,
like xyx, abcdefeghi (efe), or even aaa.

For example:

qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a letter that
repeats with exactly one letter between them (zxz).

xxyxx is nice because it has a pair that appears twice and a letter that repeats with one
between, even though the letters used by each rule overlap.

uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a single letter
between them.

ieodomkazucvgmuy is naughty because it has a repeating letter with one between (odo), but
no pair that appears twice.

How many strings are nice under these new rules?

|#
