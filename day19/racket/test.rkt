#lang racket
(provide (all-defined-out))

(define molecule "CRnCaCaCaSiRnBPTiMgArSiRnSiRnMgArSiRnCaFArTiTiBSiThFYCaFArCaCaSiThCaPBSiThSiThCaCaPTiRnPBSiThRnFArArCaCaSiThCaSiThSiRnMgArCaPTiBPRnFArSiThCaSiRnFArBCaSiRnCaPRnFArPMgYCaFArCaPTiTiTiBPBSiThCaPTiBPBSiRnFArBPBSiRnCaFArBPRnSiRnFArRnSiRnBFArCaFArCaCaCaSiThSiThCaCaPBPTiTiRnFArCaPTiBSiAlArPBCaCaCaCaCaSiRnMgArCaSiThFArThCaSiThCaSiRnCaFYCaSiRnFYFArFArCaSiRnFYFArCaSiRnBPMgArSiThPRnFArCaSiRnFArTiRnSiRnFYFArCaSiRnBFArCaSiRnTiMgArSiThCaSiThCaFArPRnFArSiRnFArTiTiTiTiBCaCaSiRnCaCaFYFArSiThCaPTiBPTiBCaSiThSiRnMgArCaF")

(define subs '(("Al" "ThF") ("Al" "ThRnFAr") ("B" "BCa") ("B" "TiB") ("B" "TiRnFAr") ("Ca" "CaCa") ("Ca" "PB") ("Ca" "PRnFAr") ("Ca" "SiRnFYFAr") ("Ca" "SiRnMgAr") ("Ca" "SiTh") ("F" "CaF") ("F" "PMg") ("F" "SiAl") ("H" "CRnAlAr") ("H" "CRnFYFYFAr") ("H" "CRnFYMgAr") ("H" "CRnMgYFAr") ("H" "HCa") ("H" "NRnFYFAr") ("H" "NRnMgAr") ("H" "NTh") ("H" "OB") ("H" "ORnFAr") ("Mg" "BF") ("Mg" "TiMg") ("N" "CRnFAr") ("N" "HSi") ("O" "CRnFYFAr") ("O" "CRnMgAr") ("O" "HP") ("O" "NRnFAr") ("O" "OTi") ("P" "CaP") ("P" "PTi") ("P" "SiRnFAr") ("Si" "CaSi") ("Th" "ThCa") ("Ti" "BP") ("Ti" "TiTi") ("e" "HF") ("e" "NAl") ("e" "OMg")))

;; what is n ? length of substring want to match against
;; takes substrings but not needed until middle matches with
;; s whole string
;; b part of subsitution table want to (match part of s with)
;;
(define strSuper
  (lambda (s fn n b)
    (let ((lim (max 0 (+ (string-length s) 0))))
      (for ([i (range 0 (+ 0 (- (+ lim 1) n)))])
	      ;;(loop for i from 0 to (- (+ lim 1) n) do
	      (let ((middle (substring s i (+ i n)))
                    (before (substring s 0 i))
                    (after  (substring s (+ i n) (+ lim 0))))
                ;;(printf (format "before/mid/after : [~a] ([~a]<>[~a]) [~a]   ~%" before middle b after ))
		(cond
		 ((string=? middle b)
		    (fn before middle after))))))))


(define check-strSuper
  (lambda (sub-b mole)
    (printf (format "check-str [~a] [~a] ~%" sub-b mole))    
      (strSuper mole (lambda (b m e) ;; before middle end
		       (printf (format "matched ~a against ~a ~%" sub-b m)))
		(string-length sub-b)
		sub-b)
    (printf "\n\n")))   

(define best-depth 0)


(define find-backward-match
  (lambda (s depth exit)
    ;;(printf (format "depth ~a : str => ~a~%" depth s))
    (cond
     ((string=? s "e")
      ;;(printf (format "solved with depth ~a ~%" depth))
      (cond
       ((< depth best-depth)
	(set! best-depth depth)
	(printf (format "best depth so far ~a ~%" best-depth))
	(exit depth)	
	)))
     (#t
      (for/list ([sub subs])
	       (match sub
		 [(list a b)
		  (let ((blen (string-length b))
			(fn (lambda (be mi af)
			      (when (string=? mi b)			      
				;;(format #t "found match with ~a [~a <<== ~a ]-> ~a ~%" sub a b mi)
				(let ((built (string-append be a af)))
				  (find-backward-match built (+ depth 1) exit))))))
		    (strSuper s fn blen b))]))))))

(define run
  (lambda ()
    (let ((depth 0))
      ;; some arbitrary large value
      (set! best-depth 9999)
      (call/cc (lambda (exit)
                 (find-backward-match molecule depth exit))))))

;; ;; tests
;; (check-strSuper "X" "")
;; (check-strSuper "X" "a")
;; (check-strSuper "X" "ab")
;; (check-strSuper "X" "abc")
;; (check-strSuper "X" "abcd")
;; (check-strSuper "X" "abcde")
;; (check-strSuper "X" "abcdef")
;; (check-strSuper "X" "abcdefg")
;; (check-strSuper "X" "abcdefgh")
;; (check-strSuper "X" "abcdefghi")
;; 
;; (check-strSuper "XY" "")
;; (check-strSuper "XY" "a")
;; (check-strSuper "XY" "ab")
;; (check-strSuper "XY" "abc")
;; (check-strSuper "XY" "abcd")
;; (check-strSuper "XY" "abcde")
;; (check-strSuper "XY" "abcdef")
;; (check-strSuper "XY" "abcdefg")
;; (check-strSuper "XY" "abcdefgh")
;; (check-strSuper "XY" "abcdefghi")

(time (run))
;;(find-backward-match molecule 0)

#|
Welcome to Racket v8.13 [cs].
> ; stdin:1:0: read-syntax: `#lang` not enabled
;   possible reason: not allowed again inside a module that already starts
;     `#lang`, or not enabled for interactive evaluation
; [,bt for context]
> ; racket: undefined;
;  cannot reference an identifier before its definition
;   in module: top-level
; [,bt for context]
> ; stdin:2:0: provide: not at module level
;   in: (provide (all-defined-out))
; [,bt for context]
> > > > > > > > best depth so far 212 
cpu time: 120 real time: 120 gc time: 28
212
> 

real	0m0.355s
user	0m0.311s
sys	0m0.044s

  ......... ACCEPTED ANSWER ........  212

|#


