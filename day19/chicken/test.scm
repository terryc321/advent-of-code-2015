

(import matchable)
(import (chicken format))
(import simple-loops)


(define molecule "CRnCaCaCaSiRnBPTiMgArSiRnSiRnMgArSiRnCaFArTiTiBSiThFYCaFArCaCaSiThCaPBSiThSiThCaCaPTiRnPBSiThRnFArArCaCaSiThCaSiThSiRnMgArCaPTiBPRnFArSiThCaSiRnFArBCaSiRnCaPRnFArPMgYCaFArCaPTiTiTiBPBSiThCaPTiBPBSiRnFArBPBSiRnCaFArBPRnSiRnFArRnSiRnBFArCaFArCaCaCaSiThSiThCaCaPBPTiTiRnFArCaPTiBSiAlArPBCaCaCaCaCaSiRnMgArCaSiThFArThCaSiThCaSiRnCaFYCaSiRnFYFArFArCaSiRnFYFArCaSiRnBPMgArSiThPRnFArCaSiRnFArTiRnSiRnFYFArCaSiRnBFArCaSiRnTiMgArSiThCaSiThCaFArPRnFArSiRnFArTiTiTiTiBCaCaSiRnCaCaFYFArSiThCaPTiBPTiBCaSiThSiRnMgArCaF")


(define subs '(("Al" "ThF") ("Al" "ThRnFAr") ("B" "BCa") ("B" "TiB") ("B" "TiRnFAr") ("Ca" "CaCa") ("Ca" "PB") ("Ca" "PRnFAr") ("Ca" "SiRnFYFAr") ("Ca" "SiRnMgAr") ("Ca" "SiTh") ("F" "CaF") ("F" "PMg") ("F" "SiAl") ("H" "CRnAlAr") ("H" "CRnFYFYFAr") ("H" "CRnFYMgAr") ("H" "CRnMgYFAr") ("H" "HCa") ("H" "NRnFYFAr") ("H" "NRnMgAr") ("H" "NTh") ("H" "OB") ("H" "ORnFAr") ("Mg" "BF") ("Mg" "TiMg") ("N" "CRnFAr") ("N" "HSi") ("O" "CRnFYFAr") ("O" "CRnMgAr") ("O" "HP") ("O" "NRnFAr") ("O" "OTi") ("P" "CaP") ("P" "PTi") ("P" "SiRnFAr") ("Si" "CaSi") ("Th" "ThCa") ("Ti" "BP") ("Ti" "TiTi") ("e" "HF") ("e" "NAl") ("e" "OMg")))


;; what is n ? length of substring want to match against
;; takes substrings but not needed until middle matches with
;; s whole string
;; b part of subsitution table want to (match part of s with)
;;
(define strSuper
  (lambda (s fn n b)
    (let ((lim (max 0 (- (string-length s) 1))))
      (do-for (i 0 (- (+ lim 1) n))
	      ;;(loop for i from 0 to (- (+ lim 1) n) do
	      (let ((middle (substring s i (+ i n))))
		(cond
		 ((string=? middle b)
		  (let ((before (substring s 0 i))
			(after  (substring s (+ i n) (+ lim 1))))
		    (fn before middle after)))))))))


(define check-strSuper
  (lambda ()
    (let* ((mole "CRnCaCaCaSiRnBPTiMgAr")
	   (sub ("Ca" "CaCa"))
	   (sub-a (car sub))
	   (sub-b (car (cdr sub))))
      (strSuper mole (lambda (b m e) ;; before middle end
		       (format #t "matched ~a against ~a ~%" sub-b m))
		(string-length sub-b)
		sub-b))))



(define find-backward-match
  (lambda (s depth)
    (format #t "~a : ~a~%" depth s)
    (cond
     ((string=? s "e") (format #t "solved with depth ~a ~%" depth))
     (#t
      (do-list (sub subs)
	       (match sub
		 ((a b) 
		  (let ((blen (string-length b))
			(fn (lambda (be mi af)
			      (when (string=? mi b)			      
				;;(format #t "found match with ~a [~a <<== ~a ]-> ~a ~%" sub a b mi)
				(let ((built (string-append be a af)))				
				  (find-backward-match built (+ depth 1)))))))
		    (strSuper s fn blen b)))))))))


(define run
  (lambda ()
    (let ((depth 0))
      (find-backward-match molecule depth))))


;;(run)
