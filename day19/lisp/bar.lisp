
#|

in order evolve e into molecule

surely we can go back wards from molecule to e
always be reducing size of problem so hopefully makees a smaller search space ?
proof ?
reasoning ?
smaller example ?



|#

(defpackage :foo
  (:use :cl))

(in-package :foo)

;; go hell for leather
(declaim (optimize (speed 0) (safety 3)(debug 3)))


(defparameter molecule "CRnCaCaCaSiRnBPTiMgArSiRnSiRnMgArSiRnCaFArTiTiBSiThFYCaFArCaCaSiThCaPBSiThSiThCaCaPTiRnPBSiThRnFArArCaCaSiThCaSiThSiRnMgArCaPTiBPRnFArSiThCaSiRnFArBCaSiRnCaPRnFArPMgYCaFArCaPTiTiTiBPBSiThCaPTiBPBSiRnFArBPBSiRnCaFArBPRnSiRnFArRnSiRnBFArCaFArCaCaCaSiThSiThCaCaPBPTiTiRnFArCaPTiBSiAlArPBCaCaCaCaCaSiRnMgArCaSiThFArThCaSiThCaSiRnCaFYCaSiRnFYFArFArCaSiRnFYFArCaSiRnBPMgArSiThPRnFArCaSiRnFArTiRnSiRnFYFArCaSiRnBFArCaSiRnTiMgArSiThCaSiThCaFArPRnFArSiRnFArTiTiTiTiBCaCaSiRnCaCaFYFArSiThCaPTiBPTiBCaSiThSiRnMgArCaF")


(defparameter subs '(("Al" "ThF") ("Al" "ThRnFAr") ("B" "BCa") ("B" "TiB") ("B" "TiRnFAr") ("Ca" "CaCa") ("Ca" "PB") ("Ca" "PRnFAr") ("Ca" "SiRnFYFAr") ("Ca" "SiRnMgAr") ("Ca" "SiTh") ("F" "CaF") ("F" "PMg") ("F" "SiAl") ("H" "CRnAlAr") ("H" "CRnFYFYFAr") ("H" "CRnFYMgAr") ("H" "CRnMgYFAr") ("H" "HCa") ("H" "NRnFYFAr") ("H" "NRnMgAr") ("H" "NTh") ("H" "OB") ("H" "ORnFAr") ("Mg" "BF") ("Mg" "TiMg") ("N" "CRnFAr") ("N" "HSi") ("O" "CRnFYFAr") ("O" "CRnMgAr") ("O" "HP") ("O" "NRnFAr") ("O" "OTi") ("P" "CaP") ("P" "PTi") ("P" "SiRnFAr") ("Si" "CaSi") ("Th" "ThCa") ("Ti" "BP") ("Ti" "TiTi") ("e" "HF") ("e" "NAl") ("e" "OMg")))

(defparameter hash (make-hash-table :test 'equalp))

(defparameter mole "e")

(defun strSuper (s fn n)
  (let ((lim (max 0 (- (length s) 1))))
    (loop for i from 0 to (- (+ lim 1) n) do
             (let ((before (subseq s 0 i))
                   (middle (subseq s i (+ i n)))
                   (after  (subseq s (+ i n) (+ lim 1))))
               (funcall fn before middle after)
               ;;(format t "took ~a [~a] ~a  ~%" before middle after)
	       ))))


(defun find-backward-match (s)
  (dolist (sub subs)
    (destructuring-bind (a b) sub
      (let ((blen (length b)))
	(labels
	    ((fn (lambda (be mi af)
		  (format t "found match with ~a ~%" mi))))
	  (strSuper s #'fn blen))))))

;; (labels ((fn (lambda (x) (+ x 2))))
;;   (#'fn 3))
;; 
;; (flet (fn (lambda (x) (+ x 2)))
;;   (fn 3))




;; find single character matches
(defun strSub (s fn)
  (let ((lim (max 0 (- (length s) 1))))
    (loop for i from 0 to lim do
          (let ((before (subseq s 0 i))
                (middle (subseq s i (+ i 1)))
                (after  (subseq s (+ i 1) (+ lim 1))))
            (funcall #'fn before middle after)))))

;; find two character matches
(defun strSub2 (s fn)
  (let ((lim (max 0 (- (length s) 1))))
    (loop for i from 0 to (- lim 1) do
             (let ((before (subseq s 0 i))
                   (middle (subseq s i (+ i 2)))
                   (after  (subseq s (+ i 2) (+ lim 1))))
               (funcall fn before middle after)))))


    
;; (strSuper "abcde" 
;;           (lambda (b m e)
;;             (format t "~%~%we got -> [~a] [~a] [~a] ~%~%" b m e))
;;           1)
;; 
;; 
;; (strSuper "abcde" 
;;           (lambda (b m e)
;;             (format t "~%~%we got -> [~a] [~a] [~a] ~%~%" b m e))
;;           2)



;; (strSub word (lambda (b m e) (format t "we received ~a ~a ~a ~%" b m e)))
;; (strSub2 word (lambda (b m e) (format t "we received ~a ~a ~a ~%" b m e)))
;; ))

(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%" key value))

;; runs down list of substitutions A -> B
;; take B compare it against word
;;      if middle matches against B
;;;     then substitute  middle with A
;;      to build a new word => " before + A + after "
;; ----- unfortunately starting with "e" does not get us anywhere
;;           as there are no matches 
(defun play (word)
  (let ((hash (make-hash-table)))
          (dolist (sub subs)
            (let ((sub-x (car sub))
                  (sub-y (cadr sub)))
              (strSuper word
                        (lambda (b m e)
                          (cond
                            ((equalp m sub-y)
                             (let ((built (concatenate 'string b sub-x e)))
                               (cond
                                 ((gethash built hash nil) t)
                                 (t (setf (gethash built hash) t)))))))
                        (length sub-y))))
    (maphash #'print-hash-entry hash)))



(defun play2 (words depth)
  (let ((hash (make-hash-table)))
    (dolist (word words)
      ;;(format t "checking against word [~a]~%" word)
      (dolist (sub subs)
	;;(format t "using pair [~a]~%" sub)      	
      (let ((sub-x (car sub))
            (sub-y (cadr sub)))
	;;(format t "left ~a : right ~a ~%" sub-x sub-y)      
        (strSuper word
                  (lambda (b m e)
		    ;;(format t "comparing [~a] to [~a] " m sub-x)
                    (cond
                      ((equalp m sub-x)
                       (let ((built (concatenate 'string b sub-y e)))
                         ;;(format t "built ~a ~%" built)
                         (cond
                           ((gethash built hash nil) 1)
                           (t (setf (gethash built hash) 2)))))
		      (t
		       ;;(format t "no match")
		       nil
		       )))
                  (length sub-x)))))
    (let ((result '())
	  (count 0))
      (maphash (lambda (key _)
		 (incf count)
		 (cond
		   ((> (length key) (length molecule)) nil)
		   ((= (length key) (length molecule))
		    (cond
		      ((equalp key molecule)
		       (format t " Solved at depth ~a ~%" depth))
		      (t (setq result (cons key result)))))
		   (t (setq result (cons key result)))))
             hash)
      (setq hash nil)
      ;;(format t "~a ~%" words)
      (format t "play2 generated ~a words ~%" count)
      result)))


(defparameter molecule-length (length molecule))



;; given a word and hash , if sub-match with table entry - build a new word -> 
(defun play3 (word hash depth)
  (dolist (sub subs)
    ;;(format t "using pair [~a]~%" sub)      	
    (let ((sub-x (car sub))
          (sub-y (cadr sub)))
      ;;(format t "left ~a : right ~a ~%" sub-x sub-y)      
      (strSuper word
                (lambda (b m e)
		  ;;(format t "comparing [~a] to [~a] " m sub-x)
                  (cond
                   ((equalp m sub-x)
                    (let* ((built (concatenate 'string b sub-y e))
			   (built-len (length built)))
                      ;;(format t "built ~a ~%" built)
                      (cond
		       ((> built-len molecule-length)
			;;(format t "too long at depth of ~a :  blen ~a~%" depth built-len)
			nil) ;; too long a guess
		       ((equalp built molecule) (format t "SOLUTION at depth ~a ~%" depth))
                       ((gethash built hash nil) nil) ;; already visited
                       (t			
			(setf (gethash built hash) 2) ;; record we been here
			(play3 built hash (+ depth 1)) ;; recurse ... 
			;; unwind - remove string built from hash
			(remhash built hash)
			))))))
                (length sub-x)))))



;; ;; start off with one word in a list
;; ;; "e" starting point
;; (defun run ()
;;   (let ((words (list "e"))
;; 	(depth 0))
;;     (loop while t do
;;       (incf depth)
;;       (setq words (play2 words depth)))))

(defun run ()
  (let ((hash (make-hash-table))
	(depth 0))
    (play3 "e" hash depth)))


;;(run)






    



