

(use-modules (system base lalr))

;; lalr parser in guile

(define expr-parser
  (lalr-parser
   ;; Terminal symbols
   (ID
    (left: + -)
    (left: * /)
    (nonassoc: uminus))
   (e (e + e)              : (+ $1 $3)
      (e - e)              : (- $1 $3)
      (e * e)              : (* $1 $3)
      (e / e)              : (/ $1 $3)
      (- e (prec: uminus)) : (- $2)
      (ID)                 : $1)))

