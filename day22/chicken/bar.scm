#|

optimize if mana spent > 1387 then curtail as too far


|#

(define *best-guess* 953)


;; no srfi-9
(import chicken.format)


;; (define-syntax pr
;;   (syntax-rules () 
;;     ((pr x y) (format #t x y))))
;;(use-modules (srfi srfi-9))

;; shield poison recharge cannot be cast if already in play
(define-record-type state
  (make-state hits mana armour shield poison recharge boss-hits boss-damage mana-spent)
  state?
  (hits    hits   hits!)  
  (mana    mana   mana!)
  (armour  armour armour!)
  (shield  shield shield!) 
  (poison  poison poison!)
  (recharge  recharge recharge!)
  (boss-hits  boss-hits boss-hits!)
  (boss-damage boss-damage boss-damage!)
  (mana-spent mana-spent mana-spent!))


;; exemplar copy <state>
(define (copy-state s)
  (make-state (hits s) (mana s) (armour s) (shield s) (poison s) (recharge s) (boss-hits s) (boss-damage s) (mana-spent s)))


(define lowest-mana #f)

;; apply-effects to state s - return a new state 
(define (apply-effects s)
  (apply-recharge (apply-shield (apply-poison s))))

;; Recharge costs 229 mana.
;; It starts an effect that lasts for 5 turns.
;; At the start of each turn while it is active, it gives you 101 new mana.
(define (apply-recharge s)
  (cond
   ((> (recharge s) 0) ;; apply recharge effect
    (let ((s2 (copy-state s)))
      (mana! s2 (+ (mana s2) 101))  ; give 101 new mana 
      (recharge! s2 (- (recharge s2) 1)) ;; decrease recharge 
      s2))
   ;; otherwise no effect 
   (#t s)))


;; Shield costs 113 mana.
;; It starts an effect that lasts for 6 turns.
;; While it is active, your armor is increased by 7.
(define (apply-shield s)
  (cond
   ((> (shield s) 0) ;; apply effect
    (let ((s2 (copy-state s)))      
      (armour! s2 (+ (armour s2) 7)) ;; the effect
      (shield! s2 (- (shield s2) 1)) ;; decrease 
      s2))
   ;; otherwise no effect 
   (#t s)))


;; Poison costs 173 mana.
;; It starts an effect that lasts for 6 turns.
;; At the start of each turn while it is active, it deals the boss 3 damage.
(define (apply-poison s)
  (cond
   ((> (poison s) 0) ;; apply effect
    (let ((s2 (copy-state s)))      
      (boss-hits! s2 (+ (boss-hits s2) -3)) ;; the effect
      (poison! s2 (- (poison s2) 1)) ;; decrease 
      s2))
   ;; otherwise no effect 
   (#t s)))


;; after any effects have taken effect 
(define (play1-move s)
  (cond
   ((>= (mana-spent s) *best-guess*) #f)
   ((< (mana s) 53) ;;(format #t "player dies - cannot cast any spell !~%")
    'player-dies)
   (#t
    (cast-magic-missile s)
    (cast-drain s)
    (cast-shield s)
    (cast-poison s)
    (cast-recharge s))))



(define (player-dead? s)
  (<= (hits s) 0))

(define (boss-dead? s)
  (<= (boss-hits s) 0))


(define (record-lowest-mana n)
  (cond
   ((not lowest-mana) (set! lowest-mana n))
   ((< n lowest-mana) (set! lowest-mana n)
    (format #t "lowest-mana found ~a~%" lowest-mana))))

;; apply any effects 
(define (play1 s)
  (let ((s3 (copy-state s)))
    ;; lose 1 hit point
    (hits! s3 (+ (hits s3) -1))
    (cond
     ((player-dead? s3) 'player-died)
     (#t
      (let ((s2 (apply-effects s3)))
	(cond
	 ((player-dead? s2)   ;;   (format #t "player dies !~%")
	  'player-died)
	 ((boss-dead? s2)
	  (record-lowest-mana (mana-spent s2))
	  ;;(format #t "boss dies ! mana-spent is ~a~%" (mana-spent s2))
	  )
	 (#t (play1-move s2))))))))


(define (play2 s)
  (let ((s3 (copy-state s)))
    ;; lose 1 hit point
    (hits! s3 (+ (hits s3) -1))
    (cond
     ((player-dead? s3) 'player-died)
     (#t
      (let ((s2 (apply-effects s3)))
	(cond
	 ((player-dead? s2)
	  (format #t "player dies !~%")
	  'player-died)
	 ((boss-dead? s2)
	  (record-lowest-mana (mana-spent s2))
	  ;;(format #t "boss dies ! mana spent is ~a~%" (mana-spent s2))
	  )
	 (#t (play2-move s2))))))))


(define (play2-move s)
  (let ((attack 8))
    (cond
     ;; some absorbed by armour , some by body
     ((> attack (armour s)) (let* ((attack2 (- attack (armour s)))
				   (a2 0)
				   (hnew (- (hits s) attack2))
				   (s2 (copy-state s)))
			      (armour! s2 0)
			      (hits! s2 hnew)
			      (play1 s2)))
     ;; absorb attack entirely by armour
     (#t (let* ((a2 (- (armour s) attack))
		(s2 (copy-state s)))
	   (armour! s2 a2)
           (play1 s2))))))


;;Magic Missile costs 53 mana. It instantly does 4 damage.
(define (cast-magic-missile s)
  (let ((cost 53))
    (when
	(>= (mana s) cost)
      (let ((s2 (copy-state s)))      
	(boss-hits! s2 (+ (boss-hits s2) -4)) ;; the effect
	(mana! s2 (- (mana s) cost)) ;; decrease
	(mana-spent! s2 (+ (mana-spent s2) cost))
	(play2 s2)))))


;; Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.
(define (cast-drain s)
  (let ((cost 73))
    (when
	(>= (mana s) cost)
      (let ((s2 (copy-state s)))      
	(boss-hits! s2 (+ (boss-hits s2) -2)) ;; the effect
	(mana! s2 (- (mana s) cost)) ;; decrease
	(hits! s2 (+ (hits s2) 2)) ;; heals 
	(mana-spent! s2 (+ (mana-spent s2) cost))
	(play2 s2)))))


;;Shield costs 113 mana. It starts an effect that lasts for 6 turns. While it is active, your armor is increased by 7.
(define (cast-shield s)
  (let ((cost 113))
    (when
	(and (= (shield s) 0) (>= (mana s) cost))
      (let ((s2 (copy-state s)))      
	(shield! s2 6) ;; 
	(mana! s2 (- (mana s) cost)) ;; decrease
	(mana-spent! s2 (+ (mana-spent s2) cost)) ;; track costs
	(play2 s2)))))


;;Poison costs 173 mana. It starts an effect that lasts for 6 turns. At the start of each turn while it is active, it deals the boss 3 damage.
(define (cast-poison s)
  (let ((cost 173))
    (when
	(and (= (poison s) 0) (>= (mana s) cost))
      (let ((s2 (copy-state s)))      
	(poison! s2 6) ;; 
	(mana! s2 (- (mana s) cost)) ;; decrease
	(mana-spent! s2 (+ (mana-spent s2) cost)) ;; track costs
	(play2 s2)))))

;;Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At the start of each turn while it is active, it gives you 101 new mana.    
(define (cast-recharge s)
  (let ((cost 229))
    (when
	(and (= (recharge s) 0) (>= (mana s) cost))
      (let ((s2 (copy-state s)))      
	(recharge! s2 5) ;; 
	(mana! s2 (- (mana s) cost)) ;; decrease
	(mana-spent! s2 (+ (mana-spent s2) cost)) ;; track costs
	(play2 s2)))))


(define (reset!)
  (set! lowest-mana #f))

(define (run)
  (let* ((mana 500)
	 (hits 50)
	 (armour 0)
	 (c-shield 0)
	 (c-poison 0)
	 (c-recharge 0)
	 (boss-hits 55)
	 (boss-damage 8)
	 (mana-spent 0)
	 (state  (make-state hits mana armour c-shield c-poison c-recharge boss-hits boss-damage mana-spent)))
    (reset!)
    (play1 state)))

;; For example, suppose the player has 10 hit points and 250 mana, and that the boss has 13 hit points and 8 damage:
(define (example1)
  (let* ((mana 250)
	 (hits 10)
	 (armour 0)
	 (c-shield 0)
	 (c-poison 0)
	 (c-recharge 0)
	 (boss-hits 13)
	 (boss-damage 8)
	 (mana-spent 0)
	 (state  (make-state hits mana armour c-shield c-poison c-recharge boss-hits boss-damage mana-spent)))
    (reset!)
    (play1 state)))




;; 
;; scheme@(guile-user)> (run)
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; player dies !
;; boss dies ! mana spent is 1493  <<<---------- WRONMG !
;; player dies !
;; player dies !
;; boss dies ! mana spent is 1768
;; boss dies ! mana spent is 1841
;; boss dies ! mana spent is 1861
;; boss dies ! mana spent is 1954
;; boss dies ! mana spent is 1974
;; boss dies ! mana spent is 1961
;; boss dies ! mana spent is 1881
;; boss dies ! mana spent is 1954
;; boss dies ! mana spent is 1974
;; boss dies ! mana spent is 1994
;; boss dies ! mana spent is 2001
;; boss dies ! mana spent is 1888
;; boss dies ! mana spent is 1997
;; boss dies ! mana spent is 2070
;; boss dies ! mana spent is 2090
;; boss dies ! mana spent is 2183
;; boss dies ! mana spent is 2203
;; boss dies ! mana spent is 2296
;;
;; WRONG . answer is too high
;; 

;; scheme@(guile-user)> (run)
;; lowest-mana found 1440
;; lowest-mana found 1387  ...... WRONG!!
;;
;;

;; if we stop if get more than or equal to 1387 then we find other solutions
;; lowest-mana found 1288
;; lowest-mana found 1235
;; lowest-mana found 1182
;; lowest-mana found 953

;; how do we conditionally compile this ?
(run)

;; 953 is proposed as best mana spent to give a solution win

