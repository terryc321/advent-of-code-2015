#|

optimize if mana spent > 1387 then curtail as too far

player has so many mana , armour , hits 
when player.hits <= 0 player dies

------- puzzle --------
magic-missile 53 mana and does 4 damage
drain 73 mana , does 2 damage and heals for 2 hit points 
shield 113 mana , 6 turns , effect armour +7 
poison 173 mana , 6 turns , effect boss 3 damage 
recharge 229 mana , 5 turns , effect + 101 mana to player
------- puzzle --------

boss has hits 
boss will always deal X damage , casts no spells
when boss.hits <= 0 boss dies

effects apply at start of both players turns and boss turns

solution look like

m a h r s p

ma : mana
ar : armour
hi : hits

rc : recharge counter
sc : shield
pc : poison

bh : boss hits
ms : mana spent

mv : moves made

recharge
  x   x     x   x   y
 1u   2b    3u  4b  5u   6   7   8  
r:5   4     3   2   1

shield
  x   x     x   x   x    y
 1u   2b    3u  4b  5u   6   7   8  
s:6   5     4   3   2    1

poison
  x   x     x   x   x    y
 1u   2b    3u  4b  5u   6   7   8  
p:6   5     4   3   2    1

drain 
  x
 1u    

magic-missile
  x
 1u


|#

;;(define *best-guess* 1348)


;; no srfi-9
(import (chicken format))
(import (chicken process-context))
(import (chicken pretty-print))

(define (make-state mana armour hits recharge shield poison boss-hits mana-spent moves)
  (list->vector (list mana armour hits recharge shield poison boss-hits mana-spent moves)))

(define (copy-state s)
  (list->vector (list (vector-ref s 0)
		      (vector-ref s 1)
		      (vector-ref s 2)
		      (vector-ref s 3)
		      (vector-ref s 4)
		      (vector-ref s 5)
		      (vector-ref s 6)
		      (vector-ref s 7)
		      (vector-ref s 8))))

;;
(make-state 100 15 13 0 1 2 16 0 '())
(copy-state (make-state 100 15 13 0 1 2 16 0 '()))

(equal? (make-state 100 15 13 0 1 2 16 0 '())
	(copy-state (make-state 100 15 13 0 1 2 16 0 '())))

(define (state-mana s) (vector-ref s 0))
(define (state-armour s) (vector-ref s 1))
(define (state-hits s) (vector-ref s 2))
(define (state-recharge s) (vector-ref s 3))
(define (state-shield s) (vector-ref s 4))
(define (state-poison s) (vector-ref s 5))
(define (state-boss-hits s) (vector-ref s 6))
(define (state-mana-spent s) (vector-ref s 7))
(define (state-moves s) (vector-ref s 8))

(define (state-mana! s n) (let ((cp (copy-state s))) (vector-set! cp 0 n) cp))
(define (state-armour! s n) (let ((cp (copy-state s))) (vector-set! cp 1 n) cp))
(define (state-hits! s n) (let ((cp (copy-state s))) (vector-set! cp 2 n) cp))
(define (state-recharge! s n) (let ((cp (copy-state s))) (vector-set! cp 3 n) cp))
(define (state-shield! s n) (let ((cp (copy-state s))) (vector-set! cp 4 n) cp))
(define (state-poison! s n) (let ((cp (copy-state s))) (vector-set! cp 5 n) cp))
(define (state-boss-hits! s n) (let ((cp (copy-state s))) (vector-set! cp 6 n) cp))
(define (state-mana-spent! s n) (let ((cp (copy-state s))) (vector-set! cp 7 n) cp))
(define (state-moves! s n) (let ((cp (copy-state s))) (vector-set! cp 8 n) cp))

(define (check)
  (let ((s (make-state 'mana 'armour 'hits 'recharge 'shield 'poison 'boss-hits 'mana-spent 'moves)))
    (set! s (state-mana! s 'mana2))
    (set! s (state-armour! s 'armour2))
    (set! s (state-hits! s 'hits2))
    (set! s (state-recharge! s 'recharge2))
    (set! s (state-shield! s 'shield2))
    (set! s (state-poison! s 'poison2))
    (set! s (state-boss-hits! s 'boss-hits2))
    (set! s (state-mana-spent! s 'mana-spent2))
    (set! s (state-moves! s 'moves2))
    
    (format #t "mana => ~a~%" (state-mana s))
    (format #t "armour => ~a~%" (state-armour s))
    (format #t "hits => ~a~%" (state-hits s))
    (format #t "recharge => ~a~%" (state-recharge s))
    (format #t "shield => ~a~%" (state-shield s))
    (format #t "poison => ~a~%" (state-poison s))
    (format #t "boss-hits => ~a~%" (state-boss-hits s))
    (format #t "mana-spent => ~a~%" (state-mana-spent s))
    (format #t "moves => ~a~%" (state-moves s))
    ))

(define *max-mana-spent* 2000)


(define (deduct-player s f)
  ;; deduct 1 hit point  
  (let ((s2 (state-hits! s (- (state-hits s) 1))))
    (cond
     ((<= (state-hits s2) 0)
      (let ((s3 (state-moves! s2 (cons 'player-dead (state-moves s2)))))
	;;(format #t "~a~%" s3)
	'died	
	))
     (#t
      (let ((s3 (state-moves! s2 (cons 'deduct (state-moves s2)))))
	(f s3))))))

(define (play1 s)
  (deduct-player s play1b))

;; effects
(define (play1b s)
  (apply-effects s play1c))

(define (apply-effects s f)
  (apply-recharge-effect s f))

(define (apply-recharge-effect s f)
  (cond
   ((> (state-recharge s) 0) ;; recharge gives 101 mana
    (let* ((s2 (state-recharge! s (- (state-recharge s) 1)))
	   (s3 (state-mana! s2 (+ 101 (state-mana s2))))
	   (s4 (state-moves! s3 (cons 'recharge-effect (state-moves s3))))
	   )
      (apply-shield-effect s4 f)))
   (#t 
    (apply-shield-effect s f))))

(define (apply-shield-effect s f)
  (cond
   ((> (state-shield s) 0) ;; shield gives 7 armour
    (let* ((s2 (state-shield! s (- (state-shield s) 1)))
	   (s3 (state-armour! s2 (+ 7 (state-armour s2))))
	   (s4 (state-moves! s3 (cons 'shield-effect (state-moves s3))))
	   )
      (apply-poison-effect s4 f)))
   (#t 
    (apply-poison-effect s f))))

(define (apply-poison-effect s f)
  (cond
   ((> (state-poison s) 0) ;; poison take 3 boss
    (let* ((s2 (state-poison! s (- (state-poison s) 1)))
	   (s3 (state-boss-hits! s2 (+ -3 (state-boss-hits s2))))
	   (s4 (state-moves! s3 (cons 'poison-effect (state-moves s3))))
	   )
      (f s3)))
   (#t 
    (f s))))


(define (play1c s)
  (cond ;; cannot cast any spells - die 
   ((< (state-mana s) 53)
    (let ((s2 (state-moves! s (cons 'player-died-insufficient-mana (state-moves s)))))
	;;(format #t "~a~%" s2)
      'died))
   ((> (state-mana-spent s) *max-mana-spent*)
    'too-costly)
   (#t
    (cast-magic-missile s)
    (cast-drain s) 
    (cast-poison s) 
    (cast-recharge s)
    (cast-shield s))))

;;Magic Missile costs 53 mana. It instantly does 4 damage.
(define (cast-magic-missile s)
  (when (>= (state-mana s) 53)
    (let* ((s2 (state-mana! s (- (state-mana s) 53)))
	   (s3 (state-mana-spent! s2 (+ 53 (state-mana-spent s2))))
	   (s4 (state-boss-hits! s3 (+ -4 (state-boss-hits s3))))
	   (s5 (state-moves! s4 (cons 'cast-magic-missile (state-moves s4)))))
      (play2 s5))))


;;Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.
(define (cast-drain s)
  (when (>= (state-mana s) 73)
    (let* ((s2 (state-mana! s (- (state-mana s) 73)))
	   (s3 (state-mana-spent! s2 (+ 73 (state-mana-spent s2))))
	   (s4 (state-boss-hits! s3 (+ -2 (state-boss-hits s3))))
	   (s5 (state-hits! s4 (+ 2 (state-hits s4))))
	   (s6 (state-moves! s5 (cons 'cast-drain (state-moves s5)))))
      (play2 s6))))


;;Shield costs 113 mana. It starts an effect that lasts for 6 turns. While it is active, your armor is increased by 7.
(define (cast-shield s)
  (when (>= (state-mana s) 113)
    (let* ((s2 (state-mana! s (- (state-mana s) 113)))
	   (s3 (state-mana-spent! s2 (+ 113 (state-mana-spent s2))))
	   (s4 (state-shield! s3 6))
	   (s5 (state-moves! s4 (cons 'cast-shield (state-moves s4)))))
      (play2 s5))))

;;Poison costs 173 mana. It starts an effect that lasts for 6 turns. At the start of each turn while it is active, it deals the boss 3 damage.
(define (cast-poison s)
  (when (>= (state-mana s) 173)
    (let* ((s2 (state-mana! s (- (state-mana s) 173)))
	   (s3 (state-mana-spent! s2 (+ 173 (state-mana-spent s2))))
	   (s4 (state-poison! s3 6))
	   (s5 (state-moves! s4 (cons 'cast-poison (state-moves s4)))))
      (play2 s5))))


;;Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At the start of each turn while it is active, it gives you 101 new mana.
(define (cast-recharge s)
  (let ((cost 229))
    (when (>= (state-mana s) cost)
      (let* ((s2 (state-mana! s (- (state-mana s) cost)))
	     (s3 (state-mana-spent! s2 (+ cost (state-mana-spent s2))))
	     (s4 (state-recharge! s3 5))
	     (s5 (state-moves! s4 (cons 'cast-recharge (state-moves s4)))))
	(play2 s5)))))


(define (play2 s)
  (deduct-player s play2b))

;; effects
(define (play2b s)
  (cond
   ((<= (state-hits s) 0) 'player-died)
   (#t
    (apply-effects s play2c))))

(define (play2c s)
  (cond
   ((<= (state-boss-hits s) 0)
    (let ((s2 (state-moves! s (cons "boss is dead" (state-moves s)))))
      (cond
       ((< (state-mana-spent s) *max-mana-spent*)
	(set! *max-mana-spent* (state-mana-spent s))
	(format #t "~a~%" s2)))))
   (#t
    (let ((boss-attack 8))
      (cond
       ((>= (state-armour s) boss-attack)
	(let ((ar2 (- (state-armour s) boss-attack))
	      (dam 1))
	  ;; armour and body took damage
	  (let* ((s2 (state-hits! s (- (state-hits s) dam)))
		 (s3 (state-armour! s2 ar2)))
	    (play1 s3))))
       (#t ;;(< ar boss-attack)
	(let ((ar2 0)
	      (dam (max 1 (- boss-attack (state-armour s)))))
	  (let* ((s2 (state-hits! s (- (state-hits s) dam)))
		 (s3 (state-armour! s2 ar2)))
	    (play1 s3)))))))))


(define (run)
  (let ((mana 500)
	(hits 50)
	(armour 0)
	(recharge 0)
	(shield 0)
	(poison 0)
	(boss-hits 55) ;; 55 hit points with boss damage 8 
	(mana-spent 0)
	(moves '()))
    (let ((s (make-state mana armour hits recharge shield poison boss-hits mana-spent moves)))
      (play1 s))))


#|
#;8542> (run)
#(29 11 2 0 0 0 -1 1481 (boss is dead deduct cast-drain deduct deduct cast-drain shield-effect deduct shield-effect deduct cast-drain shield-effect deduct shield-effect recharge-effect deduct cast-poison shield-effect recharge-effect deduct shield-effect recharge-effect deduct cast-shield recharge-effect deduct recharge-effect deduct cast-recharge deduct deduct cast-poison deduct recharge-effect deduct cast-drain shield-effect recharge-effect deduct shield-effect recharge-effect deduct cast-magic-missile shield-effect recharge-effect deduct shield-effect recharge-effect deduct cast-recharge shield-effect deduct shield-effect deduct cast-shield deduct deduct cast-magic-missile deduct deduct cast-magic-missile deduct))
#(69 5 2 0 0 1 -2 1441 (boss is dead deduct cast-drain deduct deduct cast-drain deduct deduct cast-poison shield-effect deduct shield-effect deduct cast-magic-missile shield-effect deduct shield-effect recharge-effect deduct cast-magic-missile shield-effect recharge-effect deduct shield-effect recharge-effect deduct cast-shield recharge-effect deduct recharge-effect deduct cast-recharge deduct recharge-effect deduct cast-poison shield-effect recharge-effect deduct shield-effect recharge-effect deduct cast-magic-missile shield-effect recharge-effect deduct shield-effect recharge-effect deduct cast-recharge shield-effect deduct shield-effect deduct cast-shield deduct deduct cast-magic-missile deduct deduct cast-magic-missile deduct))
#(162 13 1 0 0 1 -2 1348 (boss is dead deduct cast-magic-missile deduct deduct cast-magic-missile shield-effect deduct shield-effect deduct cast-poison shield-effect deduct shield-effect recharge-effect deduct cast-magic-missile shield-effect recharge-effect deduct shield-effect recharge-effect deduct cast-shield recharge-effect deduct recharge-effect deduct cast-recharge deduct recharge-effect deduct cast-poison shield-effect recharge-effect deduct shield-effect recharge-effect deduct cast-magic-missile shield-effect recharge-effect deduct shield-effect recharge-effect deduct cast-recharge shield-effect deduct shield-effect deduct cast-shield deduct deduct cast-magic-missile deduct deduct cast-magic-missile deduct))
#(114 21 4 0 0 0 -1 1295 (boss is dead deduct cast-magic-missile deduct deduct cast-magic-missile shield-effect deduct shield-effect deduct cast-magic-missile shield-effect deduct shield-effect recharge-effect deduct cast-poison shield-effect recharge-effect deduct shield-effect recharge-effect deduct cast-shield recharge-effect deduct recharge-effect deduct cast-recharge shield-effect recharge-effect deduct shield-effect recharge-effect deduct cast-poison shield-effect recharge-effect deduct shield-effect recharge-effect deduct cast-recharge shield-effect deduct shield-effect deduct cast-shield deduct deduct cast-magic-missile deduct deduct cast-magic-missile deduct))
#(52 3 3 0 0 0 -1 953 (boss is dead deduct cast-magic-missile deduct deduct cast-magic-missile deduct deduct cast-magic-missile shield-effect deduct shield-effect deduct cast-poison shield-effect deduct shield-effect recharge-effect deduct cast-magic-missile shield-effect recharge-effect deduct shield-effect recharge-effect deduct cast-shield recharge-effect deduct recharge-effect deduct cast-recharge deduct deduct cast-poison deduct deduct cast-magic-missile deduct))
died
#;8778> *max-mana-spent*
953

wrong answer apparently...
|#
