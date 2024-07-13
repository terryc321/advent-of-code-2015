
(import (chicken format))
(import (chicken pretty-print))
(import (chicken base))
(import (chicken sort))
(import (chicken read-syntax))
(import simple-loops)
(import srfi-1) ;; filter

;; ---------------------------------------------------
(define-record-type rect
  (make-rect x y w h) ;; constructor
  rect? ;; predicate
  (x rect-x rect-x-set!) ;; field accessor mutator
  (y rect-y rect-y-set!)
  (w rect-w rect-w-set!)
  (h rect-h rect-h-set!)
  )

(define-record-type rect
  (make-rect x y w h) ;; constructor
  rect? ;; predicate
  (x rect-x (setter rect-x)) ;; field accessor mutator
  (y rect-y rect-y-set!)
  (w rect-w rect-w-set!)
  (h rect-h rect-h-set!)
  )

(define-record-printer (rect rt out)
  (fprintf out "#,(rect ~s ~s ~s ~s)"
	   (rect-x rt) (rect-y rt) (rect-w rt) (rect-h rt)))

(define-reader-ctor 'rect make-rect)

(define rt '#,(rect 0 0 1 2))

rt  ;;#,(rect 0 0 1 2)
(set! (rect-x rt) 3)
rt  ;;#,(rect 3 0 1 2)
;; -----------------------------------------------------

(define-record-type player
  (make-player name damage armour hits) ;; constructor
  player? ;; predicate
  (name player-name (setter player-name)) ;; name
  (damage player-damage (setter player-damage)) ;; damage
  (armour player-armour (setter player-armour)) ;; armour
  (hits player-hits (setter player-hits)) ;; hits
  )

(define-record-printer (player rt out)
  (fprintf out "#,(player ~a 'damage-> ~s 'armour-> ~s hits-> ~s)"
	  (player-name rt) (player-damage rt) (player-armour rt) (player-hits rt)))

(define-reader-ctor 'player make-player)

(define rt '#,(player 'fred 0 1 2))
;; ---------------------------------------------------------
(define-record-type weapon
  (make-weapon name cost damage armour) ;; constructor
  weapon? ;; predicate
  (name weapon-name (setter weapon-name)) ;; weapon name
  (cost weapon-cost (setter weapon-cost)) ;; cost field accessor mutator
  (damage weapon-damage (setter weapon-damage)) ;; damage
  (armour weapon-armour (setter weapon-armour)) ;; armour
  )

(define-record-printer (weapon rt out)
  (fprintf out "#,(weapon 'name-> ~s 'cost-> ~s 'damage-> ~s 'armour-> ~s"
	   (weapon-name rt) (weapon-cost rt) (weapon-damage rt) (weapon-armour rt) ))

(define-reader-ctor 'weapon make-weapon)
;;----------------------------------------------------------------
(define-record-type armour
  (make-armour name cost damage armour) ;; constructor
  armour? ;; predicate
  (name armour-name (setter armour-name)) ;; armour name
  (cost armour-cost (setter armour-cost)) ;; cost field accessor mutator
  (damage armour-damage (setter armour-damage)) ;; damage
  (armour armour-armour (setter armour-armour)) ;; armour
  )

(define-record-printer (armour rt out)
  (fprintf out "#,(armour 'name-> ~s 'cost-> ~s 'damage-> ~s 'armour-> ~s"
	   (armour-name rt) (armour-cost rt) (armour-damage rt) (armour-armour rt) ))

(define-reader-ctor 'armour make-armour)
;; --------------------------------------------------------------------
(define-record-type ring
  (make-ring name cost damage armour) ;; constructor
  ring? ;; predicate
  (name ring-name (setter ring-name)) ;; ring name
  (cost ring-cost (setter ring-cost)) ;; cost field accessor mutator
  (damage ring-damage (setter ring-damage)) ;; damage
  (armour ring-armour (setter ring-armour)) ;; armour
  )


(define-record-printer (ring rt out)
  (fprintf out "#,(ring 'name-> ~s 'cost-> ~s 'damage-> ~s 'armour-> ~s"
	   (ring-name rt) (ring-cost rt) (ring-damage rt) (ring-armour rt) ))

(define-reader-ctor 'ring make-ring)
;;-----------------------------------------------------------------------


(define weapon-null '#,(weapon "null-weapon" 0 0 0))
(define weapon-dagger '#,(weapon "dagger" 8 4 0))
(define weapon-shortsword '#,(weapon "shortsword" 10 5 0))
(define weapon-warhammer '#,(weapon "warhammer" 25 6 0))
(define weapon-longsword '#,(weapon "longsword" 40 7 0))
(define weapon-greataxe '#,(weapon "greataxe" 74 8 0))

(define armour-null '#,(armour "null-armour" 0 0 0))
(define armour-leather '#,(armour "leather" 13 0 1))
(define armour-chainmail '#,(armour "chainmail" 31 0 2))
(define armour-splintmail '#,(armour "splintmail" 53 0 3))
(define armour-brandedmail '#,(armour "brandedmail" 75 0 4))
(define armour-platemail '#,(armour "platemail" 102 0 5))

(define ring-null '#,(ring "null-ring" 0 0 0))
(define ring-damage1 '#,(ring "damage1" 25 1 0))
(define ring-damage2 '#,(ring "damage2" 50 2 0))
(define ring-damage3 '#,(ring "damage3" 100 3 0))
(define ring-defense1 '#,(ring "defense1" 20 0 1))
(define ring-defense2 '#,(ring "defense2" 40 0 2))
(define ring-defense3 '#,(ring "defense3" 80 0 3))

(define ring-selection (list ring-null ring-damage1 ring-damage2 ring-damage3
			     ring-defense1 ring-defense2 ring-defense3 ))

(define armour-selection (list armour-null armour-leather armour-chainmail
			       armour-splintmail armour-brandedmail armour-platemail))

;; phrase "must buy one weapon! woopsies !"
(define weapon-selection (list ;; weapon-null
			       weapon-dagger weapon-shortsword
			       weapon-warhammer weapon-longsword weapon-greataxe))


#|
Weapons:    Cost  Damage  Armor
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0

Armor:      Cost  Damage  Armor
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5

Rings:      Cost  Damage  Armor
Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3

one of each item

opponent
Hit Points: 103
Damage: 9
Armor: 2

me
Hit Points: 100
Damage: ?
Armor: ?


state of game
------ player 1 ----- | ------ player 2 --------- |
          me          |        opponent

none or one weapon
none or one armor
none one or two different rings - shop only has one of each item 
give player
  cost damage armour hits     vs   damage armour hits
   ?    ?      ?     100             9    2     103

simulate until one of the player dies 

|#

(define simulation
  (lambda (player opponent)
    ;; (format #t "simulation : ~%~a : ~%~a~%~%" player opponent)
    ;; player attacks opponent
    (attack! player opponent)
    ;; has opponent died ?
    (cond
     ((dead? opponent player) (player-name player))
     (#t
      ;; opponent attacks player
      (attack! opponent player)
      ;; has player died?
      (cond
       ((dead? player opponent) (player-name opponent))
       (#t (simulation player opponent)))))))


(define attack!
  (lambda (player1 player2)
    ;;compute amount of damage inflicted
    ;; (format #t "~a attacks ~a inflicting " (player-name player1) (player-name player2))      
    (let ((damage (player-damage player1))
	  (armour (player-armour player2)))
      (let ((damage-inflicted (max 1 (max 0 (- damage armour )))))
	;; (format #t "damage => ~a : armour => ~a ~%" damage armour)
	;; (format #t "~a damage ~%" damage-inflicted)
	;; declare damage inflicted on player2
	(set! (player-hits player2) (max 0 (- (player-hits player2) damage-inflicted)))
	;; done
	))))

(define dead?
  (lambda (player1 player2)
    (cond
     ((<= (player-hits player1) 0)
      ;; (format #t "~a has died in combat ~%" (player-name player1))
      ;; (format #t "~a wins  ~%" (player-name player2))      
      #t)
     (#t #f))))



#|
For example, suppose you have 8 hit points, 5 damage, and 5 armor, and that the boss has 12 hit points, 7 damage, and 2 armor:

    The player deals 5-2 = 3 damage; the boss goes down to 9 hit points.
    The boss deals 7-5 = 2 damage; the player goes down to 6 hit points.
    The player deals 5-2 = 3 damage; the boss goes down to 6 hit points.
    The boss deals 7-5 = 2 damage; the player goes down to 4 hit points.
    The player deals 5-2 = 3 damage; the boss goes down to 3 hit points.
    The boss deals 7-5 = 2 damage; the player goes down to 2 hit points.
    The player deals 5-2 = 3 damage; the boss goes down to 0 hit points.

In this scenario, the player wins! (Barely.)

|#
(define test
  (lambda ()
    (simulation (make-player 'you 5 5 8)
		(make-player 'boss 7 2 12))))

(define shop
  (lambda ()
    (let ((wins '()))
    (do-list (weapon1 weapon-selection)
	    (do-list (armour1 armour-selection)
		    (do-list (ring1 ring-selection)
			    (do-list (ring2 ring-selection)
				    (cond
				     ;; different name rings
				     ((not (string=? (ring-name ring1)
						     (ring-name ring2)))				      
				      (let ((cost (+ (weapon-cost weapon1)
						     (armour-cost armour1)
						     (ring-cost ring1)
						     (ring-cost ring2)))
					    (damage (+ (weapon-damage weapon1)
						       (armour-damage armour1)
						       (ring-damage ring1)
						       (ring-damage ring2)))
					    (armour (+ (weapon-armour weapon1)
						       (armour-armour armour1)
						       (ring-armour ring1)
						       (ring-armour ring2))))
					;; (format #t "equipment : (cost ~a) W ~a A ~a R ~a R ~a~%"						
					;; 	cost
					;; 	weapon
					;; 	armour
					;; 	ring1
					;; 	ring2)
					
					;; player  name - damage - armour - hits
					(let* ((player (make-player "you" damage armour 100))
					       (boss (make-player "boss" 9 2 103 ))
					       (outcome (simulation player boss)))
					  (cond
					   ((string=? outcome "you") ;; you still win
					    (set! wins (cons `(,cost (COST ,cost) (WP ,weapon1) (ARMOUR ,armour1) (RING*1* ,ring1) (RING*2* ,ring2)) wins))
					    (format #t "win at cost ~a ~%" cost)))))))))))
    (format #t "~%--------------------------------------------------------------------------------~%")
    (set! wins (sort wins (lambda (x y) (< (car x) (car y))) ))
    (pp wins)
    wins)))

(define winners
  (lambda () (filter (lambda (x) (= (car x) 121)) (shop))))

;; spend the most money and still lose
(define shop2
  (lambda ()
    (let ((wins '()))
    (do-list (weapon1 weapon-selection)
	    (do-list (armour1 armour-selection)
		    (do-list (ring1 ring-selection)
			    (do-list (ring2 ring-selection)
				    (cond
				     ;; different name rings
				     ((not (string=? (ring-name ring1)
						     (ring-name ring2)))				      
				      (let ((cost (+ (weapon-cost weapon1)
						     (armour-cost armour1)
						     (ring-cost ring1)
						     (ring-cost ring2)))
					    (damage (+ (weapon-damage weapon1)
						       (armour-damage armour1)
						       (ring-damage ring1)
						       (ring-damage ring2)))
					    (armour (+ (weapon-armour weapon1)
						       (armour-armour armour1)
						       (ring-armour ring1)
						       (ring-armour ring2))))
					;; (format #t "equipment : (cost ~a) W ~a A ~a R ~a R ~a~%"						
					;; 	cost
					;; 	weapon
					;; 	armour
					;; 	ring1
					;; 	ring2)
					
					;; player  name - damage - armour - hits
					(let* ((player (make-player "you" damage armour 100))
					       (boss (make-player "boss" 9 2 103 ))
					       (outcome (simulation player boss)))
					  (cond
					   ((string=? outcome "boss") ;; you still lose
					    (set! wins (cons `(,cost (COST ,cost) (WP ,weapon1) (ARMOUR ,armour1) (RING*1* ,ring1) (RING*2* ,ring2)) wins))
					    (format #t "win at cost ~a ~%" cost)))))))))))
    (format #t "~%--------------------------------------------------------------------------------~%")
    (set! wins (sort wins (lambda (x y) (< (car x) (car y))) ))
    (pp wins)
    wins)))

;; (define winners2
;;   (lambda () (filter (lambda (x) (= (car x) 121)) (shop))))

#|

least expensive and still wins
> (shop)
;; depending on which finger , which ring goes ....
((121
  (COST 121)
  (WP #,(weapon 'name-> "longsword" 'cost-> 40 'damage-> 7 'armour-> 0)
  (ARMOUR #,(armour 'name-> "chainmail" 'cost-> 31 'damage-> 0 'armour-> 2)
  (RING*1* #,(ring 'name-> "damage2" 'cost-> 50 'damage-> 2 'armour-> 0)
  (RING*2* #,(ring 'name-> "null-ring" 'cost-> 0 'damage-> 0 'armour-> 0))
 (121
  (COST 121)
  (WP #,(weapon 'name-> "longsword" 'cost-> 40 'damage-> 7 'armour-> 0)
  (ARMOUR #,(armour 'name-> "chainmail" 'cost-> 31 'damage-> 0 'armour-> 2)
  (RING*1* #,(ring 'name-> "null-ring" 'cost-> 0 'damage-> 0 'armour-> 0)
  (RING*2* #,(ring 'name-> "damage2" 'cost-> 50 'damage-> 2 'armour-> 0)))

121 ...... ACCEPTED answer


most expensive and still loses
> (shop2)
(282
  (COST 282)
  (WP #,(weapon 'name-> "null-weapon" 'cost-> 0 'damage-> 0 'armour-> 0)
  (ARMOUR #,(armour 'name-> "platemail" 'cost-> 102 'damage-> 0 'armour-> 5)
  (RING*1* #,(ring 'name-> "damage3" 'cost-> 100 'damage-> 3 'armour-> 0)
  (RING*2* #,(ring 'name-> "defense3" 'cost-> 80 'damage-> 0 'armour-> 3)))
me hits 100 cost 282

cost : 282 
me    damage 3 armour 8 hits 100
boss  damage 9 armour 2 hits 103

282 ... Too high ...

(let ((d1 3)(a1 8)(h1 100)(d2 9)(a2 2)(h2 103))
 (simulation (make-player "me" d1 a1 h1)
             (make-player "boss" d2 a2 h2)))

AHA ! if we change weapon-selection to remove weapon-null ie no weapon
we should get correct answer
failiing to read the phrase "must" buy a weapon was a little funny to be honest
> (shop2)
 (201
  (COST 201)
  (WP #,(weapon 'name-> "dagger" 'cost-> 8 'damage-> 4 'armour-> 0)
  (ARMOUR #,(armour 'name-> "leather" 'cost-> 13 'damage-> 0 'armour-> 1)
  (RING*1* #,(ring 'name-> "damage3" 'cost-> 100 'damage-> 3 'armour-> 0)
  (RING*2* #,(ring 'name-> "defense3" 'cost-> 80 'damage-> 0 'armour-> 3)))

what it shows is that the code is correct , but solves wrong problem


|#




















