#|
Aoc 2015 - Day 21: 


|#

(import scheme)
(import simple-exceptions)
(import (chicken repl))
(import (chicken string))
(import (chicken pretty-print))
(import (chicken io))
(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day17")
;; (get-current-directory)
(import procedural-macros)
(import regex)
(import simple-md5)
(import simple-loops)
(import srfi-69)
;; hash-table-ref  hash key thunk
;; hash-table-set! hash key val
;; sudo chicken-install srfi-178
(import srfi-178)
;; srfi-178 provides bit-vectors
;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))
(import sequences)
(import srfi-1)
(import matchable)
(define pp pretty-print)

;;--------------------------------------

#|
Weapons:    Cost  Damage  Armor
--------------------------------
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0
Nothing       0     0       0

choose one of these weapons 

(dagger 8 4 0)
(shortsword 10 5 0)
(warhammer 25 6 0)
(longsword 40 7 0)
(greataxe 74 8 0)


Armor:      Cost  Damage  Armor
-------------------------------
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5

cost damage armour

((leather 13 0 1)
(chainmail 31 0 2)
(splintmail 53 0 3)
(bandedmail 75 0 4)
(platemail 102 0 5)) 

Rings:      Cost  Damage  Armor
-------------------------------
Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3
rings
cost damage armour

((damage-1 25 1 0)
(damage-2 50 2 0)
(damage-3 100 3 0)
(defense-1 20 0 1)
(defense-2 40 0 2)
(defense-3 80 0 3))

only one weapon
only one armor armour
two rings one on each hand , two different rings
have 100 hit points

**Input**

Hit Points: 103
Damage: 9
Armor: 2

each attack takes atleast 1 point away
must use any items you buy ?

|#


#|

For example, suppose you have 8 hit points, 5 damage, and 5 armor,
and that the boss has 12 hit points, 7 damage, and 2 armor:

    The player deals 5-2 = 3 damage; the boss goes down to 9 hit points.
    The boss deals 7-5 = 2 damage; the player goes down to 6 hit points.
    The player deals 5-2 = 3 damage; the boss goes down to 6 hit points.
    The boss deals 7-5 = 2 damage; the player goes down to 4 hit points.
    The player deals 5-2 = 3 damage; the boss goes down to 3 hit points.
    The boss deals 7-5 = 2 damage; the player goes down to 2 hit points.
    The player deals 5-2 = 3 damage; the boss goes down to 0 hit points.

In this scenario, the player wins! (Barely.)

hit points = life left

damage points =

armour points = 

|#
(define (atleast-one x)
  (cond
   ((< x 1) 1)
   (#t x)))

(define (battle h d a h2 d2 a2)
  (battle-player-one h d a h2 d2 a2))

(define (battle-player-one h d a h2 d2 a2)
  ;;(format #t "b1 : HDA ~a ~a ~a : HDA2 : ~a ~a ~a ~%" h d a h2 d2 a2)
  (let* ((di (atleast-one (- d a2)))
	 (new-h2 (- h2 di)))
    ;;(format #t "player one inflicts damage of ~a : player2 new health ~a ~%" di new-h2)
    (cond
     ((<= new-h2 0)
      ;;(format #t "player one wins ! : player 2 health ~a ~%" new-h2)
      1)
     (#t (battle-player-two h d a new-h2 d2 a2)))))

(define (battle-player-two h d a h2 d2 a2)
  ;;(format #t "b2 : HDA ~a ~a ~a : HDA2 : ~a ~a ~a ~%" h d a h2 d2 a2)
  (let* ((di (atleast-one (- d2 a)))
	 (new-h (- h di)))
    ;;(format #t "player two inflicts damage of ~a : player1 new health ~a ~%" di new-h)
    (cond
     ((<= new-h 0)
      ;;(format #t "player two wins ! : player 1 health ~a ~%" new-h)
      2)
     (#t (battle-player-one new-h d a h2 d2 a2)))))


;; hit points player is 100
;; vary damage and armour
;;

(define (make-player)
  (let ((weapons '((dagger 8 4 0)
		   (shortsword 10 5 0)
		   (warhammer 25 6 0)
		   (longsword 40 7 0)
		   (greataxe 74 8 0)
		   (no-weapon 0 0 0)))
	(armours '((leather 13 0 1)
		   (chainmail 31 0 2)
		   (splintmail 53 0 3)
		   (bandedmail 75 0 4)
		   (platemail 102 0 5)
		   (no-armours 0 0 0)
		   ))
	(rings '((damage-1 25 1 0)
		 (damage-2 50 2 0)
		 (damage-3 100 3 0)
		 (defense-1 20 0 1)
		 (defense-2 40 0 2)
		 (defense-3 80 0 3)
		 (no-rings 0 0 0)
		 )))
    (let ((best-cost 0))
      (do-list (weapon weapons)
	       (do-list (armour armours)
			(do-list (ring1 rings)
				 (do-list (ring2 rings)
					  (cond
					   ((not (equal? ring1 ring2))
					    (let* ((bag (list weapon armour ring1 ring2))
						   (cost (apply + (map (lambda (x) (second x)) bag)))
						   (dam (apply + (map (lambda (x) (third x)) bag)))
						   (arm (apply + (map (lambda (x) (fourth x)) bag)))
						   (hits 100))
					      (let ((opp-hits 103)(opp-dam 9)(opp-arm 2))
						(let ((out (battle hits dam arm opp-hits opp-dam opp-arm)))
						  (cond
						   ((= out 2)
						    (when (> cost best-cost)
						      (set! best-cost cost)
						      (format #t "lose - dam :~a  arm ~a : cost ~a ~%" dam arm cost)))))))))))
			(format #t "~%~% best cost : ~a ~%" best-cost))))))




(make-player)

#|

282 rejected ...

|#

