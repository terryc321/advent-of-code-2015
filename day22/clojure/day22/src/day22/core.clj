(ns day22.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; puzzle input
;;
;; boss has 
;; Hit Points: 55
;; Damage: 8
;;
;; presumably boss does 8 damage consistently
;;
;; spells can cast
;;
;; start with 500 mana , 0 armour
;; collecting armour negates boss attack by that much , atleast 1 damage
;; when boss attacks , your hit points get deducted by that amount
;;
;; Magic Missile costs 53 mana. It instantly does 4 damage.
;; Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.
;; Shield costs 113 mana. It starts an effect that lasts for 6 turns. While it is active, your armor is increased by 7.
;; Poison costs 173 mana. It starts an effect that lasts for 6 turns. At the start of each turn while it is active, it deals the boss 3 damage.
;; Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At the start of each turn while it is active, it gives you 101 new mana.
;;
;;
;;
;; mana hits damage effects
;;
;; way to replay action - each move id increased each time some kind of state { }
;; initial state
;; boss  55=hits 8=damage 0=armor {}=effects
;; me   500 mana 50=hits ?=damage 0=armour {}=effects
;;
;; what is least amount mana spend and still win the fight ?
;;
;; h m a e t h2

;; (defn player1
;;   [h m a e t h2]
;;   (let [p1 (:p1 state)
;;         p2 (:p2 state)
;;         mana1 (:mana p1)
;;         armor1 (:armor p1)
;;         effects1 (:effects p1)
;;         timer1 (:timer p1)
;;         hits1 (:hits p1)
;;         hits2 (:hits p2)
;;         damage2 (:damage p2)
;;         ]

;;     ;; apply any effect in place
;;     (when (nil? effects1)
;;       (let [timer1 0]
;;         (player1-magic-missile (hash-map
;;                                 :moves []
;;                                 :p1 (hash-map :hits hits1 :mana new-mana :armour armor1 :effects effects1 :timer timer1)
;;                                 :p2 (hash-map :hits 55 :damage 8)))))
    
    

;;     ;; magic missile costs 53 mana. It instantly does 4 damage
;;     ;; have i got 53 mana ?
;;     (when (>= mana 53)
;;       (let [new-mana (- mana 53)
;;             new-state (hash-map
;;                        :moves []
;;                        :p1 (hash-map :hits hits1 :mana new-mana :armour armor1 :effects effects1 :timer timer1)
;;                        :p2 (hash-map :hits 55 :damage 8))]
;;         (player2 new-state)))
    
      
    
;;   (player2 s)
;;   ;; drain

;;   ;; shield

;;   ;; poison

;;   ;; recharge
  
;;   )

;; (defn player2
;;   [state]
;;   ;; do 8 damage to player 1 
;;   (player1 s))


;; (defn start
;;   []
;;   (let ((state (hash-map
;;                 :moves []
;;                 :p1 (hash-map :hits 50 :mana 500 :armour 0 :effects (hash-map :name nil :timer 0))
;;                 :p2 (hash-map :hits 55 :damage 8))))
;;     (player1 state)))


;; h = hits
;; m = mana
;; a = armor
;; e = effects
;; t = timer
;; h2 = boss hits
;;
;; since boss is static does 8 damage each time only boss hits h2 we need consider
;; 


(defn play1-move 
  [h m a e t h2 mv]
  (cond
    (= t 0) (let [e 'none]
              (do (magic-missile h m a e t h2 mv)
                  (drain h m a e t h2 mv)
                  (shield h m a e t h2 mv)
                  (poison h m a e t h2 mv)
                  (recharge h m a e t h2 mv)))
    true (do (magic-missile h m a e t h2 mv)
             (drain h m a e t h2 mv))))


(defn play1
  [h m a e t h2 mv]
  (cond
    (and (= e 'shield) (> t 0)) (play1-move h m (+ a 7) e (max 0 (- t 1)) h2 mv)
    (and (= e 'poison) (> t 0)) (play1-move h m a e (max 0 (- t 1)) (max 0 (- h2 3)) mv)
    (and (= e 'recharge) (> t 0)) (play1-move h (+ m 101) a e (max 0 (- t 1)) h2 mv)
    true (play1-move h m a e t h2 mv)))


(defn play2
  [h m a e t h2 mv]
  (let [dam (max 1 (- a 8))
        h (- h dam)]
    (cond
      (<= h2 0) (conj mv 'boss-dies)
      true (let [attack 8]
             (cond
               ;; some absorbed by armour , some by body
               (> attack a) (let [attack2 (- attack a)
                                  a2 0
                                  hnew (- h attack2)]
                              (play1 hnew m a2 e t h2 mv))
               ;; absorb attack entirely by armour
               (<= attack a) (let [a2 (- a attack)]
                               (play1 h m a2 e t h2 mv)))))))


            
(defn magic-missile
  [h m a e t h2 mv]
  (when
    (>= m 53)    (play2 h (- m 53) a e t (max 0 (- h2 4)) mv)))

(defn drain
  [h m a e t h2 mv]
  (when
    (>= m 73) (play2 (+ h 2) (- m 73) a e t (max 0 (- h2 2)) mv)))

(defn shield
  [h m a e t h2 mv]
  (when
    (>= m 113)  (play2 h m (+ a 1) 'shield 6 h2 mv)))

(defn poison
  [h m a e t h2 mv]
  (when
    (>= m 173)  (play2 h (- m 173) a 'poison 6 (- h2 3) mv)))

(defn recharge
  [h m a e t h2 mv]
  (when
    (>= m 229) (play2 h (- m 229) a 'recharge 5 h2 mv)))


(defn run []
  (let [h 55
        m 500
        a 0
        e 'none
        t 0
        h2 55
        mv []]
    (play1 h m a e t h2 mv)))






