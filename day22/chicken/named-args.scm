#|

optimize if mana spent > 1387 then curtail as too far

player has so many mana , armour , hits 
when player.hits <= 0 player dies

recharge 229 mana , 5 turns , effect + 101 mana to player
shield 113 mana , 6 turns , effect armour +7 
poison 173 mana , 6 turns , effect boss 3 damage 
drain 73 mana , does 2 damage and heals for 2 hit points 
magic-missile 53 mana and does 4 damage

boss has hits 
boss will always deal X damage , casts no spells
when boss.hits <= 0 boss dies

effects apply at start of both players turns and boss turns

solution look like

m a h r s p
m : mana
a : armour
h : hits
r : recharge  r s p counters or timers 
s : shield
p : poison 


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

