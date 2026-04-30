(s myHits <= 0) ifTrue:[ ^ #playerDead ].

"effects"
(s missileCount > 0) ifTrue: [ s bossHits: (s bossHits - 4). 
                               s missileCount: (s missileCount - 1 max: 0) ].
                               
(s drainCount > 0) ifTrue: [ s bossHits: (s bossHits - 2). 
                             s myHits: (s myHits + 2) . 
                             s drainCount: (s drainCount - 1 max: 0) ].
                             
(s shieldCount > 0) ifTrue: [ s myArmor: 7.   
                              s shieldCount: (s shieldCount - 1 max: 0) ].
                              
(s poisonCount > 0) ifTrue: [ s bossHits: (s bossHits - 3) .   
                             s poisonCount: (s poisonCount - 1 max: 0) ].
                               
(s rechargeCount > 0) ifTrue: [ s myMana:  (s myMana + 101) . 
	                            s rechargeCount: (s rechargeCount - 1 max: 0)].

                                
"check if boss dead"
(s bossHits <= 0) ifTrue:[ 
	Transcript show: { 'PLAYER Wins !! mana spent => ' . (s myManaSpent) } ; cr .
	((s myManaSpent) < (s class best)) ifTrue: [ s class best: (s myManaSpent) ] .	
	^ #bossDead ].

"decide on effect to cast based on amount mana player has"
s spells: OrderedCollection new.
((s myMana >= 53) & (s missileCount = 0)) ifTrue: [ s spells add: #missile ].
((s myMana >= 73) & (s drainCount = 0)) ifTrue: [ s spells add: #drain ].
((s myMana >= 113) & (s shieldCount = 0)) ifTrue: [ s spells add: #shield ].
((s myMana >= 173) & (s poisonCount = 0)) ifTrue: [ s spells add: #poison ].
((s myMana >= 229) & (s rechargeCount = 0)) ifTrue: [ s spells add: #recharge ].
"no spells can be cast - player dies"
(s spells size = 0) ifTrue:[ ^ #playerDead ].

s spells do: [:spell |
 (spell = #missile) 
 ifTrue: [ | t | t:= s deepCopy .  t myMana: (t myMana - 53) . t myManaSpent: (t myManaSpent + 53) . t missileCount: 1 . t bossMove  ].
 
 (spell = #drain) 
 ifTrue: [ | t | t:= s deepCopy . t myMana: (t myMana - 73) .  t myManaSpent: (t myManaSpent + 73) . t drainCount: 1 . t bossMove ].
 
 (spell = #shield) 
 ifTrue: [ | t | t:= s deepCopy . t myMana: (t myMana - 113) . t myManaSpent: (t myManaSpent + 113) . t shieldCount: 6 . t bossMove ].
 
 (spell = #poison) 
 ifTrue: [ | t | t:= s deepCopy . t myMana: (t myMana - 173) . t myManaSpent: (t myManaSpent + 173) . t poisonCount: 6 . t bossMove ].
 
 (spell = #recharge) 
 ifTrue: [ | t | t:= s deepCopy . t myMana: (t myMana - 229) . t myManaSpent: (t myManaSpent + 229) . t rechargeCount: 5 . t bossMove ].
 
]

