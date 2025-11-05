#lang racket
;;
;;
;;
;;
;;
;;
;;

(define-syntax pr
  (syntax-rules () 
    ((pr x y) (format x y))))



(define (play1-move h m a e t h2 mv ms)
  (cond
    ((= t 0) (let ((e 'none))
               (magic-missile h m a e t h2 mv ms)
               (drain h m a e t h2 mv ms)
               (shield h m a e t h2 mv ms)
               (poison h m a e t h2 mv ms)
               (recharge h m a e t h2 mv ms)))
    (#t  (magic-missile h m a e t h2 mv ms)
         (drain h m a e t h2 mv ms))))


(define (play1 h m a e t h2 mv ms)
  (cond
   ((<= h 0)
    ;;(format #t "~a~%" (reverse (cons 'player-dies mv)))
    'player-dies
    )
    ((<= h2 0) (pr "~a~%" (reverse (cons 'boss-dies mv))))
    ((and (eq? e 'shield) (> t 0)) (play1-move h m (+ a 7) e (max 0 (- t 1)) h2 (cons 'shield mv) ms))
    ((and (eq? e 'poison) (> t 0)) (play1-move h m a e (max 0 (- t 1)) (max 0 (- h2 3)) (cons 'poison mv) ms))
    ((and (eq? e 'recharge) (> t 0)) (play1-move h (+ m 101) a e (max 0 (- t 1)) h2 (cons 'recharge mv) ms))
    (#t (play1-move h m a e t h2 mv ms))))


(define (play2 h m a e t h2 mv ms)
  (cond
   ((<= h 0)
    ;;(format #t "~a~%" (reverse (cons 'player-dies mv)))
    'player-dies
    )
    ((<= h2 0) (pr "~a~%" (reverse (cons 'boss-dies mv))))
    ((and (eq? e 'shield) (> t 0)) (play2-move h m (+ a 7) e (max 0 (- t 1)) h2 (cons 'shield mv) ms))
    ((and (eq? e 'poison) (> t 0)) (play2-move h m a e (max 0 (- t 1)) (max 0 (- h2 3)) (cons 'poison mv) ms))
    ((and (eq? e 'recharge) (> t 0)) (play2-move h (+ m 101) a e (max 0 (- t 1)) h2 (cons 'recharge mv) ms))
    (#t (play2-move h m a e t h2 mv ms))))


(define (play2-move h m a e t h2 mv ms)
  (let* ((dam (max 1 (- a 8)))
         (h (- h dam)))
    (let ((attack 8))
      (cond
       ;; some absorbed by armour , some by body
       ((> attack a) (let* ((attack2 (- attack a))
			    (a2 0)
                            (hnew (- h attack2)))
                       (play1 hnew m a2 e t h2 (cons 'attacked mv) ms)))
       ;; absorb attack entirely by armour
       (#t (let ((a2 (- a attack)))
             (play1 h m a2 e t h2 (cons 'attacked mv) ms)))))))


(define (magic-missile h m a e t h2 mv ms)
  (when
    (>= m 53)    (play2 h (- m 53) a e t (max 0 (- h2 4)) mv (+ 53 ms))))

(define (drain h m a e t h2 mv ms)
  (when
    (>= m 73) (play2 (+ h 2) (- m 73) a e t (max 0 (- h2 2)) mv (+ ms 73))))

(define (shield h m a e t h2 mv ms)
  (when
    (>= m 113)  (play2 h (- m 113) (+ a 1) 'shield 6 h2 mv (+ ms 113))))

(define (poison h m a e t h2 mv ms)
  (when
    (>= m 173)  (play2 h (- m 173) a 'poison 6 (- h2 3) mv (+ ms 173))))

(define (recharge h m a e t h2 mv ms)
  (when
    (>= m 229) (play2 h (- m 229) a 'recharge 5 h2 mv (+ ms 229))))

(define (run)
  (let ((h 50) ; hits
        (m 500) ; mana
        (a 0) ; armour
        (e 'none) ; effect
        (t 0) ; timer
        (h2 55) ; boss hits
        (ms 0) ;; mana spent by player
        (mv '()))
    (play1 h m a e t h2 mv ms)))


