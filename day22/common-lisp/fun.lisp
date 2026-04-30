;; WOW -- working wizard example

(defpackage :fun
  (:use :cl))
(in-package :fun)

;; the wizards apprentice

(defparameter *best* 0)

(defparameter *boss-hits* 55)
(defparameter *boss-damage* 8)

(defparameter *missile-cost* 53)
(defparameter *missile-damage* 4)
(defparameter *missile-duration* 1)

(defparameter *drain-cost* 73)
(defparameter *drain-damage* 2)
(defparameter *drain-heal* 2)
(defparameter *drain-duration* 1)

(defparameter *shield-cost* 113)
(defparameter *shield-duration* 6)
(defparameter *shield-armor* 7)

(defparameter *poison-cost* 173)
(defparameter *poison-damage* 3)
(defparameter *poison-duration* 6)

(defparameter *recharge-cost* 229)
(defparameter *recharge-mana* 101)
(defparameter *recharge-duration* 5)

(defparameter *cheapest-cost* (min *missile-cost*
				   *drain-cost*
				   *shield-cost*
				   *poison-cost*
				   *recharge-cost*))


(defstruct state
  "Represents a game state"
  (mana-spent 0)
  (mana 500)
  (hits 50)
  (armor 0)
  (boss-hits *boss-hits*)
  (boss-damage *boss-damage*)
  (missile 0)
  (drain 0)
  (shield 0)
  (poison 0)
  (recharge 0))


(defun missile-effect! (s)
  (when (> (state-missile s) 0)
    (decf (state-boss-hits s) *missile-damage*)
    (decf (state-missile s))))

(defun drain-effect! (s)
  (when (> (state-drain s) 0)
    (decf (state-boss-hits s) *drain-damage*)
    (incf (state-hits s) *drain-heal*)    
    (decf (state-drain s))))

;; shield may be important when it gets to zero - armor goes back to zero !
;; no actual side effect of having armor until combat when
;; armor must be taken into account if state-shield > 0 
(defun shield-effect! (s)
  (when (> (state-shield s) 0)    
    (decf (state-shield s))))

(defun poison-effect! (s)
  (when (> (state-poison s) 0)
    (decf (state-boss-hits s) *poison-damage*)
    (decf (state-poison s))))

(defun recharge-effect! (s)
  (when (> (state-recharge s) 0)
    (incf (state-mana s) *recharge-mana*)
    (decf (state-recharge s))))

(defun cast-missile (s)
  (when (and (>= (state-mana s) *missile-cost*)
	     (= (state-missile s) 0))
    (let ((s3 (copy-state s)))
      (decf (state-mana s3) *missile-cost*)
      (incf (state-mana-spent s3) *missile-cost*)
      (setf (state-missile s3) *missile-duration*)
      (boss-move s3))))

(defun cast-drain (s)
  (when (and (>= (state-mana s) *drain-cost*)
	     (= (state-drain s) 0))
    (let ((s3 (copy-state s)))
      (decf (state-mana s3) *drain-cost*)
      (incf (state-mana-spent s3) *drain-cost*)
      (setf (state-drain s3) *drain-duration*)
      (boss-move s3))))

(defun cast-shield (s)
  (when (and (>= (state-mana s) *shield-cost*)
	     (= (state-shield s) 0))
    (let ((s3 (copy-state s)))
      (decf (state-mana s3) *shield-cost*)
      (incf (state-mana-spent s3) *shield-cost*)
      (setf (state-shield s3) *shield-duration*)
      (boss-move s3))))

(defun cast-poison (s)
  (when (and (>= (state-mana s) *poison-cost*)
	     (= (state-poison s) 0))
    (let ((s3 (copy-state s)))
      (decf (state-mana s3) *poison-cost*)
      (incf (state-mana-spent s3) *poison-cost*)
      (setf (state-poison s3) *poison-duration*)
      (boss-move s3))))

(defun cast-recharge (s)
  (when (and (>= (state-mana s) *recharge-cost*)
	     (= (state-recharge s) 0))
    (let ((s3 (copy-state s)))
      (decf (state-mana s3) *recharge-cost*)
      (incf (state-mana-spent s3) *recharge-cost*)
      (setf (state-recharge s3) *recharge-duration*)
      (boss-move s3))))


(defun player-move (s)
  (cond
    ((> (state-mana-spent s) *best*) 'overspend)    
    ((<= (state-hits s) 0) 'player-dead)
    (t
     ;; player still alive
     (let ((s (copy-state s)))
       (missile-effect! s)
       (drain-effect! s)
       (shield-effect! s)
       (poison-effect! s)
       (recharge-effect! s)

       (cond
	 ((<= (state-boss-hits s) 0) 'boss-dead
	  (when (< (state-mana-spent s) *best*)
	    (setq *best* (state-mana-spent s))
	    (format t "new best - win player with ~a mana~%" *best*)))
	 (t ;; boss still alive
	  ;; cast spells 
	  (cast-missile s)
	  (cast-drain s)
	  (cast-shield s)
	  (cast-poison s)
	  (cast-recharge s)       
	  ;; if cannot afford to cast any spell i die
	  (when (< (state-mana s) *cheapest-cost*)
	    'die)))))))


(defun boss-move (s)
  (cond
    ((> (state-mana-spent s) *best*) 'overspend)    
    ((<= (state-hits s) 0) 'player-dead)
    (t
     ;; player still alive
     (let ((s (copy-state s)))
       (missile-effect! s)
       (drain-effect! s)
       (shield-effect! s)
       (poison-effect! s)
       (recharge-effect! s)
       ;; check if boss dead
       (cond
	 ((<= (state-boss-hits s) 0) 'boss-dead
	  (when (< (state-mana-spent s) *best*)
	    (setq *best* (state-mana-spent s))
	    (format t "new best - win player with ~a mana~%" *best*)))
	 (t ;; boss still alive
	  ;; boss deals damage - if armor active
	  (let ((s (copy-state s)))
	    (let ((damage (state-boss-damage s)))
	      (cond
		((> (state-shield s) 0) ;; shield active
		 (setq damage (max 1 (- damage *shield-armor*)))
		 (decf (state-hits s) damage)
		 (player-move s))
		(t ;; shield inactive
		 (decf (state-hits s) damage)
		 (player-move s)))))))))))


(defun run ()
  (setq *best* 9999999999999999)
  (let ((s (make-state)))
    (player-move s)))

(defun example ()
  (setq *best* 9999999999999999)
  (let ((s (make-state)))
    (setf (state-hits s) 10)
    (setf (state-mana s) 250)
    (setf (state-boss-hits s) 13)
    (setf (state-boss-damage s) 8)
    (player-move s)
    (format t "expected ~a ~%" (+ *poison-cost* *missile-cost*))))


