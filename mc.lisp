;;; =============================================================
;;;  CMPU-365,Fall 2014
;;;  Asmt. 3
;;;  Author: Jarrett Holtz & Cat Morgan
;;;  FILE:  mc.lisp
;;; =============================================================
;;;  Implementation of the Missionaries and Cannibals Domain
;;; =============================================================

(defconstant +groupSize+ 3)

;;;  MC-STATE data structure
;;; ---------------------------
;;;   BOATSIDE -- side of the river the boat is on (0 left,1 right)
;;;   WAITING -- Number of cannibals and missionaries on the left side
(defstruct (mc-state (:print-function print-mc-state))
	waitingM
	waitingC
	boatSide)

;;;  PRINT-MC-STATE
;;; ---------------------------------------------------
;;;  To be linked to the MC-STATE struct, this function is required
;;;  to take the following three inputs:
;;;   INPUTS:  STATE, a vacuum-world state struct
;;;            STR, an output stream to print to (usually t)
;;;            DEPTH, printing depth (we'll ignore this)
;;;   OUTPUT:  None
;;;   SIDE-EFFECT:  Displays the given VW-STATE struct.
(defun print-mc-state (state str depth)
	(declare (ignore depth))
	(let ((boatSide (mc-state-boatSide state))
		  (waitingC (mc-state-waitingC state))
		  (waitingM (mc-state-waitingM state)))
	(format str "Boat Side: ~A,~% " boatSide)
	(format str "Left Side --> Missionaries: ~A, Cannibals: ~A ~%" 
		waitingM waitingC)
	(format str "Right Side --> Missionaries: ~A, Cannibals: ~A ~%"
		(- +groupSize+ waitingM) (- +groupSize+ waitingC))))

;;;  MC-STATE-EQUAL?
;;; ---------------------------------------------------
;;;   INPUT:  STATE1, STATE2 -- Two VW-WORLD structs
;;;   OUTPUT:  T if the given VW-STATES have the same contents, NIL otherwise.
(defun mc-state-equal? (state1 state2)
	(and
		(equal (mc-state-boatSide state1) 
			(mc-state-boatSide state2))
		(equal (mc-state-waitingM state1)
				(mc-state-waitingM state2))
		(equal (mc-state-waitingC state1)
			(mc-state-waitingC state2))))

;;;  MC-GOAL-TEST
;;; ----------------------------------------------------
;;;   INPUT:  STATE, a MC-STATE struct
;;;   OUTPUT:  T if STATE is a goal-state.
(defun mc-goal-test (state)
	(let ((waitingM (mc-state-waitingM state))
			(waitingC (mc-state-waitingC state)))
	(and 
		(equal waitingM 0)
		(equal waitingC 0))))

;;;  VALID-WAITING
;;; --------------------------
;;;  A helper function for the DO-MOVE function.
;;;  INPUTS:  waitingM, waitingC number of waiting Missionaries and Cannibals
;;;  OUTPUT:  T if the ratio of cannibals to missionaries is acceptable
(defun valid-waiting (waitingM  waitingC)
  (and 
                (or
		 (>= waitingM waitingC)
		 (= waitingM 0))
		(or
		 (>= (- +groupSize+ waitingM) (- +groupSize+ waitingC))
		 (= (- +groupSize+ waitingM) 0))
		(>= waitingM 0)
		(>= waitingC 0)
		(<= waitingM 3)
		(<= waitingC 3)))

;;;  DO-MOVE
;;; -------------------------------------------------
;;;   A generic move operator.  Various values of DELTA-M and
;;;   DELTA-M determine who crosses the river.  Returns the
;;;   new child state, or NIL.
;;;     INPUTS:  ORIG-STATE, original state of the vacuum world
;;;              DELTA-M, DELTA-M -- number of missionaries or cannibals to move
;;;     OUTPUT:  The resulting state (if the move was legal)
;;;              NIL if the move was not legal.
(defun do-cross (orig-state delta-m delta-c)
	(let* ((orig-waitingM (mc-state-waitingM orig-state))
			(orig-waitingC (mc-state-waitingC orig-state))
			(orig-boatSide (mc-state-boatSide orig-state))
			(new-waitingM (+ orig-waitingM (- delta-m (* orig-boatSide 2 delta-m))))
			(new-waitingC (+ orig-waitingC (- delta-c (* orig-boatSide 2 delta-c)))))
	  ;;If vaild state achieved
		(if (valid-waiting new-waitingM new-waitingC)
			;;Return new child state
			(make-mc-state 
				:waitingM new-waitingM
				:waitingC new-waitingC
				:boatSide (mod (+ orig-boatSide 1) 2))
		  ;;else NIL
		  nil)))

;;;  The movement options for crossing the river
;;; --------------------------------------------------------------------
(defun cross20 (orig-state) (do-cross orig-state 2 0))
(defun cross10 (orig-state) (do-cross orig-state 1 0))
(defun cross11 (orig-state) (do-cross orig-state 1 1))
(defun cross01 (orig-state) (do-cross orig-state 0 1))
(defun cross02 (orig-state) (do-cross orig-state 0 2))

;;;  MAKE-MC-PROBLEM
;;; 
;;; --------------------------------------------------
;;;  OUTPUT: A Missionaries and Cannibals search problem
;;;  initialized to 3 waiting Missionaries, 3 waiting Cannibals
;;;  and the boat on the left side.
(defun make-mc-problem()
  (make-search-problem 
   :init-state (make-mc-state :waitingM 3 :waitingC 3 :boatSide 1)
   :actions (list #'cross20 #'cross10 #'cross11 #'cross01 #'cross02)
   :goal-test-func #'mc-goal-test
   :state-eq-func #'mc-state-equal?))

(setf tempState (make-mc-state :waitingM 3 :waitingC 3 :boatside 1))
(setf goalState (make-mc-state :waitingM 0 :waitingC 0 :boatside 0))
(format t "~A ~%" tempState)
(setf tempState2 (cross11 tempState))
(format t "~A ~%" tempState2)
(setf state3 (cross10 tempState2))
(format t "~A ~%" state3)
(setf state4 (cross02 state3))
(format t "~A ~%" state4)
(setf state5 (cross01 state4))
(format t "~A ~%" state5)
(setf state6 (cross20 state5))
(format t "~A ~%" state6)
(setf state7 (cross11 state6))
(format t "~A ~%" state7)
(setf state8 (cross20 state7))
(format t "~A ~%" state8)
(setf state9 (cross01 state8))
(format t "~A ~%" state8)
(setf state10 (cross02 state9))
(format t "~A ~%" state10)
(setf state11 (cross10 state10))
(format t "~A ~%" state11)
(setf state12 (cross11 state11))
(format t "~A ~%" state12)
(format t "~A ~%" (mc-goal-test state12))
;;(print-mc-state tempState2 t 0)
