;;; =============================================================
;;;  CMPU-365, Fall 2014
;;;  Asmt. 3
;;;  FILE:  vw.lisp
;;; =============================================================
;;;  Implementation of the VACUUM WORLD
;;; =============================================================


;;; Specify size of vacuum world in a global variable

(defconstant +vw-size+ 4)


;;;  VW-STATE data structure
;;; ---------------------------
;;;   ROB-LOC -- location of Robby the robot.
;;;   DIRT-LOCATIONS -- list of locations that are dirty
;;;                     (i.e., a list of pairs).

(defstruct (vw-state (:print-function print-vw-state))
  rob-loc 
  dirt-locations)


;;;  PRINT-VW-STATE
;;; ---------------------------------------------------
;;;  To be linked to the VW-STATE struct, this function is required
;;;  to take the following three inputs:
;;;   INPUTS:  STATE, a vacuum-world state struct
;;;            STR, an output stream to print to (usually t)
;;;            DEPTH, printing depth (we'll ignore this)
;;;   OUTPUT:  None
;;;   SIDE-EFFECT:  Displays the given VW-STATE struct.

(defun print-vw-state (state str depth)
  ;; Tell the compiler not to warn us about the fact that we 
  ;; don't make use of DEPTH
  (declare (ignore depth))
  (let ((rob-loc (vw-state-rob-loc state))
	(dirt-locs (vw-state-dirt-locations state)))
    (format str "Rob-Loc: ~A, " rob-loc)
    (format str "Dirt-Locs: ~A~%" dirt-locs)))


;;;  VW-STATE-EQUAL?
;;; ---------------------------------------------------
;;;   INPUT:  STATE1, STATE2 -- Two VW-WORLD structs
;;;   OUTPUT:  T if the given VW-STATES have the same contents, NIL otherwise.

(defun vw-state-equal? (state1 state2)
  (and
   (equal (vw-state-rob-loc state1)
	  (vw-state-rob-loc state2))
   ;; assumes dirt locations are in the same order...
   ;; this is safe to assume given how we deal with them...
   (equal (vw-state-dirt-locations state1)
	  (vw-state-dirt-locations state2)))) 


;;;  VW-GOAL-TEST
;;; ----------------------------------------------------
;;;   INPUT:  STATE, a VW-STATE struct
;;;   OUTPUT:  T if STATE is a goal-state.

(defun vw-goal-test (state)
  (null (vw-state-dirt-locations state)))

;;;  VALID-XY
;;; --------------------------
;;;  A helper function for the DO-MOVE function.
;;;  INPUTS:  X, Y -- numerical coordinates
;;;  OUTPUT:  T if (x,y) is a location in this vacuum world.
;;;  Note:  Coordinates can't go below zero or above +VW-SIZE+.

(defun valid-xy (x y)
  (and (>= x 0)
       (>= y 0)
       (< x +vw-size+)
       (< y +vw-size+)))


;;;  DO-MOVE
;;; -------------------------------------------------
;;;   A generic move operator.  Various values of DELTA-X and
;;;   DELTA-Y determine which way robbie moves.  Returns the
;;;   new child state, or NIL.
;;;     INPUTS:  ORIG-STATE, original state of the vacuum world
;;;              DELTA-X, DELTA-Y -- amount to move in the X and Y directions
;;;     OUTPUT:  The resulting state (if the move was legal)
;;;              NIL if the move was not legal.

(defun do-move (orig-state delta-x delta-y)
  (let* ((orig-loc (vw-state-rob-loc orig-state))
	 (new-x (+ (first orig-loc) delta-x))
	 (new-y (+ (second orig-loc) delta-y)))
    ;; if move would be valid...
    (if (valid-xy new-x new-y) 
	;; then create a new child state and return it...
	;; NOTE: We use SAME dirt-locations list, not a copy.
	;;       This is okay because we're non-destructive!
	(make-vw-state :rob-loc (list new-x new-y)
		       :dirt-locations (vw-state-dirt-locations orig-state))
      ;; otherwise, return nil
      nil)))

;;;  The NORTH, SOUTH, EAST and WEST move operators (they use DO-MOVE)
;;; --------------------------------------------------------------------

(defun north (orig-state) (do-move orig-state 0 -1))

(defun south (orig-state) (do-move orig-state 0 1))

(defun east (orig-state) (do-move orig-state 1 0))

(defun west (orig-state) (do-move orig-state -1 0))


;;;  VACUUM -- an important operator!
;;; ----------------------------------------------

(defun vacuum (orig-state)
  (let* ((robbie-loc (vw-state-rob-loc orig-state))
	 (dirt-locs (vw-state-dirt-locations orig-state)))
    (if ;; robbie is in a dirty room...
	(member robbie-loc dirt-locs :test #'equal)
	;; then create a new child state and return it...
	(make-vw-state :rob-loc robbie-loc
		       ;; REMOVE generates a new list of dirt locations
		       ;; It is non-destructive...
		       :dirt-locations (remove robbie-loc
					       dirt-locs
					       :test #'equal))
      ;; otherwise, return nil
      nil)))


