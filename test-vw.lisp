;;; -----------------------------------
;;;   CMPU-365, Fall 2014
;;;   Asmt. 3
;;;   FILE:  test-vw.lisp
;;; -----------------------------------
;;;  This file implements the MAKE-VW-PROBLEM, DO-VW-DEPTH and 
;;;  DO-VW-BREADTH functions that can be used to test the generic search 
;;;  code in "gen-search.lisp" on the implementation of the Vacuum World 
;;;  problem in "vw.lisp".


;;;  MAKE-VW-PROBLEM
;;; ----------------------------------------------------
;;;   Defines an instance of a search problem for the Vacuum World.
;;;   Use a simple problem instance until you get things working.

(defun make-vw-problem ()
  (make-search-problem 
   :init-state (make-vw-state :rob-loc '(0 0)
			      :dirt-locations '((0 0) (0 3) (1 2) (3 1)
						;; (3 3) 
						))
   :actions (list #'north #'south #'east #'west #'vacuum)
   :goal-test-func #'vw-goal-test
   :state-eq-func #'vw-state-equal?))


;;; ===============================================================
;;;   Helper functions for testing.
;;;   When ready to test, call:  (DO-VW-DEPTH) or (DO-VW-BREADTH).
;;; ===============================================================

(defun do-vw-depth ()
  (print-results (depth-first-search (make-vw-problem))))

(defun do-vw-breadth()
  (print-results (breadth-first-search (make-vw-problem))))
