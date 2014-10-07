;;; ========================================
;;;   CMPU-365, Fall 2014
;;;   Asmt. 3
;;;   Cat Morgan, Jarrett Holtz
;;; ========================================
;;;   FILE:  gen-search-starter.lisp
;;; ========================================
;;;   General tree-search algorithm.  Special cases
;;;   include breadth-first and depth-first search.

;;;  A global variable for keeping track of the number
;;;  of nodes created so far.
;;; --------------------------------------------------
;;;  We will *destructively* modify this parameter!
;;;  This is one of *only two* uses of destructive programming
;;;  in this assignment!  The other is in the queuing functions
;;;  defined at the end of this assignment.  
;;;  ==> All other code must be *NON-DESTRUCTIVE*!!!

(defparameter *node-count* 0)


;;;  PROBLEM struct -- a search problem
;;; --------------------------------------

(defstruct search-problem
  init-state				; the initial state
  actions				; the list of actions
  goal-test-func			; the goal test function
  state-eq-func				; the state-equality function
  )

;;;  NODE struct -- a node in a search tree
;;; -----------------------------------------------------------
;;;  Note the use of the :PRINT-FUNCTION syntax that enables us to specify 
;;;  which function should be used to display a NODE struct. 
;;;  (Kind of like providing a "toString" method in Java.)

(defstruct (node (:print-function print-node))
  state					; the state associated with this node
  (parent nil)				; the parent node for this node
  (action nil)				; the most recent action performed
  (depth 0)				; the depth of this node
  )
	    
;;;  PRINT-NODE 
;;; ------------------------------------------------------------------
;;;  Note:  The three inputs specified below are required if we
;;;         want to use this function as the printing function for
;;;         the node struct defined above.
;;; ------------------------------------------------------------------
;;;  INPUTS:  NODE, a search node
;;;           STR, an output stream (usually t)
;;;           DEPTH, printing depth parameter (ignored)
;;;  OUTPUT:  None
;;;  SIDE EFFECT:  Displays given NODE to the given output stream.

(defun print-node (node str depth)
  ;; Tell the compiler not to issue warning messages about the fact
  ;; that we don't use this parameter!
  (declare (ignore depth))
  
  (let (;; ST -- the state associated with NODE
	(st (node-state node)))
    ;; Display the most recent action (i.e., the one that got us here)
    ;; as well as the current state.
    (format str "NODE:  (action = ~A)~%" (node-action node))
    (format str "         STATE: ~A~%" st)))


;;;   BUILD-PATH
;;; -----------------------------------------------------------
;;;  INPUT:  NODEY, a node somewhere in a search tree
;;;  OUTPUT:  A list of nodes obtained by tracing the PARENT links 
;;;           starting from NODEY all the way back to the root node

(defun build-path (nodey)
  
  ;; BUILD-PATH-ACC is an accumulator-based recursive helper function
  ;;   INPUTS:  NODE, a search node
  ;;            ACC, a list of accumulated nodes
  ;;   OUTPUT:  A list of accumulated nodes from ROOT node to NODEY
  ;; Note the use of LABELS, which is similar to LETREC in Scheme.
  ;; However, unlike LETREC in Scheme, defining functions with LABELS 
  ;; does not require using the LAMBDA special form.  Thus, LABELS is
  ;; similar to DEFUN in this respect.
  
  (labels ((build-path-acc (node acc)
	     ;; Base Case:  NODE is NIL 
	     ;;   This happens when previous function call involved
	     ;;   the root node.  The root node's parent is NIL.
	     ;;   In this case, we are done!
	     (if (not node)
		 ;; So return the accumulator
		 acc
	       ;; Recursive Case:  
	       ;;   Accumulate the current node as we move
	       ;;   to the PARENT...
	       (build-path-acc (node-parent node) (cons node acc)))))

    ;; call recursive helper with accumulator initialized to nil
    (build-path-acc nodey nil)))


;;;  PRINT-RESULTS
;;; -------------------------------------------------------------
;;;  INPUT:  NODEY, either a NODE struct or NIL
;;;  OUTPUT:  Nothing
;;;  SIDE EFFECT:  If NODEY is NIL, then it prints out a failure message.
;;;                Otherwise, it prints out a success message and
;;;                prints out the nodes from the ROOT all the way to 
;;;                NODEY.

(defun print-results (nodey)

  (cond

   ;;  CASE 1:  NODEY is an instance of a search NODE
   ;; --------------------------------------------------
   
   ((node-p nodey)
    ;; We assume this means that NODEY is a goal node
    (format t "SUCCESS!!!~%~%")
    ;; use BUILD-PATH to generate the list of nodes from the 
    ;; root node to NODEY, then display these nodes.
    (format t "*** BEGIN SOLUTION PATH ***~%~%")
    (let ((soln-path (build-path nodey)))
      (format t "~A~%" soln-path)
      (format t "==============================================~%")
      (format t "*** END SOLUTION PATH (length: ~A) ***~%"
	      (1- (length soln-path)))
      (format t "==============================================~%")))
   
   ;;  CASE 2:  NODEY is not a NODE struct...
   ;; -----------------------------------------------------
   
   (t
    ;; We assume that this means failure...
    (format t "~%No solution path was found... ~%")))
  
  ;; In either case, show how many nodes were explored...
  (format t "Overall Node Count: ~A~%" *node-count*))


;;;   CYCLE? 
;;; -------------------------------------------------------------
;;;  INPUTS:  STATE, a problem state
;;;           NODE, a search node
;;;           STATE-EQ-FUNC, a function that determines whether
;;;             two states are equal
;;;  OUTPUT:  T if the given STATE is the same as the state of
;;;    the given NODE or the state of any of NODE's ancestors.
;;;    Uses STATE-EQ-FUNC to determine whether two states are equal.
;;;    Otherwise returns NIL. 
;;;    NOTE:  If NODE is NIL it returns NIL.

(defun cycle? (state node state-eq-func)
  ;create the list of parent nodes
  (let 
      ((nodeList (build-path node)))
    ;returns T if T is found in the list, otherwise NIL
    (member T
            ;checks if each node's state equals the current state
    (maplist #'(lambda (listy)
                 (funcall state-eq-func state (node-state (first listy))))
             nodeList))))

;;;  MAKE-ROOT-NODE 
;;; ---------------------------------------
;;;  INPUT:  PROB, a search problem
;;;  OUTPUT:  A search NODE that will be the ROOT NODE of a
;;;           brand new search tree.

(defun make-root-node (prob)
  ;make a blank node with no parents, no actions done to it yet, and no depth
  (make-node :state (search-problem-init-state prob)))

;;;  EXPAND 
;;; ---------------------------------
;;;  INPUTS:  NODE, a search node
;;;           ACTS, a list of actions
;;;           ST-EQ-FUNC, a function for testing whether two
;;;              states are equal
;;;  OUTPUT:  A list of child nodes obtained by applying the
;;;           given list of actions to NODE's state.  However, it
;;;           does *NOT* include child nodes whose states already
;;;           appear on the path from the root node to NODE.
;;;           (Uses CYCLE? to determine this.)

(defun expand (node acts st-eq-func)
  ;;helper with an accumulator
  (labels ((expand-acc (node acts st-eq-func acc)
             (cond
              ;; if the action list is empty, return the accumulator 
              ((eq acts nil)
               acc)
              ;;otherwise
              (t 
               ;;let the potential new state be the action called on the node
               (let ((potential-state (funcall (first acts) (node-state node))))
                 ;;if the potential new state isn't nil, and it hasn't been seen before
                 (if (and (not (eq potential-state NIL))
                          (not (cycle? potential-state node st-eq-func)))
                     (progn 
                       ;;make a new child node
                      (let ((new-node (make-node :state potential-state
                                                 :parent node 
                                                 :action (first acts)
                                                 :depth (+ 1 (node-depth node)))))
                        ;;add one to the total nodes generated
                        (setf *node-count* (+ *node-count* 1))
                        ;;prints the new node
                        ;(print-node new-node t 0)
                        ;;add the new-node to our accumulator of child nodes,
                        ;; and recursive call for rest of the actions 
                        (expand-acc node (rest acts) st-eq-func (cons new-node acc))))
                   ;;else just recursive call on rest of actions
                   (expand-acc node (rest acts) st-eq-func acc)))))))
    ;;call the helper with all the parameters, plus an empty accumulator
    (expand-acc node acts st-eq-func nil)))                         

;;; --------------------------------------------------------
;;;  GEN-SEARCH -- a *wrapper* function for GEN-SEARCH-GUTS
;;; --------------------------------------------------------
;;;   INPUTS:  PROBLEM, a search problem
;;;            QUEUE-FN, a queuing function
;;;   OUTPUT:  Either NIL, indicating failure
;;;            or a GOAL-NODE indicating success.
;;; --------------------------------------------------------
;;;   NOTE:  This is just a wrapper for GEN-SEARCH-GUTS
;;;          which does most of the work.  The wrapper function
;;;          initializes the global variable *NODE-COUNT* to
;;;          zero, starts and stops the timer, and prints out
;;;          information about the search results.

(defun gen-search (problem queue-fn)

  ;; Set global variable, *NODE-COUNT*, to 0
  (setf *node-count* 0)

  (let* (;; START -- the time at which the search started
	 (start (get-universal-time))
	 ;; SEARCH-RESULT -- the results of performing the search
	 ;;  on the indicated problem using the indicated QUEUE-FN.
	 ;;  Either NIL or a goal NODE.
	 (search-result (gen-search-guts problem queue-fn))
	 (elapsed-time (- (get-universal-time) start)))
    
    (cond
     
     ;; Case 1:  The search returned a node... presumably a GOAL node!
     ((node-p search-result)
      (format t "Hey!  We found a goal node!!~%"))

     ;; Case 2:  The search returned something else... indicating failure
     (t
      (format t "Uh oh... queue empty!~%")))
    
    ;; Report statistics     
    (if (> *node-count* 0)
	(format t "~%Generated ~A nodes in ~A seconds (~A sec/node)~%"
		*node-count* elapsed-time
		(/ elapsed-time *node-count* 1.0)))
    
    ;; Return the search-result 
    search-result
    ))


;;;  GEN-SEARCH-GUTS
;;; ---------------------------------------------------------------
;;;  INPUTS: PROBLEM, a search problem
;;;          QUEUE-FN, a queuing function (used to insert newly
;;;            created nodes into the search queue).  The queuing
;;;            function determines which kind of search we're doing.
;;;  OUTPUT:  Either NIL, indicating failure
;;;           Or a goal NODE, indicating success.
;;; ---------------------------------------------------------------
;;;  This function performs the indicated search problem using
;;;  the search strategy determined by QUEUE-FN.

(defun gen-search-guts (problem queue-fn)
  ;;initial queue with the root node of the problem
  (let ((searchy (list (make-root-node problem))))
    
    ;;local function to do recursion on the queue
    (labels ((gen-guts-acc (searchQ)
               (cond 
                ;; if the queue is empty, return empty 
                ((eq searchQ nil) nil)
                ;;otherwise 
                (t
                 ;;if the current node is the goal node
                 (if (funcall (search-problem-goal-test-func problem) 
                              (node-state (first searchQ)))
                     ;;return the node
                     (first searchQ)
                   ;;else recursive call, with the old nodes being the rest of the queue
                   ;;and the new nodes being the expansion of the current node
                   (gen-guts-acc 
                    (funcall queue-fn (rest searchQ)
                             (expand (first searchQ) 
                                     (search-problem-actions problem)
                                     (search-problem-state-eq-func problem)))))))))
      ;;call local with the initialize queue
      (gen-guts-acc searchy))))


;;;  Helper functions for BREADTH-FIRST and DEPTH-FIRST search
;;; ------------------------------------------------------------
;;;  FRONT-ENQUEUE! and END-ENQUEUE! are DESTRUCTIVE functions!
;;;  That is why their names end with an exclamation point.
;;;  They use NCONC which is a DESTRUCTIVE function (whose name
;;;  for some reason doesn't have a ! at the end).  NCONC outputs
;;;  the concatenation of two input lists, just like APPEND.  However,
;;;  NCONC does it not by recreating the entire first list, but 
;;;  instead by DESTRUCTIVELY modifying the last cons cell in the
;;;  first list to point to the second list!  Neat trick!  Does
;;;  not require creating *any* new cons cells!  But it is destructive.
;;;  Here it is okay, for efficiency reasons, to use NCONC.  Also,
;;;  we only use it to destructively modify the search queue. 
;;;  Since the search queue is only used by GEN-SEARCH-GUTS, that's
;;;  okay.  Of course, if it makes you nervous, you can replace each
;;;  occurrence of NCONC with APPEND... but your code will probably
;;;  run more slowly, albeit more safely!

;;;  FRONT-ENQUEUE!
;;; -------------------------------------------------
;;;  INPUT:  OLD-NODES, a list of old nodes (i.e., old search queue)
;;;          NEW-NODES, a list of new nodes (i.e., new child nodes)
;;;  OUTPUT:  A list containing new-nodes (at the front) and 
;;;           old nodes (at the rear)

(defun front-enqueue! (old-nodes new-nodes)
  ;; NCONC is a destructive version of APPEND (see Paul Graham's book)
  ;; We use it here for efficiency reasons
  (nconc new-nodes old-nodes))

;;;  END-ENQUEUE!
;;; ---------------------------------------------------
;;;  Just like FRONT-ENQUEUE! except that the old-nodes go at the front.

(defun end-enqueue! (old-nodes new-nodes)
  ;; NCONC is a destructive version of APPEND
  ;; We use it here for efficiency reasons
  (nconc old-nodes new-nodes))

;;;  BREADTH-FIRST-SEARCH
;;; ---------------------------------------------------
;;;  INPUT:  PROB, a search problem
;;;  OUTPUT:  The result of doing breadth-first search on the given
;;;             problem:  either NIL or a goal NODE.

(defun breadth-first-search (prob)
  ;; Note that since END-ENQUEUE! is not the first element of the
  ;; list, we need to use #' to ensure that its "function value"
  ;; is passed as input to the GEN-SEARCH function.
  (gen-search prob #'end-enqueue!))

;;;  DEPTH-FIRST-SEARCH
;;; ---------------------------------------------------
;;;  Just like breadth-first-search, except that it uses a different
;;;  queuing function.

(defun depth-first-search (prob)
  (gen-search prob #'front-enqueue!))

;;; TESTS 
;;;  ---------------------------------------------------
;;;

;;; make-root-node

;(print-results (make-root-node (make-vw-problem)))

;;; expand (uncomment print-results line within expand
;;; function to see all the child nodes generated

;(defparameter prob (make-vw-problem))
;(defparameter children (expand (make-root-node prob)
;                       (search-problem-actions prob)
;                      (search-problem-state-eq-func prob)))
;(print-results children)

;;; cycle?
;(format t "cycle?: ~a~%" (cycle? (node-state (second children)) (first children)
;        (search-problem-state-eq-func prob)))

;(format t "cycle?: ~a~%" (cycle? (node-state (first children)) (first children)
;        (search-problem-state-eq-func prob)))
                       

;;;  Call breadth and depth searches on both Vacumm-World and 
;;;  Missionaries/Cannibal domain

;;Depth Search for Vacuum-World
(format t "Depth Search for Vacuum-World")
(format t "==================================================")
(do-vw-depth)
(format t "~%~%")

;;Breadth Search for Vacuum-World
(format t "Breadth Search for Vacuum-World")
(format t "==================================================")
(do-vw-breadth)
(format t "~%~%")

;;Depth Search for Missionary/Cannibal
(format t "Depth Search for Missionary/Cannibal")
(format t "==================================================")
(print-results (depth-first-search (make-mc-problem)))
(format t "~%~%")

;;Breadth Search for Missionary/Cannibal
(format t "Breadth Search for Missionary/Cannibal")
(format t "==================================================")
(print-results (breadth-first-search (make-mc-problem)))
