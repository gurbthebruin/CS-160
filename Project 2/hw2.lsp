;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

; Gurbir Arora
; 105178554 
; gurbthebruin@g.ucla.edu


;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; BFS takes in the argument FRINGE, which is a list and returns a top-level list of how the nodes of a tree would 
; be traversed in a left-to-right BFS. The base case seeks to detect if the current node is null and if so, 
; it returns NIL. Then, the function checks if the current node is an atom and moves through the entire list in this 
; fashion. If the current node is a list, then check the next node until the base case breaks the chain
(defun BFS (FRINGE)
    (cond ((null FRINGE) ())
    ((atom (car FRINGE)) (cons (car FRINGE) (BFS (cdr FRINGE))))
    (t (BFS (append (cdr FRINGE) (car FRINGE))))
    )
)


;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (equal S '(T T T T))
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).

(defun NEXT-STATE (S A)
; when homer goes alone, we must make sure that the baby is safe from the dog and the poison
    (cond ((equal A 'h)
        (cond ((and (equal (first S) (second S)) (or (equal (second S) (third S)) (equal (second S) (fourth S)))(not (equal (second S) (third S))) ) ()) 
                (T (list (cons (NOT (car S)) (cdr S))))
                ((not (equal (first S) (second S))) (list (list (not (first S)) (second S) (third S) (fourth S))))
				(T NIL)
                ))
; when homer and the baby go, we must make sure that they are on the same side to begin with
            ((equal 'b A) 
			    (if (equal (first S) (second S))
				    (list (list (not (first S)) (not (second S)) (third S) (fourth S)))
				    NIL
			    )
		    )
; when it's homer and the dog, we must make sure that the poison and the baby won't end up alone 
            ((equal A 'd)
                (cond 
                    ((not (equal (car S) (third S))) ())((equal (second S) (fourth S)) ()) (T (list (list (NOT (car S)) (second S) (NOT (third S)) (fourth S))))))
; when it's homer and the poison, then we must check if they're on the same side and that the baby isn't left alone with dog 
            ((equal A 'p)
                (cond ((not (equal (first S) (fourth S))) ())((equal (second S) (third S)) ())
                    (T (list (list (not (first S)) (second S) (third S) (not (fourth S)))))))
            (T NIL)
    )
)
; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))    
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
    (cond ((null STATES) NIL)
        ((equal (car STATES) S) t)
            (t (ON-PATH S (cdr STATES)))
    )
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
    (cond 
        ((null STATES) NIL)
        ((DFS (car STATES) PATH) (DFS (car STATES) PATH))
        ((null (DFS (car STATES) PATH)) (MULT-DFS (cdr STATES) PATH))
    )
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
    (cond
        ((FINAL-STATE S) (append PATH (list S)))
        ((ON-PATH S PATH) NIL)
        (t (MULT-DFS (SUCC-FN S) (append PATH (list S))))
    )
)

;Question 1: 
; (print(BFS '(ROOT)))
; (print(BFS '((((L E) F) T))))
; (print (BFS '((R (I (G (H T)))))))
; (print(BFS '(((A (B)) (D) C))))
; (print(BFS '((T (H R E) E))))
; (print (BFS '((A ((C ((E) D)) B)))))


;(ROOT) 
;(T F L E) 
;(R I G H T) 
;(C A D B) 
;(T E H R E) 
;(A B C D E) 

;Question 2: 
;(print (DFS '(NIL NIL NIL NIL) NIL))

;((NIL NIL NIL NIL) (T T NIL NIL) (NIL T NIL NIL) (T T T NIL) (NIL NIL T NIL) (T NIL T T) (NIL NIL T T) (T T T T)) 