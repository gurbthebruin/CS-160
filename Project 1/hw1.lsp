; Gurbir Arora 
; 105178554
; gurbthebruin@g.ucla.edu 

; This file contains my solutions to HW #1 and includes the functions
; PAD, SUMS, and ANON. PAD  takes in an integer N and computes the
; Nth Padovan number. SUMS also takes in an integer N and computes
; the number of additions required to compute the Nth Padovan
; number. Finally, ANON takes in the argument TREE, which represents
; a tree and outputs a tree with the same structure as TREE, but with 
; 0 in each branch. 


; Question 1: Function PAD takes in a single argument N and returns
; the corresponding Nth Padovan number. 
; This function recursively finds the Nth Padovan number by having
; the stopping condition when N <= 2 and recursively adding 
; PAD(n − 1) + PAD(n − 2) + PAD(n − 3) to get P(n) 

(defun PAD (N)
    (cond 
        ((<= N 2) 1)
        (t (+ (PAD (- N 1)) (PAD (- N 2)) (PAD (- N 3))))
    )  
)

; Question 2: The function SUMS takes in an integer N and outputs
; the number of additions required by the PAD function to compute
; the Nth Padovan number. This was accomplished by using the base
; case when N <= 2 to return 0, as these require no additions,
; then recursively adding the numbers of sums generated by 
; (PAD (- N 1)) (PAD (- N 2)) (PAD (- N 3) and then adding 2 to 
; this result as 2 more additions are required to add up these 3
; numbers. 

(defun SUMS (N)
    (cond
        ((<= N 2) 0)

        (t (+ (SUMS (- N 1)) (SUMS (- N 2)) (SUMS (- N 3)) 2))
    )
)

; Question 3: ANON takes the argument TREE, which represents a tree
; and outputs a tree with the same structure as TREE, but with 0 
; placed in each branch. This is done by having a base case that 
; detects an empty tree and returns and empty list. Then, another
; base case returns a single 0 when a signle node is inputed and
; finally the function recursively constructs the solution list by
; merging the ANON car with the ANON cdr. 

(defun ANON (TREE)
    (cond ((not TREE) '())
    ((atom Tree) '0)
    (t (cons (ANON (car TREE)) (ANON (cdr TREE))))
    )
)

(print (ANON '(((L E) F) T)))

(defun COMP-DUP(L)
    (if (NULL L) L
        (if (equal (car L) (cadr L))
            (COMP-DUP (cons (car L) (cddr L)))
            (cons (car L) (COMP-DUP (cdr L)))
        )
    )
)

(print (COMP-DUP '(A B B F F C C A A A)))
(print (COMP-DUP '(B B C C C D A A B B C)))