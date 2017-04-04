;;;FUNCTION NAME: rm-nonmode
;;;DESCRIPTION: function that returns true if the count of occurrence
;;;is the same as mode. Used for filter.
(defun rm-nonmode (modenum)
  (lambda (L) (= modenum (first (rest L)))))


;;;FUNCTION NAME: model
;;;DESCRIPTION: Gets all the elements that are the mode alongside their counts.
(defun model (L)
  (filter (rm-nonmode (maximum (occr L) 0)) (occr L)))

;;; FUNCTION NAME: my-reduce 
;;; DESCRIPTION: Takes a starting element and applies the given function to that starting element and the first element of the list.
;;; Then uses the result as the start for the next step with the second element of the list and so on returning the built value. 
(defun my-reduce (func building L) (if (null L) 
			       building
			       (my-reduce func (funcall func building (first L)) (rest L))))

;;; FUNCTION NAME: insfront 
;;; DESCRIPTION: Creates a function that will insert the given element into a list 
(defun insfront(n) 
  (lambda (L) (cons n L)))

;;; FUNCTION NAME: inseach 
;;; DESCRIPTION:   inserts an element into every possible location in a list
(defun inseach(n L) (if (null L) (list (list n)) (cons (cons n L) (mapcar (insfront (first L)) (inseach n (rest L))))))

;;; FUNCTION NAME: into 
;;; DESCRIPTION: puts the elements of one list into another, much like append 
(defun into(L L2) (if (null L2)
		       L
		       (cons (first L2) (into L (rest L2)))))

;;; FUNCTION NAME: permu 
;;; DESCRIPTION:  Generates all permutations of the identity list 1 to the given variable n 
(defun permu(n) (if (= n 1) 
		    (list (list 1))
		    (my-reduce #'into nil (mapcar (lambda (L) (inseach n L)) (permu (- n 1))))))
;;;Sample Runs
(dispnth '(1 (2 3) 4 5) 2)
(delnth '(1 2 (3 4) 5) 3)
(lastele '(1 (2 3) 4 5))
(lastele2 '(1 (2 3) 4 5))
(remv2 'a '(a (b) a c))
(remv2 '(a b) '(a b (a b) c))
(remvdub2 '(a b a c b a))
(remvdub2 '(a b (a) c b (a)))
(lists '(1 (2 3) (4) 5))
(min2 '(1 3 2 5 4))
(inde 1 '(1 2 1 1 2 2 1))
(nele '(1 2) 3)
(istrin 21)
(model '(1 3 5 2 3 5))
(permu 3)

;;;Output Of Sample Runs
(2 3)
(1 2 5)
5
(2 3)
((B) C)
(A B C)
(A B C)
(A B (A) C)
((2 3) (4))
2
(1 3 4 7)
(1 1 1 2 2 2)
T
((5 2) (3 2))
((3 1 2) (1 3 2) (1 2 3) (3 2 1) (2 3 1) (2 1 3))
