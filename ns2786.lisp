(defstruct q
"define the struct of queue, a-star-enqueue is my enqueue function, manhattan is my heuristic function(including the cost), fn=gn+hn"
	   (enqueue #'a-star-enqueue)
	   (key #'manhattan)
	   (last nil)
	   (elements nil))

(defun q-emptyp(q)
	   (= (length (q-elements q)) 0))

(defun q-remove (q)
  "Removes and returns the element at the front of the queue."
  (if (listp (q-elements q))
      (pop (q-elements q))             
    (heap-pop (q-elements q) (q-key q))))


(defun q-insert (q items)
	   (funcall (q-enqueue q) q items)
	   q)


(defun make-heap (&optional(size 100000))
	     (make-array size :fill-pointer 0 :adjustable t))

(defun heap-val (heap i key) (funcall key (elt heap i)))

(defun heap-parent (i) (floor (1- i) 2))

(defun heap-left(i) (+ 1 i i))

(defun heap-right(i) (+ 2 i i))

(defun heap-leafp (heap i) (> i (1- (floor (length heap) 2))))

(defun heap-find-pos (heap i val key)
	   (cond ((or (zerop i) (< (heap-val heap (heap-parent i) key) val))
		  i)
		 (t (setf (elt heap i) (elt heap (heap-parent i)))
		    (heap-find-pos heap (heap-parent i) val key))
		 ))
	       

(defun heap-insert(heap item key)
	   (vector-push-extend nil heap)
	   (setf (elt heap (heap-find-pos heap (1- (length heap)) (funcall key item) key))
		 item)
	   )

(defun heapify (heap i key)
	   (unless (heap-leafp heap i)
	     (let ((left-index (heap-left i))
		   (right-index (heap-right i)))
	       (let ((smaller-index
		      (if (and (< right-index (length heap))
			       (< (heap-val heap right-index key)
				  (heap-val heap left-index key)))
			  right-index
			  left-index)))
		 (when (> (heap-val heap i key)
			  (heap-val heap smaller-index key))
		   (rotatef (elt heap i)
			    (elt heap smaller-index))
		   (heapify heap smaller-index key))))
	     ))

(defun heap-pop (heap key)
	   (let ((min (elt heap 0)))
	     (setf (elt heap 0) (elt heap (1- (length heap))))
	     (decf (fill-pointer heap))
	     (heapify heap 0 key)
	     min))



(defun make-node(&key state (parent nil) (action nil) (gn 0) (hn 0) (depth 0))
	   (list state parent action gn hn depth))

(defun node-state (node) (car node))
(defun node-parent (node) (cadr node))
(defun node-action (node) (caddr node))
(defun node-gn (node) (cadddr node))
(defun node-fn (node) (car (cddddr node)))
(defun node-depth (node) (cadr (cddddr node)))

(defstruct node 
"define the stuct of the node"
 state (parent nil) (action nil) (gn 0) (hn 0) (depth 0))

(defun a-star-enqueue(q newnode)
"define the enqueue function"
	   (when (null (q-elements q))
	     (setf (q-elements q)(make-heap)))
	   (mapc (lambda (item)
		   (heap-insert (q-elements q) item (q-key q)))
		 newnode)
	   newnode)

(defun action-sequence (node &optional (actions nil))
	   (if (node-parent node)
	       (action-sequence (node-parent node)
				(cons (node-action node) actions))
	       actions))

(defvar *nodes-expanded*)

(defun expand(successor node)
"define the expand function and push the childen enqueue. "
	   (let ((triples (funcall successor (node-state node))))
	     (mapcar (lambda (action-state-cost)
		       (let ((action (car action-state-cost))
			     (state (cadr action-state-cost))
			     (cost (caddr action-state-cost)))
			 (make-node :state state
				    :parent node
				    :action action
				    :gn (+ (node-gn node) cost)
				    :depth (1+ (node-depth node)))
		       ))
	     triples)
	     ))

(defun 8-puzzle (initial-state key &key(enqueue #'a-star-enqueue) (successor #'successor) (goalp #'goalp) (samep #'samep))
           "8-Puzzle function to get the action sequence, total moves and the number of expanded nodes"
	   (setf *nodes-expanded* 0)
	   (let ((fringe (make-q :enqueue enqueue :key key)))
	     (q-insert fringe (list (make-node :state initial-state)))
	     (setf result (graph-search fringe nil successor goalp samep))
	     (list result (length result)  *nodes-expanded* )
)
)



(defun graph-search (fringe closed successor goalp samep)
"using graph-search to realize the 8-puzzle problem"
	   (unless (q-emptyp fringe)
	     (let((node (q-remove fringe)))
	     (cond ((funcall goalp (node-state node))
		    (action-sequence node))
		   ((member (node-state node) closed
			    :test #'samep :key #'node-state)
		    (graph-search fringe closed successor goalp samep))
		   (t (let ((successors (expand successor node)))
			(setf *nodes-expanded* (length closed))
			(graph-search (q-insert fringe successors)
				      (cons node closed)
				      successor goalp samep)
			
			))
		 ))))

(defun goalp(state)
"The goal state"
	   (equal state '(0 1 2 3 4 5 6 7 8))
	   )

(defun samep (state1 state2)
"To justify whether two states are the same"
	   (cond((and (null state1)(null state2)) T)
		((eql (car state1) (car state2)) (samep (cdr state1)(cdr state2)))
		(t nil)
		)
	   )

(defun index (el state &optional(pos 0))
"find the index of an element"
	   (if (null state) nil)
	   (if (eq el (car state))
	       pos
	       (index el (cdr state) (1+ pos))))

(defun x-position(index)
"find the x position in the 8 puzzle by the index"
	    (mod index 3)
	   )

(defun y-position(index)
	   (setf quotient (floor index 3))
	   quotient
	   )


(defun swap (blankindex otherindex state)
"change the value of two index"
	   (setf temp (copy-list state))
	    (setf otherval (elt temp otherindex))
	    (setf (elt temp blankindex) otherval)
	    (setf (elt temp otherindex) 0)
	    temp)

(defun newstate (state action)
"generate the new state by the current state and the action"	   
	     (setf blankindex (index '0 state))
	     
	     (cond((equal action "right")(swap blankindex (+ 1 blankindex) state))
		  ((equal action "left")(swap blankindex (- blankindex 1) state))
		  ((equal action "up")(swap blankindex (- blankindex 3) state))
		  ((equal action "down")(swap blankindex(+ 3 blankindex) state))
		  (t nil)
	     
	     )
	   )

(defun successor (state)
"generate the legal successors as the list of {(action, state, cost) ....}"
	    (setf blankindex (index 0 state))
	    (cond((eql blankindex 0) (list (list "right" (newstate state "right") 1) (list "down" (newstate state "down") 1)))
		 ((eql blankindex 1) (list (list "down" (newstate state "down") 1) (list "left" (newstate state "left") 1) (list "right" (newstate state "right")1)))
		 ((eql blankindex 2) (list (list "down" (newstate state "down") 1) (list "left" (newstate state "left") 1) ))
		 ((eql blankindex 3) (list (list "up" (newstate state "up") 1) (list "down" (newstate state "down") 1) (list "right" (newstate state "right") 1)))
		 ((eql blankindex 4) (list (list "up" (newstate state "up") 1) (list "down" (newstate state "down") 1)(list "left" (newstate state "left") 1)  (list "right" (newstate state "right") 1)))
		 ((eql blankindex 5) (list (list "up" (newstate state "up") 1) (list "down" (newstate state "down") 1) (list "left" (newstate state "left") 1)))
		 ((eql blankindex 6) (list (list "up" (newstate state "up") 1) (list "right" (newstate state "right")1)))
		 ((eql blankindex 7) (list (list "up" (newstate state "up") 1)(list "right" (newstate state "right") 1)(list "left" (newstate state "left") 1)))
		 ((eql blankindex 8) (list (list "up" (newstate state "up") 1) (list "left" (newstate state "left") 1)))
		 )
	   )

(defun misplaced (item &optional(count 0))
"misplaced heuristic function,but including the cost,actually evaluation function with misplace heuristic function, fn=hn+gn"
	   (setf gn (node-gn item))
	   (setf state (node-state item))
	   (do ((index 0 (+ 1 index))) 
	       ((eql 9 index) (+ count gn))
	     (unless (or (eql index 0) (eql index (elt state index)))
	       
	       (setf count (+ 1 count))
	       )))

(defun manhattan (item &optional(count 0))
"manhattan heuristic function,but including the cost,so it is actually evaluation function with manhattan heuristic function,fn=hn+gn"
	   (setf state (node-state item))
	   (setf cost (node-gn item))
	   (do((index 0 (+ 1 index)))
	      ((eql 9 index) (+ count cost))
	     (cond ((eql 1 (elt state index)) 
		(setf count   (+ count (+ (abs(- (x-position index) 1)) (abs(- (y-position index) 0))))))
		   ((eql 2 (elt state index)) 
		   (setf count   (+ count  (+ (abs(- (x-position index) 2)) (abs(- (y-position index) 0))))))
		   ((eql 3 (elt state index)) 
		   (setf count   (+ count   (+ (abs(- (x-position index) 0)) (abs(- (y-position index) 1))))))
		   ((eql 4 (elt state index)) 
		   (setf count   (+ count  (+ (abs(- (x-position index) 1)) (abs(- (y-position index) 1))))))
		   ((eql 5 (elt state index)) 
		   (setf count   (+ count  (+ (abs(- (x-position index) 2)) (abs(- (y-position index) 1))))))
		   ((eql 6 (elt state index)) 
		   (setf count   (+ count  (+ (abs(- (x-position index) 0)) (abs(- (y-position index) 2))))))
		   ((eql 7 (elt state index)) 
		   (setf count   (+ count  (+ (abs(- (x-position index) 1)) (abs(- (y-position index) 2))))))
		   ((eql 8 (elt state index)) 
		   (setf count   (+ count  (+ (abs(- (x-position index) 2)) (abs(- (y-position index) 2))))))
		   ))
	   )



(defun random-move-blank (state)
"move the blank tile randomly"
	   (setf blankindex (index 0 state))
	   (cond ((eql blankindex 0) (let ((r0 (random 2)))
				      (cond((eql r0 0) (setf state (newstate state "right")))
					   ((eql r0 1) (setf state (newstate state "down")))
					   )))
		 
		 ((eql blankindex 1)(let ((r1 (random 3)))
				     (cond((eql r1 0) (setf state (newstate state "right")))
					   ((eql r1 1) (setf state (newstate state "down")))
					   ((eql r1 2) (setf state (newstate state "left")))
					   ))) 
		 ((eql blankindex 2)(let ((r2 (random 2)))
				     (cond((eql r2 0) (setf state (newstate state "left")))
					   ((eql r2 1) (setf state (newstate state "down")))
					  
					   ))) 
		  ((eql blankindex 3)(let ((r3 (random 3)))
				     (cond((eql r3 0) (setf state (newstate state "right")))
					   ((eql r3 1) (setf state (newstate state "down")))
					   ((eql r3 2) (setf state (newstate state "up")))
					   )))
		  ((eql blankindex 4)(let ((r4 (random 4)))
				     (cond((eql r4 0) (setf state (newstate state "right")))
					   ((eql r4 1) (setf state (newstate state "down")))
					   ((eql r4 2) (setf state (newstate state "up")))
					   ((eql r4 3) (setf state (newstate state "left")))
					   )))
		  ((eql blankindex 5)(let ((r5 (random 3)))
				     (cond((eql r5 0) (setf state (newstate state "left")))
					   ((eql r5 1) (setf state (newstate state "down")))
					   ((eql r5 2) (setf state (newstate state "up")))
					   )))
		   ((eql blankindex 6)(let ((r6 (random 2)))
				     (cond((eql r6 0) (setf state (newstate state "right")))
					   ((eql r6 1) (setf state (newstate state "up")))
					  
					   ))) 
		    ((eql blankindex 7)(let ((r7 (random 3)))
				     (cond((eql r7 0) (setf state (newstate state "right")))
					   ((eql r7 1) (setf state (newstate state "left")))
					   ((eql r7 2) (setf state (newstate state "up")))
					   )))
		     ((eql blankindex 8)(let ((r8 (random 2)))
				     (cond((eql r8 0) (setf state (newstate state "left")))
					   ((eql r8 1) (setf state (newstate state "up")))
					  
					   )))
		     ))

(defun random-initial-state(&optional (num-moves 100)(state '(0 1 2 3 4 5 6 7 8)))
"generate the solvable initial state by 100 random moves"
	   (loop for i from 1 to num-moves do 
		(setf state (random-move-blank state)))
	   state)

(defun random-case (&optional (num-moves 100)(state '(0 1 2 3 4 5 6 7 8)))
"generate 5 solvable initial states"
	   (setf a (random-initial-state num-moves state))
	   (setf b (random-initial-state num-moves state))
	   (setf c (random-initial-state num-moves state))
	   (setf d (random-initial-state num-moves state))
	   (setf f (random-initial-state num-moves state))
	   (list a b c d f)
	   )
 (defun extracredit (item)
"bonus: modify the original manhattan heuristic function to solve the linear conflict. Given two tiles in their goal row. but reversed in position. additional vertical moves can be added to Manhattan distance. "
	   (setf stat (node-state item))
	   (setf cost (node-gn item))	
	   (let ((distance 0))
	   (do ((x 0 (+ 1 x))
		(s stat (cdr s)))
	       ((> x 8) (+ distance cost))
	     (if (> (car s) 0)
		 (setq distance (+ distance  (+ (abs (- (elt '(1 1 1 2 2 2 3 3 3) (car s)) (elt '(1 1 1 2 2 2 3 3 3) x)))
			      (abs (- (elt '(1 2 3 1 2 3 1 2 3) (car s)) (elt '(1 2 3 1 2 3 1 2 3) x)))))))
	     (if (and (> (car s) 0)
		      (eql x (elt stat (car s)))
		      (not (eql (car s) x)))
		 (if (and (eql (mod x 3) 0) 
			  (or (eql (car s) (+ 1 x))
			      (eql (car s) (+ 2 x))
			      (eql (mod (car s) 3) 0)))
		     (setf distance (+ 1 distance))
		     (if (and (eql (mod x 3) 1)
			      (or (eql (car s) (- x 1))
				  (eql (car s) (+ x 1))
				  (eql (mod (car s) 3) 1)))
			 (setf distance (+ 1 distance))
			 (if (and (eql (mod x 3) 2)
				  (or (eql (car s) (- x 1))
				      (eql (car s) (- x 2))
				      (eql (mod (car s) 3) 2)))
			     (setf distance (+ 1 distance))))))
	   )))