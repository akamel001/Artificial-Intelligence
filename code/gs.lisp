;; gs.lisp
;; by Kerstin Voigt, version Feb 2009; updated Jan 2010;

;; implementation of Graph Search after Nilsson, AI: A New Synthesis;

(load "utils.lisp")

(defparameter *limit* 1000)

;; define structure node

(defstruct node
  (id -1)
  (state nil)
  (depth 0)
  (h-eval 0)
  (parent nil))


;; iterative graphsearch implementation

(defun itgs (start &optional bfs-or-dfs)
  (let ((OPEN (list (make-node :id 0 :state start)))
	CLOSED
	node 
	succs
	(steps 0)
	(nextid 1))
    (declare (special nextid))
    (loop

      (if (> steps *limit*)
	  (return 'over-limit))
      
      (terpri)
      (format t "~D. OPEN:" steps)(terpri)
      (mapcar #'(lambda(x)
		  (format t "~A [~D+~D, ~D]" (node-state x)
			  (node-depth x)
			  (node-h-eval x)
			  (node-parent x))
		  (terpri))
	      OPEN)
      ;;(format t "... OPEN cont. ...")
      (terpri)
      
      (if (null OPEN)
	  (return nil)
	)
      
      (setq node (car OPEN))
      (setq OPEN (cdr OPEN))
      (setq CLOSED (cons node CLOSED))

      ;; if goal-fct is true, done;
      (if (goal-fct (node-state node))
	  (progn
	    (terpri)(terpri)
	    (format t "Solution Found!")
	    (terpri)
	    (pprint node)
	    (terpri)(terpri)
	    (return (extract-path node CLOSED))
	    ))

      ;; succ-fct provides list of successors;
      ;; new-nodes creates a list of nodes with
      ;; successors as states, depth as 1+ node's depth,
      ;; and node as parent;

      (setq succs (new-nodes
		   (rand-permute (succ-fct (node-state node)))
		   (node-depth node)
		   (node-id node)
		   ))
      
      ;; place each successor node on open, provided it is not
      ;; yet contained on open or closed; 

      (dolist (newnode succs)
	(if (and (not (member newnode OPEN :test #'node-equal))
		 (not (member newnode CLOSED :test #'node-equal)))
	    (setq OPEN (append OPEN
			       (list newnode)))
	  )
	)
      
      ;; sort OPEN by eval fct f = depth + h-eval;
      
      (if (null bfs-or-dfs)
	  (setq OPEN (sort OPEN #'(lambda (x y)
				    (<= (+ (node-depth x)
					   (node-h-eval x))
					(+ (node-depth y)
					   (node-h-eval y))))
			   ))
	(if (equal bfs-or-dfs 'bfs)
	    (setq OPEN (sort OPEN #'(lambda (x y)
				      (<= (node-depth x)
					  (node-depth y)))))
	  (if (equal bfs-or-dfs 'dfs)
	      (setq OPEN (sort OPEN #'(lambda (x y)
					(>= (node-depth x)
					    (node-depth y)))))
	    (break))))
      
      (setq steps (1+ steps))
      )
    ))


;; create new nodes for the sucessor states in succs

(defun new-nodes (succs dpth parid)
  (mapcar #'(lambda (x y)
	      (make-node :id y
			 :state x
			 :depth (1+ dpth)
			 :h-eval (eval-fct x)
			 :parent parid))
	  succs (get-nextids (length succs)))
  )

;; two nodes are defined to be equal if their node-states are equal

(defun node-equal (n1 n2)
  (state-equal (node-state n1) (node-state n2)))

;; generate the k next  id numbers; trick is "special" variable nextid
;; as declared in function gs;

(defun get-nextids (k)
  (let (ids)
    (dotimes (i k)
      (setq ids (append ids (list nextid)))
      (setq nextid (1+ nextid))
      )
    ids))

;; given a node (with .node-parent), extract path to node from
;; starting state; 

(defun extract-path (node closed)
  (let ((path (list node))
	(curr node)
	parid
	)
    (loop
      (if (null (node-parent curr))
	  (return (mapcar #'node-state path))
	)
      (setq parid (node-parent curr))
      (setq curr (car (member parid closed 
			      :test #'(lambda (x y)
					(equal x (node-id y))))))
      (setq path (cons curr path))
      )
    ))

   

