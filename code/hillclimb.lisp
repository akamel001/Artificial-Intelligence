;; hillclimb.lisp
;; by Kerstin Voigt, Feb 2010

;; implementation of Hillclimb after Nilsson, AI: A New Synthesis;

;; a modified version of gs.lisp ...

(load "utils.lisp")

(defparameter *limit* 1000)

;; define structure node

(defstruct node
  (id -1)
  (state nil)
  (depth 0)
  (h-eval 0)
  (parent nil))

;; hillclimb derived from graphsearch

(defun hillclimb (start)
  (let ((FRONT (make-node :state start
			  :h-eval (eval-fct start)))
	node 
	best
	succs
	(steps 0)
	)
    (loop

      (if (> steps *limit*)
	  (return 'over-limit))
      
      (terpri)
      (format t "~D. FRONT:" steps)(terpri)
      (format t "~A [~D]" (node-state FRONT)
			  (node-h-eval FRONT))
      (terpri)
      
      ;; if goal-fct is true, done;
      (if (goal-fct (node-state front))
	  (progn
	    (terpri)(terpri)
	    (format t "Solution Found!")
	    (terpri)
	    (terpri)(terpri)
	    (return t)
	    ))

      ;; succ-fct provides list of successors;
      ;; new-nodes creates a list of nodes with
      ;; successors as states, depth as 1+ node's depth,
      ;; and node as parent;

      (setq best (car (sort (succ-fct (node-state front))
			     #'(lambda (x y)
				 (bettereq-eval (eval-fct x)
					      (eval-fct y))))))
      
      (if (better-eval (eval-fct best) (node-h-eval front))
	  (setq front (make-node :state best 
				 :h-eval (eval-fct best)))
	(progn
	  (terpri)
	  (format t "Local max/min at state ~A  [~D]" 
		  (node-state front)
		  (node-h-eval front))
	  (terpri)
	  (format t "Best successor: ~A  [~D]" best (eval-fct best))
	  (terpri)(terpri)
	  (return nil)))
      
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

   

