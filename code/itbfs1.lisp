;; itbfs1.lisp
;; by Kerstin Voigt, after Paul Graham, pp. 52, 
;; but simpler plain breadth-first search, paths;
;; ITERATIVE VERSION;

;; assume the following:
;; (1) graph is a list of sublists (n1 n2 n3 ... nk) where
;; n2 ... nk are neighbours of n1; graph is DIRECTED
;; and ACYCLIC (for now);
;; (2) start is the start node of search
;; (3) goal is the goal node to be reached;
;; (4) open is a list of nodes whose successors have
;;     not yet been "tried" yet;

;; search tries to move from start to goal
;; with plain breadth-first search;

(defun gsearch (start goal graph)
  (plain-bfs goal (list start) graph)
  )

;; plain bfs;
(defun plain-bfs (goal open graph)
  (let (node
	succs
	)
    (loop 
      (if (null open)
	  (return 'FAIL)
	)
      (setq node (car open))
      (if (eql node goal)
	  (return goal)
	)
      (setq succs (successors node graph))
      (setq open (append (cdr open) succs))
      )
    ))



;; list of successors of node in graph
(defun successors (node graph)
  (cdr (assoc node graph)))

;; a graph for testing: a to b and c, b to c, c to d;
;;(setq graph '((a b c) (b c) (c d)))

(setq graph '((a b c) (b c e) (c d) (d e f) (e f) (f g h) (h g)))

