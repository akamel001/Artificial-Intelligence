;; bfs2.lisp
;; by Kerstin Voigt, after Paul Graham, pp. 52, 
;; collects paths, returns shortest solution path;

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
  (path-bfs goal (list (list start)) graph)
  )

;; plain bfs;
(defun path-bfs (goal open graph)
  (if (null open)
      nil
    (let* ((path (car open))
	   (node (car path))
	   )
      
      (terpri)
      (format t "OPEN: ~A PATH: ~A  NODE: ~A" open path node)
      (terpri)
      
      (if (eql node goal)
	  (reverse path)
	(path-bfs goal 
		   (append (cdr open)
			   (successor-paths path node graph))
		   graph)
	))
    ))

;; list of successor paths; there are as many successor paths 
;; as neighvors of node in graph; each successor path is the path
;; that results when adding one such neighbors to the current path;

(defun successor-paths (path node graph)
  (mapcar #'(lambda (x)
	      (cons x path))
	  (cdr (assoc node graph))))


;; a graph for testing: a to b and c, b to c, c to d;
;;(setq graph '((a b c) (b c) (c d)))

(setq graph '((a b c) (b c e) (c d) (d e f) (e f) (f g h) (h g)))

