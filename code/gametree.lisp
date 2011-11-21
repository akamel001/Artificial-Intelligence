;; gametree.lisp
;; Kv, March 2010

;; option 1
 
 (setq *gt1* '((a b c d) (b e f g) (c h i) (d j k)
               (e  l m) (f n o) (g  p q) (h r s)
               (i t u) (j v w) (k x y)))
 
(defstruct gamenode
  (id nil)
  (player nil)
  (evalfct nil)
  (parent nil)
  )

(setq *a* (make-gamenode :id 'a :player 'maxi))
(setq *b* (make-gamenode :id 'b :player 'mini))
(setq *c* (make-gamenode :id 'c :player 'mini))
(setq *d* (make-gamenode :id 'd :player 'mini))
(setq *e* (make-gamenode :id 'e :player 'maxi))
(setq *f* (make-gamenode :id 'f :player 'maxi))
(setq *g* (make-gamenode :id 'g :player 'maxi))
(setq *h* (make-gamenode :id 'h :player 'maxi))
(setq *i* (make-gamenode :id 'i :player 'maxi))
(setq *j* (make-gamenode :id 'j :player 'maxi))
(setq *k* (make-gamenode :id 'k :player 'maxi))
(setq *l* (make-gamenode :id 'l :player 'mini :evalfct 2))
(setq *m* (make-gamenode :id 'm :player 'mini :evalfct 3))
(setq *n* (make-gamenode :id 'n :player 'mini :evalfct 8))
(setq *o* (make-gamenode :id 'o :player 'mini :evalfct 5))
(setq *p* (make-gamenode :id 'p :player 'mini :evalfct 7))
(setq *q* (make-gamenode :id 'q :player 'mini :evalfct 6))
(setq *r* (make-gamenode :id 'r :player 'mini :evalfct 0))
(setq *s* (make-gamenode :id 's :player 'mini :evalfct 1))
(setq *t* (make-gamenode :id 't :player 'mini :evalfct 5))
(setq *u* (make-gamenode :id 'u :player 'mini :evalfct 2))
(setq *v* (make-gamenode :id 'v :player 'mini :evalfct 8))
(setq *w* (make-gamenode :id 'w :player 'mini :evalfct 4))
(setq *x* (make-gamenode :id 'x :player 'mini :evalfct 10))
(setq *y* (make-gamenode :id 'y :player 'mini :evalfct 2)) 


(setf (gamenode-parent *b*) *a*)
(setf (gamenode-parent *c*) *a*)
(setf (gamenode-parent *d*) *a*) 
(setf (gamenode-parent *e*) *b*)
(setf (gamenode-parent *f*) *b*)
(setf (gamenode-parent *g*) *b*)
(setf (gamenode-parent *h*) *c*)
(setf (gamenode-parent *i*) *c*)
(setf (gamenode-parent *j*) *d*)
(setf (gamenode-parent *k*) *d*)
(setf (gamenode-parent *l*) *e*)
(setf (gamenode-parent *m*) *e*)
(setf (gamenode-parent *n*) *f*)
(setf (gamenode-parent *o*) *f*)
(setf (gamenode-parent *p*) *g*)
(setf (gamenode-parent *q*) *g*)
(setf (gamenode-parent *r*) *h*)
(setf (gamenode-parent *s*) *h*)
(setf (gamenode-parent *t*) *i*)
(setf (gamenode-parent *u*) *i*)
(setf (gamenode-parent *v*) *j*)
(setf (gamenode-parent *w*) *j*)
(setf (gamenode-parent *x*) *k*)
(setf (gamenode-parent *y*) *k*)


;; prtial game tree
(setq *gt2* '((a *b* *c* *d*) (b *e* *f* *g*) 
										(c *h* *i*) (d *j* *k*)
										(e *l* *m*) (f *n* *o*)
										(g *p* *q*) (h *r* *s*)
										(i *t* *u*) (j *v* *w*)
										(k *x* *y*)))

(defun successors (gnode)
  (let ((id (gamenode-id gnode))
	)
    (mapcar #'eval (cdr (assoc id *gt2*)))
	 ))

(defparameter *lst* (list))
(defparameter *lst2* (list))
(defparameter *lst3* 0)
(defparameter *nodes* (list))

(defun best-move (node)
  	(setq succ (successors node))
	
	(if (null succ)
		(return-from best-move (gamenode-evalfct node)))
	 
	(dolist (sub-node succ)  
	  		(setq *lst* (cons (best-move sub-node) *lst*)))
	
	(setq *lst* (delete nil *lst*))

	(if (eq (gamenode-player node) 'maxi)
	  		(progn
				(setq *lst2* (append *lst2* (list (apply 'max *lst*))))
				(setq *nodes* (append *nodes* (list node)))
				))
	
	(if (eq (gamenode-player node) 'mini)
			(progn	
			  (setq *lst3* (apply 'min *lst2*))
			  (setq counter '0)
			  (dolist (n *lst2*)
				 (if (eq *lst3* n)
					(format t "Best node is: ~A ~%" (nth counter *nodes*)))
				 (setq counter (1+ counter)))
			  	
			  )
	)
	;(pprint *lst*)
	;(pprint *lst2*)
	;(pprint *nodes*)
	;(pprint *lst3*)
	(setq *lst* (list))
	)	
