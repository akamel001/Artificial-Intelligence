;; jugs34.lisp
;; KV, March 2010; water-jug operatrors as STRIPS ops

;; more complete version of jugs.lisp

(setq *jugops*
  (list
   '("jug4-to-jug3-a"
     (pre ((jug3 x) (jug4 y) #'(lambda (x y)
				 (and (< x 3) (> y 0) (<= y (- 3 x))))))
     (add ((jug3 (+ x y)) (jug4 0)))
     (del ((jug3 x) (jug4 y)))
     )
   '("jug4-to-jug3-b"
     (pre ((jug3 x) (jug4 y) #'(lambda (x y)
				 (and (< x 3) (> y 0)
				      (> y (- 3 x))))))
     (add ((jug3 3) (jug4 (- y (- 3 x)))))
     (del ((jug3 x) (jug4 y)))
     )
   '("jug3-to-jug4-a"
     (pre ((jug3 x) (jug4 y) #'(lambda (x y)
				 (and (> x 0) (< y 4) (<= x (- 4 y))))))
     (add ((jug3 0) (jug4 (+ y x))))
     (del ((jug3 x) (jug4 y)))
     )
   '("jug3-to-jug4-b"
     (pre ((jug3 x) (jug4 y) #'(lambda (x y)
				 (and (> x 0) (< y 4) (> x (- 4 y))))))
     (add ((jug3 (- x (- 4  y))) (jug4 4)))
     (del ((jug3 x) (jug4 y)))
     )
   '("emtpy-jug3"
     (pre ((jug3 x) (jug4 y) #'(lambda (x y) (> x 0))))
     (add ((jug3 0)))
     (del ((jug3 x)))
     )
   '("empty-jug4"
     (pre ((jug3 x) (jug4 y) #'(lambda (x y) (> y 0))))
     (add ((jug4 0)))
     (del ((jug4 y)))
     )
   '("fill-jug3"
     (pre ((jug3 x) (jug4 y) #'(lambda (x y) (< x 3))))
     (add ((jug3 3)))
     (del ((jug3 x)))
     )
   '("fill-jug4"
     (pre ((jug3 x) (jug4 y) #'(lambda (x y) (< y 4))))
     (add ((jug4 4)))
     (del ((jug4 y)))
     )
   ))

;; apply operator op to state; produce new state;
(defun apply-stripsop (op state)
  (let ((applic (apply-pre (get-preconds op) state))
	binds)
    (if (equal applic 'fail)
	'failed-op
      (if (equal 'succeed applic)
	  (apply-dels (get-dels op)
		      (apply-adds (get-adds op) state))
	(progn
	  (setq binds applic)
	  (apply-dels (get-dels op)
		      (apply-adds (get-adds op) state binds) binds)
	  )
	))
    ))

;; find list of operatators with goal on adds;
(defun ops-for-goal (goal &optional ground)
  (let (ops
	)
    (if ground
	(setq allops *grnd-jugops*)
      (setq allops *jugops*))
      
    (dolist (x allops)
      (if (member goal (get-adds x) :test #'equal)
	  (setq ops (cons x ops))
	))
    ops))

;; test equality of states;
(defun jugs-equal (state1 state2)
  (if (not (= (length state1)
	      (length state2)))
      nil
    (let ((answ t)
	  )
      (dolist (x state1)
	(if (not (member x state2 :test #'equal))
	    (progn
	      (setq answ nil)
	      (return)))
	)
      answ)
    ))

;; are all goals in list goals satisfied in state?
(defun goals-sat-in-state (goals state)
  (let ((answ t)
	)
    (dolist (x goals)
      (if (not (member x state :test #'equal))
	  (progn
	    (setq answ nil)
	    (return))
	))
    answ))

;; subset of goals not sat in state;
(defun unsat-in-state (goals state)
  (let (unsat
	)
    (dolist (x goals)
      (if (not (member x state :test #'equal))
	  (setq unsat (cons x unsat))
	))
    unsat))

(defun get-label (op)
  (first op))

(defun get-preconds (op)
  (second (second op)))

(defun get-preconds-test (op)
  (eval (car (reverse (get-preconds op)))))

(defun get-preconds-preds (op)
  (reverse (cdr (reverse (get-preconds op)))))

(defun get-adds (op)
  (second (third op)))

(defun get-dels (op)
  (second (fourth op)))

;; apply preconditions to state; either fail, or succeed with possible
;; list of variable bindings ("unification lite");

(defun apply-pre (pre state)
  (let (
	(currstate state)
	preds
	test
	statep
	binds
	fail
	)
    (setq preds (reverse (cdr (reverse pre))))
    (setq test (eval (car (reverse pre))))
    
    ;; deals with operators that have ground preconditions;
    ;;(if (all-ground-pred preds)
    ;;	(if (apply-pre-ground pre state)
    ;;	    'succeed
    ;;'fail)
    (if t
      (progn
        ;; predicates are all not ground;
	(dolist (p preds)
	  (if (setq statep (member p currstate
				   :test #'(lambda (x y)
					     (equal (car x) (car y)))))
	      (progn
		(setq statep (car statep))
		(setq binds (append binds
				    (mapcar #'cons (cdr p) (cdr statep))))
		;; remove pairs that are not var/value bindings, or redundant;
		(setq binds (remove-if #'(lambda (x)
					   (equal (car x) (cdr x))) binds))
		;;(numberp (car x)))) binds))
		(setq currstate (apply-bindings binds currstate))
		)
	    (progn
	      (setq fail t)
	      (return)
	      ))
	  )
	;; 3 cases: (1) fail is t -> preconds not satisfied in state;
	;; (2) fail is nil, binds is nil; preconds satisfied in state; ground case;
	;; (3) fail is nil, non-nil binds; preconds are satisfied with binds;
	(if fail
	    'fail
	  (if (null binds)
	      'succeed
	    (progn
	      ;;(terpri)
	      ;;(format t "bindings: ~A" binds)
	      ;;(terpri)
	      (if (and (valid-bindings binds)
		       (apply test (mapcar #'cdr binds)))
		  binds
		'fail)))
	  )
	))
    ))

;; valid binding cannot have number as key;

(defun valid-bindings (binds)
  (let ((answ t)
	)
    (dolist (b binds)
      (if (numberp (car b))
	  (progn
	    (setq answ nil)
	    (return)))
      )
    answ))

    
(defun apply-pre-ground (pre state)
  (let ((answ t)
	)
    (dolist (x pre)
      (if (not (member x state :test #'equal))
	  (progn
	    (setq answ nil)
	    (return)))
      )
    answ))

(defun all-ground-pred (preds)
  (let ((answ t))
    (dolist(p preds)
      (if (not (ground-pred p))
	  (progn
	    (setq answ nil)
	    (return))))
    answ))

;; predicate is ground; here: all arguments are numeric;
(defun ground-pred (pred)
  (let ((answ t))
    (dolist (x (cdr pred))
      (if (not (numberp x))
	  (progn
	    (setq answ nil)
	    (return))))
    answ))

;; apply variables to a list of predicates;

(defun apply-bindings (binds predlst)
  (let (newpreds
	)
    (dolist (pred predlst)
      (dolist (bnd binds)
	(if (member (car bnd) pred :test #'contained)
	    (setq pred (subst (cdr bnd) (car bnd) pred :test #'equal))
	  )
	)
      (setq newpreds (append newpreds (list pred)))
      )
    newpreds))

(defun contained (x y)
  (cond ((not (atom x)) 
	 nil)
	((atom y)
	 (equal x y))
	((null y)
	 nil)
	(t
	 (or (contained x (car y))
	     (contained x (cdr y))
	     ))
	))
	    
;; apply adds to state; generic adds may need to be instantiated 
;; according to list of bindings;

(defun apply-adds (adds state &optional binds)
  (let (appadds 
	newadds
	)
    (if binds
	(setq appadds (apply-bindings binds adds))
      (setq appadds adds)
      )
    (dolist (ad appadds)
      (dolist (x (cdr ad))
	(if (listp x)
	    (setq ad (subst (eval x) x ad :test #'equal)))
	)
      (setq newadds (append newadds (list ad)))
      )
    (append state newadds)
    ))

(defun apply-dels (dels state &optional binds)
  (let (appdels
	newdels
	)
    (if binds
	(setq appdels (apply-bindings binds dels))
      (setq appdels dels)
      )
    (dolist (del appdels)
      (dolist (x (cdr del))
	(if (listp x)
	    (setq del (subst (eval x) x del :test #'equal)))
	)
      (setq newdels (append newdels (list del)))
      )
    (dolist (del newdels)
      (setq state (remove del state :test #'equal))
      )
    state))

(setq *domains* '((x . (0 1 2 3)) (y . (0 1 2 3 4))))

;; op is element in *jugsops*
(defun make-ground-ops (op)
  (let (gops
	tryop
	test 
	pre
	)
    ;;(terpri)
    ;;(format t "Grounding operator ~A" (get-label op))
    ;;(terpri)
	    
    (dolist (xi '(0 1 2 3))
      (dolist (yi '(0 1 2 3 4))
	(setq test (get-preconds-test op))
	(if (apply test (list xi yi))
	    (progn
	      (setq tryop
		(subst yi 'y (subst xi 'x op :test #'equal)
		       :test #'equal))

	      ;;(pprint tryop) 
	      ;;(terpri)
	      (setq gops (append gops (list tryop)))
	      ))
	))
    (post-process gops)
    ))

(defun post-process (gops)
  (let (newops
	)
    (dolist (op gops)
      (setq newops 
	(append newops (list (process-gop op))))
      )
    newops))

(defun process-gop (op)
  (let ((label (get-label op))
	(pre (get-preconds-preds op))
	(add (get-adds op))
	(del (get-dels op))
	newadd newdel
	)
    (dolist (x add)
      (if (listp (second x))
	  (setq newadd (append newadd
			       (list (list (first x)
					   (eval (second x))))))
	(setq newadd (append newadd (list x))))
      )
    (dolist (x del)
      (if (listp (second x))
	  (setq newdel (append newdel
			       (list (list (first x)
					   (eval (second x))))))
	(setq newdel (append newdel (list x))))
      )
    ;;(pprint
     
    (list label (list 'pre (append pre '(#'(lambda () t))))
	  (list 'add newadd)
	  (list 'del newdel))
    ;;)
    ;;(break)
    
    ))


	

		
	       
    


	    
			    
  
     
     
     
     
				  