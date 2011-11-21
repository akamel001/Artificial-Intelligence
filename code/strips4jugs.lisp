;;  strips4jugs.lisp
;; KV, March 2010;

;; functions that allow STRIPS planning for water-jug via
;; graphsearch in gs.lisp

;; LOAD gs.lisp first ...

(load "utils.lisp")
(load "jugs34.lisp")

(setq *goal* '((jug4 2)))
;;(setq *start* '((jug3 0) (jug4 0)))
(setq *start* '((jug3 0) (jug4 0)))

;; each state is represented by the list of goals (individual conditions)
;; that are to be satisfied, and by the current version of the start
;; state that preconds must match up with;

(defstruct jugs34
  start
  goals
  pending
  plan)

;; TOP-LEVEL FUNCTION
(defun plan-jugs34(&optional preselect)
  (if preselect
      (preselect-jugops)
    )
  ;;(itgs *init*)
  (itgs *init* nil t)
  )

(defun make-start-state ()
  (make-jugs34 :start *start* :goals (list *goal*)))

(defun ground-all-ops (ops)
  (apply #'append (mapcar #'make-ground-ops ops)))

;; GROUND ALL OPERATORS FOR JUGS34 ... necessary for
;; backwards reasoning through functional terms in add; such
;; (jugs4 (+ x y)) ... grounding works around the problem;

(setq *grnd-jugops* (ground-all-ops *jugops*))

(setq *init* (make-start-state))

;; goal is reached when eiter jugs state j34 has no more goals to
;; satisfy, or all goals listed with the state are satisfied by
;; the start state listed with the state;

(defun GOAL-FCT (j34)
  (state-contained *goal* (jugs34-start j34)))

;; dummy eval-fct; gs will default to bfs;
(defun EVAL-FCT (j34)
  0)

(defun SUCC-FCT(j34)
  (if (null (jugs34-goals j34))
      nil
    (let (ops
	  succs
	  (topgoals (car (jugs34-goals j34))) ;; list of empty
	  )
      (if (null topgoals)
	  (let ((pends (jugs34-pending j34))
		newstart
		)
	    (setq newstart (apply-stripsop (car pends) (jugs34-start j34)))
	
	    ;; if apply-stripsop succeeds ...
	    ;; commit to top pending operator by putting it on path
	    ;; move down into next batch of goals
	    (if (not (equal newstart 'failed-op))
		(succ-fct 
		 (make-jugs34 :start newstart
			      :goals (cdr (jugs34-goals j34))
			      :pending (cdr pends)
			      :plan (append (jugs34-plan j34)
					    (list (get-label (car pends))))
			      ))
	      (progn
		;; leave (car pends) on pending; add preconds of (car pends)
		;; back to front of goals; generate succs for this state;
		(terpri)
		(format t "FAILING OPERATOR ...")
		(terpri)
		(pprint (car pends))
		(terpri)
		(format t "TO STATE")
		(terpri)
		(pprint (jugs34-start j34))
		(terpri)
			
			
		(succ-fct
		 (make-jugs34 :start (jugs34-start j34)
			      :goals (cons (get-preconds-preds (car pends))
					   (cdr (jugs34-goals j34)))
			      :pending pends
			      :plan (jugs34-plan j34)))
		)
	      ))
	(let ((nextgoal (car topgoals))
	      (toprest (cdr topgoals))
	      ;;(restgoals (cdr (jugs34-goals j34)))
	      )
	  ;; there are goals left in topgoals batch;
	  ;; get successors based of first of these;
	  (setq ops (rand-permute (ops-for-goal nextgoal t)))
	  ;;(setq restgoals (cons (cdr topgoals)
	  ;;			(cdr (jugs34-goals j34))))
      
	  (dolist (x ops)
	    (if (goals-sat-in-state (get-preconds-preds x) (jugs34-start j34))
		(progn
		  ;; preconds of x are sat is :start; apply x to :start;
		  ;; add x to path; cont. to work on remaining goals ...
		  (setq succs (cons 
			       (make-jugs34 :start 
					    (apply-stripsop x 
							    (jugs34-start j34))
					    :goals (cons toprest 
							 (cdr (jugs34-goals j34)))
					    :pending (jugs34-pending j34)
					    :plan 
					    (append (jugs34-plan j34)
						    (list (get-label x))))
			       succs))
		  )
	      (progn
		;; there  are preconds of x that are not sat in :start;
		;; all unsat preconds become new goals (topgoals);
		(setq succs (cons
			     (make-jugs34 :start (jugs34-start j34)
					  :goals (cons
						  (rand-permute
						   (unsat-in-state 
						    (get-preconds-preds x)
						    (jugs34-start  j34)))
						  (cons toprest 
							(jugs34-goals j34)))
					  :pending (cons x (jugs34-pending j34))
					  :plan (jugs34-plan j34))
			     succs))
		))
	    )
	  succs))
      ))
  )

(defun STATE-EQUAL (jug1 jug2)
  (if (not (jugs-equal (jugs34-start jug1)
		       (jugs34-start jug2)))
	 nil
    (state-equal-goals jug1 jug2)
    ))

(defun state-equal-goals (jug1 jug2)
  (if (not (= (length (jugs34-goals jug1))
	      (length (jugs34-goals jug2))))
      nil
    (let ((answ t)
	  (goals1 (jugs34-goals jug1))
	  (goals2 (jugs34-goals jug2))
	  )
      (do ((i 0 (1+ i)))
	  ((>= i (length (jugs34-goals jug1))))
	(if (not (jugs-equal (nth i goals1) (nth i goals2)))
	    (progn
	      (setq answ nil)
	      (return))
	  )
	)
      answ)
    ))

;; all predicates in state1 are included in state2;
(defun state-contained (state1 state2)
  (let ((answ t)
	)
    (dolist (x state1)
      (if (not (member x state2 :test #'equal))
	  (progn
	    (setq answ nil)
	    (return)))
      )
    answ))
  
(defun preselect-jugops ()
  (let ((pairs '((0 2) (2 0) (2 4) (3 3) (0 3) (3 0) (0 0)))
	select
	pre 
	)
    (dolist (op *grnd-jugops*)
      (setq pre (get-preconds-preds op))
      (dolist (p pairs)
	(if (and (member (list 'jug3 (first p)) pre :test #'equal)
		 (member (list 'jug4 (second p)) pre :test #'equal))
	    (progn
	      (setq select (append select (list op)))
	      (return)
	      ))
	))
    ;;(setq *grnd-jugops* select)
    (setq *grnd-jugops*
      (rand-permute
       (list (nth 3 select)
	     (nth 4 select)
	     (nth 5 select)
	     (nth 12 select)
	     (nth 14 select)
	     (nth 16 select)))
	     )
    ))

(defun custom-result (j34)
  (jugs34-plan j34))


(defun verify-plan (plan opers start goal)
  (let ((state start)
	theop
	result
	)
    (dolist (op plan)
      (setq theop (find-op op opers))
      (if (null theop)
	  (progn
	    (setq result 'fail)
	    (return)
	    ))
      (setq state (apply-stripsop theop state))
      )
    
    (if (equal result 'fail)
	nil
      (if (state-contained goal state)
	  t
	nil))
    ))

(defun find-op (label ops)
  (let (op
	)
    (dolist (x ops)
      (if (equal label (first x))
	  (progn
	    (setq op x)
	    (return)
	    )))
    op))

