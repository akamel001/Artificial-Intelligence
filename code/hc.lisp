;;Abdelrahman Kamel
;;Anthony De La Loza
;;Feb 6 2010
;;Howework 2 
;;hc.lisp
;;HillCLIMB using Nilsson's alg pg. 190

(load "twocolor.lisp")

;;Example problem, same as in Nillson pg. 191
(print "Symbol x has been loaded with Nillson's sample node on pg. 191")
(setq x '((R R R) (B R R) (B B B)))
(show-twocol x)

(defun hc (&optional start) 
  (let (curr_nod
		  curr_val
		  successors
		  v
		  m_max)

	 ;;Step 1
	 ;seting curr_node to a random node
	 (if (null start)
		  (progn 
	  		 (setf curr_node (twocol-problem))
			 (format t "Random node (n): ~A " curr_node))
		 (progn
			(setf curr_node start)
			(format t "Starting node (n): ~A " curr_node)))

	 ;curr_val = eval-fct(curr_node)
	 (setf curr_val (eval-fct curr_node))
	 
	 (format t " and v= ~A ~%" curr_val)
	 (loop 
	 ;;Step 2a
	 ;successors = succ-fct(curr_node)
	 (setf successors (succ-fct curr_node))
	 
	 ;;Step 2b
	 (dolist (successor successors)
		(setf v (cons (eval-fct successor) v)))
	 (setf v (reverse v))

	 ;;Step 3
	 ;;find highest eval-fct value 
	 ;;and get its corrisponding node
	 (setq h-val (apply 'min v))
	 (dolist (successor successors)
		(if (= (eval-fct successor) h-val)
				(setf m_max successor)))
	 ;;Step 3a
	 (if (goal-fct m_max)
		  (progn
	 		 (format t "    --> Successor node: ~A  and v= ~A ~%" m_max h-val)
			 (return-from hc (format t "Success! ~% ~A" m_max))))
	
 	 ;;Step 3b
	 (if (>= (eval-fct m_max) curr_val)
		  (return-from hc (format t "Failure! ~% Local Maximum: ~A" curr_node)))
	
	 ;;Step 4
	 (setf curr_node m_max)
	 (setf curr_val (eval-fct m_max))
	 (format t "    --> Successor node: ~A  and v= ~A ~%" curr_node curr_val)
	 );;loop
  ))
