;; twocolor.lisp
;; KV, Feb 2010
;; two-coloring problem according to Nilsson, chapter 11

;; state is a 3x3 grid with tiles colored red or blue (r,b) ;

(load "neighs.lisp")

;; some sample problems

(setq prob1 '((R R B) (R B B) (B B R)))

(setq prob2 '((R R B) (R B R) (B B B)))

(setq prob3 '((R R R) (B B B) (R B R)))

(setq prob4 '((R B R) (B B R) (R B R)))

(setq prob5 '((R R B) (B R R) (B R B)))

;; generates a random two-color problem

(defun twocol-problem ()
  (let (prob
	row
	)
    (do ((i 1 (1+ i)))
	((> i 3))
      (setq row nil)
      (do ((j 1 (1+ j)))
	  ((> j 3))
	(setq row (cons (red-or-blue) row))
	)
      (setq prob (cons row prob))
      )
    prob))

(defun red-or-blue ()
  (let ((r (random 2))
	)
    (if (= 0 r)
	'r
      'b)
    ))

(defun show-twocol (state)
  (terpri)
  (do ((i 0 (1+ i)))
      ((>= i 3))
    (do ((j 0 (1+ j)))
	((>= j 3))
      (format t "~A " (nth j (nth i state)))
      )
    (terpri)
    )
  (terpri)
  t)

;; number of neighboring tiles with same color ; 
;; want to reduce it			
;; smaller eval-fct is better;

(defun eval-fct (state)
  (let ((val 0)
	i1 j1 i2 j2
	)
    (dolist (ij (all-neighsij 3 3))
      (setq i1 (1- (first (first ij))))
      (setq j1 (1- (second (first ij))))
      (setq i2 (1- (first (second ij))))
      (setq j2 (1- (second (second ij))))
      
     (if (equal (nth j1 (nth i1 state))
		(nth j2 (nth i2 state)))
	 (setq val (1+ val))
       )
      )
    val))

;; each tile produces a next successor state, but flipping its current
;; color between red and blue		;

(defun succ-fct (state)
  (let (succs)
    (do ((i 0 (1+ i)))
	((>= i 3))
      (do ((j 0 (1+ j)))
	  ((>= j 3))
	(setq succs (cons (flip-color i j state)
			  succs))
	))
    succs))

(defun flip-color (i j state)
  (let ((flip (mapcar #'copy-list state))
	)
    (do ((k 0 (1+ k)))
	((>= k 3))
      (if (= i k)
	  (if (eql 'r (nth j (nth i flip)))
	      (setf (nth j (nth i flip)) 'b)
	    (setf (nth j (nth i flip)) 'r)
	    )
	)
      )
    flip))

(defun goal-fct (state)
  (= 0 (eval-fct state)))

(defun state-equal (state1 state2)
  (equal state1 state2))

;; smaller eval-fct value is better

(defun bettereq-eval (x y)
  (<= x y))

(defun better-eval (x y)
  (< x y))

      
