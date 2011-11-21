;; puzz8.lisp
;; by Kerstin Voigt, Feb 2009

;; represent 8-puzzle state
;; 2 3 8
;; 1 5 7
;; 4   6

;; very hard ...
(setq prob1 '((2 3 8) (1 5 7) (4 B 6)))

(setq prob2 '((2 3 8) (1 6 4) (7 B 5)))

(setq prob3 '((1 2 3) (7 8 5) (B 6 4)))

(setq prob4 '((1 2 3) (8 B 5) (7 6 4)))

(setq prob5 '((B 2 3) (1 8 4) (7 6 5)))

(setq prob6 '((2 3 B) (1 8 4) (7 6 5)))

(setq prob7 '((2 3 4) (1 8 5) (7 6 B)))

(defun succ-fct (puzz)
  (remove nil
	  (list (move-blank-up puzz)
		(move-blank-down puzz)
		(move-blank-left puzz)
		(move-blank-right puzz))
	  ))


;; the hard-coded goal state:
;; 1 2 3
;; 8   4
;; 7 6 5

(defun goal-fct (puzz)
  (equal puzz '((1 2 3) (8 B 4) (7 6 5))))

;; needed for graphsearch in gs.lisp
;; true if both puzzle states p1 and p2 are equal;
;; puzzle states are assumed to be in the form of prob1 and prob 2 above;

(defun state-equal (p1 p2)
  (equal p1 p2))

;; needed for graphsearch in gs.lisp
;; heuristic evaluation function; like h function in f = g + h;
;; number of tiles out of place;

;; change name to eval-fct-alt, to "activate" the other eval-fct ...

(defun eval-fct (puzz)
  (let ((goal '((1 2 3) (8 B 4) (7 6 5)))
	(count 0)
	)
    (do ((i 0 (1+ i)))
	((> i 2))
      (do ((j 0 (1+ j)))
	  ((> j 2))
	(if (not (equal (nth j (nth i puzz))
			(nth j (nth i goal))))
	    (setq count (1+ count))
	  )
	))
    count))

;; another admissible eval-fct: sum of "Manhattan distances" = sum of horizontal 
;; andi vertical displacements of each tile relative to the tile's location in
;; the goal state;

(defun eval-fct-alt (puzz)
  (let (loc gloc
	(sum 0)
	)
    (dolist (tile '(1 2 3 4 5 6 7 8))
      (setq loc (locate-tile tile puzz))
      (setq gloc (locate-tile tile '((1 2 3) (8 B 4) (7 6 5))))
      (setq sum (+ sum (+ (abs (- (first loc) (first gloc)))
			  (abs (- (second loc) (second gloc))))))
      )
    sum))
	    
;; blank left

(defun move-blank-left (puzz)
  (let* ((bloc (locate-blank puzz))
	 (r (first bloc))
	 (c (second bloc))
	 (row (nth r puzz))
	 newrow
	 )
    (if (= 0 c)
	nil
      (progn
	(if (= c 1)
	    (setq newrow (list 'B (first row) (third row)))
	  (if (= c 2)
	      (setq newrow (list (first row) 'B (second row)))
	    (break)))
	
	
	(if (= r 0)
	    (list newrow (nth 1 puzz) (nth 2 puzz))
	  (if (= r 1)
	      (list (nth 0 puzz) newrow (nth 2 puzz))
	    (if (= r 2)
		(list (nth 0 puzz) (nth 1 puzz) newrow)
	      (break)
	      )))
	))
    ))

;; (B _ _) -> (_ B _)
;; (_ B _) -> (_ _ B)
;; (_ _ B) -> nil

(defun move-blank-right (puzz)
  (let* ((bloc (locate-blank puzz))
	 (r (first bloc))
	 (c (second bloc))
	 (row (nth r puzz))
	 newrow
	 )
    (if (= 2 c)
	nil
      (progn
	(if (= c 0)
	    (setq newrow (list  (second row) 'B (third row)))
	  (if (= c 1)
	      (setq newrow (list (first row) (third row) 'B))
	    (break)))
	
	
	(if (= r 0)
	    (list newrow (nth 1 puzz) (nth 2 puzz))
	  (if (= r 1)
	      (list (nth 0 puzz) newrow (nth 2 puzz))
	    (if (= r 2)
		(list (nth 0 puzz) (nth 1 puzz) newrow)
	      (break)
	      )))
	))
    ))

;; move up;

(defun move-blank-up (puzz)
  (let* ((bloc (locate-blank puzz))
	 (r (first bloc))
	 (c (second bloc))
	 (row0 (nth 0 puzz))
	 (row1 (nth 1 puzz))
	 (row2 (nth 2 puzz))
	 newrow0 newrow1 newrow2 numtile
	 newpuzz
	 )
    (if (= 0 r)
	nil
      (if (= r 1)
	  (progn
	    ;; get number tile from row 0;
	    (setq numtile (nth c row0))
	    ;; in row 0, subst numtile with B;
	    (setq newrow0 (subst 'B numtile row0))
	    ;; in row1, subst B with numtile;
	    (setq newrow1 (subst numtile 'B row1))
	    (setq newpuzz (list newrow0 newrow1 row2))
	    )
	(if (= r 2)
	    (progn
	      ;; get number tile from row1;
	      (setq numtile (nth c row1))
	      ;; in row1, subst numtile with B;
	      (setq newrow1 (subst 'B numtile row1))
	      ;; in row2, subst B with numtile;
	      (setq newrow2 (subst numtile 'B row2))
	      (setq newpuzz (list row0 newrow1 newrow2))
	      )
	  (break)
	  )))
    newpuzz))

;; move down;

(defun move-blank-down (puzz)
  (let* ((bloc (locate-blank puzz))
	 (r (first bloc))
	 (c (second bloc))
	 (row0 (nth 0 puzz))
	 (row1 (nth 1 puzz))
	 (row2 (nth 2 puzz))
	 newrow0 newrow1 newrow2 numtile
	 newpuzz
	 )
    (if (= 2 r)
	nil
      (if (= r 0)
	  (progn
	    ;; get number tile from row 1;
	    (setq numtile (nth c row1))
	    ;; in row 1, subst numtile with B;
	    (setq newrow1 (subst 'B numtile row1))
	    ;; in row0, subst B with numtile;
	    (setq newrow0 (subst numtile 'B row0))
	    (setq newpuzz (list newrow0 newrow1 row2))
	    )
	(if (= r 1)
	    (progn
	      ;; get number tile from row2;
	      (setq numtile (nth c row2))
	      ;; in row2, subst numtile with B;
	      (setq newrow2 (subst 'B numtile row2))
	      ;; in row1, subst B with numtile;
	      (setq newrow1 (subst numtile 'B row1))
	      (setq newpuzz (list row0 newrow1 newrow2))
	      )
	  (break)
	  )))
    newpuzz))

;; computes row and col of blank;

(defun locate-blank (puzz)
  (locate-tile 'B puzz))

;; computes row and col of tile in puzzle puzz;

(defun locate-tile (tile puzz)
  (let ((r -1)
	(c -1)
	row)
    (do ((i 0 (1+ i)))
	((>= i 3))
      (setq row (nth i puzz))
      (do ((j 0 (1+ j)))
	  ((>= j 3))
	(if (equal tile (nth j row))
	    (progn
	      (setq r i)
	      (setq c j)
	      (return))  
	  ))
      (if (> r -1)
	  (return))
      )
    (list r c)
    ))

(defun show-puzz (puzz)
  (terpri)
  (format t "~A" (nth 0 puzz))
  (terpri)
  (format t "~A" (nth 1 puzz))
  (terpri)
  (format t "~A" (nth 2 puzz))
  (terpri)
  t)


(defun make-puzz8 (k)
  (let ((puzz '((1 2 3) (8 B 4) (7 6 5)))
	try
	move)
    (do ((i 1 (1+ i)))
	((> i k))
      (setq move (nth (random 4) '(left right up down)))
      
      (cond ((equal move 'left)
	     (setq try  (move-blank-left puzz)))
	    ((equal move 'right)
	     (setq try  (move-blank-right puzz)))
	    ((equal move 'up)
	     (setq try  (move-blank-up puzz)))
	    ((equal move 'down)
	     (setq try  (move-blank-down puzz)))
	    (t
	     (break)))
      
      (if try
	  (setq puzz try)
	)
      )
    puzz))

;; for hillclimb.lisp

(defun bettereq-eval (x y)
  (<= x y))

(defun better-eval (x y)
  (< x y))


	     
	     
	     
    
