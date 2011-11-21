;; puzz8.lisp
;; by Kerstin Voigt, Jan 2010

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
;; puzzle states are assumed to be in thgoal-fcte form of prob1 and prob 2 above;

(defun state-equal (p1 p2)
  (equal p1 p2))

;; needed for graphsearch in gs.lisp
;; heuristic evaluation function; like h function in f = g + h;
;; number of tiles out of place;

;; change name to eval-fct-alt, to "activate" the other eval-fct ...

(defun eval-fct (puzz)
  ; fill in
  0)
	    
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

	; assemble new row in row with blank
	(if (= c 1)
	    (setq newrow (list 'B (first row) (third row)))
	  (if (= c 2)
	      (setq newrow (list (first row) 'B (second row)))
	    (break)))
	
	; assemble new puzzle state row by row
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

	; assemble new row in row with blank
	(if (= c 0)
	    (setq newrow (list (second row) 'B (third row)))
	  (if (= c 1)
	      (setq newrow (list (first row) (third row) 'B))
	    (break)))
	
	; assemble new puzzle state row by row
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
  )

;; move down;

(defun move-blank-down (puzz)
  )

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


	     
	     
    
