;; ttt.lisp
;; by Kerstin Voigt
;; samples solution to lab6a; simple tic-tac-toe game playing
;; program; no game-playing strategy;

(defun print-ttt (state)
  (terpri)
  (dolist (row state)
          (dolist (col row)
                  (cond ((equal 'X (third col))
                         (format t "X"))
                        ((equal 'O (third col))
                         (format t "O"))
                        (t
                         (format t "-")))
           )
          (terpri)
   )
   (terpri)
   t)

(defun free-squares (state)
  (let (free)
     (dolist (row state)
             (dolist (col row)
                     (if (equal (third col) -1)
                         (setq free (append free (list col)))
                     ))
              )
     free))

(defun place-mark (mark row col state)
  (if (not (equal -1 (third (nth col (nth row state)))))
      (break))
  
  (subst (subst (list row col mark) (list row col -1)
		(nth row state) :test #'equal)
	 (nth row state) state :test #'equal)
  )

(defun placeX (row col state)
  (place-mark 'X row col state))

(defun placeO (row col state)
  (place-mark 'O row col state))

(defun random-select (lst)
  (nth (random (length lst)) lst))

(defun respondO (state)
  (let ((freesqs (free-squares state))
	randfree
	)
    (setq randfree (nth (random (length freesqs)) freesqs))
    (placeO (first randfree) (second randfree) state)
    ))

(defun winsX (state)
   (or (row-of 'X state)
       (col-of 'X state)
       (diag-of 'X state)))

(defun winsO (state)
   (or (row-of 'O state)
       (col-of 'O state)
       (diag-of 'O state)))

(defun row-of (mark state)
   (member mark state
           :test #'(lambda (x y)
                        (and (equal x (third (first y)))
                             (equal x (third (second y)))
                             (equal x (third (third y))))))
    )

(defun col-of (mark state)
   (let ((col1 (col1-of state))
         (col2 (col2-of state))
         (col3 (col3-of state)))
     (member mark (list col1 col2 col3)
	     :test #'(lambda (x y)
		       (and (equal x (third (first y)))
			    (equal x (third (second y)))
			    (equal x (third (third y))))))
    ))

(defun diag-of (mark state)
   (let ((diag1 (diag1-of state))
         (diag2 (diag2-of state)))
     (member mark (list diag1 diag2)
           :test #'(lambda (x y)
                        (and (equal x (third (first y)))
                             (equal x (third (second y)))
                             (equal x (third (third y))))))
    ))

(defun col1-of (state)
   (list (nth 0 (nth 0 state))
         (nth 0 (nth 1 state))
         (nth 0 (nth 2 state))))

(defun col2-of (state)
   (list (nth 1 (nth 0 state))
         (nth 1 (nth 1 state))
         (nth 1 (nth 2 state))))

(defun col3-of (state)
   (list (nth 2 (nth 0 state))
         (nth 2 (nth 1 state))
         (nth 2 (nth 2 state))))

(defun diag1-of (state)
   (list (nth 0 (nth 0 state))
         (nth 1 (nth 1 state))
         (nth 2 (nth 2 state))))

(defun diag2-of (state)
   (list (nth 0 (nth 2 state))
         (nth 1 (nth 1 state))
         (nth 2 (nth 0 state))))

(defun is-free (row col state)
  (equal -1 (third (nth col (nth row state))))
  )

(defun random-seed ()
  (let ((n (mod (get-internal-real-time) 100)))
    (loop 
     (if (= n 0)
	 (return))
     (random 100)
     (setq n (1- n))
     )
    ))

(defun blank-ttt ()
  (let (ttt row)
    (do ((i 0 (1+ i)))
	((> i 2))
      (setq row nil)
      (do ((j 0 (1+ j)))
          ((> j 2))
          (setq row (append row (list (list i j -1))))
	  )
      (setq ttt (append ttt (list row)))
      )
    ttt))
      
(defun play-ttt ()
  (let (state r c (answ 'n))
     (setq state (blank-ttt))
     (random-seed)
     (loop
          (format t "Current State:") (terpri)
          (print-ttt state)

	   (if (winsO state)
	       (progn
		 (format t "YOU LOSE :-(")
		 (terpri)
		 (return nil)))

          (format t "Choose row and column for next X: ")
          (setq r (read))
          (setq c (read))

          (if (not (is-free r c state))
              (break))

          (setq state (placeX r c state))
          (format t "After your move:") (terpri)
          (print-ttt state)

	  (if (winsX state)
	      (progn
		(format t "YOU WIN :-)")
		(terpri)
		(return t)))

	  (if (null (free-squares state))
	     (progn
	       (format t "NOBODY WINS :-|")
	       (terpri)
	       (return nil)
	       ))

          (loop
              (if (equal answ 'n)
                  (progn
                   (format t "Ready for Os move? [y/n]: ")
                   (setq answ (read)))
		(progn
		  (setq answ 'n)
		  (return))
               ))
           (setq state (respondO state))
	   )
      ))

          
          

          

          
    






      
     



      
