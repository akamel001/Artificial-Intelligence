;; hw1.lisp


(defun map-pos+ (lst)
  (mapcar #'+ (positions lst) lst))

(defun positions (lst)
  (let ((pos 0))
    (mapcar #'(lambda (x) (setq pos (1+ pos)) (1- pos)) lst)
    ))

(defun rec-pos+ (lst)
  (rec-pos+-aux 0 lst))

(defun rec-pos+-aux (pos lst)
  (if (null lst)
      nil
    (append (list (+ pos (car lst)))
	    
	    (rec-pos+-aux (1+ pos) (cdr lst)))))

(defun it-pos+ (lst)
  (let (plst)
    (do ((i 0 (1+ i)))
	((>= i (length lst)))
      (setq plst (cons (+ i (nth i lst)) plst))
      )
    (reverse plst)))

	 
(defun random-puzz15 ()
  (let(puzz
       tile)
    (loop
      (if (= 16 (length puzz))
	  (return))
      (setq tile (1+ (random 16)))
      
      (if (= tile 16)
	  (setq tile 'B))
      
      (if (not (member tile puzz :test #'eql))
	  (setq puzz (cons tile puzz))
	)
      )
    (make-4-sublists puzz)
    ))

(defun make-4-sublists (lst)
  (if (not (= 16 (length lst)))
      (break)
    (let (lst4
	  sub4
	  (rest lst))
      (loop
	(if (null rest)
	    (return lst4))
	(setq sub4 nil)
	(do ((i 1 (1+ i)))
	    ((> i 4))
	  (setq sub4 (cons (car rest) sub4))
	  (setq rest (cdr rest))
	  )
	(setq lst4 (cons sub4 lst4))
	)
      )))

      
	
	    