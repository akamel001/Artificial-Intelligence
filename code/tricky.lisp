;; tricky.lisp
;; Tricky Fingers domain; KV, Feb 2010

(setq *mapping* '((1 . R) (2 . R) (3 . R) (4 . R) 
		(5 . B) (6 . B) (7 . B) (8 . B)
		(9 . G) (10 . G) (11 . G) (12 . G)
		(13 . Y) (14 . Y) (15 . Y) (16 . Y)))

(defun random16 ()
  (let (perm r)
    (loop
      (if (= 16 (length perm))
	  (return perm))
      (setq r (1+ (random 16)))
      (if (not (member r perm :test #'eql))
	  (setq perm (cons r perm))
	)
      )
    perm))

(defun random-tricky ()
  (let ((perm16 (random16))
	colperm
	tricky)
    (setq colperm (mapcar #'(lambda (r) (cdr (assoc r *mapping*)))
			  perm16))
    
