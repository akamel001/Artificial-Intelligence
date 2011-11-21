;; occurrences.lisp
;; soln to Graham, problem 3. p. 56;

;; (occurrences '(a b a c a b c a d c)) 
;;         --> ((a . 4) (c .  3) (b . 2) (d . 1))

(defun occurrences (lst)
  (let (occs ; assoc list of occurrences (elm . count)
	count
	)
    (dolist (x lst)
      (if (null (assoc x occs))
	  (setq occs (cons (cons x 1) occs))
	(progn
	  (setq count (cdr (assoc x occs)))
	  (setf (cdr (assoc x occs)) (1+ count))
	  )))
    (sort occs #'greater-count)
    ))

;; x and y are dotted pairs; (_ . i) (_ . j); 
;; true if i >= j;

(defun greater-count (x y)
  (>= (cdr x) (cdr y)))



