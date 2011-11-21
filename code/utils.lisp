;; File: utils.lsp
;; By: Kerstin Voigt
;; Purpose: collection of utility functions


;; defining NEW SORT FUNCTION SSORT;

(defun ssort (lst fct)
  (rec-ssort lst fct))

;; random permutation of a list ... NOW WORKS for lists with 
;; repeated elements;

(defun rand-permute (lst)
  (let ((n (length lst))
	i
	perm temp)

    (setq i 1)
    (setq temp (mapcar #'(lambda (x)
			   (setq i (1+ i))
			   (list (1- i) x)) lst))
    (loop
     (if (= n (length perm))
	 (return perm))
     
     (setq i (random n))

     (if (not (member (nth i temp) perm :test #'equal))
	 (setq perm (cons (nth i temp) perm))
       )
     )

    (mapcar #'cadr perm)
    ))

;; untrace all currently traced functions;

(defun untrace-all ()
  (eval (cons 'untrace (trace))))

;; return list with all atoms removed ... hack to fix mysterious problem
;; in inf-val.lsp

(defun fix (lst)
  (cond ((null lst) nil)
	((atom lst) 
;;	 (terpri)
;;	 (format t "Argument ~A is not list" lst)
;;	 (terpri)
;;	 (break)
	 nil)
	((atom (car lst))
	 (fix (cdr lst)))
	(t
	 (cons (car lst)
	       (fix (cdr lst))))
	))

(defun remove* (x lst)
  (cond ((null lst)
	 nil)
	((atom lst)
	 lst)
	((equal x (car lst))
	 (cdr lst))
	(t
	 (cons (car lst) (remove* x (cdr lst)))
	 )
	))

	 
;; computes x mod y
(defun modulo (x y)
  (if (or (not (integerp x))
	  (not (integerp y)))
      (break)
    (- x (* (/ x y) y))
  ))

	       
	       
;; enumerates all permutations of a list

(defun all-perms (lst)
  (cond ((or (null lst)
	     (= 1 (length lst)))
	 (list lst))
	((atom lst)
	 (break))
	(t
	 (apply #'append
	 (mapcar #'(lambda (l) (insert (car lst) l)) 
		 (all-perms (cdr lst)))))
	))

;; computes list of lists that result from inserting x before and after
;; each element in lst;

(defun insert (x lst)
  (cond ((null lst)
	 (list (list x)))
	(t
	 (let (xlst
	       (i 0))
	   (loop
	    (if (> i (length lst))
		(return t))
	    
	    (setq xlst (append xlst
			       (list (insert-at-i i x lst))
			       ))
	    (setq i (1+ i))
	    )
	   xlst))
	)
)

;; inserts element x at ith position of list lst;

(defun insert-at-i (i x lst)
  (append (nthcar i lst)
	  (list x)
	  (nthcdr i lst)))

;; returns the list of the first k elements of list lst;

(defun nthcar (k lst)
  (let (x
	(i 0))
    (loop
     (if (= i k)
	 (return)
       )
     (setq x (append x (list (nth i lst))))
     (setq i (1+ i))
     )
    x))

(defun random-seed ()
  (let (n)
    (terpri)
    (format t "Enter any integer: ")
    (setq n (read))
    (terpri)
    (do ((i 1 (1+ i)))
	((= i n))
	(random 2)
	)
    t))


(defun pick-random (lst)
  (if (not (listp lst))
      (break))
  (nth (random (length lst)) lst))


