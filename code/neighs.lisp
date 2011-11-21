;; neighs.lisp
;; by KV; list of pairs of coordinates for hor and ver adjacent cells in 
;; a grid

(defun all-neighsij (imax jmax)
  (let ((rowijs (neighsij imax jmax))
	)
    (append rowijs (swapijs rowijs))
    ))
	    
(defun neighsij (imax jmax)
  (neighsij-aux1 1 imax jmax))

(defun neighsij-aux1 (i imax jmax)
  (if (> i imax)
      nil
    (append (neighsij-aux2 i 1 jmax)
	    (neighsij-aux1 (1+ i) imax jmax))
    ))

(defun neighsij-aux2 (i j jmax)
  (if (>= j jmax)
      nil
    (cons (list (list i j) (list i (1+ j)))
	  (neighsij-aux2 i (1+ j) jmax))
    ))

(defun swapijs (ijs)
  (mapcar #'(lambda (x) (list (swap (first x)) (swap (second x ))))
	  ijs))

(defun swap (x)
  (if (= 2 (length x))
      (list (second x) (first x))
    (break)))

