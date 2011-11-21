(defun bin-cof (pwr)
  (let ((lst))
	 (loop for z from 0 to pwr do 
			 (setq lst (cons (bin pwr z) lst))) lst)) 

(defun bin (n k)
  (if (or (= k 0) (= k n))
	    '1
	  (+ (bin (1- n) (1- k)) (bin (1- n) k))))
