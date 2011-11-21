;; practise.lisp

;; print x n times;

(defun print-n (x n)
    (do ((i 1 (1+ i)))
      ((> i n))
    (print x)
    )
  'done)

(defun print-k (x k)
  (let (i)
    (setq i 1)
    (loop
      (if (> i k)
	  (return))
      (print x)
      ;(setq i (1+ i))
      )
    )
  'done)

(defun print-rec (x n)
  (if (= n 0)
      'done
    (progn
      (print x)
      (print-rec x (1- n)))
    ))



  

 