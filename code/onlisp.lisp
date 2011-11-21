;; onlisp.lisp
;; KV, Feb 2010; some code to clarify some lisp matters;

;; OBJECTIVE: write a function with a list as parameter that
;; returns a list of elements, each + 1 or corresponding 
;; element in original;

;; destructive; changes parameter lst to result of adding
;; 1 to each element; 
;; this style of programming is not to be overdone in Lisp; 
;; Lisp was designed to be a functional programming languages, i.e.,
;; computational results are produced as values returned from
;; functions; this function produces results as SIDE-EFFECT; this
;; is NOT Lisp-style programming;

(defun add1a (lst)
  (do ((i 0 (1+ i)))
      ((>= i (length lst)))
    (setf (nth i lst) (1+ (nth i lst)))
    )
  lst) ;;... uncomment to try
  ;;t)

;; evaluates to a list of elements that are each 1+ of the elements
;; in the parameter list; does not destroy original lst; returned
;; value is in new memory; Lisp-style!

(defun add1b (lst)
  (let (newlst)
    (do ((i 0 (1+ i)))
	((>= i (length lst)))
      (setq newlst (append newlst (list (1+ (nth i lst)))))
      )
    newlst))

;; alternative to add1b; does not destroy orignal lst;
;; returned value is in new memory; Lisp-style;

(defun add1c (lst)
  (mapcar #'1+ lst))

;; this function does NOT produce a list of elements in x increased by 1;
;; (it just prints out such a list); it's returned value is nil which
;; is the value of (dolist ...); NOT GOOD, unless printing is all you
;; want;

(defun add1d (lst)
  (dolist (x lst)
    (format t "~A " (1+ x))
    )
  )


  

 
