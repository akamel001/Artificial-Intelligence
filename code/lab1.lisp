;; lab1.lisp
;; KV, Jan 2010;

(defun add1 (x)
  (if (numberp x)
      (+ x 1)
    nil))

;; if statement:
;;
;; (if (... condition ...)
;;     (progn
;;        (... do when true)
;;        (... do when true)
;;         ...
;;        (... do when true)    --> value of if for true branch
;;        )
;;   (progn
;;      (... do when false)
;;      (... do when false)
;;       ...
;;      (... do when false)    --> value of if for false branch
;;      )
;;   )




