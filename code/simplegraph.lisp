;; lab3-graph.lisp

(setq *graph* '((a b c) (b c e) (c d) (d e f) (e f) (f g h) (h g)))

;; goal fct; true if equal to g;

(defun goal-fct (x)
  (eql x 'g))

;; list of successors of node in graph
(defun succ-fct (state)
  (cdr (assoc state  *graph*)))

;; not relevant for his simple domain;

(defun eval-fct (x)
  0)

(defun state-equal (x y)
  (eql x y))

