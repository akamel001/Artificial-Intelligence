;; fragments, notes, will not load;

(defun it-pos+ (lst)
  (let (plst)
    ;; iterate over lst, for each elm in lst,
    ;; create new elem (elm + pos#), and add
    ;; to plst;

    ;; (a) add new elem to end of plst -- append, use (list ..)
    ;; (b) add new elm to front of plst -- cons
    
    ) ;; close let
  
  plst)) ;; option (a)  (reverse plst ) for option (b)


(defun rec-pos+ (lst)
  (rec-pos+-aux 0 lst))

;; let this function recurse;
(defun rec-pos+-aux (pos lst)
  ;; ...
  )



 