% Sample of use of "prog"

(de firstn (n)
   (prog (a)
      (printc "Start of the prog block")
   top
      (cond
         ((zerop n) (return a)))
      (setq a (cons (list n (times n n)) a))
      (setq n (sub1 n))
      (go top)))

(firstn 7)

(stop 0)
