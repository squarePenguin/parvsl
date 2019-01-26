(setq !*echo t)
(setq !*backtrace t)
(global '(!*b!* !*c!*))

(global '(count))
(setq count 0)

% (fluid '(g h j k))

(global '(mymutex))
(setq mymutex (mutex))

(setq !*b!* '
(prog (g j)
  (setq g 20)
  (setq j 0)
g002
  (cond ((igeq j g) (return(progn))))
  (mutexlock mymutex)
  (setq count (iadd1 count))
  (print (list '!A j g))
  (mutexunlock mymutex)
  (setq j (iadd1 j))
  (go g002)))

(setq !*c!* '
(prog (h k)
  (setq h 100)
  (setq k 0)
  (setq count 0)
g002
  (cond ((igeq k h) (return count)))
  (mutexlock mymutex)
  (print (list '!B k h))
  (mutexunlock mymutex)
  (thread !*b!*)
  (setq k (iadd1 k))
  (go g002)))


(progn
  (eval !*c!*)
  (dotimes (zz 70000))
  (print (list 'final 'count count))
  (stop))

