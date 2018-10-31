(prog (i)
  (setq i 0)
a (cond ((igreaterp i 1000) (return nil)))
  (setq i (iadd1 i))
  (go a))

((lambda (x y) (print "hi") (reclaim) (print "ho") (cons x y)) "string" 66.7)
(stop 0)
