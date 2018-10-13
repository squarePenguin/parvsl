% 3n+1 experiment. Look up "Collatz sequence" for more information

(de threenplusone (n count)
   (prin n)
   (princ " ")
   (cond
     ((or (onep n) (minusp count)) (terpri))
     ((zerop (remainder n 2))
        (threenplusone (quotient n 2) (sub1 count)))
     (t (threenplusone (add1 (times 3 n)) (sub1 count)))))

(dotimes (n 30)
   (terpri)
   (threenplusone n 140))

(stop 0)
