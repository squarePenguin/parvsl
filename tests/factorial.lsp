% Factorial function

(de factorial (n)
   (cond
      ((zerop n) 1)
      (t (times n (factorial (sub1 n))))))

(dotimes (i 40)
   (prin i)
   (princ blank)
   (print (factorial i)))

(stop 0)
