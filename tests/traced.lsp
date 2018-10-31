% Showing tracing of Lisp functions

(de fact (n)
   (if (zerop n)
       1
       (times n (fact (sub1 n)))))

(trace '(fact sub1))

(fact 3)

(untrace '(sub1))

(de fail (n)
   (if (zerop n)
       (error 1 "crashing")
       (times n (fail (sub1 n)))))

(errorset '(fail 3) nil nil)

(errorset '(fail 3) t nil)

(errorset '(fail 3) t t)

(stop 0)
