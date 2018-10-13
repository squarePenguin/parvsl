% If you like to see your Lisp code reasonably neatly
% indented then consider this "prettyprint" function. It
% is not perfect but it is perhaps a good start!

(de spaces (n)
   (cond
      ((zerop n) nil)
      (t (princ " ") (spaces (sub1 n)))))

(de prettyprint (x)
   (terpri)
   (pprint x 0)
   (terpri)
   nil)

(de length (x)
   (cond
      ((atom x) 0)
      (t (add1 (length (cdr x))))))

(de pprint (x n)
   (cond
      ((or (atom x)
           (lessp (length (explode x)) 40)) (prin x))
      (t (princ "(")
         (pprint (car x) (add1 n))
         (pprintail (cdr x) (plus n 3)))))

(de pprintail (x n)
   (cond
      ((null x) (princ ")"))
      ((atom x) (princ " . ")
                (prin x)
                (princ ")"))
      (t (terpri)
         (spaces n)
         (pprint (car x) n)
         (pprintail (cdr x) n))))

(prettyprint (getd 'pprint))

(stop 0) 