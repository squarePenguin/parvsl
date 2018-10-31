% Reverse a list...

(de rev1 (a b)
   (cond
      ((null a) b)
      (t (rev1 (cdr a) (cons (car a) b)))))

(de reverse (x) (rev1 x nil))

(rev1 nil '(a b c))
(rev1 '(a) '(b c))

(reverse '(a b c d e f))

(compress (reverse (explode "Secret message")))

(stop 0)

