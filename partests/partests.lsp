(fluid '(i))

(de sum (l) 
    (cond
        ((null l) 0)
        (t (plus2 (car l) (sum (cdr l))))))

(de numlist (n)
    (cond
        ((zerop n) nil)
        (t (cons n (numlist (sub1 n))))))

(dotimes (i 100) (thread '(sum (numlist i))))