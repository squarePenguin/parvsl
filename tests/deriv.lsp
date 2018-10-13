% Simple differentiation with respect to X, but without any simplification.

(de deriv (a)
   (cond
      ((eq a 'x) 1)
      ((atom a)  0)
      ((eqcar a 'plus)
         (list 'plus (deriv (cadr a)) (deriv (caddr a))))
      ((eqcar a 'difference)
         (list 'difference (deriv (cadr a)) (deriv (caddr a))))
      ((eqcar a 'times)
         (list 'plus (list 'times (cadr a) (deriv (caddr a)))
                     (list 'times (deriv (cadr a)) (caddr a))))
      ((eqcar a 'quotient)
         (list 'quotient
            (list 'difference
                (list 'times (deriv (cadr a)) (caddr a))
                (list 'times (cadr a) (deriv (caddr a))))
            (list 'times (caddr a) (caddr a))))
      (t 'unknown)))

(deriv '(plus x y))
(deriv '(times x y))
(deriv '(quotient 1 x))

(deriv (deriv (deriv (deriv '(quotient 1 x)))))

(stop 0)


 