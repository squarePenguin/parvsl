% Evaluation of arithmetic expressions

(de evaluate (x)
   (cond
       ((numberp x) x)
       (t (compound (operator x)
                    (arg1 x)
                    (arg2 x)))))

(de operator (x)
   (car x))

(de arg1 (x)
   (cadr x))

(de arg2 (x)
   (caddr x))

(de compound (op a1 a2)
   (arithmetic op (evaluate a1) (evaluate a2)))

(de arithmetic (op a1 a2)
   (cond
      ((eq op '+) (plus a1 a2))
      ((eq op '*) (times a1 a2))
      (t (error op "unknown operator"))))

(evaluate '(* (+ 2 3) (+ 5 7)))

