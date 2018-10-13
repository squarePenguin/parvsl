% A compiler that compiles Lisp into C, but that makes no
% attempt at optimisation.

% comval is the central dispatch that takes any Lisp expression and
% creates C code that moves the value of it into a variable called "w".

(de comval (x)
   (cond
      ((symbolp x) (loadsymbol x))
      ((atom x)    (loadliteral x))
      ((get (car x) 'compfn)
                   (eval (list (get (car x) 'compfn)
                               (list 'quote (cdr x)))))
      ((eqcar (getd (car x)) 'macro)
          (comval (apply (cdr (getd (car x))) (list x))))
      (t           (loadargs (cdr x))
                   (callfunction (car x) (length (cdr x))))))


% Literal values used within the function being compiled will
% live in a vector. This function keeps track of what will
% go in that vector.

(de findlit (x)
   (prog (w)
      (setq w (assoc x lits))
      (cond (w (return (cdr w))))
      (setq lits (cons (cons x nlits) lits))
      (setq nlits (add1 nlits))
      (return (sub1 nlits))))

% Loading the value of a variable involved loading (as a literal)
% the name of the variable and then accessing its value cell.

(de loadsymbol (x)
   (princ "    w = qvalue(elt(lits, ")
   (princ (findlit x))
   (printc "));"))

(de loadliteral (x)
   (princ "    w = elt(lits, ")
   (princ (findlit x))
   (printc ");"))

% loadargs merely arranges to push all the arguments onto a stack.

(de loadargs (l)
   (dolist (x l)
      (comval x)
      (printc "    push(w);")))

% After loadargs you can use callfunction to pop args into
% individual variables and call the function concerned.

(de callfunction (f nargs)
   (dotimes (n nargs)
      (princ "    pop(a")
      (prin (difference nargs n))
      (printc ");"))
   (loadliteral f)
   (princ "    w = (*(lispfn *)qdefn(w))(qlits(w), ")
   (princ nargs)
   (dotimes (n nargs)
      (princ ", a")
      (prin (add1 n)))
   (printc ");"))
      
% Now I will put in a collection of functions that provide
% special-case treatment. All "special forms" need this, and
% some other code can benefit from it in terms of efficiency.

(de comcar (x)
   (comval (car x))
% For simplicity I make taking CAR of an atom a fatal error here.
   (printc "    if (!isCONS(w)) disaster(__LINE__);")
   (printc "    w = qcar(w);"))

(put 'car 'compfn 'comcar)

(de comcdr (x)
   (comval (car x))
   (printc "    if (!isCONS(w)) disaster(__LINE__);")
   (printc "    w = qcdr(w);"))

(put 'cdr 'compfn 'comcdr)

(de comcond (x)
   (cond
      ((null x) (loadliteral nil))
      (t (let ((lab1 (gensym))
               (lab2 (gensym)))
         (comval (caar x))
         (princ "    if (w == nil) goto ")
         (print lab1)
         (comprogn (cdar x))
         (princ "    goto ")
         (print lab2)
         (prin lab1)
         (printc ":")
         (comcond (cdr x))
         (princ lab2)
         (printc ":")))))

(put 'cond 'compfn 'comcond)

(de comprogn (x)
   (cond
      ((null x) (loadliteral nil))
      (t (dolist (c x) (comval c)))))

(put 'progn 'compfn 'comprogn)

(de tidylits (a)
   (mapcar (reverse a) 'car))

(de localargs (l v)
   (cond
      ((null l) nil)
      (t (princ ", ")
         (prin (car v))
         (localargs (cdr l) (cdr v)))))

(de pushargs (names vars)
   (cond
      ((null names) nil)
      (t (princ "    push(qvalue(elt(lits, ")
         (princ (findlit (car names)))
         (printc ")));")
         (princ "    qvalue(elt(lits, ")
         (princ (findlit (car names)))
         (princ ")) = ")
         (prin (car vars))
         (printc ";")
         (pushargs (cdr names) (cdr vars)))))

(de popargs (l)
   (dolist (v l)
      (princ "    pop(qvalue(elt(lits, ")
      (princ (findlit v))
      (printc ")));")))

(de compile (fn)
   (prog (def pops lits nlits)
      (setq nlits 0)
      (setq def (cddr (getd fn)))
      (terpri)
      (princ "LispObject L")
      (princ fn)
      (printc "(LispObject lits, int n, ...)")
      (printc "{")
      (printc"    LispObject w, a1, a2, a3, a4;")
      (princ "    ARG")
      (princ (length (car def)))
      (princ "(""")
      (princ fn)
      (princ """")
      (localargs (car def) '(v1 v2 v3 v4))
      (printc ");")
      (setq pops (pushargs (car def) '(v1 v2 v3 v4)))
      (comprogn (cdr def))
      (popargs (reverse (car def)))
      (printc "    return w;")
      (printc "}")
      (terpri)
      (princ "lits: ")
      (print (tidylits lits))))


(compile 'cadr)

(compile 'pushargs)

% This example will generate INCORRECT code because at present
% this compiler does not have a 'compfn for PROG (or GO or RETURN).
(compile 'comprogn)

(stop 0)

