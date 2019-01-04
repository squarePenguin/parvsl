
% This is a collection of commonly useful Lisp
% functions that can be defined in terms of the
% things that are built into vsl. 

% This version is for a VSL that has all its arithmetic built into
% the kernel.

% There are a number of pre-defined symbols that stand for
% characters and the like. Define them here.

(setq blank   '! )
(setq tab     (code!-char 9))
(setq !$eol!$ (code!-char 10))
(setq dollar  '!$)
(setq lpar    '!()
(setq rpar    '!))

(setq f       nil)

% Many combinations of car and cdr are supported. Here I define
% versions that do up to four accesses. These would of course be
% trivial to move into C code!

(de caar (x)
    (car (car x)))

(de cadr (x)
    (car (cdr x)))

(de cdar (x)
    (cdr (car x)))

(de cddr (x)
    (cdr (cdr x)))

(de caaar (x)
    (car (car (car x))))

(de caadr (x)
    (car (car (cdr x))))

(de cadar (x)
    (car (cdr (car x))))

(de caddr (x)
    (car (cdr (cdr x))))

(de cdaar (x)
    (cdr (car (car x))))

(de cdadr (x)
    (cdr (car (cdr x))))

(de cddar (x)
    (cdr (cdr (car x))))

(de cdddr (x)
    (cdr (cdr (cdr x))))

(de caaaar (x)
    (car (car (car (car x)))))

(de caaadr (x)
    (car (car (car (cdr x)))))

(de caadar (x)
    (car (car (cdr (car x)))))

(de caaddr (x)
    (car (car (cdr (cdr x)))))

(de cadaar (x)
    (car (cdr (car (car x)))))

(de cadadr (x)
    (car (cdr (car (cdr x)))))

(de caddar (x)
    (car (cdr (cdr (car x)))))

(de cadddr (x)
    (car (cdr (cdr (cdr x)))))

(de cdaaar (x)
    (cdr (car (car (car x)))))

(de cdaadr (x)
    (cdr (car (car (cdr x)))))

(de cdadar (x)
    (cdr (car (cdr (car x)))))

(de cdaddr (x)
    (cdr (car (cdr (cdr x)))))

(de cddaar (x)
    (cdr (cdr (car (car x)))))

(de cddadr (x)
    (cdr (cdr (car (cdr x)))))

(de cdddar (x)
    (cdr (cdr (cdr (car x)))))

(de cddddr (x)
    (cdr (cdr (cdr (cdr x)))))

% "not" and "eqcar" are used while processing some parts of
% this file and so get defined early.

(de not (x)
   (null x))

(de eqcar (a b)                % Is (car a) the same as b?
   (and (not (atom a)) (eq (car a) b)))

% The vsl kernel checks for a function called macroexpand_list
% whenever it is about to define a function, and expects it to
% expand macros in all the expressions in a list. So before
% I define any macros that could usefully be expanded I will
% define it!

(de macroexpand_cond (l)
   (cond
      ((null l) nil)
      (t (cons (macroexpand_list (car l))
               (macroexpand_cond (cdr l))))))

(de macroexpand (x)
   (cond
      ((atom x) x)
      ((not (atom (car x)))
         (cons (macroexpand (car x))
               (macroexpand_list (cdr x))))
      ((eqcar x 'quote) x)
      ((eqcar x 'cond)
         (cons 'cond (macroexpand_cond (cdr x))))
      ((or (eqcar x 'prog) (eqcar x 'lambda))
         (cons (car x) (cons (cadr x)
            (macroexpand_list (cddr x)))))
      ((eqcar (getd (car x)) 'macro)
         (macroexpand (apply (cdr (getd (car x)))
                             (list x))))
      (t (cons (car x) (macroexpand_list (cdr x))))))

(de macroexpand_list (l)
   (cond
      ((atom l) l)
      (t (cons (macroexpand (car l))
               (macroexpand_list (cdr l))))))

(de expand (l fn)
   (cond
      ((null (cdr l)) (car l))
      (t (list fn (car l) (expand (cdr l) fn)))))

% Back to defining what are sometimes merely alternate
% names for very basic operations.

(de idp (x)
   (symbolp x))

(de pairp (x)
   (null (atom x)))

(de prog1 (a b)
   a)

(de reverse (x)
   (prog (y)
   loop
      (cond ((atom x) (return y)))
      (setq y (cons (car x) y))
      (setq x (cdr x))
      (go loop)))

% "reverse" reverses a list, while "reversip" creates the
% reversed version by overwriting the data that makes up its
% input. This may be held to save a little space, but is
% to be used with care.

(de reversip2 (a b)
   (prog (w)
   loop
      (cond ((atom a) (return b)))
      (setq w (cdr a))
      (rplacd a b)
      (setq b a)
      (setq a w)
      (go loop)))

(de reversip (x) (reversip2 x nil)) % Destructive reverse

(de append (a b)                % Append a pair of lists.
   (cond
      ((atom a) b)
      (t (cons (car a) (append (cdr a) b)))))

(de last (l)                    % Last element of a (non-empty) list.
   (cond
      ((atom l) (error 1 "last on emtpy list"))
      ((atom (cdr l)) (car l))
      (t (last (cdr l)))))

(de lastcar (x)                 % Not in Standard Lisp
   (cond
      ((atom x) nil)
      ((atom (cdr x)) (car x))
      (t (lastcar (cdr x)))))

(de lastpair (l)                % Last pair of a (non-empty) list,
   (cond                        % or nil if the input is empty.
      ((atom l) nil)
      ((atom (cdr l)) l)
      (t (lastpair (cdr l)))))

%(de member (a l)
%   (cond
%      ((atom l) nil)
%      ((equal a (car l)) l)
%      (t (member a (cdr l)))))

% "member" checks it a value is present in a list using the
% "equal" test, while "memq" uses "eq".

%(de memq (a l)
%   (cond
%      ((atom l) nil)
%      ((eq a (car l)) l)
%      (t (memq a (cdr l)))))

(de delete (a l)
   (cond
      ((atom l) l)
      ((equal a (car l)) (cdr l))
      (t (cons (car l) (delete a (cdr l))))))

(de intersection (a b)
   (cond
      ((atom a) nil)
      ((member (car a) b) (cons (car a) (intersection (cdr a) b)))
      (t (intersection (cdr a) b))))

(de union (a b)
   (cond
      ((atom a) b)
      ((member (car a) b) (union (cdr a) b))
      (t (cons (car a) (union (cdr a) b)))))

(de neq (a b)                  % Not equal.
   (null (equal a b)))

(de assoc (a l)                % Look item up in association list using equal.
   (cond
      ((atom l) nil)
      ((and (not (atom (car l)))
            (equal (caar l) a)) (car l))
      (t (assoc a (cdr l)))))

(de atsoc (a l)                % Look item up in association list using eq.
   (cond
      ((atom l) nil)
      ((and (not (atom (car l)))
            (eq (caar l) a)) (car l))
      (t (atsoc a (cdr l)))))

(de subst (a b c)              % Substitute a for b in c
   (cond
      ((equal b c) a)
      ((atom c) c)
      (t (cons (subst a b (car c)) (subst a b (cdr c))))))

(de sublis (x y)
   (if (atom x) y
      (prog (u)
         (setq u (assoc y x))
         (return (cond
            ((not (atom u)) (cdr u))
            ((atom y) y)
            (t (cons (sublis x (car y))
               (sublis x (cdr y)))))))))

(de subla (x y)
   (if (atom x) y
      (prog (u)
         (setq u (atsoc y x))
         (return (cond
            ((not (atom u)) (cdr u))
            ((atom y) y)
            (t (cons (subla x (car y))
               (subla x (cdr y)))))))))

(de pair (u v)
   (cond
      ((or (atom u) (atom v)) nil)
      (t (cons (cons (car u) (car v)) (pair (cdr u) (cdr v))))))

(de spaces (n)                 % Print n blanks.
   (cond
      ((zerop n) nil)
      (t (princ " ") (spaces (sub1 n)))))

% The prettyprint code here is a version of the code described
% as an example of the use of vsl.

(de prettyprint (x)            % Display something with indentation.
   (terpri)
   (pprint x 0)
   (terpri)
   nil)

(de pprint (x n)               % Sub-function for prettyprint.
   (cond
      ((or (atom x)
           (lessp (length (explode x)) 40)) (prin x))
      (t (princ "(")
         (pprint (car x) (add1 n))
         (pprintail (cdr x) (plus n 3)))))

(de pprintail (x n)            % Sub-function for prettyprint.
   (cond
      ((null x) (princ ")"))
      ((atom x) (princ " . ")
                (prin x)
                (princ ")"))
      (t (terpri)
         (spaces n)
         (pprint (car x) n)
         (pprintail (cdr x) n))))

(de rplacw (a b) (progn (rplaca a (car b)) (rplacd a (cdr b))))

% The "map" functions apply some function to each item defined by
% a list.
% mapc and map return nil
% mapcar and maplist build a list out of the computed values
% mapcan and mapcon expect the values to be lists, and use "nconc"
%        to join them.
% map, maplist and mapcon pass a pointer into the list itself
% as argument to the function, while mapc, mapcar and mapcon pass
% items from the list.
% Note that some other Lisp systems have functions like this that take
% the function as their first argument and the list as second, unlike the
% argument order used here.
% The use of awkward variables names such as "!~l" here is because under
% the dynamic scoping regime in vsl if the function passed relied on a
% free variable whose name clashed with a name used locally in these
% definitions there could be confusion.

(de mapcar (!~l !~fn)
  (prog (!~r)
 top (cond ((atom !~l) (return (reversip !~r))))
     (setq !~r (cons (apply !~fn (list (car !~l))) !~r))
     (setq !~l (cdr !~l))
     (go top)))

(de maplist (!~l !~fn)
  (prog (!~r)
 top (cond ((atom !~l) (return (reversip !~r))))
     (setq !~r (cons (apply !~fn (list !~l)) !~r))
     (setq !~l (cdr !~l))
     (go top)))

(de mapcan (!~l !~fn)
  (cond ((atom !~l) nil)
	(t (nconc (apply !~fn (list (car !~l))) (mapcan (cdr !~l) !~fn)))))

(de mapcon (!~l !~fn)
  (cond ((atom !~l) nil)
	(t (nconc (apply !~fn (list !~l)) (mapcon (cdr !~l) !~fn)))))

(de mapc (!~l !~fn)
  (prog ()
 top (cond ((atom !~l) (return nil)))
     (apply !~fn (list (car !~l)))
     (setq !~l (cdr !~l))
     (go top)))

(de map (!~l !~fn)
  (prog ()
 top (cond ((atom !~l) (return nil)))
     (apply !~fn (list !~l))
     (setq !~l (cdr !~l))
     (go top)))

(de copy (a)
   (cond
      ((atom a) a)
      (t (cons (copy (car a)) (copy (cdr a))))))

(de sassoc (a l fn)
  (cond
     ((atom l) (apply fn nil))
     ((equal a (caar l)) (car l))
     (t (sassoc a (cdr l) fn))))

(de rassoc (x l)        % Not in Standard Lisp
   (prog ()
loop  (cond ((atom l) (return nil))
	    ((equal x (cdar l)) (return (car l)))
	    (t (setq l (cdr l)) (go loop))) ))

(de deflist (a b)
  (prog (r)
top (cond ((atom a) (return (reversip r))))
    (put (caar a) b (cadar a))
    (setq r (cons (caar a) r))
    (setq a (cdr a))
    (go top)))

% The Lisp "backquote" capability is coped with in vsl by having
% `(a b c) read in as (!` (a b c)) [and similarly for "," and ",@"]
% and then using macro-expansion to convert into executable code.

(de expand_backquote (x)
   (cond
      ((and (symbolp x) (null (null x))) (list 'quote x))
      ((atom x) x)                  % nil, number or string
      ((eq (car x) '!,) (cadr x))
      ((eqcar (car x) '!,!@)
         (list 'append (cadar x) (expand_backquote (cdr x))))
      (t (list 'cons (expand_backquote (car x)) (expand_backquote (cdr x))))))

(dm !` (x) (expand_backquote (cadr x)))

% Now a few things not needed by Standard Lisp but maybe helpful
% when using Lisp directly.

% Reduce uses the name "let" and so if I called this what I naturally
% want to there would be a clash and trouble. So I use a mangled name here.
% I also use let!* rather than let in those cases here where I can...

% Both "let" and "let!*" expand naturally into uses of lambda-expressions.

(dm !~let (x)              % (!~let ((v1 E1) (v2 E2) ...) body)
   (cons (cons 'lambda (cons (mapcar (cadr x) 'car) (cddr x)))
         (mapcar (cadr x) 'cadr)))

(de expand_let!* (b x)
   (cond
      ((null b) x)
      (t (list (list 'lambda (list (caar b)) (expand_let!* (cdr b) x))
               (cadar b)))))

(dm let!* (x)            % As !~let, but do bindings sequentially
   (expand_let!* (cadr x) (cons 'progn (cddr x))))

% A set of macros provide various neat and easy-to-use control structures.

(dm if (x)          % (IF predicate yes no) or (IF predicate yes)
   `(cond
      (,(cadr x) ,(caddr x))
      (t ,(cond ((atom (cdddr x)) nil) (t (car (cdddr x)))))))

(dm when (x)        % (WHEN predicate yes yes yes ...)
   `(cond
      (,(cadr x) ,@(cddr x))))

(dm while (x)       % (WHILE predicate body body body ...)
   (let!* ((g (gensym)))
      `(prog nil
       ,g (cond ((null ,(cadr x)) (return nil)))
          ,@(cddr x)
          (go ,g)))))

% (psetq a A b B) must arrange to evaluate both A and B before
% either is saved in a or b. This can be expressed in terms of
% use of "let" - but the expansion process is a little messy to
% establish.

(de make_psetq_vars (u)
   (if (null u)
       nil
       (if (null (cdr u))
           (error "odd number of items in psetq")
           (cons (gensym) (make_psetq_vars (cddr u))))))

(de make_psetq_bindings (vars u)
   (if (null u)
       nil
       (cons
          (list (car vars) (cadr u))
          (make_psetq_bindings (cdr vars) (cddr u)))))

(de make_psetq_assignments (vars u)
   (if (null u)
       nil
       (cons
          (list 'setq (car u) (car vars))
          (make_psetq_assignments (cdr vars) (cddr u)))))

(dm psetq (x)             % parallel setq as in (psetq x X y Y z Z)
   (let!* ((vars (make_psetq_vars (cdr x))))
      `(let!* ,(make_psetq_bindings vars (cdr x))
              ,@(make_psetq_assignments vars (cdr x)))))

% The "do" macro provides a rather general iteration capabilty
% of the form
% (do ((var init step) ..)
%     (endcondition result ...)
%     body)
% and again can be expressed via macro-expansion into simpler or
% more basic constructions.

(de do_bindings (u)
   (if (null u)
       nil
       (if (atom (car u))
           (cons (car u) (do_bindings (cdr u)))
           (if (null (cdar u))
               (cons (list (caar u) nil) (do_bindings (cdr u)))
               (cons (list (caar u) (cadar u)) (do_bindings (cdr u)))))))

(de do_endtest (u)
   (if (null u)
       nil
       (car u)))

(de do_result (u)
   (if (null u)
       nil
       (cdr u)))

(de do_updates (u)
   (if (null u)
       nil
       (let!* ((v (car u))
               (x (do_updates (cdr u))))
          (if (or (atom v)
                  (null (cdr v))
                  (null (cddr v)))
              x
              (cons (car v) (cons (caddr v) x))))))

(de expand_do (u letter setter)
   (let!* ((bindings (do_bindings (car u)))
           (result (do_result (cadr u)))
           (updates (do_updates (car u)))
           (body (cddr u))
           (endtest (do_endtest (cadr u)))
           (upd (if updates (list (cons setter updates)) nil))
           (res (if (null result)
                    nil
                    (if (null (cdr result))
                        (car result)
                        (cons 'progn result))))
           (x (if (null endtest) nil
                  `((when ,endtest (return ,res)))))
           (g (gensym)))
      (if bindings
         `(,letter ,bindings
             (prog nil
            ,g  ,@x
                ,@body
                ,@upd
                (go ,g)))
         `(prog nil
         ,g    ,@x
               ,@body
               ,@upd
               (go ,g)))))

(dm do (u) (expand_do (cdr u) '!~let 'psetq))

(dm do!* (u) (expand_do (cdr u) 'let!* 'setq))

% "dolist" is much simpler, and is used as in
% (dolist (a '(1 2 3)) (print a))

(de expand_dolist (vir b)
   (prog (l v var init res)
     (setq var (car vir))
     (setq init (car (setq vir (cdr vir))))
     (setq res (cdr vir))
     (setq v (gensym))
     (setq l (gensym))
     (return `(prog (,v ,var)
                (setq ,v ,init)
            ,l  (cond ((null ,v) (return (progn ,@res))))
                (setq ,var (car ,v))
                ,@b
                (setq ,v (cdr ,v))
                (go ,l)))))

(dm dolist (u) (expand_dolist (cadr u) (cddr u)))

% "dotimes" arranges to perform some actions a fixed number of times,
% counting (starting from 0) in a variable that the user can name.
%   (dotimes (i 10) (prin i) (princ blank) (print (times i i)))

(de expand_dotimes (vnr b)
   (prog (l v var count res)
     (setq var (car vnr))
     (setq count (car (setq vnr (cdr vnr))))
     (setq res (cdr vnr))
     (setq v (gensym))
     (setq l (gensym))
     (return `(prog (,v ,var)
                (setq ,v ,count)
                (setq ,var 0)
           ,l   (cond ((geq ,var ,v) (return (progn ,@res))))
                ,@b
                (setq ,var (add1 ,var))
                (go ,l)))))

(dm dotimes (u) (expand_dotimes (cadr u) (cddr u)))

(de nconc (u v)
   (if (atom u) v
      (let!* ((w u))
         (while (not (atom (cdr u))) (setq u (cdr u)))
         (rplacd u v)
         w)))

(de complexp (u) nil)

% At some stage if I get keen I might migrate this stuff into the kernel too!

(setq small!-modulus 3)

(de set!-small!-modulus (n)
   (let!* ((r small!-modulus))
      (setq small!-modulus n)
      r))

(de small!-modular!-number (n)
   (setq n (remainder n small!-modulus))
   (when (minusp n) (setq n (plus n small!-modulus)))
   n)

(de small!-modular!-plus (a b)
   (small!-modular!-number (plus a b)))

(de small!-modular!-difference (a b)
   (small!-modular!-number (difference a b)))

(de small!-modular!-times (a b)
   (small!-modular!-number (times a b)))

(de small!-modular!-minus (a)
   (small!-modular!-number (minus a)))

(de small!-modular!-quotient (a b)
   (error "small-modular-quotient not implemented yet" (cons a b)))

% "fluid" and "global" are concepts that mainly belong with
% a compiler, but versions are provided here even if they
% are not terribly useful.
% (de ensure_defined (v)
%    (when (not (boundp v))
%          (eval (list 'setq v nil))))
% (de fluid (x)
%    (remflag x 'global)
%    (flag x 'fluid)
%    (dolist (v x) (ensure_defined v)))
% (de global (x)
%    (remflag x 'fluid)
%    (flag x 'global)
%    (dolist (v x) (ensure_defined v)))
% (de unfluid (x)
%    (remflag x 'fluid))
% (de unglobal (x)
%    (remflag x 'global))

(de fluidp (x) (flagp x 'fluid))

(de globalp (x) (flagp x 'global))

% Now some more general-purpose small functions. Including
% cases that are alternative names for built-in ones that
% it is convenient to have for the support of some historic
% code.

(de flag (l tag)
   (dolist (v l) (put v tag t)))

(de remflag (l tag)
   (dolist (v l) (remprop v tag)))

(de flagp (v tag) (get v tag))

(de prin2 (x) (princ x))

(de explode2 (x) (explodec x))

(de mkquote (x) (list 'quote x))

(de apply1 (fn a1) (apply fn (list a1)))

(de apply2 (fn a1 a2) (apply fn (list a1 a2)))

(de apply3 (fn a1 a2 a3) (apply fn (list a1 a2 a3)))

(de special!-char (n)
   (cond
      ((equal n 0) (code!-char 32))
      ((equal n 1) (code!-char 10))
      ((equal n 2) (code!-char 8))
      ((equal n 3) (code!-char 9))
      ((equal n 4) (code!-char 11))
      ((equal n 5) (code!-char 12))
      ((equal n 6) (code!-char 13))
      ((equal n 7) (code!-char 127))
      ((equal n 8) !$eof!$)
      ((equal n 9) (code!-char 7))
      ((equal n 10) (code!-char 27))
      (t (error "special-char" n))))

% Testing for letters and digits as done here makes
% assumptions about the character-code that is in use.

(de liter (x)
   (let!* ((c (char!-code x)))
      (or (and (leq 65 c) (leq c 90))
          (and (leq 97 c) (leq c 122)))))

(de digit (x)
   (let!* ((c (char!-code x)))
      (and (leq 48 c) (leq c 57))))

(de tolower (x)
   (let!* ((c (char!-code x)))
      (if (and (leq 65 c) (leq c 90))
          (code!-char (plus c 32))
          x)))

(de explode2lc (x)
    (mapcar (explodec x) 'tolower))

(de addescapes (l)
  (cond
    ((null l) nil)
    (t (cons '!! (cons (car l) (addescapes (cdr l)))))))

(de intern (x)
  (cond
    ((symbolp x) x)
    ((stringp x) (compress (addescapes (explodec x))))
    (t (error 1 "bad arg to intern"))))

(setq !*raise nil)
(setq !*lower t)

(setq !*redefmsg nil)

(de set!-print!-precision (n) n)

(de constantp (x)
   (or (null x)
       (numberp x)
       (stringp x)
       (eq x t)))

(dm declare (x) nil)

% The code for ordering items that is given here is
% required by Reduce, and the exact behaviour is
% intended to support what is needed there.

(de ordp (u v)
   (cond
      ((null u) (null v))
      ((vectorp u) (cond
         ((vectorp v) (ordpv u v))
         (t (atom v))))
      ((atom u) (cond
         ((atom v) (cond
            ((numberp u) (and (numberp v) (not (lessp u v))))
            ((idp v) (orderp u v))
            (t (numberp v))))
         (t nil)))
      ((atom v) t)
      ((equal (car u) (car v)) (ordpl (cdr u) (cdr v)))
      ((flagp (car u) 'noncom) (cond
         ((flagp (car v) 'noncom) (ordp (car u) (car v)))
         (t t)))
      ((flagp (car v) 'noncom) nil)
      (t (ordp (car u) (car v)))))

(de ordpl (u v)
   (cond
      ((atom u) (ordp u v))
      ((equal (car u) (car v)) (ordpl (cdr u) (cdr v)))
      (t (ordp (car u) (car v)))))

(de ordpv (u v)
    (error "ordpv not yet implemented" (cons u v)))

(de orderp (u v)
   (prog ()
      (setq u (explodec u))
      (setq v (explodec v))
      (while (and u v (eq (car u) (car v)))
         (setq u (cdr u) v (cdr v)))
      (cond
         ((and u v)
            (return (lessp (char!-code (car u)) (char!-code (car v)))))
         (v (return t))
         (t (return nil)))))

(dm function (x) (cons 'quote (cdr x)))

(de sort (items fn)
   (prog (tree)
      (dolist (x items)
         (setq tree (sort_insert x tree fn)))
      (return (sort_flatten tree))))

(de sort_insert (item tree fn)
   (cond
      ((null tree) (list!* item nil nil))
      ((apply2 fn item (car tree))
         (sort_insertleft item tree fn))
      (t (sort_insertright item tree fn))))

(de sort_insertleft (item tree fn)
   (list!*
      (car tree)
      (sort_insert item (cadr tree) fn)
      (cddr tree)))

(de sort_insertright (item tree fn)
   (list!*
      (car tree)
      (cadr tree)
      (sort_insert item (cddr tree) fn)))

(de sort_flatten (x)
   (cond
      ((null x) nil)
      (t (append (sort_flatten (cadr x))
         (cons (car x) (sort_flatten (cddr x)))))))

(de gcdn (a b)
   (cond
      ((minusp a) (gcdn (minus a) b))
      ((minusp b) (gcdn a (minus b)))
      ((greaterp b a) (gcdn b a))
      ((zerop b) a)
      (t (gcdn b (remainder a b)))))

(de lcmn (a b) (times a (quotient b (gcdn a b))))

(de abs (x)
   (if (minusp x) (minus x) x))

(de max2 (a b)
   (if (greaterp a b) a b))

(de min2 (a b)
   (if (lessp a b) a b))

(de evenp (x) (zerop (remainder x 2)))

(de msd (n)
   (prog (r)
      (setq r 0)
      (while (not (zerop n))
         (setq n (quotient n 2))
         (setq r (add1 r)))
      (return r)))

(de lsd (n)
   (if (zerop n)
      0
      (prog (r)
         (setq r 0)
         (while (zerop (remainder n 2))
            (setq n (quotient n 2))
            (setq r (add1 r)))
         (return r))))

(de ash (a n) (leftshift a n))
(de ashift (a n) (leftshift a n))

(de ash1 (a n)
   (if (minusp a) (minus (leftshift (minus a) n)) (leftshift a n)))

(de remd (x) nil)

% The "fasl" scheme here is used when building large programs.
% Code gets put in files in a directory called "modules".

(de modulepath (x)
   (compress
      (cons '!"
         (append (explodec (cdr (assoc 'image lispsystem!*)))
            (append '(!. m o d u l e s !/)
               (append (explodec x) '(!. f a s l !")))))))

(de modulep (x)
   (filep (modulepath x)))

(de filedate (x) 0)

(de datelessp (a b) t)

(setq faslinfile!* nil faslinstack!* nil fasloutfile!* nil faslname!* nil)

(de start!-module (x)
   (cond
      ((null x)
       (close fasloutfile!*)
       (setq fasloutfile!* nil)
       (princ "+++ FASLEND ")
       (printc faslname!*)
       t)
      (t
       (setq faslname!* x)
%       (setq x (modulepath x))
       (setq fasloutfile!* (open!-module x 'output))
       t)))

(de faslread ()
   (let!* ((s (rds faslinfile!*)))
      (prog1
         (read)
         (rds s))))

(de write!-module (x)
   (let!* ((s (wrs fasloutfile!*)))
      (prog1
         (print x)
         (wrs s))))

(setq dfprint!* nil)

(de faslout (u)
   (prog nil
      (terpri)
      (princ "FASLOUT ")
      (prin u)
      (printc ": IN files;  or type in expressions")
      (printc "When all done, execute FASLEND;")
      (cond
         ((not (atom u)) (setq u (car u))))
      (cond ((not (start!-module u))
             (progn
                (cond ((neq (posn) 0) (terpri)))
                (printc "+++ Failed to open FASL output file")
                (return nil))))
      (setq s!:faslmod_name (cons u nil))
      (setq s!:dfprintsave dfprint!*)
      (setq dfprint!* (quote s!:fslout0))
      (setq !*defn t)))

(put (quote faslout) (quote stat) (quote rlis))

(de faslend nil
   (prog ()
      (cond
         ((null s!:faslmod_name) (return nil)))
      (princ "Completed FASL files for ")
      (print (car s!:faslmod_name))
      (start!-module nil)
      (setq dfprint!* s!:dfprintsave)
      (setq !*defn nil)
      (setq s!:faslmod_name nil)
      (return nil)))

(put (quote faslend) (quote stat) (quote endstat))
(flag '(faslend) 'eval)

(setq !*backtrace nil)
(setq !*debug nil)

(de s!:fasl_supervisor nil
   (prog (u w !*echo)
      (setq !*echo !*debug)
   top
      (setq u (errorset (quote (read)) t !*backtrace))
      (cond
         ((atom u) (return nil)))
      (setq u (car u))
      (cond
         ((equal u !$eof!$) (return nil)))
      (cond ((not (atom u)) (setq u (macroexpand u))))
      (cond
         ((atom u) (go top))
         ((eqcar u (quote faslend))
            (return (apply (quote faslend) nil)))
         ((eqcar u (quote rdf))
            (setq w (open (setq u (eval (cadr u))) (quote input)))
            (cond (w (progn (terpri)
                     (princ "Reading file ")
                     (prin u)
                     (terpri)
                     (setq w (rds w))
                     (s!:fasl_supervisor)
                     (princ "End of file ")
                     (prin u)
                     (terpri)
                     (close (rds w))))
                  (t (progn (princ "Failed to open file ")
                     (prin u)
                     (terpri)))))
          (t (s!:fslout0 u)))
      (go top)))

(de s!:fslout0 (u) (s!:fslout1 u nil))

(de s!:fslout1 (u loadonly)
   (prog (w)
% Note that I check for eval-when BEFORE I do any macro-expansion here
% because otherwise eval-when gets expanded away. The consequence here is
% that eval-when is only honoured at the top-level.
      (when (eqcar u 'eval!-when)
            (if (memq 'compile (cadr u)) (eval (cons 'progn (cddr u))))
            (if (memq 'load (cadr u))
                (s!:fslout1 (cons 'progn (cddr u)) loadonly))
            (return nil))
      (cond
         ((not (atom u)) (setq u (macroexpand u))))
      (cond
         ((atom u) (return nil))
         ((eqcar u (quote progn))
            (prog (var1174)
               (setq var1174 (cdr u))
    lab1173    (cond
                  ((null var1174) (return nil)))
               (prog (v)
                  (setq v (car var1174))
                  (s!:fslout1 v loadonly))
               (setq var1174 (cdr var1174))
               (go lab1173))
            (return nil))
         ((eqcar u (quote eval!-when))
            (return (prog nil
               (setq w (cadr u))
               (setq u (cons (quote progn) (cddr u)))
               (cond
                  ((and (memq (quote compile) w)
                        (not loadonly))
                   (eval u)))
               (cond
                  ((memq (quote load) w) (s!:fslout1 u t)))
               (return nil))))
         ((or (flagp (car u) (quote eval))
              (and (equal (car u) (quote setq))
                   (not (atom (caddr u)))
                   (flagp (caaddr u) (quote eval))))
            (cond
               ((not loadonly) (errorset u t !*backtrace)))))
      (cond
         ((eqcar u (quote rdf))
            (prog nil
               (setq w (open (setq u (eval (cadr u))) (quote input)))
               (cond 
                  (w (princ "Reading file ")
                     (prin u)
                     (terpri)
                     (setq w (rds w))
                     (s!:fasl_supervisor)
                     (princ "End of file ")
                     (prin u)
                     (terpri)
                     (close (rds w)))
                  (t (princ "Failed to open file ")
                     (prin u)
                     (terpri)))))
          ((and (not (eqcar u (quote faslend)))
                (not (eqcar u (quote carcheck))))
             (write!-module u)))))))

(de verbos (x) nil)

(de getenv (x) nil)

% Because I have onnly just migrated a version of filep into the kernel
% I leave the Lisp-coded copy here for use when I have an old copy of vsl
% in use. In a while I will just remove this!

(cond
  ((null (getd 'filep))
   (de filep (x)
      (let!* ((h (errorset (list 'open x ''input) nil nil)))
         (if (atom h) nil
             (progn (close (car h)) t))))))

(de lengthc (x) (length (explodec x)))

% widelengthc takes something and counts the number of characters (not bytes).
% To do this it ignores any octents of the form 10xxxxxx.

(de widelengthc (x)
  (prog (n)
    (setq n 0)
    (setq x (explodecn x))
    (while x
      (cond ((not (eq (land (car x) 0xc0) 0x80)) (setq n (add1 n))))
      (setq x (cdr x)))
    (return n)))

% Various of the functions defined here are needed by Reduce
% and simplified or dummy versions are provided here so that the
% program as a whole can be built.

(de gctime () 0)

(de eqn (a b) (equal a b))

(cond
  ((null (getd 'setpchar))
   (de setpchar (u) nil)))

(de threevectorp (x)
   (and (vectorp x) (equal (upbv x) 2)))

(de list!-to!-vector (l)
   (prog (n v)
      (setq n (length l))
      (setq v (mkvect (sub1 n)))
      (setq n 0)
      (while l
         (putv v n (car l))
         (setq n (add1 n))
         (setq l (cdr l)))
      (return v)))

(de frexp (x)
   (prog (n)
      (if (zerop x) (return '(0 . 0.0)))
      (setq n 0)
      (while (geq x 1.0)
             (setq x (times x 0.5))
             (setq n (add1 n)))
      (while (lessp x 0.5)
             (setq x (times x 2.0))
             (setq n (sub1 n)))
      (return (cons n x))))

(de verbos (x) nil)

(de window!-heading (x) (print x))

(de make!-special (x)
   (set x nil)
   (flag (list x) 'fluid))

(de compile!-all () nil)

(de compd (a b c) (put a b c))

(de carcheck (fg) nil)

(de library!-members () nil)

(de delete!-module (x) nil)

(de list!-directory (x) nil)

(de list!-to!-string (a)
   (prog (r)
      (setq r '(!"))
      (dolist (c a)
         (if (eq c '!") (setq r (cons c r)))
         (setq r (cons c r)))
      (return (compress (reverse (cons '!" r))))))

(de gensym1 (x) (gensym))

(de md60 (x) 123456789)

(dm eval!-when (u)
   (if (member 'eval (cadr u))
       (cons 'progn (cddr u))
       nil))

(de tmpnam () "./temporary-file.tmp")

% Reduce these days needs unwind-protect. I will not provide its
% key function here, but will support the easy case where the
% protected block does NOT fail...

(dm unwind!-protect (u)
  (list
    (list 'lambda '(!*x!*)
      (cons 'progn (append (cddr u) '(!*x!*))))
    (cadr u)))

(de smember (u v)
  (cond
    ((eq u v) t)
    ((atom v) nil)
    (t (or (smember u (car v))
           (smember u (cdr v))))))

(de smemql (u v)
  (cond
    ((null v) nil)
    ((smemq (car u) v) (cons (car u) (smemq (cdr u) v)))
    (t (smemql (cdr u) v))))

(de smemqlp (u v)
  (cond
    ((or (null v) (numberp v)) nil)
    ((atom v) (memq v u))
    ((eq (car v) 'quote) nil)
    (t (or (smemqlp u (car v)) (smemqlp u (cdr v))))))

(de find!-gnuplot ()
  "gnuplot")

(de prin1 (x) (prin x))

(de iequal (x y) (equal x y))

(de string2list (x) (explodecn x))

(de id2string (x)
   (list2string (explodec x)))

(de string!-length (s) (length (explodec s)))

(de bldmsg1 (fmt arglist)
  (prog (r w)
    (while fmt
      (cond
         ((and (eqcar fmt '!%)
               (eqcar (cdr fmt) 'w))
          (setq r (append (reverse (explodec (car arglist))) r))
          (setq arglist (cdr arglist))
          (setq fmt (cddr fmt)))
         (t (setq r (cons (car fmt) r))
            (setq fmt (cdr fmt)))))
    (return (list2string (reversip r)))))

(dm bldmsg (u) (list 'bldmsg1 (list 'explodec (cadr u)) (cons 'list (cddr u))))

(de printprompt (u) nil)
(flag '(printprompt) 'lose)

"End of vsl.lsp"

% End of vsl.lsp

