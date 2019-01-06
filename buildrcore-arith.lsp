% buildrcore.lsp

% This builds a version of Reduce that has the "most important" modules
% included so that a useful range of tests can be performed. It can also
% serve as a base upon which futher parts of the code can be build for
% more advanced testing.


% "buildreduce.lsp"                        Copyright (C) Codemist 2016-2018
%
% Build a CSL REDUCE.
%
% Depending on how this file is used it will EITHER create a bootstrap
% version of REDUCE or a full and optimised one.
%
% The behaviour is determined by whether the version of CSL used to
% run it has a full complement of functions in the modules u01.c to u60.c.
%
%
%           bootstrapreduce -z buildreduce.lsp -D@srcdir=<DIR> -D@reduce=<DIR>
%
% Builds a system "bootstrapreduce.img" that does not depend on any
% custom C code. The main use of this slow system is for profiling
% REDUCE and then compiling the hot-spots into C. Once that has been
% done this image is logically unnecessary.
%
%
%           reduce -z buildreduce.lsp -D@srcdir=<DIR> -D@reduce=<dir>
%
% Here the files u01.c to u60.c and u01.lsp to u60.lsp must already
% have been created, and that the reduce executable has them compiled in.
% The REDUCE source files that are compiled *MUST* be the same as those used
% to create this C code.

% Author: Anthony C. Hearn, Stanley L. Kameny and Arthur Norman

% $Id: buildreduce.lsp 4265 2017-11-12 13:36:25Z arthurcnorman $

(cond
   ((eq 'vsl (car lispsystem!*)) (rdf "$srcdir/vsl-arith.lsp")))

(verbos 3)

(window!-heading "basic CSL")

(make!-special '!*savedef)
(make!-special '!*backtrace)

(setq !*savedef (and (not (memq 'embedded lispsystem!*))
                     (zerop (cdr (assoc 'c!-code lispsystem!*)))))

(make!-special '!*noinlines)
(prog (w)
   (setq w (errorset 'noinlines nil nil))
   (setq !*noinlines (and (not (atom w)) (car w)))
   (print (list '!*noinlines 'set 'to !*noinlines)))

% A command-line flag "-g" sets the variable !*backtrace and so can activate
% this. But beware that otherwise !*backtrace may be an unset variable, and
% so I use an errorset to protect myself.

(prog (w)
   (setq w (errorset '!*backtrace nil nil))
   (cond
      ((or (atom w) (null (car w))) (setq !*backtrace nil))
      (t (enable!-errorset 3 3))))

(cond
   (!*backtrace (setq !*echo t)))

(make!-special '!*native_code)
(setq !*native_code nil)

(cond ((and (null !*savedef)
       (null (memq 'vsl lispsystem!*))
       (null (memq 'jlisp lispsystem!*))
       (null (memq 'embedded lispsystem!*))) (progn

   (de c!:install (name env c!-version !&optional c1)
      (cond
        (c1 (check!-c!-code name env c!-version c1))
        (t (progn
              (put name 'c!-version c!-version)
              (cond (env (prog (v n)
                 (setq v (mkvect (sub1 (length env))))
                 (setq n 0)
            top  (cond
                    ((null env) (progn
                     (put name 'funarg v)
                     (return (symbol!-set!-env name v)))))
                 (putv v n (car env))
                 (setq n (add1 n))
                 (setq env (cdr env))
                 (go top))))
              name))))

   (rdf "$reduce/cslbuild/generated-c/u01.lsp")
   (rdf "$reduce/cslbuild/generated-c/u02.lsp")
   (rdf "$reduce/cslbuild/generated-c/u03.lsp")
   (rdf "$reduce/cslbuild/generated-c/u04.lsp")
   (rdf "$reduce/cslbuild/generated-c/u05.lsp")
   (rdf "$reduce/cslbuild/generated-c/u06.lsp")
   (rdf "$reduce/cslbuild/generated-c/u07.lsp")
   (rdf "$reduce/cslbuild/generated-c/u08.lsp")
   (rdf "$reduce/cslbuild/generated-c/u09.lsp")
   (rdf "$reduce/cslbuild/generated-c/u10.lsp")
   (rdf "$reduce/cslbuild/generated-c/u11.lsp")
   (rdf "$reduce/cslbuild/generated-c/u12.lsp")
   (rdf "$reduce/cslbuild/generated-c/u13.lsp")
   (rdf "$reduce/cslbuild/generated-c/u14.lsp")
   (rdf "$reduce/cslbuild/generated-c/u15.lsp")
   (rdf "$reduce/cslbuild/generated-c/u16.lsp")
   (rdf "$reduce/cslbuild/generated-c/u17.lsp")
   (rdf "$reduce/cslbuild/generated-c/u18.lsp")
   (rdf "$reduce/cslbuild/generated-c/u19.lsp")
   (rdf "$reduce/cslbuild/generated-c/u20.lsp")
   (rdf "$reduce/cslbuild/generated-c/u21.lsp")
   (rdf "$reduce/cslbuild/generated-c/u22.lsp")
   (rdf "$reduce/cslbuild/generated-c/u23.lsp")
   (rdf "$reduce/cslbuild/generated-c/u24.lsp")
   (rdf "$reduce/cslbuild/generated-c/u25.lsp")
   (rdf "$reduce/cslbuild/generated-c/u26.lsp")
   (rdf "$reduce/cslbuild/generated-c/u27.lsp")
   (rdf "$reduce/cslbuild/generated-c/u28.lsp")
   (rdf "$reduce/cslbuild/generated-c/u29.lsp")
   (rdf "$reduce/cslbuild/generated-c/u30.lsp")
   (rdf "$reduce/cslbuild/generated-c/u31.lsp")
   (rdf "$reduce/cslbuild/generated-c/u32.lsp")
   (rdf "$reduce/cslbuild/generated-c/u33.lsp")
   (rdf "$reduce/cslbuild/generated-c/u34.lsp")
   (rdf "$reduce/cslbuild/generated-c/u35.lsp")
   (rdf "$reduce/cslbuild/generated-c/u36.lsp")
   (rdf "$reduce/cslbuild/generated-c/u37.lsp")
   (rdf "$reduce/cslbuild/generated-c/u38.lsp")
   (rdf "$reduce/cslbuild/generated-c/u39.lsp")
   (rdf "$reduce/cslbuild/generated-c/u40.lsp")
   (rdf "$reduce/cslbuild/generated-c/u41.lsp")
   (rdf "$reduce/cslbuild/generated-c/u42.lsp")
   (rdf "$reduce/cslbuild/generated-c/u43.lsp")
   (rdf "$reduce/cslbuild/generated-c/u44.lsp")
   (rdf "$reduce/cslbuild/generated-c/u45.lsp")
   (rdf "$reduce/cslbuild/generated-c/u46.lsp")
   (rdf "$reduce/cslbuild/generated-c/u47.lsp")
   (rdf "$reduce/cslbuild/generated-c/u48.lsp")
   (rdf "$reduce/cslbuild/generated-c/u49.lsp")
   (rdf "$reduce/cslbuild/generated-c/u50.lsp")
   (rdf "$reduce/cslbuild/generated-c/u51.lsp")
   (rdf "$reduce/cslbuild/generated-c/u52.lsp")
   (rdf "$reduce/cslbuild/generated-c/u53.lsp")
   (rdf "$reduce/cslbuild/generated-c/u54.lsp")
   (rdf "$reduce/cslbuild/generated-c/u55.lsp")
   (rdf "$reduce/cslbuild/generated-c/u56.lsp")
   (rdf "$reduce/cslbuild/generated-c/u57.lsp")
   (rdf "$reduce/cslbuild/generated-c/u58.lsp")
   (rdf "$reduce/cslbuild/generated-c/u59.lsp")
   (rdf "$reduce/cslbuild/generated-c/u60.lsp")
    )))

(fluid '(!*nocompile))
(setq !*nocompile nil)
(cond
  ((and (boundp 'interpreted) (eq (compress (explodec interpreted)) 'yes))
   (setq !*nocompile t)))
(progn (terpri)
       (princ "### !*nocompile = ")
       (print !*nocompile)
       nil)

(setq !*comp (null !*nocompile))

(cond
  ((memq 'vsl lispsystem!*)
   (faslout 'cslcompat)
% Ha ha. faslout does not support this usage!
   (rdf "$srcdir/vsl-arith.lsp")
   (faslend))
  (t
    (rdf "$srcdir/fastgets.lsp")
    (rdf "$srcdir/compat.lsp")
    (rdf "$srcdir/extras.lsp")
    (rdf (cond
      ((memq 'jlisp lispsystem!*) "$srcdir/compiler-for-jlisp.lsp")
      (t "$srcdir/compiler.lsp")))
% Compile some important things first to improve bootstrapping speed.
    (cond
      ((null !*nocompile)
       (compile '(
         s!:improve s!:literal_order s!:comval s!:outopcode0
         s!:plant_basic_block s!:remlose s!:islocal
         s!:is_lose_and_exit s!:comatom s!:destination_label
         s!:record_literal s!:resolve_labels s!:expand_jump
         s!:outopcode1lit stable!-sortip s!:iseasy s!:outjump
         s!:add_pending s!:comcall s!:resolve_literals)))
       (compile!-all))

    (setq !*comp t)
% Tidy up be deleting any modules that are left over in this image
    (dolist (a (library!-members)) (delete!-module a))
% Build fasl files for the compatibility code and the two
% versions of the compiler.
    (faslout 'cslcompat)
    (rdf "$srcdir/fastgets.lsp")
    (rdf "$srcdir/compat.lsp")
    (rdf "$srcdir/extras.lsp")
    (faslend)

    (faslout 'compiler)
    (rdf (cond
      ((memq 'jlisp lispsystem!*) "$srcdir/compiler-for-jlisp.lsp")
      (t "$srcdir/compiler.lsp")))
    (faslend)

    (setq !*comp (null !*nocompile))
    ))

(de concat (u v)
    (compress (cons '!" (append (explode2 u)
                                (nconc (explode2 v) (list '!"))))))

(global '(oldchan!*))

(setq prolog_file 'cslprolo)

(setq rend_file 'cslrend)

(setq !*argnochk t)

(setq !*int nil)                    % Prevents input buffer being saved.

(setq !*msg nil)

(window!-heading "bootstrap RLISP")

% This is dervived fron the Standard LISP BOOT File.
% Author: Anthony C. Hearn.
% Copyright (c) 1991 RAND.  All Rights Reserved.

(fluid '(fname!* !*blockp !*lower !*mode))

(global '(oldchan!*))

(fluid '(!*raise !*lower))

(global '(crchar!* cursym!* nxtsym!* ttype!* !$eol!$))

(put '!; 'switch!* '(nil !*semicol!*))

(put '!( 'switch!* '(nil !*lpar!*))

(put '!) 'switch!* '(nil !*rpar!*))

(put '!, 'switch!* '(nil !*comma!*))

(put '!. 'switch!* '(nil cons))

(put '!= 'switch!* '(nil equal))

(put '!: 'switch!* '(((!= nil setq)) !*colon!*))

(put '!< 'switch!* '(((!= nil leq) (!< nil !*lsqbkt!*)) lessp))

(put '!> 'switch!* '(((!= nil geq) (!> nil !*rsqbkt!*)) greaterp))

% When the full parser is loaded the function mkprec() will reset all
% these precedences. Until then please parenthesize expressions carefully.

(put '!*comma!* 'infix 1)

(put 'setq 'infix 2)

(put 'cons 'infix 3)

(put 'equal 'infix 4)

(put 'eq 'infix 5)

(flag '(!*comma!*) 'nary)

(flag '(!*colon!* !*semicol!* end then else) 'delim)

(put 'begin 'stat 'blockstat)

(put 'if 'stat 'ifstat)

(put 'symbolic 'stat 'procstat)

(de begin2 nil
   (prog nil
      (setq cursym!* '!*semicol!*)
a     (cond
         ((eq cursym!* 'end) (progn (rds oldchan!*) (return nil)))
         (t (prin2 (errorset '(eval (form (xread nil))) t t)) ))
      (go a)))

(de form (u) u)

(de xread (u) (progn (scan) (xread1 u)))

(de xread1 (u)
   (prog (v w x y z z2)
a     (setq z cursym!*)
a1    (cond
         ((or (null (atom z)) (numberp z)) (setq y nil))
         ((flagp z 'delim) (go end1))
         ((eq z '!*lpar!*) (go lparen))
         ((eq z '!*rpar!*) (go end1))
         ((and w (setq y (get z 'infix))) (go infx))
         ((setq y (get z 'stat)) (go stat)))
a3    (setq w (cons z w))
next  (setq z (scan))
      (go a1)
lparen(setq y nil)
      (cond
         ((eq (scan) '!*rpar!*)
            (and w (setq w (cons (list (car w)) (cdr w)))) )
         ((eqcar (setq z (xread1 'paren)) '!*comma!*)
            (setq w (cons (cons (car w) (cdr z)) (cdr w))))
         (t (go a3)))
      (go next)
infx  (setq z2 (car w))
un1   (setq w (cdr w))
      (cond
         ((null w) (go un2))
         (t (setq z2 (cons (car w) (list z2)))) )
      (go un1)
un2   (setq v (cons z2 v))
preced(cond ((null x) (go pr4)) ((lessp y (car (car x))) (go pr2)))
pr1   (setq x (cons (cons y z) x))
      (go next)
pr2   (setq v
         (cons
            (cond
               ((and (eqcar (car v) (cdar x)) (flagp (cdar x) 'nary))
                  (cons (cdar x) (cons (cadr v) (cdar v))))
               (t (cons (cdar x) (list (cadr v) (car v)))) )
            (cdr (cdr v))))
      (setq x (cdr x))
      (go preced)
stat  (setq w (cons (eval (list y)) w))
      (setq y nil)
      (go a)
end1  (cond
         ((and (and (null v) (null w)) (null x)) (return nil))
         (t (setq y 0)))
      (go infx)
pr4   (cond ((null (equal y 0)) (go pr1)) (t (return (car v)))) ))

(de eqcar (u v) (and (null (atom u)) (eq (car u) v)))

(de mksetq (u v) (list 'setq u v))

(de rread nil
   (prog (x)
      (setq x (token))
      (return
         (cond
            ((and (equal ttype!* 3) (eq x '!()) (rrdls))
            (t x)))) )

(de rrdls nil
   (prog (x r)
a     (setq x (rread))
      (cond
         ((null (equal ttype!* 3)) (go b))
         ((eq x '!)) (return (reverse r)))   % REVERSIP not yet defined.
         ((null (eq x '!.)) (go b)))
      (setq x (rread))
      (token)
      (return (nconc (reverse r) x))
b     (setq r (cons x r))
      (go a)))

(de token nil
   (prog (x y z)
      (setq x crchar!*)
a     (cond
         ((seprp x) (go sepr))
         ((digit x) (go number))
         ((liter x) (go letter))
         ((eq x '!%) (go coment))
         ((eq x '!!) (go escape))
         ((eq x '!') (go quote))
         ((eq x '!") (go string)))
      (setq ttype!* 3)
      (cond ((delcp x) (go d)))
      (setq nxtsym!* x)
a1    (setq crchar!* (readch))
      (go c)
escape(setq y (cons x y))
      (setq z (cons !*raise !*lower))
      (setq !*raise (setq !*lower nil))
      (setq x (readch))
      (setq !*raise (car z))
      (setq !*lower (cdr z))
letter(setq ttype!* 0)
let1  (setq y (cons x y))
      (cond
         ((or (digit (setq x (readch))) (liter x)) (go let1))
         ((eq x '!!) (go escape)))
      (setq nxtsym!* (intern (compress (reverse y))))
b     (setq crchar!* x)
c     (return nxtsym!*)
number(setq ttype!* 2)
num1  (setq y (cons x y))
      (cond ((digit (setq x (readch))) (go num1)))
      (setq nxtsym!* (compress (reverse y)))
      (go b)
quote (setq crchar!* (readch))
      (setq nxtsym!* (list 'quote (rread)))
      (setq ttype!* 4)
      (go c)
string(prog (raise !*lower)
         (setq raise !*raise)
         (setq !*raise nil)
   strinx(setq y (cons x y))
         (cond ((null (eq (setq x (readch)) '!")) (go strinx)))
         (setq y (cons x y))
         (setq nxtsym!* (mkstrng (compress (reverse y))))
         (setq !*raise raise))
      (setq ttype!* 1)
      (go a1)
coment(cond ((null (eq (readch) !$eol!$)) (go coment)))
sepr  (setq x (readch))
      (go a)
d     (setq nxtsym!* x)
      (setq crchar!* '! )
      (go c)))

(setq crchar!* '! )

(de delcp (u) (or (eq u '!;) (eq u '!$)))

(de mkstrng (u) u)

(de seprp (u) (or (eq u blank) (eq u tab) (eq u !$eol!$)))

(de scan nil
   (prog (x y)
      (cond ((null (eq cursym!* '!*semicol!*)) (go b)))
a     (setq nxtsym!* (token))
b     (cond
         ((or (null (atom nxtsym!*)) (numberp nxtsym!*)) (go l))
         ((and (setq x (get nxtsym!* 'newnam)) (setq nxtsym!* x))
            (go b))
         ((eq nxtsym!* 'comment) (go comm))
         ((and
             (eq nxtsym!* '!')
             (setq cursym!* (list 'quote (rread))))
            (go l1))
         ((null (setq x (get nxtsym!* 'switch!*))) (go l))
         ((eq (cadr x) '!*semicol!*)
            (return (setq cursym!* (cadr x)))) )
sw1   (setq nxtsym!* (token))
      (cond
         ((or
             (null (car x))
             (null (setq y (assoc nxtsym!* (car x)))) )
            (return (setq cursym!* (cadr x)))) )
      (setq x (cdr y))
      (go sw1)
comm  (cond ((eq (readch) '!;) (setq crchar!* '! )) (t (go comm)))
      (go a)
l     (setq cursym!*
         (cond
            ((null (eqcar nxtsym!* 'string)) nxtsym!*)
            (t (cons 'quote (cdr nxtsym!*)))) )
l1    (setq nxtsym!* (token))
      (return cursym!*)))

(de ifstat nil
   (prog (condx condit)
a     (setq condx (xread t))
      (setq condit (nconc condit (list (list condx (xread t)))) )
      (cond
         ((null (eq cursym!* 'else)) (go b))
         ((eq (scan) 'if) (go a))
         (t (setq condit
               (nconc condit (list (list t (xread1 t)))) )))
b     (return (cons 'cond condit))))

(de procstat nil
   (prog (x y)
      (cond ((eq cursym!* 'symbolic) (scan)))
      (cond
         ((eq cursym!* '!*semicol!*)
            (return (null (setq !*mode 'symbolic)))) )
      (setq fname!* (scan))
      (cond ((atom (setq x (xread1 nil))) (setq x (list x))))
      (setq y (xread nil))
      (cond ((flagp (car x) 'lose) (return nil)))
      (putd (car x) 'expr (list 'lambda (cdr x) y))
      (setq fname!* nil)
      (return (list 'quote (car x)))) )

(de blockstat nil
   (prog (x hold varlis !*blockp)
a0    (setq !*blockp t)
      (scan)
      (cond
         ((null (or (eq cursym!* 'integer) (eq cursym!* 'scalar)))
            (go a)))
      (setq x (xread nil))
      (setq varlis
         (nconc
            (cond ((eqcar x '!*comma!*) (cdr x)) (t (list x)))
            varlis))
      (go a0)
a     (setq hold (nconc hold (list (xread1 nil))))
      (setq x cursym!*)
      (scan)
      (cond ((not (eq x 'end)) (go a)))
      (return (mkprog varlis hold))))

(de mkprog (u v) (cons 'prog (cons u v)))

(de gostat nil
   (prog (x) (scan) (setq x (scan)) (scan) (return (list 'go x))))

(put 'go 'stat 'gostat)

(de rlis nil
   (prog (x)
      (setq x cursym!*)
      (return (list x (list 'quote (list (xread t)))))))

(de endstat nil (prog (x) (setq x cursym!*) (scan) (return (list x))))

% It is only a rather small number of lines of code to support
% both << >> blocks and WHILE statements here, and doing so makes
% it possible to write the full implementation of RLISP in a much
% more civilised way. What I put in here is a little more than is used
% to start with, but matches the eventual implementation. Eg the 'go
% and 'nodel flags are not relevant until the read parser has been loaded.

(de readprogn nil
   (prog (lst)
   a  (setq lst (cons (xread 'group) lst))
      (cond ((null (eq cursym!* '!*rsqbkt!*)) (go a)))
      (scan)
      (return (cons 'progn (reverse lst))))) 

(put '!*lsqbkt!* 'stat 'readprogn)
(flag '(!*lsqbkt!*) 'go)
(flag '(!*rsqbkt!*) 'delim)
(flag '(!*rsqbkt!*) 'nodel)

(de whilstat ()
   (prog (!*blockp bool bool2)
      (cond
         ((flagp 'do 'delim) (setq bool2 t))
         (t (flag '(do) 'delim)))
      (setq bool (xread t))
      (cond
         (bool2 (remflag '(do) 'delim)))
      (cond
         ((not (eq cursym!* 'do)) (symerr 'while t)))
      (return (list 'while bool (xread t)))))

(dm while (u)
   (prog (body bool lab)
      (setq bool (cadr u))
      (setq body (caddr u))
      (setq lab 'whilelabel) 
      (return (list
         'prog nil
    lab  (list 'cond
            (list (list 'not bool) '(return nil)))
         body
         (list 'go lab)))))

(put 'while 'stat 'whilstat)
(flag '(while) 'nochange)

(de repeatstat ()
   (prog (!*blockp body bool)
      (cond
         ((flagp 'until 'delim) (setq bool t))
         (t (flag '(until) 'delim)))
      (setq body (xread t))
      (cond
         ((null bool) (remflag '(until) 'delim)))
      (cond
         ((not (eq cursym!* 'until)) (symerr 'repeat t)))
      (return (list 'repeat body (xread t)))))

(dm repeat (u)
   (progn (terpri) (print (prog (body bool lab)
      (setq body (cadr u))
      (setq bool (caddr u))
      (setq lab 'repeatlabel) 
      (return (list
      'prog nil
    lab  body
         (list 'cond
            (list (list 'not bool) (list 'go lab))))))))))

(put 'repeat 'stat 'repeatstat)
(flag '(repeat) 'nochange)

% Now we have just enough to be able to start to express ourselves in
% (a subset of) rlisp.

(begin2)

rds(xxx := open("$reduce/packages/support/build.red", 'input));

(close xxx)

(load!-package!-sources prolog_file 'support)
(load!-package!-sources 'revision 'support)

(load!-package!-sources 'rlisp 'rlisp)
(load!-package!-sources 'smacros 'support)
(load!-package!-sources rend_file 'support)
(load!-package!-sources 'poly 'poly)
(load!-package!-sources 'polydiv 'poly)
(load!-package!-sources 'alg 'alg)
(load!-package!-sources 'arith 'arith)
(load!-package!-sources 'mathpr 'mathpr)
(load!-package!-sources 'ezgcd 'factor)
(load!-package!-sources 'factor 'factor)
(load!-package!-sources 'hephys 'hephys)
(load!-package!-sources 'int 'int)
(load!-package!-sources 'matrix 'matrix)
(load!-package!-sources 'solve 'solve)
(load!-package!-sources 'desir 'solve)
(load!-package!-sources 'ineq 'solve)
(load!-package!-sources 'modsr 'solve)
(load!-package!-sources 'rsolve 'solve)
(load!-package!-sources 'odesolve 'odesolve)
(load!-package!-sources 'roots 'roots)
(load!-package!-sources 'roots2 'roots)
(load!-package!-sources 'dipoly 'dipoly)
(load!-package!-sources 'groebner 'groebner)

(load!-package!-sources 'entry 'support)
(load!-package!-sources 'remake 'support)

(load!-package!-sources 'specfn 'specfn)
(load!-package!-sources 'specfn2 'specfn)
(load!-package!-sources 'specfaux 'specfn)
(load!-package!-sources 'specbess 'specfn)
(load!-package!-sources 'sfgamma 'specfn)


% The next line is a HACK and is (I hope) temporary.
(setq largest!-small!-modulus (expt 10 100))
(initreduce)
(preserve 'begin "Rcore" nil)
(stop 0)

