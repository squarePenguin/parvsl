% A first sketch of a minimalist Groebner Base package


% At present this generates a Groebner base using monic polynomials
% (which therefore have rational number coefficients). It uses just
% lexographic order using some variables whose names are in the list
% varnames).
%
% It has not worried too much about sophistication or speed anywhere!

symbolic;
on echo, backtrace, comp;

if getd 'spool then spool "babygroe.log";

% I will put the main and central and important part first!

symbolic procedure babygroe L;
  begin
    scalar pairs;
    L := for each p in L collect dfmake_monic p;
    pairs := for each p on L conc
      for each q in cdr p collect (car p . q);
    terpri();
    printc "Babygroe input:";
    for each p in L do << dfprin p; terpri() >>;
    while pairs do begin
      scalar s := s_poly(caar pairs, cdar pairs);
      if !*noisy then <<
        princ "Raw s-poly = "; dfprin s; terpri() >>;
      pairs := cdr pairs;
      s := reduce_by(s, L);
      if !*noisy then <<
        princ "Reduced s-poly = "; dfprin s; terpri()>>;
      if not null s then begin
        scalar L1 := L;  % because I will update L as I scan it.
% Now if any polynomial in the existing base would be divisible by the new
% element I should remove it and all pending pairs using it. Doing this
% should let me end up with a miminal basis.
        for each p in L1 do
          if not xless(dfx p, dfx s) then <<
            if !*noisy then <<
              princ "I can now discard "; dfprin p >>;
            L := delete(p, L);
            pairs := delete_pairlist(p, pairs) >>;
        for each p in L do pairs := (s . p) . pairs;
        if !*noisy then <<
          princ "Add new poly into the base: "; dfprin s; terpri() >>;
        L := s . L end
    end;
    terpri();
    printc "The base is:";
    for each p in L do <<
      dfprin p;
      terpri() >>;
    return L
  end;


% First I will want support for distributed polynomials.

global '(varnames);

% A polynomial will be represented as a list of terms.
% Each term will be (C . X) where C is a coefficient - a rational
% number represented as (p . q). X is an exponent vector stored
% as a list. The names of the variables are not mentioned in this list,
% but are kept in varnames.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start with rational number arithmetic

% Create a rational number if values are known to be co-prime.

symbolic inline procedure q(p, q);
  p . q;

% Create a rational number, reducing to lowest terms and forcing denominator
% to be positive.

symbolic procedure qreduce(p, q);
  begin
    scalar g := gcdn(p, q);
    p := p/g;
    q := q/g;
    if q < 0 then <<
      p := -p;
      q := -q >>;
    return q(p, q);
  end;

symbolic inline procedure qnum u; car u;
symbolic inline procedure qden u; cdr u;

% test for zero

symbolic inline procedure qzerop u;
  zerop qnum u;

% negate a number

symbolic procedure qneg u;
  q(-qnum u, qden u);

% add two numbers

symbolic procedure qadd(u, v);
  begin
    scalar g := gcdn(qden u, qden v), p, q;
    p := qnum u * (qden v / g) + qnum v * (qden u / g);
    q := qden u * (qden v / g);
    return qreduce(p, q);
  end;

% subtract

symbolic procedure qsub(u, v);
  begin
    scalar g := gcdn(qden u, qden v), p, q;
    p := qnum u * (qden v / g) - qnum v * (qden u / g);
    q := qden u * (qden v / g);
    return qreduce(p, q);
  end;

% multiply

symbolic procedure qmul(u, v);
  begin
    scalar g1 := gcdn(qnum u, qden v),
           g2 := gcdn(qnum u, qden u);
    return qreduce((qnum u/g1)*(qnum v/g2),
                   (qden u/g2)*(qden v/g1));
  end;

symbolic procedure qdiv(u, v);
  begin
    scalar g1 := gcdn(qnum u, qnum v),
           g2 := gcdn(qden v, qden u);
    if qnum u < 0 then g1 := -g1;
    return qreduce((qnum u/g1)*(qden v/g2),
                   (qden u/g2)*(qnum v/g1));
  end;

symbolic procedure qrecip u;
  if minusp qnum u then q(-qden u, -qnum u)
  else q(qden u, qnum u);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

varnames := '(x y z);

symbolic procedure zerox();
  for each v in varnames collect 0;

symbolic procedure varx name;
  for each v in varnames collect (if v = name then 1 else 0);

% comparison of exponent vectors. Both lists must be the same length.

symbolic procedure xgreaterp(L1, L2);
  if null L1 then nil
  else if car L1 > car L2 then t
  else if car L1 < car L2 then nil
  else xgreaterp(cdr L1, cdr L2);

symbolic inline procedure dflt u;
  car u;

symbolic procedure negterm w;
  (qneg car w) . cdr w;

symbolic inline procedure dflc u;
  caar u;

symbolic inline procedure dfx  u;
  cdar u;

symbolic inline procedure dfred u;
  cdr u;

symbolic procedure dfneg u;
  if null u then nil
  else (qneg dflc u . dfx u) . dfneg dfred u;

% add two distributed forms

symbolic procedure dfadd(u, v);
  if null u then v
  else if null v then u
  else if dfx u = dfx v then begin
    scalar c := qadd(dflc u, dflc v); % add leading coeffs
    if qzerop c then return dfadd(dfred u, dfred v)
    else return (c . dfx u) . dfadd(dfred u, dfred v)
    end
  else if xgreaterp(dfx u, dfx v) then dflt u . dfadd(dfred u, v)
  else dflt v . dfadd(u, dfred v);

symbolic procedure dfsub(u, v);
  if null u then dfneg v
  else if null v then u
  else if dfx u = dfx v then begin
    scalar c := qsub(dflc u, dflc v); % subtract leading coeffs
    if qzerop c then return dfsub(dfred u, dfred v)
    else return (c . dfx u) . dfsub(dfred u, dfred v)
    end
  else if xgreaterp(dfx u, dfx v) then dflt u . dfsub(dfred u, v)
  else (negterm dflt v) . dfsub(u, dfred v);

symbolic procedure addxvec(L1, L2);
  if null L1 then nil
  else (car L1 + car L2) . addxvec(cdr L1, cdr L2);

symbolic procedure dfmuln(c, u);
  if null u then nil
  else (qmul(c, dflc u) . dfx u) . dfmuln(c, dfred u);

% Make a polynomial monic.

symbolic procedure dfmake_monic u;
  if null u then nil
  else if dflc u = '(1 . 1) then u
  else dfmuln(qrecip dflc u, u);

% Multiply u by the term with coefficient c and and exponent vector x

symbolic procedure dfmulterm(c, x, u);
  if null u then nil
  else (qmul(c, dflc u) . addxvec(x, dfx u)) . dfmulterm(c, x, dfred u);

symbolic procedure dfmul(u, v);
  if null u then nil
  else dfadd(dfmulterm(dflc u, dfx u, v),
             dfmul(dfred u, v));

% Raise to an integer power

symbolic procedure dfexpt(u, n);
  if n = 0 then (q(1,1) . zerox()) . nil
  else if n = 1 then u
  else dfmul(u, dfexpt(u, n-1));

symbolic procedure prefix_to_df w;
  if numberp w then (q(w, 1) . zerox()) . nil
  else if atom w then (q(1, 1) . varx w) . nil
  else if eqcar(w, 'plus) then begin
    scalar r := prefix_to_df cadr w;
    for each u in cddr w do r := dfadd(r, prefix_to_df u);
    return r end
  else if eqcar(w, 'difference) then
    dfsub(prefix_to_df cadr w, prefix_to_df caddr w)
  else if eqcar(w, 'minus) then dfneg prefix_to_df cadr w
  else if eqcar(w, 'times) then begin
    scalar r := prefix_to_df cadr w;
    for each u in cddr w do r := dfmul(r, prefix_to_df u);
    return r end
  else if eqcar(w, 'expt) then
    dfexpt(prefix_to_df cadr w, caddr w)
  else error(1, list("Invalid input", w));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

symbolic procedure xallzero v;
  if null v then t
  else if not zerop car v then nil
  else xallzero cdr v;

symbolic procedure prinxvec(v, names);
  if null v then nil
  else if zerop car v then prinxvec(cdr v, cdr names)
  else << princ " ";
          princ car names;
          if not onep car v then <<
            princ "^";
            princ car v >>;
          prinxvec(cdr v, cdr names) >>;

symbolic procedure dfprin1 u;
  if null u then terpri()
  else <<
    if dflc u = '(1 . 1) then <<
       princ " + ";
       if xallzero dfx u then princ "1"
       else prinxvec(dfx u, varnames) >>
    else if dflc u = '(-1 . 1) then <<
       princ " - ";
       if xallzero dfx u then princ "1"
       else prinxvec(dfx u, varnames) >>
    else <<
       if qnum dflc u > 0 then princ " + " else princ " - ";
       princ abs qnum dflc u;
       if qden dflc u neq 1 then << princ "/"; princ qden dflc u >>;
       prinxvec(dfx u, varnames) >>;
    dfprin1 dfred u >>;

symbolic procedure dfprin u;
  << if null u then print 0 else dfprin1 u >>;

<< terpri(); dfprin prefix_to_df '(expt (plus x y 1) 3) >>;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This finds the extra exponents needed to make L1 at least as large
% as L2.

symbolic procedure xtrax(L1, L2);
  if null L1 then nil
  else if car L1 < car L2 then (car L2 - car L1) . xtrax(cdr L1, cdr L2)
  else 0 . xtrax(cdr L1, cdr L2);

symbolic procedure s_poly(u, v);
  begin
    scalar u1 := xtrax(dfx u, dfx v),
           v1 := xtrax(dfx v, dfx u);
% The leading term of each of u*u1 and v*v1 should have a degree max() of the
% degrees in each variable in u and v.
    u := dfmulterm(qrecip dflc u, u1, u);
    v := dfmulterm(qrecip dflc v, v1, v);
% Leading terms should now cancel when I subtract.
    return dfmake_monic dfsub(u, v)
  end;

% If any value in L1 is less then the one in L2 return true
symbolic procedure xless(L1, L2);
  if null L1 then nil
  else if car L1 < car L2 then t
  else xless(cdr L1, cdr L2);

symbolic procedure xdiff(L1, L2);
  if null L1 then nil
  else (car L1 - car L2) . xdiff(cdr L1, cdr L2);

symbolic procedure dfremainder(u, v);
  begin
    while not null u and
          not xless(dfx u, dfx v) do <<
      u := dfmake_monic dfsub(u,
                              dfmulterm(1 . 1, xdiff(dfx u, dfx v), v)) >>;
    return u
  end;

global '(!*noisy);

!*noisy := nil; % Change for debugging!

symbolic procedure reduce_by(S, L);
  begin
    scalar done := nil, s1;
    while not done do <<
      done := t;
      for each p in L do <<
% If something has reduced to 0 there is no need to try further reductions!
        if not null s then <<
          if !*noisy then <<
            princ "Reduce "; dfprin s; princ " using "; dfprin p >>;
          s1 := dfremainder(s, p);
          if !*noisy then <<
            princ " => "; dfprin s; terpri() >>;
% test if this made progress but did not reduce the polynomial all the way
% down to nil, set the "done" flag to nil so that we will try everything
% again.
          if s1 neq s then <<
            if s1 neq nil then done := nil;
            s := s1 >> >> >> >>;
    return s
  end;

symbolic procedure delete_pairlist(p, L);
  if null L then nil
  else if p = caar L or p = cdar L then delete_pairlist(p, cdr L)
  else car L . delete_pairlist(p, cdr L);

% Now partially hook this onto Reduce.

symbolic procedure babygroeeval u;
  begin
    if null u or cdr u then rederr "babygroe only expects 1 argument";
    u := prepsq simp car u;
    if not eqcar(u, 'list) then rederr "babygroe expects a list as an argument";
    u := babygroe (for each v in cdr u collect prefix_to_df v);
% At present I do not convert the output from babygroe back into a prefix form
% or an (!*sq..) form to return - I just print it. But the code I have for
% displaying distributed forms could easily be adapted to return a prefix
% representation of the list of polynomials.
    print u;
% Given nothing better to do I return the fixed value 42.
    return 42
  end;

put('babygroe, 'psopfn, 'babygroeeval); 

algebraic;

% Examples

on time;

% First S-poly should start off as -x^2+y^2 and that reduces to y^2-y
% When we have added that to the set all the rest of the S-polys we compute
% reduce to 0, so we are finished.

lisp << !*noisy := t >>;

% babygroe {x*y-x, x^2-y};
% 
% babygroe {x^3 - 2*x*y,
%           x^2*y - 2*y^2 + x};

lisp << !*noisy := nil >>;

% lisp << varnames := '(a0 a1 a2 a3 a4 a5 a6 a7 a8) >>;

% lisp verbos t;
% 
% babygroe {
%   a1^2 - 1,
%   a2^2 - a1 - 1,
%   a3^2 - a2 - 1,
%   a4^2 - a3 - 1,
%   a5^2 - a4 - 1,
%   a6^2 - a5 - 1,
%   a7^2 - a6 - 1
%   };

% lisp << varnames := '(v8 v7 v4 v3 v2 v1 v5 v6) >>;
% 
% babygroe {
%     77-25*v2
%   , v8^2-1*v6^2-1*v5^2+2*v6*v2-1*v2^2+2*v5*v1-1*v1^2
%   , -93-100*v4
%   , 3+2*v1
%   , v7^2-1*v6^2-1*v5^2+2*v6*v4-1*v4^2+2*v5*v3-1*v3^2
%   , -223-100*v3
%   , 625-50*v8^2+v8^4-50*v7^2-2*v8^2*v7^2+v7^4
% };

lisp << varnames := '(v26 v24 v25 v23 v20 v18 v19 v16 v17 v15 v10 v9 v8 v7 v6 v5 v4 v3 v2 v1 v13 v14) >>;

babygroe {
  -1*v9
, -1*v7
, -1+v10
, v8
, -1+v5
, v3
, v6
, v4
, v25^2*v24^2+-1*v26^2*v23^2
, -1*v18+v16+v5+-1*v3
, -1*v17+v15+-1*v6+v4
, v19*v18+-1*v20*v17+-1*v19*v16+v17*v16+v20*v15+-1*v18*v15
, -1*v6+v4+-1*v5*v4+v6*v3
, -1*v19*v6+v20*v5+v19*v4+-1*v5*v4+-1*v20*v3+v6*v3
, v24^2+-1*v16^2+-1*v15^2+2*v16*v14+-1*v14^2+2*v15*v13+-1*v13^2
, -1+v25^2+-1*v20^2+2*v19+-1*v19^2
, -1*v5*v4+v6*v3+v5*v2+-1*v3*v2+-1*v6*v1+v4*v1
, v23^2+-1*v16^2+-1*v15^2+2*v16*v2+-1*v2^2+2*v15*v1+-1*v1^2
, -1*v9*v8+v10*v7+v9*v2+-1*v7*v2+-1*v10*v1+v8*v1
, v26^2+-1*v20^2+-1*v19^2+2*v20*v2+-1*v2^2+2*v19*v1+-1*v1^2
, 1+-1*v16^2+-1*v15^2+2*v16*v2+-2*v1+2*v15*v1
, -1*v15*v14+v16*v13+v15*v2+-1*v13*v2+-1*v16*v1+v14*v1
};


quit;

