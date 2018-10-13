on echo;


% load!-package!-sources('poly, 'poly);

module poly;  % Header module and low-level support for poly package.

% Author: Anthony C. Hearn.

% Copyright (c) 1991 RAND.  All rights reserved.

% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%
%    * Redistributions of source code must retain the relevant copyright
%      notice, this list of conditions and the following disclaimer.
%    * Redistributions in binary form must reproduce the above copyright
%      notice, this list of conditions and the following disclaimer in the
%      documentation and/or other materials provided with the distribution.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
% THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
% PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNERS OR
% CONTRIBUTORS
% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
%


create!-package('(poly polrep polydiv quotf gcd exptf kernel mksp reord dmode
                 dmodeop rational rnelem gint cpxrn compopr modular
                 facform homog tdconv primfac specfac kronf conj diff
                 polyop decompos interpol subs2q subs3q subs4q horner),
                nil);

flag('(poly),'core_package);

fluid '(!*mcd !*nosq wtl!*);

% switch nosq;


% Particular infix operators used in standard forms.

flag('(newtok infix), 'eval); 

newtok '((!. !+) add);

newtok '((!. !*) mult);

newtok '((!. !^) to);

newtok '((!. !* !*) to);

newtok '((!. !/) over);

infix .^,.*,.+,./;

% Constructors and selectors for standard forms.

% Observe a function definition using infix notation here...

inline procedure u .+ v; % Standard (polynomial) addition constructor.
   u . v;

accessors lt . red, (lpow . lc) . !_, ((mvar . ldeg) . !_) . !_;

accessors tpow . tc, (tvar . tdeg) . !_;

accessors !_pvar!_ . pdeg;

accessors numr . denr;

inline procedure u .* v;  % Standard form multiplication constructor.
   u . v;

inline procedure u ./ v; % Constructor for standard quotient.
   u . v;

symbolic inline procedure domainp u; atom u or atom car u;


% Procedures for converting between parts of standard quotients and
% prefix forms.

symbolic procedure !*a2f u;
   % U is an algebraic expression. Value is the equivalent form
   % or an error if conversion is not possible;
   !*q2f simp!* u;

symbolic procedure !*a2k u;
   % U is an algebraic expression. Value is the equivalent kernel
   % or an error if conversion is not possible.
   % Note: earlier versions used SIMP0.
   begin scalar x;
      if kernp(x := simp!* u) then return mvar numr x
       else typerr(if null u then 0 else u,'kernel)
   end;

symbolic procedure !*a2kwoweight u;
   % U is an algebraic expression. Value is the equivalent kernel
   % neglecting any weights, or an error if conversion is not possible.
   (if kernp x then mvar numr x else typerr(u,'kernel))
    where x=simp!* u where !*uncached=t,wtl!*=nil;

symbolic procedure !*d2q u;
   % Converts domain element U into a standard quotient.
   if numberp u
     then if zerop u then nil ./ 1
   %       else if floatp u then mkfloat u ./ 1
           else u ./ 1
   % The following converts a domain rational to a SQ, which may not
   % be desirable.
   % else if eqcar(u,'!:rn!:) and !*mcd then cdr u
    else if !:zerop u then nil ./ 1 else u ./ 1;

symbolic procedure !*ff2a(u,v);
   % Converts ratio of two forms U and V to a prefix form.
   (if wtl!* then prepsq x else mk!*sq x) where x = cancel( u ./ v);

inline procedure !*f2a u; prepf u;

inline procedure !*f2q u;
   % U is a standard form, value is a standard quotient.
   u . 1;

inline procedure !*k2f u;
   % U is a kernel, value is a standard form.
   list((u .** 1) . 1);

symbolic inline procedure !*kk2f u;
   % U is a non-unique kernel, value is a standard form.
   list(mksp(u,1) . 1);

symbolic inline procedure !*kk2q u;
   % U is a non-unique kernel, value is a standard quotient.
   list(mksp(u,1) .* 1) ./ 1;

inline procedure !*k2q u;
   % U is a kernel, value is a standard quotient.
   list((u .** 1) . 1) . 1;

symbolic procedure !*n2f u;
   % U is a number. Value is a standard form.
   if zerop u then nil else u;

inline procedure !*p2f u;
   % U is a standard power, value is a standard form.
   list(u . 1);

inline procedure !*p2q u;
   % U is a standard power, value is a standard quotient.
   list(u . 1) . 1;

symbolic procedure !*q2a u;
   % U is a standard quotient, value is an algebraic expression.
   !*q2a1(u,!*nosq);

symbolic procedure !*q2a1(u,v);
   if null v then mk!*sq u else prepsqxx u;

symbolic procedure !*q2f u;
   % U is a standard quotient, value is a standard form.
   if denr u=1 then numr u else typerr(prepsq u,'polynomial);

symbolic procedure !*q2k u;
   % U is a standard quotient, value is a kernel or an error if
   % conversion not possible.
   if kernp u then mvar numr u else typerr(prepsq u,'kernel);

inline procedure !*t2f u;
   % U is a standard term, value is a standard form.
   list u;

inline procedure !*t2q u;
   % U is a standard term, value is a standard quotient.
   list u . 1;

endmodule;

module polrep; % Arithmetic operations on standard forms and quotients.

% Author: Anthony C. Hearn.

% Copyright (c) 1991 RAND.  All rights reserved.

% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%
%    * Redistributions of source code must retain the relevant copyright
%      notice, this list of conditions and the following disclaimer.
%    * Redistributions in binary form must reproduce the above copyright
%      notice, this list of conditions and the following disclaimer in the
%      documentation and/or other materials provided with the distribution.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
% THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
% PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNERS OR
% CONTRIBUTORS
% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
%


fluid '(!*asymp!* !*exp !*factor !*gcd !*lcm !*mcd !*rationalize frlis!*
        !*roundall !*rounded !*sqfree !*sub2 asymplis!* dmode!* subfg!*
        ncmp!* powlis!* wtl!* !*!*processed !*ncmp);

global '(!*group rd!-tolerance!* cr!-tolerance!* !*physop!-loaded
         !*sstools!-loaded);

put('roundall,'simpfg,'((t (rmsubs))));

switch roundall;

!*roundall := t;   % Default is on.

symbolic inline procedure subtrsq(u,v); addsq(u,negsq v);

symbolic procedure addsq(u,v);
   % U and V are standard quotients.
   % Value is canonical sum of U and V.
   if null numr u then v
    else if null numr v then u
    else if denr u=1 and denr v=1 then addf(numr u,numr v) ./ 1
    else begin scalar x,y,z;
        if null !*exp then <<u := numr u ./ mkprod denr u;
                             v := numr v ./ mkprod denr v>>;
        if !*lcm then x := gcdf!*(denr u,denr v)
         else x := gcdf(denr u,denr v);
        z := canonsq(quotf!-fail(denr u,x) ./ quotf!-fail(denr v,x));
        y := addf(multf(denr z,numr u),multf(numr z,numr v));
        if null y then return nil ./ 1;
        z := multf(denr u,denr z);
        if (x := gcdf(y,x)) neq 1 then <<y := quotf!-fail(y,x);
                                         z := quotf!-fail(z,x)>>;
        if !*gcd then return if x=1 then y ./ z else canonsq(y ./ z);
% If gcd is off (which is the default) y and z can still have an
% incomplete gcd. Therefore to ensure full simplification we need to
% take (the incomplete) gcd of y and z and divide it out.
        return if (x := gcdf(y,z))=1 then canonsq(y ./ z)
                else canonsq(quotf!-fail(y,x) ./ quotf!-fail(z,x))
    end;

symbolic procedure multsq(u,v);
   % U and V are standard quotients.
   % Value is canonical product of U and V.
   if null numr u or null numr v then nil ./ 1
    else if denr u=1 and denr v=1 then multf(numr u,numr v) ./ 1
    else begin scalar x,y,z;
        x := gcdf(numr u,denr v);
        y := gcdf(numr v,denr u);
        z := multf(quotf!-fail(numr u,x),quotf!-fail(numr v,y));
        x := multf(quotf!-fail(denr u,y),quotf!-fail(denr v,x));
        return canonsq(z ./ x)
    end;

symbolic procedure negsq u; negf numr u ./ denr u;

inline procedure multpq(u,v);
   multsq(!*p2q u,v);

symbolic procedure cancel u;
   %returns canonical form of non-canonical standard form U;
   if !*mcd or denr u=1 then multsq(numr u ./ 1,1 ./ denr u)
    else multsq(numr u ./ 1,simpexpt list(mk!*sq(denr u ./ 1),-1));


% ***** FUNCTIONS FOR ADDING AND MULTIPLYING STANDARD FORMS *****

symbolic inline procedure peq(u,v);
   %tests for equality of powers U and V;
   u = v;

%symbolic procedure addf(u,v);
%   % U and V are standard forms. Value is standard form for U+V.
%   if null u then v
%    else if null v then u
%    else if domainp u then addd(u,v)
%    else if domainp v then addd(v,u)
%    else if peq(lpow u,lpow v)
%       then (if null x then y else lpow u .* x .+ y)
%             where x=addf(lc u,lc v),y=addf(red u,red v)
%    else if ordpp(lpow u,lpow v) then lt u .+ addf(red u,v)
%    else lt v .+ addf(u,red v);

% Now a version that avoids having a recursion depth that is as
% deep as the length of the polynomial. I will still permit
% myself recursion to a depth equal to to the number of distinct
% variables that are present.

symbolic procedure addf(u,v);
  % U and V are standard forms. Value is standard form for U+V.
  begin
    scalar r, w;
% r will end up as a (reversed) list of items to be put
% in front of the terminating item w.
  top:
    if null u then << w := v; go to exit >>
    else if null v then << w := u; go to exit >>
    else if domainp u then << w := addd(u, v); go to exit >>
    else if domainp v then << w := addd(v, u); go to exit >>
    else if peq(lpow u, lpow v) then <<
      w := addf(lc u, lc v);
      if not null w then r := (lpow u .* w) .+ r;
      u := red u;
      v := red v >>
    else if ordpp(lpow u, lpow v) then <<
      r := lt u .+ r;
      u := cdr u >>
    else <<
      r := lt v .+ r;
      v := red v >>;
    go to top;
  exit:
    while r do <<
      u := cdr r;
      rplacd(r, w);
      w := r;
      r := u >>;
    return w;
  end;

symbolic procedure addd(u,v);
   % U is a domain element, V a standard form.
   % Value is a standard form for U+V.
   if null v then u
    else if domainp v then adddm(u,v)
    else lt v .+ addd(u,red v);

symbolic procedure adddm(u,v);
   % U and V are both non-zero domain elements.
   % Value is standard form for U+V.
   % The int-equiv-chk is needed to convert say (:MOD: . 0) to NIL.
   % A simpler function might therefore be possible and more efficient.
   if atom u and atom v
     then (if null dmode!* or not flagp(dmode!*,'convert) then !*n2f x
            else int!-equiv!-chk apply1(get(dmode!*,'i2d),x))
          where x=plus2(u,v)
    else dcombine(u,v,'plus);

% in poly.red
% symbolic inline procedure domainp u; atom u or atom car u;

symbolic procedure noncomp u;
   !*ncmp and noncomp1 u;

symbolic procedure noncomp1 u;
  if null pairp u then nil
   else if pairp car u then noncomfp u
   else if car u eq '!*sq then noncomfp numr cadr u
   else if car u eq 'taylor!* then nil
   else flagp(car u,'noncom) or noncomlistp cdr u;

symbolic procedure noncomlistp u;
   pairp u and (noncomp1 car u or noncomlistp cdr u);

% The physop module defines a new versions of multf and multfnc. Having name
% clashes and function redefinitions causes pain for the CSL optimisation
% model, so here I make physop just set a flag so that multf and multfnc can
% divert into its version. A nicer solution will be to consolidate the two
% versions into one, but for now I am not ready to go that far. Maybe somebody
% who really understand the physop code will feel like working on that at
% some time. 

symbolic procedure multf(u, v);
  if !*physop!-loaded then physop!-multf(u, v)
  else poly!-multf(u, v);

symbolic procedure poly!-multf(u,v);
   % U and V are standard forms.
   % Value is standard form for U*V.
   begin scalar x,y;
    a:  if null u or null v then return nil
         else if u=1 then return v     % ONEP
         else if v=1 then return u     % ONEP
         else if domainp u then return multd(u,v)
         else if domainp v then return multd(v,u)
         else if not(!*exp or ncmp!* or wtl!* or x)
          then <<u := mkprod u; v := mkprod v; x := t; go to a>>;
        x := mvar u;
        y := mvar v;
        if noncomfp v and (noncomp x or null !*!*processed)
          then return poly!-multfnc(u,v)
         else if x eq y
          then <<% Allow for free variables in rules.
                 if not fixp ldeg u or not fixp ldeg v
                   then x := x .** reval list('plus,ldeg u,ldeg v)
                  else x := mkspm(x,ldeg u+ldeg v);
                 % The order in the next line is IMPORTANT. See analysis
                 % by J.H. Davenport et al. for details.
                 y := addf(poly!-multf(red u,v),poly!-multf(!*t2f lt u,red v));
                 return if null x or null(u := poly!-multf(lc u,lc v))
                    then <<!*asymp!* := t; y>>
                   else if x=1 then addf(u,y)
                   else if null !*mcd then addf(!*t2f(x .* u),y)
                   else x .* u .+ y>>
         else if ordop(x,y)
          then <<x := poly!-multf(lc u,v);
                 y := poly!-multf(red u,v);
                 return if null x then y else lpow u .* x .+ y>>;
        x := poly!-multf(u,lc v);
        y := poly!-multf(u,red v);
        return if null x then y else lpow v .* x .+ y
   end;

symbolic procedure noncomfp u;
   % It's possible that ncmp!* would work here.
   !*ncmp and noncomfp1 u;

symbolic procedure noncomfp1 u;
   not domainp u
      and (noncomp mvar u or noncomfp1 lc u or noncomfp1 red u);

symbolic procedure multfnc(u,v);
  if !*physop!-loaded then physop!-multfnc(u, v)
   else if !*sstools!-loaded then sstools!-multfnc(u,v) 
   else poly!-multfnc(u, v);

symbolic procedure poly!-multfnc(u,v);
   % Returns canonical product of U and V, with both main vars non-
   % commutative.
   if !*sstools!-loaded then sstools!-multfnc(u,v) else
   begin scalar x,y;
      x := poly!-multf(lc u,!*t2f lt v);
      if null x then nil
       else if not domainp x and mvar x eq mvar u
        then x := addf(if null (y := mkspm(mvar u,ldeg u+ldeg x))
                         then nil
                        else if y = 1 then lc x
                        else !*t2f(y .* lc x),
                       poly!-multf(!*p2f lpow u,red x))
       else if noncomp mvar u then x := !*t2f(lpow u .* x)
       else x := poly!-multf(!*p2f lpow u,x) where !*!*processed=t;
      return addf(x,addf(poly!-multf(red u,v),poly!-multf(!*t2f lt u,red v)))
   end;

symbolic procedure multd(u,v);
   % U is a domain element, V a standard form.
   % Value is standard form for U*V.
   if null v then nil
    else if v=1 then u      % Common enough to be tested.
    else if domainp v then multdm(u,v)
    else lpow v .* multd(u,lc v) .+ multd(u,red v);


preserve('begin, "Step2", nil)
stop 0;

