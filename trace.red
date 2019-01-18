
global '(varnames!*);
varnames!* :=
  '(!~a1!~ !~a2!~ !~a3!~ !~a4!~ !~a5!~ !~a6!~ !~a7!~ !~a8!~ !~a9!~
    !~a10!~ !~a11!~ !~a12!~ !~a13!~ !~a14!~ !~a15!~ !~a16!~ !~a17!~);

symbolic procedure mytrace fn;
  begin
    scalar oldname, nargs, args, body, vv;
    vv := varnames!*;
    nargs := get(fn, 'number!-of!-args);
    if nargs = nil then <<
       princ "+++ "; prin fn; printc " not defined";
       return nil >>;
    args := for i := 1:nargs collect prog1(car vv, vv := cdr vv);
    oldname := compress append(explode '!~!~traced!-, explode fn);
    copyd(oldname, fn);
    remflag(list fn, 'lose);
    body := list(list('setq, '!~result!~, oldname . args),
                 '(cond ((null (zerop (posn))) (terpri))),
                 list('prin, mkquote fn),
                 '(princ " = "),
                 '(print !~result!~),
                 '(return !~result!~));  
    for each v in reverse args do <<
      body := list!*(
        list('princ, list2string(append(explodec "arg",
                append(explodec nargs, '(!: ! ))))),
        list('print, v),
        body);
      nargs := nargs-1 >>;
    body := list!*(
      '(cond ((null (zerop (posn))) (terpri))),
      '(princ "Calling "),
      list('print, mkquote fn), body);
    body := list!*('prog, '(!~result!~), body);
    body := list('lambda, args, body);
    putd(fn, 'expr, body);
    return fn
  end;

end;
