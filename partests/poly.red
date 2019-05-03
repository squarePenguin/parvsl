lisp;
global('(!~radix));

symbolic procedure multiply_by_constant(n, l);
  for each x in l collect n * x;

symbolic procedure add_polys(a, b);
begin
  scalar res;
  res := {};
  while a and b do <<
    res := (car a + car b) . res;
    a := cdr a;
    b := cdr b;
  >>;
  res := reverse res;
  if a then res := append(res, a);
  if b then res := append(res, b);
  return res;
end;

symbolic procedure multiply_polys(a, b);
begin
  scalar prods, res;
  prods := {};
  for each x in a do
    prods := multiply_by_constant(x, b) . prods;

  res := nil;
  for each p in prods do
    res := add_polys(p, 0 . res);

  return res;
end;

symbolic procedure interleave(a, b);
begin
  scalar res;
  res := {};
  while a and b do <<
    res := (car b) . (car a) . res;
    a := cdr a;
    b := cdr b;
  >>;
  res := reverse res;
  if a then res := append(res, a);
  if b then res := append(res, b);
  return res;
end;

symbolic procedure unwind(l);
begin
  scalar evens, odds, is_odd;
  while l do <<
    evens := (car l) . evens;
    l := cdr l;
    if l then <<
      odds := (car l) . odds;
      l := cdr l;
    >>;
  >>;
  evens := reverse evens;
  odds := reverse odds;
  return {evens, odds};
end;

% like parallel, but running sequentially O(N^2)
symbolic procedure multiply_polys2(a, b);
begin scalar a_unwind, b_unwind, a0, a1, b0, b1;
  a_unwind := unwind(a);
  b_unwind := unwind(b);
  a0 := first a_unwind;
  a1 := second a_unwind;
  b0 := first b_unwind;
  b1 := second b_unwind;

  a0b0 := multiply_polys(a0, b0);
  a0b1 := multiply_polys(a0, b1);
  a1b0 := multiply_polys(a1, b0);
  a1b1 := multiply_polys(a1, b1);

  return interleave(add_polys(a0b0, 0 . a1b1), add_polys(a0b1, a1b0));
end;

% same as above but use threads
symbolic procedure multiply_polys_par(a, b);
begin
  scalar a_unwind, b_unwind
         , a0, a1, b0, b1, t1, t2, t3
         , a0b0, a0b1, a1b0, a1b1;
  a_unwind := unwind(a);
  b_unwind := unwind(b);
  a0 := first a_unwind;
  a1 := second a_unwind;
  b0 := first b_unwind;
  b1 := second b_unwind;

  t1 := thread2('multiply_polys, {a0, b1});
  t2 := thread2('multiply_polys, {a1, b0});
  t3 := thread2('multiply_polys, {a1, b1});

  a0b0 := multiply_polys(a0, b0);
  a0b1 := jointhread(t1);
  a1b0 := jointhread(t2);
  a1b1 := jointhread(t3);

  return interleave(add_polys(a0b0, 0 . a1b1), add_polys(a0b1, a1b0));
end;

% in "partests/thread_pool.red";

symbolic procedure fut_get(fut);
begin
  scalar res;
  res := future_tryget(fut, 10); % wait max 10ms
  while null res do <<
      tp_runjob(tp);
      res := future_tryget(fut, 100) >>;
  return caar res;
end;

symbolic procedure nrand(n);
begin
    scalar res;
    res := nil;
    for i := 1:n do
        res := random 1000 . res;
    return res;
end;

% a := {21, 5, 0, -2, 3, 6};
% b := {-1, 3, 1, 4, -5};
% c := multiply_polys(a, b);
% a := nrand 4000;
% b := nrand 3900;
% c2 := multiply_polys_par(a, b);
% c2 := multiply_polys2(a, b);
% cp := multiply_polys_par(a, b);

symbolic procedure test(n);
begin
  scalar a, b, c;
  a := nrand n;
  b := nrand n;
  c := multiply_polys_par3(a, b);
end;

test 5000;

end;
