lisp;
global('(!~radix));

symbolic procedure multiply_by_constant(n, l);
  if null l then nil
  else (n*car l) . multiply_by_constant(n, cdr l);

symbolic procedure add_polys(a, b);
    if null a then b
    else if null b then a
    else
        (car a + car b) . add_polys(cdr a, cdr b);

symbolic procedure multiply_polys(a, b);
  if null a then nil
  else add_polys(
    multiply_by_constant(car a, b),
    0 . multiply_polys(cdr a, b));

symbolic procedure interleave(a, b);
  if null a then b
  else (car a) . interleave(b, cdr a);

symbolic procedure reverse0(l, acc);
  if null l then acc
  else
    reverse0(cdr l, (car l) . acc);

symbolic procedure reverse(l);
  reverse0(l, {});

symbolic procedure unwind0(l, evens, odds, is_odd);
  if null l then {reverse evens, reverse odds}
  else
    if is_odd then
      unwind0(cdr l, evens, (car l) . odds, nil)
    else
      unwind0(cdr l, (car l) . evens, odds, t);

symbolic procedure unwind(l);
  unwind0(l, {}, {}, nil);

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
begin scalar a_unwind, b_unwind, a0, a1, b0, b1, t1, t2, t3;
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

symbolic procedure nrand(n);
begin
    scalar res;
    res := nil;
    for i := 1:n do
        res := random 1000 . res;
    return res;
end;

a := {21, 5, 0, -2, 3, 6};
b := {-1, 3, 1, 4, -5};
c := multiply_polys(a, b);
% c2 := multiply_polys2(a, b);
% cp := multiply_polys_par(a, b);

end;