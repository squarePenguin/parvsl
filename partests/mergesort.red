lisp;

in "partests/thread_pool.red";

symbolic procedure insertsorted(x, xs);
    if null xs then {x}
    else if x <= car xs then x . xs
    else car xs . insertsorted(x, cdr xs);

symbolic procedure insertionsort(xs);
    if null xs then xs
    else
        insertsorted(car xs, insertionsort cdr xs);

% reverses the first half but don't care here.
symbolic procedure split0(all, k, xs);
    if k = 0 then {xs, all}
    else if null all then {xs, {}}
    else split0(cdr all, k - 1, car all . xs);

symbolic procedure split(all, k);
    split0(all, k, {});

symbolic procedure mergesorted(xs, ys);
    if null xs then ys
    else if null ys then xs
    else if car xs <= car ys then car xs . mergesorted(cdr xs, ys)
    else car ys . mergesorted(xs, cdr ys);

symbolic procedure mergesort(xs);
begin scalar n, ss, xs, ys;
    n := length xs;
    if n < 6 then return insertionsort xs
    else <<
        ss := split(xs, (n + 1) / 2);
        xs := mergesort first ss;
        ys := mergesort second ss;
        return mergesorted(xs, ys) >>
end;

fluid '(tp);
tp := thread_pool(hardwarethreads() - 1);

symbolic procedure parmergesort(xs);
begin scalar n, ss, xs, ysfut, ys;
    n := length xs;
    return
        if n < 6 then insertionsort xs
        % else if n < 1000 then mergesort xs
        else <<
            ss := split(xs, (n + 1) / 2);
            ysfut := tp_addjob(tp, 'parmergesort, {second ss});
            xs := parmergesort first ss;
            ys := future_get ysfut;
            mergesorted(xs, ys) >>
end;

symbolic procedure nrand(n);
begin
    scalar res;
    res := nil;
    for i := 1:n do
        res := random 1000 . res;
    return res;
end;

end;
