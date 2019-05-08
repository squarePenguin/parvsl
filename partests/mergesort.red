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
symbolic procedure split(all, k);
begin
    scalar xs, i;
    xs := {};
    i := 0;

    while i < k and all do <<
        xs := (car all) . xs;
        all := cdr all;
        i := i + 1;
    >>;

    return {xs, all};
end;

symbolic procedure mergesorted(xs, ys);
begin
    scalar res;
    res := {};

    while xs or ys do
        if null ys or (xs and car xs <= car ys) then <<
            res := (car xs) . res;
            xs := cdr xs;
        >> else <<
            res := (car ys) . res;
            ys := cdr ys;
        >>;

    res := reverse res;
    return res;
end;

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
% tp := thread_pool(hardwarethreads() - 1);
tp := thread_pool(16);

symbolic procedure parmergesort(xs);
begin scalar n, ss, xs, ysfut, ys;
    n := length xs;
    return
        if n < 6 then insertionsort xs
        else if n < 5000 then mergesort xs
        else <<
            % print "started split";
            ss := split(xs, (n + 1) / 2);
            % print "done split";
            ysfut := tp_addjob(tp, 'parmergesort, {second ss});
            xs := parmergesort first ss;
            ys := future_tryget(ysfut, 10); % wait max 10ms
            while null ys do <<
                tp_runjob(tp);
                % print length first first tp;
                ys := future_tryget(ysfut, 100) >>;
            ys := caar ys;
            % print "got something";
            res := mergesorted(xs, ys);
            % print "done mergesorted";
            res
        >>
end;

symbolic procedure nrand(n);
begin
    scalar res;
    res := nil;
    for i := 1:n do
        res := random 1000 . res;
    return res;
end;

symbolic procedure test(n);
begin
    scalar l, sorted;
    l := nrand n;
    sorted := parmergesort l;
    return nil;
end;

test(1000000);

tp_stop tp;

bye;
