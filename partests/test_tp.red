lisp;

in "partests/thread_pool.red";
% in "partests/poly.red";

fluid '(tp);
tp := thread_pool(1);

symbolic procedure nrand(n);
begin
    scalar res;
    res := nil;
    for i := 1:n do
        res := random 1000 . res;
    return res;
end;

symbolic procedure addall(all);
    if null all then 0 else car all + addall cdr all;

symbolic procedure start_jobs(n);
begin
    scalar fut, futs, a;
    futs := {};

    print "submitting jobs";

    for i := 1:n do <<
        a := nrand (i + 10);
        fut := tp_addjob(tp, 'addall, {a});
        futs := fut . futs;
    >>;

    for each fut in futs do print future_get(fut);
end;

print tp;

start_jobs 10000;

print "DONE!!!";

tp_stop tp;

bye;