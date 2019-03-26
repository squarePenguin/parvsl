lisp;

symbolic procedure queue;
    {nil, nil};

symbolic procedure q_push(q, x);
    begin scalar back, newback;
        back := second q;
        newback := {x};

        if null back then <<
            rplaca(q, newback);
            rplaca(cdr q, newback); >>
        else <<
            rplacd(back, newback);
            rplaca(cdr q, newback); >>;
        return q;
    end;

symbolic procedure q_pop(q);
    begin scalar front, next;
        front := first q;
        if null front then
            return {}
        else <<
            next := cdr front;
            if null next then rplaca(cdr q, {});
            rplaca(q, next);
            return (first front); >>;
    end;

symbolic procedure q_empty(q);
    null (first q);

symbolic procedure safeq();
    {queue(), mutex(), condvar()};

symbolic procedure safeq_push(sq, x);
    begin scalar q, m, cv;
        q := first sq;
        m := second sq;
        cv := third sq;

        mutexlock m;
        q_push(q, x);
        condvar_notify_one cv;
        mutexunlock m;
        return sq;
    end;

symbolic procedure safeq_pop(sq);
    begin scalar q, m, cv, res;
        q := first sq;
        m := second sq;
        cv := third sq;
        res := nil;

        mutexlock m;
        while q_empty q do condvar_wait(cv, m);
        res := q_pop q;
        mutexunlock m;
        return res;
    end;

symbolic procedure safeq_empty(sq);
    begin scalar r, m;
        m := second sq;
        mutexlock(m);
        r := q_empty (first sq);
        mutexunlock(m);
        return r;
    end;

symbolic procedure future();
    {mutex (), nil};

% blocking call to wait for future result
symbolic procedure future_get(fut);
begin
    scalar m, state, cv, res;
    m := first fut;
    mutexlock m;

    print fut;

    state := second fut;

    if state = 'done then <<
        res := third fut;
        mutexunlock m;
        return res >>;

    if state = 'waiting then
        cv := third fut
    else <<
        cv := condvar ();
        rplacd(fut, {'waiting, cv}) >>;

    print "waiting fut";
    condvar_wait(cv, m);
    print "waiting fut done";
    % ASSERT: promise is fulfilled here

    res := third fut;
    mutexunlock m;

    return res;
end;

symbolic procedure future_set(fut, value);
begin
    scalar m, state;
    m := first fut;

    mutexlock m;
    state := second fut;

    if state = 'done then
        error("future already set");

    if state = 'waiting then
        condvar_notify_all third fut;

    rplacd(fut, {'done, value});

    mutexunlock m;
end;



symbolic procedure thread_pool_job(tp_q, status);
    begin
        scalar job, resfut, f, args, res;
        while not (first status = 'kill)
              and (first status = 'run or not (safeq_empty tp_q)) do <<
            job := safeq_pop tp_q;
            print "got job";
            resfut := first job;
            f := second job;
            args := third job;
            res := apply(f, args);
            future_set(resfut, res);
            print "done job";
        >>
    end;

symbolic procedure thread_pool(numthreads);
    begin scalar tp_q, status;
        tp_q := safeq();
        status := {'run};
        for i := 1:numthreads do thread2('thread_pool_job, {tp_q, status});
        return {tp_q, status};
    end;

symbolic procedure tp_addjob(tp, f, args);
    if not (first (second tp) = 'run) then nil
    else begin
        scalar resfut;
        resfut := future ();
        safeq_push(first tp, {resfut, f, args});
        return resfut;
    end;

symbolic procedure tp_stop(tp);
    rplaca(second tp, 'stop);

symbolic procedure tp_kill(tp);
    rplaca(second tp, 'kill);

% tp := thread_pool();

end;