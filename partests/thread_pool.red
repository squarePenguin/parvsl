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

symbolic procedure thread_pool_job(tp_q, status);
    begin scalar task;
        while not (first status = 'kill)
              and (first status = 'run or not (safeq_empty tp_q)) do <<
            task := safeq_pop tp_q;
            print("got job");
            eval task; >>
    end;

symbolic procedure thread_pool();
    begin scalar tp_q, status;
        tp_q := safeq();
        status := {'run};
        nthreads := hardwarethreads() - 1;
        for i := 1:nthreads do thread2('thread_pool_job, {tp_q, status});
        return {tp_q, status};
    end;

symbolic procedure tp_addjob(tp, job);
    if not (first (second tp) = 'run) then nil
    else << 
        safeq_push(first tp, job);
        t >>;

symbolic procedure tp_stop(tp);
    rplaca(second tp, 'stop);

symbolic procedure tp_kill(tp);
    rplaca(second tp, 'kill);

tp := thread_pool();