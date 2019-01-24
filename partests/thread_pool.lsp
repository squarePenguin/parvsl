% a safeq has the form (q back mutex)
(de queue () (list nil nil))

% for now I keep it simple so no fine-grained locking
(de q_push (q x) 
    (let ((back (cadr q))
          (newback (cons x nil)))
        (cond 
            ((null back) (rplaca q newback) (rplaca (cdr q) newback))
            (t (rplacd back newback) (rplaca (cdr q) newback)))
        q))

(de q_pop (q)
    (let ((front (car q)))
    (cond 
        ((null front) nil)
        (t
            (let ((next (cdr front)))
            % if we removed the last element also set the back pointer
            (cond ((null next) (rplaca (cdr q) nil)))
            % pop the front element
            (rplaca q next)
            % return the popped element
            (car front)))
    q)))

(de q_empty (q) (null (car q)))

(de safeq () (list (queue) (mutex) (condvar)))

(de safeq_push (sq x)
    (let ((q (car sq))
          (m (cadr sq))
          (cv (caddr sq)))
    (mutexlock m)
    (q_push q x)
    (condvar_notify_one cv)
    (mutexunlock m)
    sq))

% wait and pop operation
(de safeq_pop (sq)
    (let ((q (car sq))
          (m (cadr sq))
          (cv (caddr sq))
          (res nil))
    (mutexlock m)
    (while (q_empty q) (condvar_wait cv m))
    (setq res (q_pop q))
    (mutexunlock m)
    res))

% a very simple thread thread_pool 
% will use all hardware threads
(de thread_pool ()
    (global '(tp_q))
    (setq tp_q (safeq))
    (let ((nthreads (sub1 (hardwarethreads))))
    (dotimes (i nthreads) (thread 
        % run indefinitely
        '(while t
        (let ((task (safeq_pop tp_q)))
        (while (null task)
            % busy wait for a task
            (setq task (safeq_pop tp_q)))
        (print task)
        (eval task))))))
    tp_q)

(de tp_addjob (tp job) (safeq_push tp job))

(setq tp (thread_pool))
(setq code '(let () (dotimes (i 100) (add1 i)) (print "done")))
(de test_tp (n)
    (dotimes (i n) (tp_addjob tp code)))

