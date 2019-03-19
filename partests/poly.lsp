(de gen_poly (n)
    (cond
      ((zerop n) nil)
      (t (cons n (gen_poly (sub1 n))))))

(de multiply_by_constant (n l)
    (cond
      ((null l) nil)
      (t (cons (times n (car l)) (multiply_by_constant n (cdr l))))))

(de add_polys (a b)
    (cond
      ((null a) b)
      ((null b) a)
      (t (cons (plus (car a) (car b)) (add_polys (cdr a) (cdr b))))))

(de multiply_polys (a b)
    (cond
      ((null a) nil)
      (t (add_polys (multiply_by_constant (car a) b) (cons 0 (multiply_polys (cdr a) b))))))

(de interleave (a b)
    (cond
      ((null a) b)
      (t (cons (car a) (interleave b (cdr a))))))

(de reverse0 (l acc)
    (cond
      ((null l) acc)
      (t (reverse0 (cdr l) (cons (car l) acc)))))

(de reverse (l) (reverse0 l nil))

(de unwind0 (l evens odds is_odd)
    (cond
      ((null l) (list (reverse evens) (reverse odds)))
      (is_odd (unwind0 (cdr l) evens (cons (car l) odds) nil))
      (t (unwind0 (cdr l) (cons (car l) evens) odds t))))

(de unwind (l) (unwind0 l nil nil nil))

% like parallel, but running sequentially O(N^2)
(de multiply_polys2 (a b)
    (let!* ((a_unwind (unwind a))
            (b_unwind (unwind b))
            (a0 (car a_unwind))
            (a1 (cadr a_unwind))
            (b0 (car b_unwind))
            (b1 (cadr b_unwind))

            (a0b0 (multiply_polys a0 b0))
            (a0b1 (multiply_polys a0 b1))
            (a1b0 (multiply_polys a1 b0))
            (a1b1 (multiply_polys a1 b1)))
        (interleave (add_polys a0b0 (cons 0 a1b1)) (add_polys a0b1 a1b0))))

% actually multithreaded
(de multiply_polys_par (a b)
    (let!* ((a_unwind (unwind a))
            (b_unwind (unwind b))
            (a0 (car a_unwind))
            (a1 (cadr a_unwind))
            (b0 (car b_unwind))
            (b1 (cadr b_unwind))

            (la0b0 (list a0 b0))
            (la0b1 (list a0 b1))
            (la1b0 (list a1 b0))
            (la1b1 (list a1 b1))

            (t0 (thread2 'multiply_polys 'la0b0))
            (t1 (thread2 'multiply_polys 'la0b1))
            (t2 (thread2 'multiply_polys 'la1b0))
            (t3 (thread2 'multiply_polys 'la1b1))

            (a0b0 (jointhread t0))
            (a0b1 (jointhread t1))
            (a1b0 (jointhread t2))
            (a1b1 (jointhread t3)))
        (interleave (add_polys a0b0 (cons 0 a1b1)) (add_polys a0b1 a1b0))))


% (setq a (list 21 5 0 -2 3 6))
% (setq b (list -1 3 1 4 -5))
% (setq c (multiply_polys a b))
% (setq c2 (multiply_polys2 a b))

% (setq cp (multiply_polys_par a b))

(setq a (gen_poly 1000))
(setq b (gen_poly 500))
% (setq c (multiply_polys a b))
% (setq c2 (multiply_polys2 a b))
(setq cp (multiply_polys_par a b))
