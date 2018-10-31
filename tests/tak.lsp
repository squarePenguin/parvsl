% Tak benchmark

(de tak (x y z)
   (cond
      ((not (ilessp y x)) z)
      (t (tak (tak (isub1 x) y z)
              (tak (isub1 y) z x)
              (tak (isub1 z) x y))))))

(setq a (time))
(tak 18 12 6)
(difference (time) a)

(de slowtak (x y z)
   (cond
      ((not (lessp y x)) z)
      (t (slowtak (slowtak (sub1 x) y z)
              (slowtak (sub1 y) z x)
              (slowtak (sub1 z) x y))))))

(setq a (time))
(slowtak 18 12 6)
(difference (time) a)

(stop 0)
