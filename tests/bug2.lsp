
(de plus2 (a b) (iplus a b))
(de times2 (a b) (itimes a b))

(setq countdown 100)

(de mess (x)
   (cond
      ((iminusp (setq countdown (isub1 countdown))) (reclaim) (setq countdown 100)))
   (cond
      ((zerop x) (ifloat countdown))
      (t (cons
            (mess (isub1 x))
            (mess (isub1 x))))))

(mess 11)

(stop 0)
    