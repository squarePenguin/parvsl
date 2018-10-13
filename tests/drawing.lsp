% Using the wxplot application to draw a picture or so...

(de draw (x y)
   (princ "D ")
   (princ x)
   (princ " ")
   (printc y))

(de move (x y)
   (princ "V ")
   (princ x)
   (princ " ")
   (printc y))

(de circle (n)
   (prog (w)
      (setq w (quotient (times n 7) 10))
      (move n 0)
      (draw w w)
      (draw 0 n)
      (draw (minus w) w)
      (draw (minus n) 0)
      (draw (minus w) (minus w))
      (draw 0 (minus n))
      (draw w (minus w))
      (draw n 0)))

(de spider (n)
   (cond
      ((minusp n) nil)
      (t (circle n)
         (spider (difference n 10)))))

(de drawit ()
   (prog (a)
      (setq a (open "./wxplot" 'pipe))
      (wrs a)
      (spider 180)
      (printc "Q")
      (close (wrs nil))))

(drawit)

