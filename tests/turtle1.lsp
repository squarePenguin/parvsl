% Draw a nice pattern using "turtle graphics"


(setq x -50.0)
(setq y -100.0)
(setq direction 0)

(de turn (n)    % Turn by n DEGREES (not radians)
   (setq direction (remainder (plus direction n) 360)))

(setq deg2radians 0.01745329252)

(de draw (len)  % Draw line of length len in the current direction
   (setq x (plus x (times len (cos (times direction deg2radians)))))
   (setq y (plus y (times len (sin (times direction deg2radians)))))
   (princ "D ") (prin (fix x)) (princ " ") (print (fix y)))

(prog (o a b)
   (setq o (open "./wxplot" 'pipe))
   (wrs o)
   (printc "x 200 240 250") % Select background colour
   (printc "c 200 30 180")  % Select colour for plot
   (printc "V -50 -100")      % starting point for pen
   (setq a 5 b 0)
   (dotimes (i 2160)        % experimentally found limit!
      (draw 12.0)
      (setq a (plus a 7))
      (setq b (plus b a))
      (turn b))
   (printc "Q")
   (close (wrs nil)))

(stop 0)
