% High precision integer arithmetic.

% This stores numbers in decimal with the least-significant digit
% first in the list. So for instance "100" will be held as (0 0 1).

(de carryinto (a carry)
   (cond
      ((null a) (cond
         ((zerop carry) nil)
         (t (cons (remainder carry 10) (carryinto nil (quotient carry 10))))))
      (t (cons (remainder (plus (car a) carry) 10)
               (carryinto (cdr a)
                   (quotient (plus (car a) carry) 10))))))

(de makebig (n)
   (carryinto nil n))

(de bigplus (a b)
   (bigplusc a b 0))

(de bigplusc (a b carry)
   (cond
      ((null a) (carryinto b carry))
      ((null b) (carryinto a carry))
      (t (cons (remainder (plus (car a) (car b) carry) 10)
               (bigplusc (cdr a) (cdr b)
                   (quotient (plus (car a) (car b) carry) 10))))))

(de smalltimes (a b carry)
   (cond
      ((null b) (carryinto nil carry))
      (t (cons (remainder (plus (times a (car b)) carry) 10)
          (smalltimes a (cdr b)
              (quotient (plus (times a (car b)) carry) 10))))))
      

(de bigtimes (a b)
   (cond
      ((or (null a) (null b)) nil)
      (t (bigplus (smalltimes (car a) b 0)
                  (cons 0 (bigtimes (cdr a) b))))))


(makebig 100)
(makebig 123456789)
(bigplus (makebig 123456789) (makebig 987654321))

(de bigsquare (x)
   (bigtimes x x))

(de bigpower (a n)
   (cond
      ((onep n) a)
      ((zerop (remainder n 2)) (bigsquare (bigpower a (quotient n 2))))
      (t (bigtimes a (bigsquare (bigpower a (quotient n 2)))))))

(bigpower (makebig 2) 10)
(bigpower (makebig 2) 20)
(bigpower (makebig 2) 100)
(bigpower (makebig 2) 1000)

(stop 0)

