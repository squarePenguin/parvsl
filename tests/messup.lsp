% A sample function that (deliberately) fails so you can see a backtrace,
% and some uses of errorset that control how much information is displayed.

(de failer (n)
   (cond
      ((zerop n) (error 99 "this failed"))
      (t (list 1 (failer (sub1 n)) 2))))

(failer 2)

(failer 10)

(errorset '(failer 4) nil nil)

(errorset '(failer 4) t nil)

(errorset '(failer 4) nil t)

(errorset '(failer 4) t t)



(stop 0)

