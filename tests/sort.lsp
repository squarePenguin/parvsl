% Sorting. This illustrates sorting a list of items into
% alphabetic order.

(de sort (items)
   (prog (tree)
      (dolist (x items)
         (setq tree (insert x tree)))
      (return (flatten tree))))

(de mknode (v l r)
   (cons v (cons l r)))

(de getval (x) (car x))

(de leftbranch (x) (car (cdr x)))

(de rightbranch (x) (cdr (cdr x)))

(de insert (item tree)
   (cond
      ((null tree) (mknode item nil nil))
      ((orderp item (getval tree))
         (insertleft item tree))
      (t (insertright item tree))))

(de insertleft (item tree)
   (mknode
      (getval tree)
      (insert item (leftbranch tree))
      (rightbranch tree)))

(de insertright (item tree)
   (mknode
      (getval tree)
      (leftbranch tree)
      (insert item (rightbranch tree))))

(de append (a b)
   (cond
      ((null a) b)
      (t (cons (car a) (append (cdr a) b)))))

(de flatten (x)
   (cond
      ((null x) nil)
      (t (append (flatten (leftbranch x))
         (cons (getval x) (flatten  (rightbranch x)))))))

(de orderp (a b)
   (orderp1 (explode a) (explode b)))

(de orderp1 (al bl)
   (cond
       ((null al) t)
       ((null bl) nil)
       ((equal (car al) (car bl)) 
          (orderp1 (cdr al) (cdr bl)))
       (t (lessp (char!-code (car al))
                 (char!-code (car bl))))))


(sort '(s o r t i n g))

(sort '(these things we hold to be self evident))

(sort (oblist))

(stop 0)
