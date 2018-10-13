% Route-finding

(de node (kv l r) (cons kv (cons l r)))
(de kv (x)    (car x))
(de k  (x)    (caar x))
(de v  (x)    (cdar x))
(de left (x)  (cadr x))
(de right (x) (cddr x))

(de add_to_heap (item heap)
   (cond
      ((null heap) (node item nil nil))
      ((lessp (car item) (k heap))
         (node item
               (right heap)
               (add_to_heap (kv heap) (left heap))))
      (t (node (kv heap)
               (right heap)
               (add_to_heap item (left heap))))))

(de shrink_heap (heap)
   (cond
      ((null (right heap))
       (setq removed (kv heap))
       nil)
      (t (node (kv heap)
               (shrink_heap (right heap))
               (left heap)))))

(de remove_top_item (heap)
   (let!* ((removed nil)
           (h1 (shrink_heap heap)))
      (if (null h1)
          nil
          (restore_heap (cons removed (cdr h1))))))

(de restore_heap (heap)
   (cond
      ((null heap) nil)
      ((and (left heap)
            (lessp (k (left heap)) (k heap))
            (or (null (right heap))
                (lessp (k (left heap)) (k (right heap)))))
       (node (kv (left heap))
             (restore_heap (cons (kv heap) (cdr (left heap))))
             (right heap)))
      ((and (right heap)
            (lessp (k (right heap)) (k heap)))
       (node (kv (right heap))
             (left heap)
             (restore_heap (cons (kv heap) (cdr (right heap))))))
      (t heap)))

% The above can be used to implement sorting... so just for fun
% demonstrate that!

(de heapsort (l)
   (let ((h nil))
      (dolist (x l) (setq h (add_to_heap (list x) h)))
      (setq l nil)
      (while h
         (setq l (cons (k h) l))
         (setq h (remove_top_item h)))
      (reverse l)))

(heapsort '(3 1415926535 8979323846 2643383279 5028841971 6939937510
              5820974944 5923078164 0628620899 8628034825 3421170679
              8214808651 3282306647 0938446095 5058223172 5359408128
              4811174502 8410270193 8521105559 6446229489 5493038196
              4428810975 6659334461 2847564823 3786783165 2712019091
              4564856692 3460348610 4543266482 1339360726 0249141273
              7245870066 0631558817 4881520920 9628292540 9171536436
              7892590360 0113305305 4882046652 1384146951 9415116094
              3305727036 5759591953 0921861173 8193261179 3105118548
              0744623799 6274956735 1885752724 8912279381 8301194912
              9833673362 4406566430 8602139494 6395224737 1907021798
              6094370277 0539217176 2931767523 8467481846 7669405132
              0005681271 4526356082 7785771342 7577896091 7363717872
              1468440901 2249534301 4654958537 1050792279 6892589235
              4201995611 2129021960 8640344181 5981362977 4771309960
              5187072113 4999999837 2978049951 0597317328 1609631859
              5024459455 3469083026 4252230825 3344685035 2619311881
              7101000313 7838752886 5875332083 8142061717 7669147303
              5982534904 2875546873 1159562863 8823537875 9375195778
              1857780532 1712268066 1300192787 6611195909 2164201989))


(de find_route (source destination)
   (prog (queue city distance prev)
      (setq queue (node (list!* 0 source nil) nil nil))
      (while (and queue (not (get destination 'distance)))
         (setq distance (k queue)) % Next nearest place
         (setq city (v queue))     % City and predecessor
         (setq prev (cdr city)) (setq city (car city))
         (setq queue (remove_top_item queue))
         (when (null (get city 'distance)) % Seen before?
            (put city 'distance distance)
            (put city 'previous prev)
%           (princ "Distance to ") (princ city)
%           (princ " is ") (print distance)
            (dolist (x (get city 'neighbours))
               (setq queue (add_to_heap
                   (list!* (plus distance (cdr x)) (car x) city)
                   queue)))))
      (when (null queue) (error 0 "No route exists"))
      (setq distance (get destination 'distance))
      (while (not (eq destination source))
          (princ "Via: ")
          (print (setq destination (get destination 'previous))))
      (return distance)))

(de nput (source neighbours)
   (put source 'neighbours neighbours))


(nput 'Cambridge '((Bedford . 15) (Royston . 20)))
(nput 'Royston '((Cambridge . 20) (Watford . 30) (London . 50)))
(nput 'London '((Royston . 20) (Watford . 25) (Oxford . 50)))
(nput 'Bedford '((Cambridge . 15) (Watford . 30)))
(nput 'Watford '((Bedford . 30) (Royston . 30) (London . 25) (Oxford . 40)))
(nput 'Oxford '((Royston . 50) (Watford . 25) (London . 50)))

(find_route 'Cambridge 'Oxford)

(stop 0)
