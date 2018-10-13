(getd 'mapcar)
(trace '(mapcar apply))

(de testcode (a l)
  (let ((!~l '(a b c)))
    (mapcar l '(lambda (x) (cons a x)))))

(testcode '1 '(78 79 80))

(de testcode (!~l l)
    (mapcar l '(lambda (x) (cons !~l x))))

(testcode '1 '(78 79 80))

(de testcode (!~fn l)
    (mapcar l '(lambda (x) (cons !~fn x))))

(testcode '1 '(78 79 80))

