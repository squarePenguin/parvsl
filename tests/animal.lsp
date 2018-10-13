% An animal guessing game.

(de animal nil
   (while t
      (progn (say_hello)
             (setq known_animals (guess known_animals)))))

(de say_hello nil
   (terpri)
   (lpri '(Think of an animal and I will guess it))
   (terpri))

(de guess (known_animals)
   (cond
      ((atom known_animals) (I_guess known_animals))
   (t (printc (car known_animals))   
      (cond
         ((yesp (read))
            (rplaca
               (cdr known_animals)
               (guess (cadr known_animals))))
         (t (rplacd
               (cdr known_animals)
               (guess (cddr known_animals)))))
      known_animals)))

(de I_guess (creature)
   (lpri (list 'Is 'it 'a creature))
   (cond
      ((yesp (read)) (printc 'Hurrah) creature)
      (t (give_up creature))))

(de give_up (I_thought)
   (prog (new_animal new_question)
      (lpri '(I give up))
      (lpri '(what was it?))
      (setq new_animal (read))
      (lpri '(Please type in a question that would))
      (lpri  (list 'distinguish 'a new_animal
                   'from 'a I_thought))
      (setq new_question (readline))
      (lpri '(thank you))
      (return (cons new_question (cons new_animal I_thought)))))

(de yesp (a)
   (if (eq a !$eof!$)
       (stop 0)
       (or (eq a 'yes) (eq a 'y))))

(de lpri (l)
   (dolist (x l)
      (princ x)
      (princ " "))
   (terpri))

(setq known_animals
   '(Does! it! have! a! long! neck!?
       (Does! it! live! in! africa giraffe . swan)
       Does! it! have! big! ears!?
       (Does! it! have! a! big! nose elephant . rabbit)
       . crocodile))


(animal)
yes
no
no
snake
has it got a forked tongue?
yes
no
yes
yes
