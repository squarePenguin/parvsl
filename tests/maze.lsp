% A text-adventure game

(de adventure nil
   (printc '(Welcome to the Lisp Dungeon))
   (setq location 'start)
   (setq carrying nil)
   (display_position)
   (while (not (eq location 'finish))
      (make_move (readline))
      (display_position))
   'Congratulations)

(de display_position nil
   (terpri)
   (princ "at: ")
   (lpri (get location 'description))
   (dolist (item carrying)
      (lpri (list 'you 'are 'carrying item)))
   (dolist (item (get location 'objects))
      (lpri (list 'you 'see item)))
   (princ "exits:")
   (dolist (dir '(north south east west up down))
      (cond
         ((get location dir) (princ " ") (princ dir))))
   (terpri))

(de lpri (l)
   (dolist (x l) (princ x) (princ " "))
   (terpri))

(de make_move (word)
   (cond
      ((member word carrying) (drop word))
      ((member word (get location 'objects))
         (pick_up word))
      ((get location word) (move_to word))
      (t (lpri (list word 'not 'understood)))))


(de drop (word)
   (setq carrying (delete word carrying))
   (put location 'objects
      (cons word (get location 'objects)))
   (lpri (list 'dropped word)))

(de pick_up (word)
   (put location 'objects
      (delete word (get location 'objects)))
   (setq carrying (cons word carrying))
   (lpri (list 'got word)))

(de move_to (word)
   (setq location (get location word)))

(put 'start 'description '(The entry to a maze))
(put 'hall 'description '(A fine gothic hallway))
(put 'twist 'description '(A twisty passage))
(put 'dead 'description '(Dead end))
(put 'cave 'description '(Aladdin!'s cave))
(put 'finish 'description '(Castle splendid))

(put 'start 'north 'hall)
(put 'hall 'east 'twist)
(put 'twist 'up 'cave)
(put 'cave 'east 'dead)
(put 'cave 'west 'finish)
(put 'hall 'south 'start)
(put 'twist 'north 'hall)
(put 'cave 'down 'twist)
(put 'dead 'down 'cave)

(put 'start 'objects '(lantern lasergun))
(put 'twist 'objects '(keys map))
(put 'cave 'objects '(treasure))

(adventure)
lantern
lasergun
north
east
up
treasure
lasergun
west


