% Simple parser for arithmetic expressions

(de expression ()
   (let
      ((tree (term)))
      (while
         (or (eq cursym '!+) (eq cursym '!-))
         (setq tree (list (nextsym) tree (term))))
      tree))


(de term ()
   (let
      ((tree (factor)))
      (while
         (or (eq cursym '!*) (eq cursym '!/))
         (setq tree (list (nextsym) tree (factor))))
      tree))

(de factor ()
   (cond
      ((eq cursym lpar)
         (nextsym)
         (let
            ((tree (expression)))
            (nextsym)
            tree))
      (t (nextsym))))

(de nextsym ()
   (let ((prev cursym))
      (setq cursym (readch))
      (while (or (eq cursym blank) (eq cursym !$eol!$))
             (setq cursym (readch)))
      prev))

(de parser ()
   (let ((cursym nil))
      (nextsym)
      (expression)))

(parser)
2+(a-b)*(4/x+7);

