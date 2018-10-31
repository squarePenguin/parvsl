% This is a collection of commonly useful Lisp
% functions that can be defined in terms of the
% things that are built into vsl. The idea is that
% going
%     ./vsl -z library.lsp
% will create an image file vsl.img that can be loaded
% next time vsl is started, so that the cold-start
% flag (-z) does not then have to be used and all these
% extra functions will be available.

% The library itself if in "vsl.lsp"...

(rdf "vsl.lsp")

% The compatibility file "vsl.lsp" defines a macro called
% "~let" rather than "let" because of a name-clash with the
% Reduce algbera system. I put in the more ordinary name here.

(dm let (u) (cons '!~let (cdr u)))

% DERIV -- This is the Common Lisp version of a symbolic
% derivative benchmark written by Vaughan Pratt.
% It uses a simple subset of Lisp and does a lot of
% CONSing. But converted now to run on vsl


(de deriv!-aux (a) (list '/ (deriv a) a))

(de deriv (a)
   (cond
      ((atom a)
         (cond ((eq a 'x) 1) (t 0)))
      ((eq (car a) '+)
         (cons '+ (mapcar (cdr a) 'deriv)))
      ((eq (car a) '-)
         (cons '- (mapcar (cdr a) 'deriv)))
      ((eq (car a) '*)
      (list '* a
         (cons '+ (mapcar (cdr a) 'deriv!-aux))))
      ((eq (car a) '/)
         (list '-
            (list '/
               (deriv (cadr a))
               (caddr a))
            (list '/
               (cadr a)
               (list '*
                  (caddr a)
                  (caddr a)
            (deriv (caddr a))))))
      (t (error 1 'bad)))))

% Try once to verify results.
(deriv '(+ (* 3 x x) (* a x x) (* b x) 5))

(de run ()
   (dotimes (i 100000)
      (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
      (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
      (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
      (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))
      (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))))



(setq a (time))
(run)
(difference (time) a)




