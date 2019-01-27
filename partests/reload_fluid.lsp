(setq x 5)
(fluid '(x))
(fluidp 'x)

(preserve)

% after reload
(fluidp 'x)
x