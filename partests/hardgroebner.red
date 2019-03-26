load_package groebner;

torder({x, y, z, w}, lex);

ps1 := { w^2+x^2+y^2+z^2-1
       , 2*w^2-y^2+z*y+x*z-2
       , w^2+3*y^2-x*z-x^2-1
       , w*x+w*y+w*z-1 };

g1 := groebner ps1;

torder({x, y, z, w, p, q}, lex);

% only modified the first equation
ps2 := { w^2+x^2+y^2+z^2+p^2+q^2-1
      , 2*w^2-y^2+z*y+x*z-2
      , w^2+3*y^2-x*z-x^2-1
      , w*x+w*y+w*z-1 };

g2 := groebner ps2;

torder({x, y, z, w, p}, lex);

ps3 := { w^2+x^2+y^2+z^2+p^2-1
       , 2*w^2-y^2+z*y+x*z-2
       , w^2+3*y^2-x*z-x^2-1
       , w*x+w*y+w*z-1
       , x*p-1 };

g3 := groebner ps3;

ps := {x+2*y-z+1; 2*x+y-3*z; x+y+z-2}
groebner ps;