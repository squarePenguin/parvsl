#! /bin/bash -v

./test2.sh alg alg
./test2.sh poly poly
./test2.sh poly polydiv
./test2.sh arith arith
./test2.sh factor factor
./test2.sh int int
./test2.sh matrix matrix
./test2.sh solve solve

#  (desir         "solve"       core      test                 csl psl)
#  (ineq          "solve"       core      test                 csl psl)
#  (modsr         "solve"       core      test                 csl psl)
#  (rsolve        "solve"       core      test                 csl psl)
#  (algint        "algint"      core      test                 csl psl)
#  (arnum         "arnum"       core      test                 csl psl)
#  (assist        "assist"      core      test                 csl psl)
#  (dummy         "assist"      core      test                 csl psl)
#  (cantens       "assist"      core      test                 csl psl)
#  (atensor       "atensor"     core      test                 csl psl)
#  (avector       "avector"     core      test                 csl psl)
#  (invbase       "invbase"     core      test                 csl psl)
#  (boolean       "misc"        core      test                 csl psl)
#