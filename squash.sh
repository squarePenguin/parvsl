#! /bin/bash -v

# arithlib.hpp is a header-only library. That means that ALL of it is scanned
# when it is included. For performance reasons that means that having a
# version of it with fewer characters can be good - removing comments, blank
# lines and leading whitespace...

# The "tidy up" commands here are NOT bullet-proof. Eg text within strings
# that looks too much like comments may cause pain, as could line continuation
# in awkwards ways.
# The first sed comments gets rid of "/* ... */" comments with two cases,
# one with the whole comment on one line the second with it split.
# The next deals with "//" comments but does not take care over two "/"
# characters in a row within a string.
# I then delete empty lines. Then astyle will bring a few more things onto
# single lines, but it leaves some indentation, so at the end I deelete all
# leading whitespace.

cat arithlib.hpp | \
    sed '/\/\*.*\*\// d; /\/\*/,/\*\// d' | \
    sed 's+//.*$++' | \
    sed '/^$/d' | \
    astyle -A12 -s2 | \
    sed 's/^[ \t]*//' > arithlib.xpp
