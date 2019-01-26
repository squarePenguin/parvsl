# Makefile for vsl

# I switch off the colouring of disgnostics bacause when that is
# enabled it makes a mess if I capture them in a log file and try to
# view messages with an editor that does not honour the various escape
# codes. I limit the number of messages I get to 5 so that I di not
# end up with page after page of messages. Many current C++ compilers
# support C++11 by default, but some do not so I force that issue.

CFLAGS = --std=gnu++11 -fno-diagnostics-color -fmax-errors=5 -O0 -g -pg -Wall
FASTCFLAGS = --std=gnu++11 -fno-diagnostics-color -fmax-errors=5 -O3 -Wall

# The issue of just which libraries I need to link in seems to be
# platform-specific so here are some particular cases...

ifeq ($(shell uname),Darwin)
LIBS=-lm -lz -ledit -lncurses -ltermcap
else
ifeq ($(shell uname -o),Cygwin)
LIBS=-lm -lz -ledit -lncurses
else
LIBS=-lm -lz -ledit -lncurses -ltermcap
endif
endif

# I will expact that if crlibm is to be used that its header is in
# /usr/local/include

ifneq (,$(wildcard /usr/local/include/crlibm.h))
CFLAGS += -I/usr/local/include -DCRLIBM=1
LIBS += -L/usr/local/lib -lcrlibm
endif

all:	vsl vsl.img

# This first is the VSL Lisp system more or less in its original form.
# A sequential-only Lisp coded to be compact rather than fast.

vsl:	vsl.cpp
	g++ $(CFLAGS) \
		vsl.cpp $(LIBS) -o vsl \
		2>&1 | tee vsl.log

# "fastvsl" is "vsl" but compiled for speed rather than with debugging options

fastvsl:	vsl.cpp
	g++ $(FASTCFLAGS) \
		vsl.cpp $(LIBS) -o fastvsl \
		2>&1 | tee fastvsl.log

# vsl-arith and fastvsl-arith will be VSL but with arbitrary precision
# integer arithmetic implemented in C++.

vsl-arith:	vsl-arith.cpp arithlib.hpp
	g++ $(CFLAGS) -DBIGNUM=1 \
		vsl-arith.cpp $(LIBS) -o vsl-arith \
		2>&1 | tee vsl-arith.log

fastvsl-arith:	vsl-arith.cpp arithlib.hpp
	g++ $(FASTCFLAGS) -DBIGNUM=1 \
		vsl-arith.cpp $(LIBS) -o fastvsl-arith \
		2>&1 | tee fastvsl-arith.log

# parvsl and fastparvsl represent the version of the code that will
# support threads and concurrency.

parvsl:    parvsl.cpp common.hpp thread_data.hpp
	g++ $(CFLAGS) -pthread \
		parvsl.cpp $(LIBS) -o parvsl \
		2>&1 | tee parvsl.log

fastparvsl:    parvsl.cpp common.hpp thread_data.hpp
	g++ $(FASTCFLAGS) -pthread \
		parvsl.cpp $(LIBS) -o fastparvsl \
		2>&1 | tee fastparvsl.log

# For each version of the main vsl binary there is a corresponding image
# file that turns the kernal system into a tolerably full-featured Lisp.

vsl.img:	vsl library.lsp vsl.lsp
	time ./vsl -z library.lsp | tee vsl.img.log

fastvsl.img:	fastvsl library.lsp vsl.lsp
	time ./fastvsl -z library.lsp | tee fastvsl.img.log

vsl-arith.img:	vsl-arith library-arith.lsp vsl-arith.lsp
	time ./vsl-arith -z library-arith.lsp | tee vsl-arith.img.log

fastvsl-arith.img:	fastvsl-arith library-arith.lsp vsl-arith.lsp
	time ./fastvsl-arith -z library-arith.lsp | tee fastvsl-arith.img.log

parvsl.img:	parvsl parlibrary.lsp parvsl.lsp
	time ./parvsl -z parlibrary.lsp | tee parvsl.img.log

fastparvsl.img:	fastparvsl parlibrary.lsp parvsl.lsp
		time ./fastparvsl -z parlibrary.lsp | tee fastparvsl.img.log

# In much the way that vsl.img (and friends) is a Lisp built on top of
# the VSL kernel, reduce[.img] is the Reduce algebra system.

reduce:	vsl
	mkdir -p reduce.img.modules
	rm -f reduce.img.modules/* reduce.img inline-defs.dat
	time ./vsl -z -ireduce.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildreduce.lsp | tee reduce.log

fastreduce:	fastvsl
	mkdir -p fastreduce.img.modules
	rm -f fastreduce.img.modules/* fastreduce.img inline-defs.dat
	time ./fastvsl -z -ifastreduce.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildreduce.lsp | tee fastreduce.log

parreduce:	parvsl
	mkdir -p parreduce.img.modules
	rm -f parreduce.img.modules/* parreduce.img inline-defs.dat
	time ./parvsl -z -iparreduce.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildreduce.lsp | tee parreduce.log

fastparreduce:	fastparvsl
	mkdir -p fastparreduce.img.modules
	rm -f fastparreduce.img.modules/* fastparreduce.img inline-defs.dat
	time ./fastparvsl -z -ifastparreduce.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildreduce.lsp | tee fastparreduce.log


debug_reduce:	vsl
	mkdir -p reduce.img.modules
	rm -f reduce.img.modules/* reduce.img inline-defs.dat
	cgdb --args ./vsl -z -ireduce.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildreduce.lsp | tee reduce.log

reduce-arith:	vsl-arith
	mkdir -p reduce-arith.img.modules
	rm -f reduce-arith.img.modules/* reduce-arith.img inline-defs.dat
	time ./vsl-arith -z -ireduce-arith.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildreduce-arith.lsp | tee reduce-arith.log

fastreduce-arith:	fastvsl-arith
	mkdir -p fastreduce-arith.img.modules
	rm -f fastreduce-arith.img.modules/* fastreduce-arith.img inline-defs.dat
	time ./fastvsl-arith -z -ifastreduce-arith.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildreduce-arith.lsp | tee fastreduce-arith.log

# rcore is a core of Reduce. Building it will be a LOT cheaper than
# building the full copy of Reduce, so this will be good for testing an
# much evaluation.

rcore:	vsl
	mkdir -p rcore.img.modules
	rm -f rcore.img.modules/* rcore.img inline-defs.dat
	time ./vsl -z -ircore.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildrcore.lsp | tee rcore.log

rcore-arith:	vsl-arith library-arith.lsp vsl-arith.lsp
	mkdir -p rcore-arith.img.modules
	rm -f rcore-arith.img.modules/* rcore-arith.img inline-defs.dat
	time ./vsl-arith -z -ircore-arith.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildrcore-arith.lsp | tee rcore-arith.log

parrcore:	parvsl
	mkdir -p parrcore.img.modules
	rm -f parrcore.img.modules/* parrcore.img inline-defs.dat
	time ./parvsl -z -iparrcore.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildrcore.lsp | tee parrcore.log

fastrcore:	fastvsl
	mkdir -p fastrcore.img.modules
	rm -f fastrcore.img.modules/* fastrcore.img inline-defs.dat
	time ./fastvsl -z -ifastrcore.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildrcore.lsp | tee fastrcore.log

fastrcore-arith:	fastvsl-arith library-arith.lsp vsl-arith.lsp
	mkdir -p fastrcore-arith.img.modules
	rm -f fastrcore-arith.img.modules/* fastrcore-arith.img inline-defs.dat
	time ./fastvsl-arith -z -ifastrcore-arith.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildrcore-arith.lsp | tee fastrcore-arith.log

fastparrcore:	fastparvsl
	mkdir -p fastparrcore.img.modules
	rm -f fastparrcore.img.modules/* fastparrcore.img inline-defs.dat
	time ./fastparvsl -z -ifastparrcore.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildrcore.lsp | tee fastparrcore.log

# The target "step2" at one stage activated a script that started from
# rcore and compiled additional parts of Reduce to bring it up to the
# full system. It has not been reviewed or tested recently and is liable
# to need review before further use!

step2:	vsl
	cp rcore.img step2.img
	cp -r rcore.img.modules step2.img.modules
	time ./vsl -istep2.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		step2.red | tee step2.log

# "arithtest" tests the C++ bignum arithmetic code.

arithtest:	arithtest.cpp arithlib.hpp
	g++ $(FASTCFLAGS) arithtest.cpp -o arithtest

# Once Reduce is built it becomes possible to try its various test
# scripts. The shell script "test.sh" does the work, and leaves its
# results in the testlogs directory.

testlogs/%.log:
	time ./test.sh $@

# The following is a really crude benchmark for arithlib to compare
# its performance against gmp. It ONLY tests mong-multiplication and at
# the time of writing this comment it shows that gmp is maybe faster than
# arithlib by a factor of 1.5 for what I will describe as sane-sized
# arguments. As the numbers get to involve more than about 10 64-bit
# words the fact that arithlib does not (yet) use Karatsuba leads to
# much more substantial speed ratios. This test does not look at
# operations other than multiplication, and for that it only looks at
# the case of multiplying two numbers that are the same length. But still
# that can give a crude form of evaluation!

bench_both:	bench_both.cpp arithlib.hpp
	g++ bench_both.cpp -O3 -lgmp -o bench_both


runbench:	bench_both
	./bench_both

# "make clean" does some tidying up. 

clean:
	rm -rf vsl *.exe *.log *.bak reduce.img* vsl.img* *.o *~ \
		*.ind *.idx *.aux *.ilg *.toc *.xref *.bbl *.blg \
		testlogs/*

# The original VSL had some (pretence at) documentation, and this target
# arranges to build that.

docs:   
	cd docs && $(MAKE)

# end of Makefile
