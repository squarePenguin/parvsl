# Makefile for dvsl

# I switch off the colouring of disgnostics bacause when that is
# enabled it makes a mess if I capture them in a log file and try to
# view messages with an editor that does not honour the various escape
# codes. I limit the number of messages I get to 5 so that I di not
# end up with page after page of messages. Many current C++ compilers
# support C++11 by default, but some do not so I force that issue.

CFLAGS = --std=gnu++11 -fno-diagnostics-color -fmax-errors=5 -O0 -g -rdynamic -pg -Wall
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

all:	dvsl dvsl.img

# This first is the VSL Lisp system more or less in its original form.
# A sequential-only Lisp coded to be compact rather than fast.

dvsl:	vsl.cpp
	g++ $(CFLAGS) \
		vsl.cpp $(LIBS) -o dvsl \
		2>&1 | tee dvsl.log

# "vsl" is "dvsl" but compiled for speed rather than with debugging options

vsl:	vsl.cpp
	g++ $(FASTCFLAGS) \
		vsl.cpp $(LIBS) -o vsl \
		2>&1 | tee vsl.log

# dparvsl and parvsl represent the version of the code that will
# support threads and concurrency.

dparvsl:    parvsl.cpp common.hpp thread_data.hpp
	g++ $(CFLAGS) -pthread \
		parvsl.cpp $(LIBS) -o dparvsl \
		2>&1 | tee dparvsl.log

parvsl:    parvsl.cpp common.hpp thread_data.hpp
	g++ $(FASTCFLAGS) -pthread \
		parvsl.cpp $(LIBS) -o parvsl \
		2>&1 | tee parvsl.log

# For each version of the main dvsl binary there is a corresponding image
# file that turns the kernal system into a tolerably full-featured Lisp.

dvsl.img:	dvsl library.lsp vsl.lsp
	time ./dvsl -z library.lsp | tee dvsl.img.log

vsl.img:	vsl library.lsp vsl.lsp
	time ./vsl -z library.lsp | tee vsl.img.log

dparvsl.img:	dparvsl parlibrary.lsp parvsl.lsp
	time ./dparvsl -z parlibrary.lsp | tee dparvsl.img.log

parvsl.img:	parvsl parlibrary.lsp parvsl.lsp
		time ./parvsl -z parlibrary.lsp | tee parvsl.img.log

# In much the way that dvsl.img (and friends) is a Lisp built on top of
# the VSL kernel, dreduce[.img] is the Reduce algebra system.

dreduce:	dvsl
	mkdir -p dreduce.img.modules
	rm -f dreduce.img.modules/* dreduce.img inline-defs.dat
	time ./dvsl -z -idreduce.img -D@srcdir=. -D@reduce=../.. \
		-Dnoinlines=t \
		buildreduce.lsp | tee dreduce.log

reduce:	vsl
	mkdir -p reduce.img.modules
	rm -f reduce.img.modules/* reduce.img inline-defs.dat
	time ./vsl -z -ireduce.img -D@srcdir=. -D@reduce=../.. \
		-Dnoinlines=t \
		buildreduce.lsp | tee reduce.log

dparreduce:	dparvsl
	mkdir -p dparreduce.img.modules
	rm -f dparreduce.img.modules/* dparreduce.img inline-defs.dat
	time ./dparvsl -z -idparreduce.img -D@srcdir=. -D@reduce=../.. \
		-Dnoinlines=t \
		buildparreduce.lsp | tee dparreduce.log

parreduce:	parvsl
	mkdir -p parreduce.img.modules
	rm -f parreduce.img.modules/* parreduce.img inline-defs.dat
	time ./parvsl -z -iparreduce.img -D@srcdir=. -D@reduce=../.. \
		-Dnoinlines=t \
		buildparreduce.lsp | tee parreduce.log

debug_reduce:	dvsl
	mkdir -p dreduce.img.modules
	rm -f dreduce.img.modules/* dreduce.img inline-defs.dat
	cgdb --args ./dvsl -z -idreduce.img -D@srcdir=. -D@reduce=../.. \
		-Dnoinlines=t \
		buildreduce.lsp | tee dreduce.log

# drcore is a core of Reduce. Building it will be a LOT cheaper than
# building the full copy of Reduce, so this will be good for testing an
# much evaluation.

drcore:	dvsl
	mkdir -p drcore.img.modules
	rm -f drcore.img.modules/* drcore.img inline-defs.dat
	time ./dvsl -z -ircore.img -D@srcdir=. -D@reduce=../.. \
		-Dnoinlines=t \
		buildrcore.lsp | tee drcore.log

dparrcore:	dparvsl parbuildrcore.lsp
	mkdir -p dparrcore.img.modules
	rm -f dparrcore.img.modules/* dparrcore.img inline-defs.dat
	time ./dparvsl -z -idparrcore.img -D@srcdir=. -D@reduce=../.. \
		-Dnoinlines=t \
		parbuildrcore.lsp | tee dparrcore.log

rcore:	vsl
	mkdir -p rcore.img.modules
	rm -f rcore.img.modules/* rcore.img inline-defs.dat
	time ./vsl -z -ircore.img -D@srcdir=. -D@reduce=../.. \
		-Dnoinlines=t \
		buildrcore.lsp | tee rcore.log

parrcore:	parvsl parbuildrcore.lsp
	mkdir -p parrcore.img.modules
	rm -f parrcore.img.modules/* parrcore.img inline-defs.dat
	time ./parvsl -z -iparrcore.img -D@srcdir=. -D@reduce=../.. \
		-Dnoinlines=t \
		parbuildrcore.lsp | tee parrcore.log

# The target "step2" at one stage activated a script that started from
# drcore and compiled additional parts of Reduce to bring it up to the
# full system. It has not been reviewed or tested recently and is liable
# to need review before further use!

step2:	dvsl
	cp drcore.img step2.img
	cp -r drcore.img.modules step2.img.modules
	time ./dvsl -istep2.img -D@srcdir=. -D@reduce=../.. \
		-Dnoinlines=t \
		step2.red | tee step2.log

# "make clean" does some tidying up. 

clean:
	rm -rf dvsl *.exe *.log *.bak dreduce.img* dvsl.img* *.o *~ \
		*.ind *.idx *.aux *.ilg *.toc *.xref *.bbl *.blg \
		testlogs/*

# end of Makefile
