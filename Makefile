# Makefile for vsl

CFLAGS = -O0 -g -Wall
FASTCFLAGS = -O3 -Wall

ifeq ($(shell uname),Darwin)
LIBS=-lm -lz -ledit -lncurses -ltermcap
else
ifeq ($(shell uname -o),Cygwin)
LIBS=-lm -lz -ledit -lncurses
else
LIBS=-lm -lz -ledit -lncurses -ltermcap
endif
endif

all:	vsl vsl.img

vsl:	vsl.cpp
	g++ -fno-diagnostics-color $(CFLAGS) \
		vsl.cpp $(LIBS) -o vsl \
		2>&1 | tee vsl.log

fastvsl:	vsl.cpp
	g++ -fno-diagnostics-color $(FASTCFLAGS) \
		vsl.cpp $(LIBS) -o fastvsl \
		2>&1 | tee fastvsl.log

parvsl:    parvsl.cpp common.hpp thread_data.hpp
	g++ -fno-diagnostics-color -pthread $(CFLAGS) \
		parvsl.cpp $(LIBS) -o parvsl \
		2>&1 | tee parvsl.log

fastparvsl:    parvsl.cpp common.hpp thread_data.hpp
	g++ -fno-diagnostics-color -pthread $(FASTCFLAGS) \
		parvsl.cpp $(LIBS) -o fastparvsl \
		2>&1 | tee fastparvsl.log

vsl.img:	vsl library.lsp vsl.lsp
	time ./vsl -z library.lsp | tee vsl.img.log

fastvsl.img:	fastvsl library.lsp vsl.lsp
	time ./fastvsl -z library.lsp | tee fastvsl.img.log

parvsl.img:	parvsl library.lsp vsl.lsp
	time ./parvsl -z library.lsp | tee parvsl.img.log

fastparvsl.img:	fastparvsl library.lsp vsl.lsp
	time ./fastparvsl -z library.lsp | tee fastparvsl.img.log

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

# rcore is a core of Reduce and I can get this far with a 64M heap
# without triggering a disaster...

rcore:	vsl
	mkdir -p rcore.img.modules
	rm -f rcore.img.modules/* rcore.img inline-defs.dat
	time ./vsl -z -ircore.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildrcore.lsp | tee rcore.log

parrcore:	parvsl
	mkdir -p parrcore.img.modules
	rm -f parrcore.img.modules/* parrcore.img inline-defs.dat
	time ./parvsl -z -iparrcore.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildrcore.lsp | tee parrcore.log

step2:	vsl
	cp rcore.img step2.img
	cp -r rcore.img.modules step2.img.modules
	time ./vsl -istep2.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		step2.red | tee step2.log

arith:	arith.cpp
	g++ -O0 -g -DTEST=1 arith.cpp -o arith

testlogs/%.log:
	time ./test.sh $@

clean:
	rm -rf vsl *.exe *.log *.bak reduce.img* vsl.img* *.o *~ \
		*.ind *.idx *.aux *.ilg *.toc *.xref *.bbl *.blg \
		testlogs/*


docs:   
	cd docs && $(MAKE)

