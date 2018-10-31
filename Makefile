# Makefile for vsl

CFLAGS = -O0 -g -Wall
#CFLAGS = -O3 -Wall

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

bvsl:	vsl.cpp
	g++ -fno-diagnostics-color $(CFLAGS) \
		-I./bigint-2010.04.30 -DVSL=1 \
		vsl.cpp \
		bigint-2010.04.30/BigInteger.cc \
		bigint-2010.04.30/BigIntegerAlgorithms.cc \
		bigint-2010.04.30/BigIntegerUtils.cc \
		bigint-2010.04.30/BigUnsigned.cc \
		bigint-2010.04.30/BigUnsignedInABase.cc\
		$(LIBS) -o bvsl \
		2>&1 | tee bvsl.log

vsl.img:	vsl library.lsp vsl.lsp
	./vsl -z library.lsp | tee vsl.img.log

reduce:	vsl
	mkdir -p reduce.img.modules
	rm -f reduce.img.modules/* reduce.img inline-defs.dat
	./vsl -z -ireduce.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildreduce.lsp | tee reduce.log

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
	./vsl -z -ircore.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		buildrcore.lsp | tee rcore.log

step2:	vsl
	cp rcore.img step2.img
	cp -r rcore.img.modules step2.img.modules
	./vsl -istep2.img -D@srcdir=. -D@reduce=.. \
		-Dnoinlines=t \
		step2.red | tee step2.log

arith:	arith.cpp
	g++ -O0 -g -DTEST=1 arith.cpp -o arith

testlogs/%.log:
	./test.sh $@

clean:
	rm -rf vsl *.exe *.log *.bak reduce.img* vsl.img* *.o *~ \
		*.ind *.idx *.aux *.ilg *.toc *.xref *.bbl *.blg \
		testlogs/*


docs:   
	cd docs && $(MAKE)

