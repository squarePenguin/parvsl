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

all:	vsl vsl.img vsl.pdf vslmanual.pdf

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



# What follows is for building the write-up.

vsl.pdf:	vsl.tex vslfunctions.tex vsl.bib \
		afirstsession.tex fetching.tex functionlist.tex \
		funsasvalues.tex glossary.tex insidearith.tex \
		insidecheckpoint.tex insidecompile.tex insidedata.tex \
		insideeval.tex insidegarbage.tex insidetricks.tex \
		macros.tex moredata.tex nameslistsandvariables.tex \
		purpose.tex sampleprogs.tex simpfndefs.tex \
		stylesoflisp.tex testscomparisons.tex versionsoflisp.tex \
		whatislisp.tex
	touch vsl.ind
	pdflatex vsl.tex
	bibtex vsl
	makeindex vsl.idx
	pdflatex vsl.tex
	pdflatex vsl.tex

segments:	vsl.pdf
	-mkdir pages
	pdfjam vsl.pdf 1-16    -o pages/001-016.pdf || pdfjam vsl.pdf 1-      -o pages/001-016.pdf
	pdfjam vsl.pdf 17-32   -o pages/017-032.pdf || pdfjam vsl.pdf 17-     -o pages/017-032.pdf
	pdfjam vsl.pdf 33-48   -o pages/033-048.pdf || pdfjam vsl.pdf 33-     -o pages/033-048.pdf
	pdfjam vsl.pdf 49-64   -o pages/049-064.pdf || pdfjam vsl.pdf 49-     -o pages/049-064.pdf
	pdfjam vsl.pdf 65-80   -o pages/065-080.pdf || pdfjam vsl.pdf 65-     -o pages/065-080.pdf
	pdfjam vsl.pdf 81-96   -o pages/081-096.pdf || pdfjam vsl.pdf 81-     -o pages/081-096.pdf
	pdfjam vsl.pdf 97-112  -o pages/097-112.pdf || pdfjam vsl.pdf 97-     -o pages/097-112.pdf
	pdfjam vsl.pdf 113-128 -o pages/113-128.pdf || pdfjam vsl.pdf 113-    -o pages/113-128.pdf
	pdfjam vsl.pdf 129-144 -o pages/129-144.pdf || pdfjam vsl.pdf 129-    -o pages/129-144.pdf
	pdfjam vsl.pdf 145-160 -o pages/145-160.pdf || pdfjam vsl.pdf 145-    -o pages/145-160.pdf
	pdfjam vsl.pdf 161-176 -o pages/161-176.pdf || pdfjam vsl.pdf 161-    -o pages/161-176.pdf
	pdfjam vsl.pdf 177-192 -o pages/177-192.pdf || pdfjam vsl.pdf 177-    -o pages/177-192.pdf
	pdfjam vsl.pdf 193-208 -o pages/193-208.pdf || pdfjam vsl.pdf 193-    -o pages/193-208.pdf
	pdfjam vsl.pdf 209-224 -o pages/209-224.pdf || pdfjam vsl.pdf 209-    -o pages/209-224.pdf
	pdfjam vsl.pdf 225-240 -o pages/225-240.pdf || pdfjam vsl.pdf 225-    -o pages/225-240.pdf
	pdfjam vsl.pdf 241-256 -o pages/241-256.pdf || pdfjam vsl.pdf 241-    -o pages/241-256.pdf
	pdfjam vsl.pdf 257-272 -o pages/257-272.pdf || pdfjam vsl.pdf 257-    -o pages/257-272.pdf
	pdfjam vsl.pdf 273-288 -o pages/273-288.pdf || pdfjam vsl.pdf 273-    -o pages/273-288.pdf
	pdfjam vsl.pdf 289-304 -o pages/289-304.pdf || pdfjam vsl.pdf 289-    -o pages/289-304.pdf
	pdfjam vsl.pdf 305-320 -o pages/305-320.pdf || pdfjam vsl.pdf 305-    -o pages/305-320.pdf

vslmanual.pdf:	vslmanual.tex vslfunctions.tex vsl.bib
	touch vslmanual.ind
	pdflatex vslmanual.tex
	makeindex vslmanual.idx
	pdflatex vslmanual.tex
	pdflatex vslmanual.tex

# Get approximate word count.
count:
	cat vsl.tex afirstsession.tex fetching.tex functionlist.tex \
	  funsasvalues.tex glossary.tex insidearith.tex \
	  insidecheckpoint.tex insidecompile.tex insidedata.tex \
	  insideeval.tex insidegarbage.tex insidetricks.tex \
	  insidereadprint.tex \
	  macros.tex moredata.tex nameslistsandvariables.tex \
	  purpose.tex sampleprogs.tex simpfndefs.tex \
	  stylesoflisp.tex testscomparisons.tex versionsoflisp.tex \
	  whatislisp.tex vslfunctions.tex | untex - | wc

spellvsl:
	aspell -l en-gb -c vsl.tex

spellmanual:
	aspell -l en-gb -c vslmanual.tex

spellfns:
	aspell -l en-gb -c vslfunctions.tex

# end of Makefile


