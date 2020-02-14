
include ./makefile.in

all: makefile.in
ifdef MPPFLAG
	(cd MPP; make -f Makefile.NoahMP)
endif
	(cd share;		make)
	(cd phys;			make)
	(cd hrldas;			make)
	(cd run;			make)

clean:
	(cd share;		make clean)
	(cd phys;			make clean)
	(cd hrldas;			make clean)
	(cd run;			make clean)
ifdef MPPFLAG
	(cd MPP; make -f Makefile.NoahMP clean)
endif

### explicitly point to other land model options
NoahMP: makefile.in
ifdef MPPFLAG
	(cd MPP; make -f Makefile.NoahMP)
endif
	(cd share;		make)
	(cd phys;			make)
	(cd hrldas;			make NoahMP MOD_OPT="-DNoahMP")
	(cd run;			make -f Makefile NoahMP)
