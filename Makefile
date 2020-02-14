
include ./makefile.in

all: makefile.in
ifdef MPPFLAG
	(cd MPP; make -f Makefile.NoahMP)
endif
	(cd Utility_routines;		make)
	(cd phys;			make)
	(cd IO_code;			make)
	(cd run;			make)

clean:
	(cd Utility_routines;		make clean)
	(cd phys;			make clean)
	(cd IO_code;			make clean)
	(cd run;			make clean)
ifdef MPPFLAG
	(cd MPP; make -f Makefile.NoahMP clean)
endif

### explicitly point to other land model options
NoahMP: makefile.in
ifdef MPPFLAG
	(cd MPP; make -f Makefile.NoahMP)
endif
	(cd Utility_routines;		make)
	(cd phys;			make)
	(cd IO_code;			make NoahMP MOD_OPT="-DNoahMP")
	(cd run;			make -f Makefile NoahMP)
