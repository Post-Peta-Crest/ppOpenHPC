include ../Mkinclude

.SUFFIXES: .f90 .o

MODEL = atm
TARGET = $(MODEL).exe
OBJS = dynamics.o atm.o field_def.o driver.o land.o
SRCS = $(OBJS:.o=.f90)
MODS = $(OBJS:.o=.mod)

${TARGET} : $(OBJS)
	cd $(BINDIR) ; $(FC) $(LDFLAGS) -o $@ $^ $(JOBJ) $(LIBS) $(LIBJCUP)

clean:
	-rm -f *mod *o

dynamics.o:
	cd $(DYNAMICS) ; make

.f90.o:	
	$(FC) $(FFLAGS) -I$(INCJCUP) -I$(BINDIR) -c $<
	mv $(MODEL).o $(BINDIR) 
	mv mod_$(MODEL).mod $(BINDIR)

field_def.o:
	cd $(COMMONDIR) ; make

driver.o:
	cd $(DRIVER) ; make $(MODEL)

land.o:
	cd $(LANDDIR) ; make 
