include ../Mkinclude

.SUFFIXES: .f90 .o

MODEL = land
TARGET = $(MODEL).exe
OBJS = $(MODEL).o field_def.o
SRCS = $(OBJS:.o=.f90)
MODS = $(OBJS:.o=.mod)

${TARGET} : $(OBJS)
#	cd $(BINDIR) ; $(FC) $(LDFLAGS) -o $@ $^ $(LIBS) $(LIBJCUP)

clean:
	-rm -f *mod *o

.f90.o:	
	$(FC) $(FFLAGS) -I$(INCJCUP) -I$(BINDIR) -c $<
	mv $(MODEL).o $(BINDIR) 
	mv mod_$(MODEL).mod $(BINDIR)

field_def.o:
	cd $(COMMONDIR) ; make

