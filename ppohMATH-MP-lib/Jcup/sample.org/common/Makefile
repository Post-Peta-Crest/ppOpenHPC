include ../Mkinclude

.SUFFIXES: .f90 .o

OBJS = jcup_interpolation_sample.o field_def.o
SRCS = $(OBJS:.o=.f90)
MODS = $(OBJS:.o=.mod)

all: $(SRCS) 
	$(FC) $(FFLAGS) -c $^ -I$(INCJCUP) -I$(BINDIR) 
	mv $(OBJS) $(BINDIR) ; mv *.mod $(BINDIR)
clean:
	-rm -f *.mod *.o
