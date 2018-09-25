include ../../Mkinclude

.SUFFIXES: .f90 .o

OBJS = dynamics.o
SRCS = $(OBJS:.o=.f90)
MODS = $(OBJS:.o=.mod)

dynamics : $(SRCS) 
	$(FC) $(FFLAGS) -I$(INCJCUP) -c $^
	mv $(OBJS) $(BINDIR)
	mv $(MODS) $(BINDIR)
clean:
	-rm -f *.mod *.o
