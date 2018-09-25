include ../Mkinclude

.SUFFIXES: .f90 .o

OBJS = driver.o
SRCS = $(OBJS:.o=.f90)
MODS = $(OBJS:.o=.mod)

atm: $(SRCS) 
	$(FC) $(FFLAGS) -c $^ -I$(BINDIR) -DATM
	mv $(OBJS) $(BINDIR) 
ocn: $(SRCS) 
	$(FC) $(FFLAGS) -c $^ -I$(BINDIR) -DOCN
	mv $(OBJS) $(BINDIR) 
chm: $(SRCS) 
	$(FC) $(FFLAGS) -c $^ -I$(BINDIR) -DCHM
	mv $(OBJS) $(BINDIR) 
mi6: $(SRCS) 
	$(FC) $(FFLAGS) -c $^ -I$(BINDIR) -DMI6
	mv $(OBJS) $(BINDIR) 
clean:
	-rm -f *.mod *.o
