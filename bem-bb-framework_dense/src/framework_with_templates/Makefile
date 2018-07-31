#SYSTEM = FX10
SYSTEM = INTEL

#FX10
ifeq ($(SYSTEM),FX10)
F90		= mpifrtpx
FFLAGS		= -Kfast,openmp
endif

#intel
ifeq ($(SYSTEM),INTEL)
F90		= mpiifort
FFLAGS		= -qopenmp
endif

EXEC		= bem-bb-SCM.out 
SRC90		= bem-bb-fw-dense-0.5.0.f90  bem-bb-template-SCM-0.5.0.f90
OBJD		=  $(SRC90:.f90=.o)

.SUFFIXES :	.o .f90

$(EXEC) : $(OBJD)
	$(F90)	 $(OBJD) $(LIBS) $(FFLAGS) -o $(EXEC)

.f90.o :
	$(F90) -c $< $(FFLAGS)


clean:
	/bin/rm -f *.o *~ $(EXEC)


