#SYSTEM = FX10
SYSTEM = INTEL

#FX10
ifeq ($(SYSTEM),FX10)
F90		= mpifrtpx
FFLAGS		= -openmp
endif

#intel
ifeq ($(SYSTEM),INTEL)
F90		= mpiifort
FFLAGS		= -qopenmp
endif

EXEC		= bem-bb.out 
SRC90		= bem-bb-fw-dense-0.5.0.f90 user_matrix_element_ij_dummy.f90  user_rhs_dummy.f90
OBJD		=  $(SRC90:.f90=.o)

.SUFFIXES :	.o .f90

$(EXEC) : $(OBJD)
	$(F90)	 $(OBJD) $(LIBS) $(FFLAGS) -o $(EXEC)

.f90.o :
	$(F90) -c $<


clean:
	/bin/rm -f *.o *~ $(EXEC)


