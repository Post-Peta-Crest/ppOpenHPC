SYSTEM = FX10
#SYSTEM = INTEL

#FX10
ifeq ($(SYSTEM),FX10)
F90		= mpifrtpx
FFLAGS		= -Kfast,openmp
#F90		= mpif90
#FFLAGS		= -fopenmp
endif

#intel
ifeq ($(SYSTEM),INTEL)
F90		= mpiifort
FFLAGS		= -openmp
endif

EXEC		= bem-bb-SCM_AT.out
SRC90		= user_element_ij_dummy.f90  user_rhs_dummy.f90 OAT_InstallRoutines.f90 OAT_DynamicRoutines.f90  OAT_StaticRoutines.f90 OAT_ControlRoutines.f90 OAT_bem-bb-fw-dense-0.1.0.f90
#SRC90		= bem-bb-template-SCM-0.1.0.f90 bem-bb-fw-dense-0.1.0-modified-for-SCM.f90

OBJD		=  $(SRC90:.f90=.o)

.SUFFIXES :	.o .f90

$(EXEC) : $(OBJD)
	$(F90) $(OBJD) $(LIBS) $(FFLAGS) -o $(EXEC)

.f90.o :
	$(F90) $(FFLAGS) -c $<


clean:
	/bin/rm -f *.o *.mod *~ $(EXEC)


