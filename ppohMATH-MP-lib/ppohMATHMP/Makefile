JCUPINCDIR = ../Jcup/include
JCUPLIBDIR = ../Jcup/lib


CC  = mpfccpx
F90 = mpifrtpx

OBJS = mod_kml.o grid_base.o fem_grid.o fdm_grid.o fs_grid.o fs_namelist.o fs_interpolation.o m_ppohMATHMP_FEM.o  m_ppohMATHMP_FDM.o m_ppohMATHMP_FILE.o
FSOBJS = prg_mkmappingtable.o
TARGET = mkmappingtable
F90OPTFLAGS = -O2

.SUFFIXES:
.SUFFIXES: .o .o .c .f90 .f03 .f

.c.o:
	$(CC) -c $(ALL_CFLAGS) $(OPTFLAGS) $< -o $@

.f90.o:
	$(F90) -c $(ALL_F90FLAGS) $(F90OPTFLAGS) -I$(JCUPINCDIR) $< -o $@

.f03.o:
	$(F90) -c $(ALL_F90FLAGS) $(F90OPTFLAGS) -I$(JCUPINCDIR) $< -o $@

.f.o:
	$(F90) -c $(ALL_F90FLAGS) $(F90OPTFLAGS) $< -o $@

all : $(OBJS) $(FSOBJS)
	$(F90) $(OBJS) $(FSOBJS) -o $(TARGET) -L$(JCUPLIBDIR) -ljcup
	ar r libppohMATHMP.a *.o
clean : 
	rm lib*a *mod *.o
