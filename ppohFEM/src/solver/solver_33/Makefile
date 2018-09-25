include ../../../../Makefile.in

HECMWBINDIR       = ../../../bin
HECMWLIBDIR       = ../../../lib
HECMWINCDIR       = ../../../include
HECMWLIBS         = -lfppohFEM -lppohFEM

REFINERDIR        = $(HOME)/REVOCAP_Refiner
REFINERINCDIR     = $(REFINERDIR)/Refiner
REFINERLIBDIR     = $(REFINERDIR)/lib
REFINERLIBS       = -lRcapRefiner

MLDIR             = 
MLLIBDIR          = 
MLINCDIR          = 
MLLIBS            = -lml -lzoltan
ML_CFLAGS         = 
ML_LDFLAGS        = 
ML_F90FLAGS       = 
ML_F90LDFLAGS     = 

#CC                = mpicc
BASE_CFLAGS       = -I.
MPI_CFLAGS        = 
HECMW_CFLAGS      = -I$(HECMWINCDIR)
ALL_CFLAGS        = $(BASE_CFLAGS) $(HECMW_CFLAGS) $(MPI_CFLAGS) $(ML_CFLAGS) $(CFLAGS)
MPI_LDFLAGS       = 
HECMW_LDFLAGS     = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_LDFLAGS       = $(HECMW_LDFLAGS) $(ML_LDFLAGS) $(MPI_LDFLAGS) $(LDFLAGS)

#F90               = mpif90
BASE_F90FLAGS     = -I.
MPI_F90FLAGS      = 
HECMW_F90FLAGS    = -I$(HECMWINCDIR)
ALL_F90FLAGS      = $(BASE_F90FLAGS) $(HECMW_F90FLAGS) $(MPI_F90FLAGS) $(ML_F90FLAGS) $(F90FLAGS)
MPI_F90LDFLAGS    = 
HECMW_F90LDFLAGS  = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_F90LDFLAGS    = $(HECMW_F90LDFLAGS) $(ML_F90LDFLAGS) $(MPI_F90LDFLAGS) $(F90LDFLAGS)

RANLIB            = ranlib

#-----------------------------------------------------------------------------#
TARGET            = libppohFEM.a
F90TARGET         = libfppohFEM.a

F90MODULEPOSTFIX  = mod
COBJFILEPOSTFIX   = o
F90OBJFILEPOSTFIX = o

#-----------------------------------------------------------------------------#
.SUFFIXES:
.SUFFIXES: .o o .c .f90 .f

.c.o:
	$(CC) -c $< $(ALL_CFLAGS) $(OPTFLAGS)

.f90.o:
	$(F90) -c $< $(ALL_F90FLAGS) $(F90OPTFLAGS)

.f.o:
	$(F90) -c $< $(ALL_F90FLAGS) $(F90OPTFLAGS)

#-----------------------------------------------------------------------------#
OBJS = \
	hecmw_ML_wrapper.o \

OBJSF = \
	hecmw_jadm.o \
	hecmw_tuning_fx.o \
	hecmw_solver_las_33.o \
	hecmw_solver_scaling_33.o \
	hecmw_precond_BILU_33.o \
	hecmw_precond_DIAG_33.o \
	hecmw_precond_SSOR_33.o \
	hecmw_mat_id.o \
	hecmw_ML_helper.o \
	hecmw_precond_ML_33.o \
	hecmw_precond_33.o \
	hecmw_solver_CG_33.o \
	hecmw_solver_BiCGSTAB_33.o \
	hecmw_solver_GMRES_33.o \
	hecmw_solver_GPBiCG_33.o \
	hecmw_solver_33.o

OBJSF_LAPACK = hecmw_estimate_condition.o

OBJSF_LAPACK_DUMM = hecmw_estimate_condition_dumm.o

HEADERS =

#-----------------------------------------------------------------------------#
all: build-default

build-default: $(TARGET) $(F90TARGET) install-module

build-serial: $(TARGET) $(F90TARGET) install-module

build-with-lapack: $(TARGET) F90TARGET_LAPACK install-module

build-serial-with-lapack: $(TARGET) F90TARGET_LAPACK install-module

build-without-f: $(TARGET)

$(TARGET): $(OBJS)
	$(AR) $(HECMWLIBDIR)/$@ $(OBJS)

$(F90TARGET): $(OBJSF_LAPACK_DUMM) $(OBJSF)
	$(AR) $(HECMWLIBDIR)/$@ $(OBJSF_LAPACK_DUMM) $(OBJSF)

F90TARGET_LAPACK: $(OBJSF_LAPACK) $(OBJSF)
	$(AR) $(HECMWLIBDIR)/$(F90TARGET) $(OBJSF_LAPACK) $(OBJSF)

$(OBJS): $(HEADERS)

$(OBJSF):

install-header: $(HEADERS)
	$(CP) $(HEADERS) $(HECMWINCDIR)/.

install-module:
	$(CP) *.$(F90MODULEPOSTFIX) $(HECMWINCDIR)/.

install:

clean:
	$(RM) *.$(COBJFILEPOSTFIX) *.$(F90OBJFILEPOSTFIX) *.$(F90MODULEPOSTFIX)

distclean: clean
