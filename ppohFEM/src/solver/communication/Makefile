include ../../../../Makefile.in

HECMWBINDIR       = ../../../bin
HECMWLIBDIR       = ../../../lib
HECMWINCDIR       = ../../../include
HECMWLIBS         = -lfppohFEM -lppohFEM

REFINERDIR        = $(HOME)/REVOCAP_Refiner
REFINERINCDIR     = $(REFINERDIR)/Refiner
REFINERLIBDIR     = $(REFINERDIR)/lib
REFINERLIBS       = -lRcapRefiner

#CC                = mpicc
BASE_CFLAGS       = -I.
MPI_CFLAGS        = 
HECMW_CFLAGS      = -I$(HECMWINCDIR)
ALL_CFLAGS        = $(BASE_CFLAGS) $(HECMW_CFLAGS) $(MPI_CFLAGS) $(CFLAGS)
MPI_LDFLAGS       = 
HECMW_LDFLAGS     = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_LDFLAGS       = $(HECMW_LDFLAGS) $(MPI_LDFLAGS) $(LDFLAGS)

#F90               = mpif90
BASE_F90FLAGS     = -I.
MPI_F90FLAGS      = 
HECMW_F90FLAGS    = -I$(HECMWINCDIR)
ALL_F90FLAGS      = $(BASE_F90FLAGS) $(HECMW_F90FLAGS) $(MPI_F90FLAGS) $(F90FLAGS)
MPI_F90LDFLAGS    = 
HECMW_F90LDFLAGS  = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_F90LDFLAGS    = $(HECMW_F90LDFLAGS) $(MPI_F90LDFLAGS) $(F90LDFLAGS)

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
OBJS =

OBJSF = \
	hecmw_solver_misc.o

OBJF_COMM = hecmw_comm_f.o
OBJF_COMM_DUMM = hecmw_comm_f_dumm.o

OBJSF_SR = \
	hecmw_solver_SR_11.o \
	hecmw_solver_SR_11i.o \
	hecmw_solver_SR_22.o \
	hecmw_solver_SR_22i.o \
	hecmw_solver_SR_33.o \
	hecmw_solver_SR_33i.o \
	hecmw_solver_SR_44.o \
	hecmw_solver_SR_44i.o \
	hecmw_solver_SR_66.o \
	hecmw_solver_SR_66i.o \
	hecmw_solver_SR_mm.o \
	hecmw_solver_SR_mmi.o

OBJSF_SR_DUMM = \
	hecmw_solver_SR_11_dumm.o \
	hecmw_solver_SR_11i_dumm.o \
	hecmw_solver_SR_22_dumm.o \
	hecmw_solver_SR_22i_dumm.o \
	hecmw_solver_SR_33_dumm.o \
	hecmw_solver_SR_33i_dumm.o \
	hecmw_solver_SR_44_dumm.o \
	hecmw_solver_SR_44i_dumm.o \
	hecmw_solver_SR_66_dumm.o \
	hecmw_solver_SR_66i_dumm.o \
	hecmw_solver_SR_mm_dumm.o \
	hecmw_solver_SR_mmi_dumm.o

HEADERS =

#-----------------------------------------------------------------------------#
all: build-default

build-default: $(TARGET) with-mpi $(F90TARGET) install-module

build-serial: $(TARGET) without-mpi $(F90TARGET) install-module

build-without-f: $(TARGET)

$(TARGET): $(OBJS)
	$(AR) $(HECMWLIBDIR)/$@ $(OBJS)

$(F90TARGET): $(OBJSF)
	$(AR) $(HECMWLIBDIR)/$@ $(OBJSF)

with-mpi: $(OBJSF_SR) $(OBJF_COMM)
	$(AR) $(HECMWLIBDIR)/$(F90TARGET) $(OBJF_COMM) $(OBJSF_SR)

without-mpi: $(OBJSF_SR_DUMM) $(OBJF_COMM_DUMM)
	$(AR) $(HECMWLIBDIR)/$(F90TARGET) $(OBJF_COMM_DUMM) $(OBJSF_SR_DUMM)

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
