MPIDIR                  = 
MPIBINDIR               = 
MPILIBDIR               = 
MPIINCDIR               = 
MPILIBS                 = 

HECMWDIR                = ../../../../hecmw1
HECMWBINDIR             = $(HECMWDIR)/bin
HECMWLIBDIR             = $(HECMWDIR)/lib
HECMWINCDIR             = $(HECMWDIR)/include
HECMWLIBS               = -lfhecmw -lhecmw

FSTRBINDIR              = ../../../bin
FSTRLIBDIR              = ../../../lib
FSTRINCDIR              = ../../../include
FSTRLIBS                = -lffistr -lfistr

REVOCAPDIR              = $(HOME)/REVOCAP_Coupler
REVOCAPINCDIR           = $(REVOCAPDIR)/librcap
REVOCAPLIBDIR           = $(REVOCAPDIR)/librcap
REVOCAPLIBS             = -lrcapf

CC                      = mpifccpx
CFLAGS                  = -Kfast,openmp
BASE_CFLAGS             = -I.
MPI_CFLAGS              = 
HECMW_CFLAGS            = -I$(HECMWINCDIR)
FSTR_CFLAGS             = -I$(FSTRINCDIR)
ALL_CFLAGS              = $(BASE_CFLAGS) $(FSTR_CFLAGS) $(HECMW_CFLAGS) $(MPI_CFLAGS) $(CFLAGS)
LDFLAGS                 = -lm
MPI_LDFLAGS             = 
HECMW_LDFLAGS           = -L$(HECMWLIBDIR) $(HECMWLIBS)
FSTR_LDFLAGS            = -L$(FSTRLIBDIR) $(FSTRLIBS)
ALL_LDFLAGS             = $(FSTR_LDFLAGS) $(HECMW_LDFLAGS) $(MPI_LDFLAGS) $(LDFLAGS)
OPTFLAGS                =  

F90                     = mpifrtpx
F90FLAGS                = -Kfast,openmp
BASE_F90FLAGS           = -I.
MPI_F90FLAGS            = 
HECMW_F90FLAGS          = -I$(HECMWINCDIR)
FSTR_F90FLAGS           = -I$(FSTRINCDIR)
ALL_F90FLAGS            = $(BASE_F90FLAGS) $(FSTR_F90FLAGS) $(HECMW_F90FLAGS) $(MPI_F90FLAGS) $(F90FLAGS)
F90LDFLAGS              = -SCALAPACK -SSL2BLAMP
MPI_F90LDFLAGS          = 
HECMW_F90LDFLAGS        = -L$(HECMWLIBDIR) $(HECMWLIBS)
FSTR_F90LDFLAGS         = -L$(FSTRLIBDIR) $(FSTRLIBS)
ALL_F90LDFLAGS          = $(FSTR_F90LDFLAGS) $(HECMW_F90LDFLAGS) $(MPI_F90LDFLAGS) $(F90LDFLAGS)
F90OPTFLAGS             = 

AR                      = ar ruv
CP                      = cp -f
RM                      = rm -f
RANLIB                  = ranlib
MKDIR                   = mkdir -p

#------------------------------------------------------------------------------#
TARGET                  = libfistr.a
F90TARGET               = libffistr.a

F90MODULEPOSTFIX        = mod
COBJFILEPOSTFIX         = o
F90OBJFILEPOSTFIX       = o

#------------------------------------------------------------------------------#
.SUFFIXES:
.SUFFIXES: .o .o .c .f90 .f

.c.o:
	$(CC) -c $(ALL_CFLAGS) $(OPTFLAGS) $< -o $@

.f90.o:
	$(F90) -c $(ALL_F90FLAGS) $(F90OPTFLAGS) $< -o $@

.f.o:
	$(F90) -c $(ALL_F90FLAGS) $(F90OPTFLAGS) $< -o $@

#------------------------------------------------------------------------------#
OBJS =

OBJSF = \
        fstr_Restart.o \
        fstr_NodalStress.o \
        fstr_StiffMatrix.o \
        fstr_Update.o \
        readtemp.o \
        static_echo.o \
        static_make_result.o \
        fstr_ass_load.o \
        static_output.o \
        static_mat_ass_main.o \
        fstr_mat_con_contact.o \
        fstr_Residual.o \
        fstr_AddContactStiff.o \
        fstr_AddBC.o \
        static_mat_ass.o \
        fstr_solve_LINEAR.o \
        fstr_mat_resid_contact.o \
        sparse_matrix_contact.o \
        solve_LINEQ_MUMPS_contact.o \
        set_arrays_DirectSolver.o \
        solve_LINEQ_mkl.o \
        solve_LINEQ_direct_serial_lag.o \
        solve_LINEQ_contact.o \
        fstr_solve_NonLinear.o \
        fstr_solve_NLGEOM.o

OBJSF_MKL_DUMM = \
        dummy_solve_LINEQ_mkl.o

HEADERS =

#------------------------------------------------------------------------------#
all: build-without-mkl

build-default: $(TARGET) $(F90TARGET) install-module

build-without-mkl: $(TARGET) F90TARGET_MKL install-module

$(TARGET): $(OBJS)
	$(AR) $(FSTRLIBDIR)/$@ $(OBJS)

$(F90TARGET): $(OBJSF)
	$(AR) $(FSTRLIBDIR)/$@ $(OBJSF) 

F90TARGET_MKL: $(OBJSF) $(OBJSF_MKL_DUMM)
	$(AR) $(FSTRLIBDIR)/$(F90TARGET) $(OBJSF) $(OBJSF_MKL_DUMM)

install-header:
	$(CP) $(HEADERS) $(FSTRINCDIR)/.

install-module:
	$(CP) *.$(F90MODULEPOSTFIX) $(FSTRINCDIR)/.

$(OBJS): $(HEADERS)

$(OBJSF):

install:

clean:
	$(RM) *.$(COBJFILEPOSTFIX) *.$(F90OBJFILEPOSTFIX) *.$(F90MODULEPOSTFIX)

distclean: clean
