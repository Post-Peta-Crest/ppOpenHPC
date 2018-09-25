MPIDIR                  = 
MPIBINDIR               = 
MPILIBDIR               = 
MPIINCDIR               = 
MPILIBS                 = 

HECMWDIR                = ../../../hecmw2
HECMWBINDIR             = $(HECMWDIR)/bin
HECMWLIBDIR             = $(HECMWDIR)/lib
HECMWINCDIR             = $(HECMWDIR)/include
HECMWLIBS               = -lfhecmw2 -lhecmw2 -lstdc++
HECMWDIR2               = ../../../hecmw1
HECMWBINDIR2            = $(HECMWDIR2)/bin
HECMWLIBDIR2            = $(HECMWDIR2)/lib
HECMWINCDIR2            = $(HECMWDIR2)/include
HECMWLIBS2              = -lfhecmw -lhecmw

FSTRBINDIR              = ../../bin
FSTRLIBDIR              = ../../lib
FSTRINCDIR              = ../../include
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
HECMW_CFLAGS2           = -I$(HECMWINCDIR2)
FSTR_CFLAGS             = -I$(FSTRINCDIR)
ALL_CFLAGS              = $(BASE_CFLAGS) $(FSTR_CFLAGS) $(HECMW_CFLAGS) $(HECMW_CFLAGS2) $(MPI_CFLAGS) $(CFLAGS)
LDFLAGS                 = -lm
MPI_LDFLAGS             = 
HECMW_LDFLAGS           = -L$(HECMWLIBDIR) $(HECMWLIBS)
HECMW_LDFLAGS2          = -L$(HECMWLIBDIR2) $(HECMWLIBS2)
FSTR_LDFLAGS            = -L$(FSTRLIBDIR) $(FSTRLIBS)
ALL_LDFLAGS             = $(FSTR_LDFLAGS) $(HECMW_LDFLAGS) $(HECMW_LDFLAGS2) $(MPI_LDFLAGS) $(LDFLAGS)
OPTFLAGS                =  

REVOCAP_F90FLAGS        = 
REVOCAP_F90LDFLAGS      = 

F90                     = mpifrtpx
F90FLAGS                = -Kfast,openmp
BASE_F90FLAGS           = -I.
MPI_F90FLAGS            = 
HECMW_F90FLAGS          = -I$(HECMWINCDIR)
HECMW_F90FLAGS2         = -I$(HECMWINCDIR2)
FSTR_F90FLAGS           = -I$(FSTRINCDIR)
ALL_F90FLAGS            = $(BASE_F90FLAGS) $(FSTR_F90FLAGS) $(HECMW_F90FLAGS) $(HECMW_F90FLAGS2) $(MPI_F90FLAGS) $(F90FLAGS) $(REVOCAP_F90FLAGS)
F90LDFLAGS              = -SCALAPACK -SSL2BLAMP
MPI_F90LDFLAGS          = 
HECMW_F90LDFLAGS        = -L$(HECMWLIBDIR) $(HECMWLIBS)
HECMW_F90LDFLAGS2       = -L$(HECMWLIBDIR2) $(HECMWLIBS2)
FSTR_F90LDFLAGS         = -L$(FSTRLIBDIR) $(FSTRLIBS)
ALL_F90LDFLAGS          = $(FSTR_F90LDFLAGS) $(HECMW_F90LDFLAGS) $(HECMW_F90LDFLAGS2) $(MPI_F90LDFLAGS) $(F90LDFLAGS) $(REVOCAP_F90LDFLAGS)
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
OBJS = \
	hecmw2fstr_connect_conv.o \
	fstr_ctrl_util.o \
	fstr_sort_index.o

OBJSF = \
	hecmw2fstr_mesh_conv.o \
	fstr_setup_util.o \
	fstr_ctrl_modifier.o \
	fstr_contact.o \
	fstr_ctrl_common.o \
	fstr_ctrl_material.o \
	fstr_ctrl_static.o \
	fstr_ctrl_heat.o \
	fstr_ctrl_eigen.o \
	fstr_ctrl_dynamic.o \
	fstr_get_prop.o \
	fstr_setup.o \
	fstr_debug_dump.o \
	fstr_precheck.o

OBJSF_RCAP = \
	fstr_rcap_io.o

OBJSF_RCAP_DUMM = \
	fstr_rcap_io_dummy.o

INCLUDESF = \
	fstr_ctrl_util_f.inc

HEADERS = \
	hecmw2fstr_connect_conv.h \
	fstr_ctrl_util.h \
	fstr_sort_index.h

#------------------------------------------------------------------------------#
all: build-default

build-default: $(TARGET) $(F90TARGET) install-module

build-with-rcap: $(TARGET) F90TARGET_RCAP install-module

$(TARGET): $(OBJS)
	$(AR) $(FSTRLIBDIR)/$@ $(OBJS)

$(F90TARGET): $(OBJSF) $(OBJSF_RCAP_DUMM)
	$(AR) $(FSTRLIBDIR)/$@ $(OBJSF) $(OBJSF_RCAP_DUMM)

F90TARGET_RCAP: $(OBJSF) $(OBJSF_RCAP_DUMM)
	$(AR) $(FSTRLIBDIR)/$(F90TARGET) $(OBJSF) $(OBJSF_RCAP_DUMM)

install-header:
	$(CP) $(HEADERS) $(FSTRINCDIR)/.
	$(CP) $(INCLUDESF) $(FSTRINCDIR)/.

install-module:
	$(CP) *.$(F90MODULEPOSTFIX) $(FSTRINCDIR)/.

$(OBJS): $(HEADERS)

$(OBJSF): $(INCLUDESF)

install:

clean:
	$(RM) *.$(COBJFILEPOSTFIX) *.$(F90OBJFILEPOSTFIX) *.$(F90MODULEPOSTFIX)

distclean: clean
