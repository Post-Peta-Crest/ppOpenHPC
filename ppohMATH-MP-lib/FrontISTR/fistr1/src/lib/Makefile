MPIDIR                  = 
MPIBINDIR               = 
MPILIBDIR               = 
MPIINCDIR               = 
MPILIBS                 = 

HECMWDIR                = ../../../hecmw1
HECMWBINDIR             = $(HECMWDIR)/bin
HECMWLIBDIR             = $(HECMWDIR)/lib
HECMWINCDIR             = $(HECMWDIR)/include
HECMWLIBS               = -lfhecmw -lhecmw

FSTRBINDIR              = ../../bin
FSTRLIBDIR              = ../../lib
FSTRINCDIR              = ../../include
FSTRLIBS                = -lffistr -lfistr

METISDIR                = $(HOME)/tools/metis-4.0.3/
METISLIBDIR             = $(METISDIR)
METISINCDIR             = $(METISDIR)/Lib
METISLIBS               = -lmetis
METIS_CFLAGS            = -I$(METISINCDIR)
METIS_LDFLAGS           = -L$(METISLIBDIR) $(METISLIBS)
METIS_F90FLAGS          = -I$(METISINCDIR)
METIS_F90LDFLAGS        = -L$(METISLIBDIR) $(METISLIBS)

MUMPSDIR                = $(HOME)/MUMPS_4.10.0
MUMPSLIBDIR             = $(MUMPSDIR)/lib
MUMPSINCDIR             = $(MUMPSDIR)/include
MUMPSLIBS               = -ldmumps -lmumps_common -lpord
MUMPS_CFLAGS            = 
MUMPS_LDFLAGS           = 
MUMPS_F90FLAGS          = 
MUMPS_F90LDFLAGS        = 

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

REVOCAP_F90FLAGS        = 
REVOCAP_F90LDFLAGS      = 

F90                     = mpifrtpx
F90FLAGS                = -Kfast,openmp
BASE_F90FLAGS           = -I.
MPI_F90FLAGS            = 
HECMW_F90FLAGS          = -I$(HECMWINCDIR)
FSTR_F90FLAGS           = -I$(FSTRINCDIR)
ALL_F90FLAGS            = $(BASE_F90FLAGS) $(FSTR_F90FLAGS) $(HECMW_F90FLAGS) $(MPI_F90FLAGS) $(F90FLAGS) $(REVOCAP_F90FLAGS) $(MUMPS_F90FLAGS)
F90LDFLAGS              = -SCALAPACK -SSL2BLAMP
MPI_F90LDFLAGS          = 
HECMW_F90LDFLAGS        = -L$(HECMWLIBDIR) $(HECMWLIBS)
FSTR_F90LDFLAGS         = -L$(FSTRLIBDIR) $(FSTRLIBS)
ALL_F90LDFLAGS          = $(FSTR_F90LDFLAGS) $(HECMW_F90LDFLAGS) $(MPI_F90LDFLAGS) $(F90LDFLAGS) $(MUMPS_F90LDFLAGS)
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
        fstr_lczparm.o \
        m_step.o \
        m_Makrose.o \
        m_MakrosePartMesh.o \
        m_out.o \
		m_common_struct.o \
        m_fstr.o \
        m_fstr_freqdata.o \
        GaussM.o \
        static_LIB_1d.o \
        static_LIB_2d.o \
        static_LIB_3d.o \
        static_LIB_C3D8.o \
        static_LIB_3dIC.o \
		static_LIB_beam.o \
        static_LIB_shell.o \
        heat_LIB_CAPACITY.o \
        heat_LIB_DFLUX.o \
        heat_LIB_FILM.o \
        heat_LIB_NEUTRAL.o \
        heat_LIB_RADIATE.o \
        heat_LIB_THERMAL.o \
        eigen_LIB_2d1mass.o \
        eigen_LIB_2d2mass.o \
        eigen_LIB_3d1mass.o \
        eigen_LIB_3d2mass.o \
        precheck_LIB_2d.o \
        precheck_LIB_3d.o \
        precheck_LIB_shell.o

OBJSF2 = \
        solve_LINEQ.o \
        static_LIB.o \
        eigen_LIB.o \
        heat_LIB.o

OBJSF_MUMPS = \
        m_Metis403API_f.o \
        m_fstr_para_contact.o

OBJSF_MUMPS_DUMM = \
        m_Metis403API_f_dumm.o \
        m_fstr_para_contact.o

HEADERS =

#------------------------------------------------------------------------------#
all: build-default

build-default:
	@cd utilities && $(MAKE) build-default
	@cd user      && $(MAKE) build-default
	@cd element   && $(MAKE) build-default
	@cd physics   && $(MAKE) build-default
	@cd contact   && $(MAKE) build-default
	$(MAKE) BUILD_CURR

build-with-mumps:
	@cd utilities && $(MAKE) build-default
	@cd user      && $(MAKE) build-default
	@cd element   && $(MAKE) build-default
	@cd physics   && $(MAKE) build-default
	@cd contact   && $(MAKE) build-default
	$(MAKE) BUILD_CURR_WITH_MUMPS

build-with-rcap: build-default

BUILD_CURR: $(TARGET) $(F90TARGET) install-module

BUILD_CURR_WITH_MUMPS: $(TARGET) F90TARGET_MUMPS install-module

$(TARGET): $(OBJS)
	$(AR) $(FSTRLIBDIR)/$@ $(OBJS)

$(F90TARGET): $(OBJSF) $(OBJSF_MUMPS_DUMM) $(OBJSF2)
	$(AR) $(FSTRLIBDIR)/$@ $(OBJSF) $(OBJSF_MUMPS_DUMM) $(OBJSF2)
	F90="$(F90)" F90FLAGS="$(F90FLAGS)" F90TARGET="$(F90TARGET)" AR="$(AR)" RM="$(RM)" sh test_flush.sh

F90TARGET_MUMPS: $(OBJSF) $(OBJSF_MUMPS) $(OBJSF2)
	$(AR) $(FSTRLIBDIR)/$(F90TARGET) $(OBJSF) $(OBJSF_MUMPS) $(OBJSF2)
	F90="$(F90)" F90FLAGS="$(F90FLAGS)" F90TARGET="$(F90TARGET)" AR="$(AR)" RM="$(RM)" sh test_flush.sh

install-header:
	$(CP) $(HEADERS) $(FSTRINCDIR)/.

install-module:
	$(CP) *.$(F90MODULEPOSTFIX) $(FSTRINCDIR)/.

$(OBJS): $(HEADERS)

$(OBJSF):

install:

clean:
	@cd utilities  && $(MAKE) clean
	@cd user       && $(MAKE) clean
	@cd element    && $(MAKE) clean
	@cd physics    && $(MAKE) clean
	@cd contact    && $(MAKE) clean
	$(RM) *.$(COBJFILEPOSTFIX) *.$(F90OBJFILEPOSTFIX) *.$(F90MODULEPOSTFIX)
	${RM} test_flush.log

distclean: clean
