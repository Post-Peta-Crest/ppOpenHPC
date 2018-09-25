include ../../../Makefile.in

HECMWDIR                = ../../../ppohFEM
HECMWBINDIR             = $(HECMWDIR)/bin
HECMWLIBDIR             = $(HECMWDIR)/lib
HECMWINCDIR             = $(HECMWDIR)/include
HECMWLIBS               = -lfppohFEM -lppohFEM -lfppohFEM

FSTRBINDIR              = ../../bin
FSTRLIBDIR              = ../../lib
FSTRINCDIR              = ../../include
FSTRLIBS                = -lffistr -lfistr

METISDIR                = $(HOME)/tools/metis
METISLIBDIR             = $(METISDIR)
METISINCDIR             = $(METISDIR)/Lib
METISLIBS               = -lmetis
METIS_CFLAGS            = 
METIS_LDFLAGS           = 
METIS_F90FLAGS          = 
METIS_F90LDFLAGS        = 

PARMETISDIR             = $(HOME)/ParMetis-3.1
PARMETISLIBDIR          = $(PARMETISDIR)
PARMETISINCDIR          = $(PARMETISDIR)/ParMETISLib
PARMETISLIBS            = -lparmetis -lmetis
PARMETIS_CFLAGS         = 
PARMETIS_LDFLAGS        = 
PARMETIS_F90FLAGS       = 
PARMETIS_F90LDFLAGS     = 

MUMPSDIR                = $(HOME)/MUMPS_4.10.0
MUMPSLIBDIR             = $(MUMPSDIR)/lib
MUMPSINCDIR             = $(MUMPSDIR)/include
MUMPSLIBS               = -ldmumps -lmumps_common -lpord
MUMPS_CFLAGS            = 
MUMPS_LDFLAGS           = 
MUMPS_F90FLAGS          = 
MUMPS_F90LDFLAGS        = 

MLDIR                   = 
MLLIBDIR                = 
MLLIBS                  = -lml -lzoltan
ML_LDFLAGS              = 
ML_F90LDFLAGS           = 

REVOCAPDIR              = $(HOME)/REVOCAP_Coupler
REVOCAPINCDIR           = $(REVOCAPDIR)/librcap
REVOCAPLIBDIR           = $(REVOCAPDIR)/librcap
REVOCAPLIBS             = -lrcapf

REFINERDIR              = $(HOME)/REVOCAP_Refiner
REFINERINCDIR           = $(REFINERDIR)/Refiner
REFINERLIBDIR           = $(REFINERDIR)/lib
REFINERLIBS             = -lRcapRefiner

BASE_CFLAGS             = -I.
MPI_CFLAGS              = 
HECMW_CFLAGS            = -I$(HECMWINCDIR)
FSTR_CFLAGS             = -I$(FSTRINCDIR)
ALL_CFLAGS              = $(BASE_CFLAGS) $(FSTR_CFLAGS) $(HECMW_CFLAGS) $(MPI_CFLAGS) $(METIS_CFLAGS) $(PARMETIS_CFLAGS) $(MUMPS_CFLAGS) $(CFLAGS)
MPI_LDFLAGS             = 
HECMW_LDFLAGS           = -L$(HECMWLIBDIR) $(HECMWLIBS)
FSTR_LDFLAGS            = -L$(FSTRLIBDIR) $(FSTRLIBS)
ALL_LDFLAGS             = $(FSTR_LDFLAGS) $(HECMW_LDFLAGS) $(ML_LDFLAGS) $(MUMPS_LDFLAGS) $(METIS_LDFLAGS) $(PARMETIS_LDFLAGS) $(MPI_LDFLAGS) $(LDFLAGS)

REVOCAP_F90FLAGS        = 
REVOCAP_F90LDFLAGS      = 

BASE_F90FLAGS           = -I.
MPI_F90FLAGS            = 
HECMW_F90FLAGS          = -I$(HECMWINCDIR)
FSTR_F90FLAGS           = -I$(FSTRINCDIR)
ALL_F90FLAGS            = $(BASE_F90FLAGS) $(FSTR_F90FLAGS) $(HECMW_F90FLAGS) $(MPI_F90FLAGS) $(METIS_F90FLAGS) $(PARMETIS_F90FLAGS) $(MUMPS_F90FLAGS) $(F90FLAGS) $(REVOCAP_F90FLAGS)
MPI_F90LDFLAGS          = 
HECMW_F90LDFLAGS        = -L$(HECMWLIBDIR) $(HECMWLIBS)
FSTR_F90LDFLAGS         = -L$(FSTRLIBDIR) $(FSTRLIBS)
ALL_F90LDFLAGS          = $(FSTR_F90LDFLAGS) $(HECMW_F90LDFLAGS) $(ML_F90LDFLAGS) $(MUMPS_F90LDFLAGS) $(METIS_F90LDFLAGS) $(PARMETIS_F90LDFLAGS) $(REVOCAP_F90LDFLAGS) $(MPI_F90LDFLAGS) $(F90LDFLAGS)
F90LINKER               = $(F90)

RANLIB                  = ranlib

#------------------------------------------------------------------------------#
TARGET                  = app_heat

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
	app_heat_main.o


HEADERS =

#------------------------------------------------------------------------------#
all: build-default

build-default: $(OBJSF) $(HECMWLIBDIR) $(FSTRLIBDIR)
	$(F90LINKER) $(ALL_F90FLAGS) $(F90OPTFLAGS) \
	-o $(TARGET) \
	$(OBJSF) $(ALL_F90LDFLAGS)
	$(CP) $(TARGET) $(FSTRBINDIR)/.

build-with-paracon: $(OBJSFP) $(HECMWLIBDIR) $(FSTRLIBDIR)
	$(F90LINKER) $(ALL_F90FLAGS) $(F90OPTFLAGS) \
	-o $(TARGET) \
	$(OBJSFP) $(ALL_F90LDFLAGS)
	$(CP) $(TARGET) $(FSTRBINDIR)/.

$(OBJS): $(HEADERS)

$(OBJSF):

$(OBJSFP):

install:

clean:
	$(RM) *.$(COBJFILEPOSTFIX) *.$(F90OBJFILEPOSTFIX) *.$(F90MODULEPOSTFIX)

distclean: clean
	$(RM) $(TARGET)
