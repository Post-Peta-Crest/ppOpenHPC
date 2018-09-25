include ../../../Makefile.in

HECMWBINDIR       = ../../bin
HECMWLIBDIR       = ../../lib
HECMWINCDIR       = ../../include
HECMWLIBS         = -lfppohFEM -lppohFEM

REFINERDIR        = $(HOME)/REVOCAP_Refiner
REFINERINCDIR     = $(REFINERDIR)/Refiner
REFINERLIBDIR     = $(REFINERDIR)/lib
REFINERLIBS       = -lRcapRefiner

BASE_CFLAGS       = -I.
MPI_CFLAGS        = 
HECMW_CFLAGS      = -I$(HECMWINCDIR)
ALL_CFLAGS        = $(BASE_CFLAGS) $(HECMW_CFLAGS) $(MPI_CFLAGS) $(CFLAGS)
MPI_LDFLAGS       = 
HECMW_LDFLAGS     = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_LDFLAGS       = $(HECMW_LDFLAGS) $(MPI_LDFLAGS) $(LDFLAGS)

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
.SUFFIXES: .o .o .c .f90 .f

.c.o:
	$(CC) -c $< $(ALL_CFLAGS) $(OPTFLAGS)

.f90.o:
	$(F90) -c $< $(ALL_F90FLAGS) $(F90OPTFLAGS)

.f.o:
	$(F90) -c $< $(ALL_F90FLAGS) $(F90OPTFLAGS)

#-----------------------------------------------------------------------------#
OBJS =

OBJSF = \
	hecmw.o \
	ppohFEM.o

SRCF = \
	hecmw.f90 \
	ppohFEM.f90

HEADERS = \
	hecmw.h

#-----------------------------------------------------------------------------#
all: build-default

build-default: clean $(TARGET) $(F90TARGET) install-header install-module

build-serial: clean $(TARGET) $(F90TARGET) install-header install-module

build-without-f: clean install-header

$(TARGET): $(OBJS)
	$(AR) $(HECMWLIBDIR)/$@ $(OBJS)

$(F90TARGET): $(OBJSF)
	$(AR) $(HECMWLIBDIR)/$@ $(OBJSF)

$(OBJS): $(HEADERS)

$(OBJSF): $(SRCF)

$(SRCF):
	@src=$@; \
	echo module hecmw > $$src; \
	ls $(HECMWINCDIR)/*.$(F90MODULEPOSTFIX) | \
	sed -e 's#$(HECMWINCDIR)/\(.*\)\.$(F90MODULEPOSTFIX)#use \1#i' | \
	sed -e 's#^use hecmw$$##i' >> $$src; \
	echo end module hecmw >> $$src; \
	echo created $$src

$(HEADERS):
	@src=$@; \
	echo "#ifndef HECMW_H_INCLUDED" > $$src; \
	echo "#define HECMW_H_INCLUDED" >> $$src; \
	ls $(HECMWINCDIR)/*.h | \
	sed -e 's!$(HECMWINCDIR)/\(.*\)!#include "\1"!i' | \
	sed -e 's!^#include "hecmw.h"$$!!i' >> $$src; \
	echo "#endif" >> $$src; \
	echo created $$src

install-header: $(HEADERS)
	$(CP) $(HEADERS) $(HECMWINCDIR)/.

install-module:
	$(CP) *.$(F90MODULEPOSTFIX) $(HECMWINCDIR)/.

install:

clean:
	$(RM) *.$(COBJFILEPOSTFIX) *.$(F90OBJFILEPOSTFIX) *.$(F90MODULEPOSTFIX)

distclean: clean
