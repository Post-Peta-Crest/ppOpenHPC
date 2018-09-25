MPIDIR            = 
MPIBINDIR         = 
MPILIBDIR         = 
MPIINCDIR         = 
MPILIBS           = 

PREFIX            = $(HOME)/FrontISTR
BINDIR            = $(PREFIX)/bin
LIBDIR            = $(PREFIX)/lib
INCLUDEDIR        = $(PREFIX)/include

NEUINCDIR	      = ../include
NEULIBDIR	      = ../lib
NEULIBS           = -lnfd -lhecd -lconv
NEU_CPPFLAGS      = -I$(NEUINCDIR)
NEU_CPPLDFLAGS    = -L$(NEULIBDIR) $(NEULIBS)

CC                = mpifccpx
CFLAGS            = -Kfast,openmp
BASE_CFLAGS       = -I.
MPI_CFLAGS        = 
ALL_CFLAGS        = $(BASE_CFLAGS) $(MPI_CFLAGS) $(CFLAGS)
LDFLAGS           = -lm
MPI_LDFLAGS       = 
ALL_LDFLAGS       = $(MPI_LDFLAGS) $(LDFLAGS)
OPTFLAGS          =  

CPP               = mpiFCCpx
CPPFLAGS          = -Kfast,openmp -DMPICH_IGNORE_CXX_SEEK
BASE_CPPFLAGS     = -I.
MPI_CPPFLAGS      = 
ALL_CPPFLAGS      = $(BASE_CPPFLAGS) $(MPI_CPPFLAGS) $(CPPFLAGS)
CPPLDFLAGS        = 
MPI_CPPLDFLAGS    = 
ALL_CPPLDFLAGS    = $(MPI_CPPLDFLAGS) $(CPPLDFLAGS)
CPPOPTFLAGS       = 

F90               = mpifrtpx
F90FLAGS          = -Kfast,openmp
BASE_F90FLAGS     = -I.
MPI_F90FLAGS      = 
ALL_F90FLAGS      = $(BASE_F90FLAGS) $(MPI_F90FLAGS) $(F90FLAGS)
F90LDFLAGS        = -SCALAPACK -SSL2BLAMP
MPI_F90LDFLAGS    = 
ALL_F90LDFLAGS    = $(MPI_F90LDFLAGS) $(F90LDFLAGS)
F90OPTFLAGS       = 

AR                = ar ruv
CP                = cp -f
RM                = rm -f
RANLIB            = ranlib
MKDIR             = mkdir -p

#-----------------------------------------------------------------------------#
TARGET            = libhecd.a

F90MODULEPOSTFIX  = mod
COBJFILEPOSTFIX   = o
CPPOBJFILEPOSTFIX = o
F90OBJFILEPOSTFIX = o

#-----------------------------------------------------------------------------#
.SUFFIXES:
.SUFFIXES: .o .o .o .c .cpp .f90

.c.o:
	$(CC) -c $< $(ALL_CFLAGS) $(OPTFLAGS)

.cpp.o:
	$(CPP) -c $< $(ALL_CPPFLAGS) $(CPPOPTFLAGS) $(NEU_CPPFLAGS)

.f90.o:
	$(F90) -c $< $(ALL_F90FLAGS) $(F90OPTFLAGS)

#-----------------------------------------------------------------------------#
OBJS =

OBJSP = \
	hecd_util.o \
	CHECData.o \
	HECDB_util.o \
	CHECDB_Amplitude.o \
	CHECDB_EGroup.o \
	CHECDB_Element.o \
	CHECDB_Header.o \
	CHECDB_Material.o \
	CHECDB_NGroup.o \
	CHECDB_Node.o \
	CHECDB_Section.o \
	CHECDB_SGroup.o \
	CHECDB_Visual.o \
	CHECDB_Zero.o \
	CFSTRData.o \
	FSTRDB_util.o \
	CFSTRDB_Boundary.o \
	CFSTRDB_CFlux.o \
	CFSTRDB_CLoad.o \
	CFSTRDB_DFlux.o \
	CFSTRDB_DLoad.o \
	CFSTRDB_Echo.o \
	CFSTRDB_Eigen.o \
	CFSTRDB_Film.o \
	CFSTRDB_Fixtemp.o \
	CFSTRDB_Heat.o \
	CFSTRDB_Radiate.o \
	CFSTRDB_Reftemp.o \
	CFSTRDB_SFilm.o \
	CFSTRDB_SFlux.o \
	CFSTRDB_Solution.o \
	CFSTRDB_Solver.o \
	CFSTRDB_SRadiate.o \
	CFSTRDB_Static.o \
	CFSTRDB_Step.o \
	CFSTRDB_Temperature.o \
	CFSTRDB_Write.o

OBJSF =

HEADERS = \
	hecd_util.h \
	CFSTRData.h \
	CFSTRDB.h \
	CHECData.h \
	CHECDataBlock.h \
	CHECDB.h \
	CHECDB_Visual.h

#-----------------------------------------------------------------------------#
all: build-default

build-default: $(TARGET) install-header

$(TARGET): $(OBJS) $(OBJSP)
	$(AR) $(NEULIBDIR)/$@ $(OBJS)
	$(AR) $(NEULIBDIR)/$@ $(OBJSP)

$(F90TARGET): $(OBJSF)
	$(AR) $(NEULIBDIR)/$@ $(OBJSF)

$(OBJS): $(HEADERS)

$(OBJSP): $(HEADERS)

$(OBJSF):

install-header: $(HEADERS)
	$(CP) $(HEADERS) $(NEUINCDIR)/.

install-module:
	$(CP) *.$(F90MODULEPOSTFIX) $(NEUINCDIR)/.

install:

clean:
	$(RM) *.$(COBJFILEPOSTFIX) *.$(CPPOBJFILEPOSTFIX) *.$(F90OBJFILEPOSTFIX) *.$(F90MODULEPOSTFIX)

distclean: clean
