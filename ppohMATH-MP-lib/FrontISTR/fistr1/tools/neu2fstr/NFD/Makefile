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
TARGET            = libnfd.a

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
	CNFData.o \
	CNFDB_100.o \
	CNFDB_402.o \
	CNFDB_403.o \
	CNFDB_404.o \
	CNFDB_405.o \
	CNFDB_408.o \
	CNFDB_506.o \
	CNFDB_507.o \
	CNFDB_601.o \
	CNFMessage.o

OBJSF =

HEADERS = \
	CNFData.h \
	CNFDataBlock.h \
	CNFDB_100.h \
	CNFDB_402.h \
	CNFDB_403.h \
	CNFDB_404.h \
	CNFDB_405.h \
	CNFDB_408.h \
	CNFDB_506.h \
	CNFDB_507.h \
	CNFDB_601.h \
	CNFMessage.h

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
	$(CP) *.$(F90MODULEPOSTFIX) $(MEUINCDIR)/.

install:

clean:
	$(RM) *.$(COBJFILEPOSTFIX) *.$(CPPOBJFILEPOSTFIX) *.$(F90OBJFILEPOSTFIX) *.$(F90MODULEPOSTFIX)

distclean: clean
