MPIDIR            = 
MPIBINDIR         = 
MPILIBDIR         = 
MPIINCDIR         = 
MPILIBS           = 

PREFIX            = $(HOME)/FrontISTR
BINDIR            = $(PREFIX)/bin
LIBDIR            = $(PREFIX)/lib
INCLUDEDIR        = $(PREFIX)/include

HECMWBINDIR       = ../../../bin
HECMWLIBDIR       = ../../../lib
HECMWINCDIR       = ../../../include
HECMWLIBS         = -lfhecmw -lhecmw

REFINERDIR        = $(HOME)/REVOCAP_Refiner
REFINERINCDIR     = $(REFINERDIR)/Refiner
REFINERLIBDIR     = $(REFINERDIR)/lib
REFINERLIBS       = -lRcapRefiner

CC                = mpifccpx
CFLAGS            = -Kfast,openmp
BASE_CFLAGS       = -I.
MPI_CFLAGS        = 
HECMW_CFLAGS      = -I$(HECMWINCDIR)
ALL_CFLAGS        = $(BASE_CFLAGS) $(HECMW_CFLAGS) $(MPI_CFLAGS) $(CFLAGS)
LDFLAGS           = -lm
MPI_LDFLAGS       = 
HECMW_LDFLAGS     = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_LDFLAGS       = $(HECMW_LDFLAGS) $(MPI_LDFLAGS) $(LDFLAGS)
OPTFLAGS          =  -DNDEBUG

F90               = mpifrtpx
F90FLAGS          = -Kfast,openmp
BASE_F90FLAGS     = -I.
MPI_F90FLAGS      = 
HECMW_F90FLAGS    = -I$(HECMWINCDIR)
ALL_F90FLAGS      = $(BASE_F90FLAGS) $(HECMW_F90FLAGS) $(MPI_F90FLAGS) $(F90FLAGS)
F90LDFLAGS        = -SCALAPACK -SSL2BLAMP
MPI_F90LDFLAGS    = 
HECMW_F90LDFLAGS  = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_F90LDFLAGS    = $(HECMW_F90LDFLAGS) $(MPI_F90LDFLAGS) $(F90LDFLAGS)
F90OPTFLAGS       = 

AR                = ar ruv
CP                = cp -f
RM                = rm -f
RANLIB            = ranlib
MKDIR             = mkdir -p

#-----------------------------------------------------------------------------#
TARGET            = libhecmw.a
F90TARGET         = libfhecmw.a

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
	hecmw_allocate.o \
	hecmw_matrix_contact.o \
	hecmw_matrix_misc.o \
	hecmw_matrix_ordering_CM.o \
	hecmw_matrix_ordering_MC.o \
	hecmw_matrix_reorder.o \
	hecmw_mat_con.o \
	hecmw_mat_ass.o \
	hecmw_matrix_dump.o

HEADERS =

#-----------------------------------------------------------------------------#
all: build-default

build-default: $(TARGET) $(F90TARGET) install-module

build-serial: $(TARGET) $(F90TARGET) install-module

build-without-f: $(TARGET)

$(TARGET): $(OBJS)
	$(AR) $(HECMWLIBDIR)/$@ $(OBJS)

$(F90TARGET): $(OBJSF)
	$(AR) $(HECMWLIBDIR)/$@ $(OBJSF)

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
