MPIDIR                 = 
MPIBINDIR              = 
MPILIBDIR              = 
MPIINCDIR              = 
MPILIBS                = 

PREFIX                 = $(HOME)/FrontISTR
BINDIR                 = $(PREFIX)/bin
LIBDIR                 = $(PREFIX)/lib
INCLUDEDIR             = $(PREFIX)/include

HECMWBINDIR            = ../../bin
HECMWLIBDIR            = ../../lib
HECMWINCDIR            = ../../include
HECMWLIBS              = -lfhecmw2 -lhecmw2 -lstdc++
HECMWBINDIR2           = ../../../hecmw1/bin
HECMWLIBDIR2           = ../../../hecmw1/lib
HECMWINCDIR2           = ../../../hecmw1/include
HECMWLIBS2             = -lfhecmw -lhecmw

REFINERDIR             = $(HOME)/REVOCAP_Refiner
REFINERINCDIR          = $(REFINERDIR)/Refiner
REFINERLIBDIR          = $(REFINERDIR)/lib
REFINERLIBS            = -lRcapRefiner

METISDIR               = $(HOME)/tools/metis-4.0.3/
METISLIBDIR            = $(METISDIR)
METISINCDIR            = $(METISDIR)/Lib
METISLIBS              = -lmetis
METIS_CFLAGS           = -I$(METISINCDIR)
METIS_LDFLAGS          = -L$(METISLIBDIR) $(METISLIBS)

CC                     = mpifccpx
CFLAGS                 = -Kfast,openmp
BASE_CFLAGS            = -I.
MPI_CFLAGS             = 
HECMW_CFLAGS           = -I$(HECMWINCDIR)
METIS_CFLAGS           = -I$(METISINCDIR)
ALL_CFLAGS             = $(METIS_CFLAGS) $(BASE_CFLAGS) $(HECMW_CFLAGS) $(MPI_CFLAGS) $(CFLAGS)
LDFLAGS                = -lm
MPI_LDFLAGS            = 
HECMW_LDFLAGS          = -L$(HECMWLIBDIR) $(HECMWLIBS)
METIS_LDFLAGS          = -L$(METISLIBDIR) $(METISLIBS)
ALL_LDFLAGS            = $(METIS_LDFLAGS) $(HECMW_LDFLAGS) $(HECMW_LDFLAGS2) $(MPI_LDFLAGS) $(LDFLAGS)
OPTFLAGS               =  -DNDEBUG -DHAVE_MPI

CPP                    = mpiFCCpx
CPPFLAGS               = -Kfast,openmp -DMPICH_IGNORE_CXX_SEEK
BASE_CPPFLAGS          = -I.
HECMW_CPPFLAGS         = -I$(HECMWINCDIR)
ALL_CPPFLAGS           = $(BASE_CPPFLAGS) $(HECMW_CPPFLAGS) $(METIS_CFLAGS) $(MPI_CFLAGS) $(CPPFLAGS)
CPPLDFLAGS             = 
HECMW_CPPLDFLAGS       = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_CPPLDFLAGS         = $(HECMW_CPPLDFLAGS) $(METIS_LDFLAGS) $(MPI_LDFLAGS) $(CPPLDFLAGS)
CPPOPTFLAGS            =  -DHAVE_MPI

F90                    = mpifrtpx
F90FLAGS               = -Kfast,openmp
BASE_F90FLAGS          = -I.
MPI_F90FLAGS           = 
HECMW_F90FLAGS         = -I$(HECMWINCDIR)
ALL_F90FLAGS           = $(BASE_F90FLAGS) $(HECMW_F90FLAGS) $(MPI_F90FLAGS) $(F90FLAGS)
F90LDFLAGS             = -SCALAPACK -SSL2BLAMP
MPI_F90LDFLAGS         = 
HECMW_F90LDFLAGS       = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_F90LDFLAGS         = $(HECMW_F90LDLAGS) $(MPI_F90LDFLAGS) $(F90LDFLAGS)
F90OPTFLAGS            = 

AR                     = ar ruv
CP                     = cp -f
RM                     = rm -f
RANLIB                 = ranlib
MKDIR                  = mkdir -p

#-----------------------------------------------------------------------------#
TARGET                 = hecmw_part2

F90MODULEPOSTFIX       = mod
COBJFILEPOSTFIX        = o
CPPOBJFILEPOSTFIX      = o
F90OBJFILEPOSTFIX      = o

#-----------------------------------------------------------------------------#
.SUFFIXES:
.SUFFIXES: .o .o .o .c .cpp .f90

.c.o:
	$(CC) -c $< $(ALL_CFLAGS) $(OPTFLAGS)

.cpp.o:
	$(CPP) -c $< $(ALL_CPPFLAGS) $(CPPOPTFLAGS)

.f90.o:
	$(F90) -c $< $(ALL_F90FLAGS) $(F90OPTFLAGS)

#-----------------------------------------------------------------------------#
OBJS =

OBJSP = \
	func.o \
	Partitioner.o \
	part.o

OBJSF =

HEADERS =

#-----------------------------------------------------------------------------#
all: build-default

build-default: $(TARGET)

$(TARGET): $(OBJSP) $(HECMWLIBDIR)/*.* $(HECMWLIBDIR2)/*.*
	$(CPP) $(ALL_CPPFLAGS) $(CPPOPTFLAGS) \
	-o $@ $(OBJSP) $(ALL_CPPLDFLAGS)
	$(CP) $(TARGET) $(HECMWBINDIR)/.

$(OBJS): $(HEADERS)

$(OBJSP): $(HEADERS)

$(OBJSF):

install:

clean:
	$(RM) *.$(COBJFILEPOSTFIX) *.$(CPPOBJFILEPOSTFIX) *.$(F90OBJFILEPOSTFIX) *.$(F90MODULEPOSTFIX)

distclean: clean
	$(RM) $(TARGET)
