MPIDIR            = 
MPIBINDIR         = 
MPILIBDIR         = 
MPIINCDIR         = 
MPILIBS           = 

PREFIX            = $(HOME)/FrontISTR
BINDIR            = $(PREFIX)/bin
LIBDIR            = $(PREFIX)/lib
INCLUDEDIR        = $(PREFIX)/include

NEUINCDIR	      = ./include
NEULIBDIR	      = ./lib
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
TARGET            = neu2fstr

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
	neu2fstr.o

OBJSF =

HEADERS =

#-----------------------------------------------------------------------------#
all: build-default

build-default:
	if [ ! -d include ]; then \
		$(MKDIR) include; \
	fi
	if [ ! -d lib ]; then \
		$(MKDIR) lib; \
	fi
	$(RM) ./include/*
	$(RM) ./lib/*
	@cd NFD       && $(MAKE) build-default
	@cd HECD      && $(MAKE) build-default
	@cd converter && $(MAKE) build-default
	$(MAKE) BUILD_CURR

BUILD_CURR: $(TARGET)

$(TARGET): $(OBJSP)
	$(CPP) $(ALL_CPPFLAGS) $(CPPOPTFLAGS) \
	-o $@ $(OBJSP) $(ALL_CPPLDFLAGS) $(NEU_CPPLDFLAGS)

$(OBJS): $(HEADERS)

$(OBJSP): $(HEADERS)

$(OBJSF):

install:
	$(CP) $(TARGET) $(BINDIR)

clean:
	@cd NFD       && $(MAKE) clean
	@cd HECD      && $(MAKE) clean
	@cd converter && $(MAKE) clean
	$(RM) *.$(COBJFILEPOSTFIX) *.$(CPPOBJFILEPOSTFIX) *.$(F90OBJFILEPOSTFIX) *.$(F90MODULEPOSTFIX)

distclean:
	@cd NFD       && $(MAKE) distclean
	@cd HECD      && $(MAKE) distclean
	@cd converter && $(MAKE) distclean
	$(RM) *.$(COBJFILEPOSTFIX) *.$(CPPOBJFILEPOSTFIX) *.$(F90OBJFILEPOSTFIX) *.$(F90MODULEPOSTFIX)
	$(RM) $(TARGET)
	$(RM) ./include/*
	$(RM) ./bin/*
	$(RM) ./lib/*
