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
HECMWLIBS              = -lfhecmw -lhecmw

REFINERDIR             = $(HOME)/REVOCAP_Refiner
REFINERINCDIR          = $(REFINERDIR)/Refiner
REFINERLIBDIR          = $(REFINERDIR)/lib
REFINERLIBS            = -lRcapRefiner

CC                     = mpifccpx
CFLAGS                 = -Kfast,openmp
BASE_CFLAGS            = -I.
MPI_CFLAGS             = 
HECMW_CFLAGS           = -I$(HECMWINCDIR)
VISUALIZER_CFLAGS      = 
ALL_CFLAGS             = $(VISUALIZER_CFLAGS) $(BASE_CFLAGS) $(HECMW_CFLAGS) $(MPI_CFLAGS) $(CFLAGS)
LDFLAGS                = -lm
MPI_LDFLAGS            = 
HECMW_LDFLAGS          = -L$(HECMWLIBDIR) $(HECMWLIBS)
VISUALIZER_LDFLAGS     = -lm
ALL_LDFLAGS            = $(VISUALIZER_LDFLAGS) $(HECMW_LDFLAGS) $(MPI_LDFLAGS) $(LDFLAGS)
OPTFLAGS               =  -DNDEBUG
CLINKER                = mpifccpx

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
TARGET                 = hecmw_vis1

COBJFILEPOSTFIX        = o

#-----------------------------------------------------------------------------#
.SUFFIXES:
.SUFFIXES: .o .c

.c.o:
	$(CC) -c $< $(ALL_CFLAGS) $(OPTFLAGS)

#-----------------------------------------------------------------------------#
OBJS = \
	visualizer.o

HEADERS =

#-----------------------------------------------------------------------------#
all: build-default

build-default: $(TARGET)

$(TARGET): $(OBJS) $(HECMWLIBDIR)/*.*
	$(CLINKER) $(ALL_CFLAGS) $(OPTFLAGS) \
	-o $@ $(OBJS) $(ALL_LDFLAGS)
	$(CP) $(TARGET) $(HECMWBINDIR)/.

$(OBJS): $(HEADERS)

install:

clean:
	$(RM) *.$(COBJFILEPOSTFIX)

distclean: clean
	$(RM) $(TARGET)
