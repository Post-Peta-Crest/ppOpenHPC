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

CC                     = mpifccpx
CFLAGS                 = -Kfast,openmp
BASE_CFLAGS            = -I.
MPI_CFLAGS             = 
HECMW_CFLAGS           = -I$(HECMWINCDIR)
ALL_CFLAGS             = $(BASE_CFLAGS) $(HECMW_CFLAGS) $(MPI_CFLAGS) $(CFLAGS)
LDFLAGS                = -lm
MPI_LDFLAGS            = 
HECMW_LDFLAGS          = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_LDFLAGS            = $(HECMW_LDFLAGS) $(MPI_LDFLAGS) $(LDFLAGS)
OPTFLAGS               =  -DNDEBUG -DHAVE_MPI

CPP                    = mpiFCCpx
CPPFLAGS               = -Kfast,openmp -DMPICH_IGNORE_CXX_SEEK
BASE_CPPFLAGS          = -I.
HECMW_CPPFLAGS         = -I$(HECMWINCDIR)
ALL_CPPFLAGS           = $(BASE_CPPFLAGS) $(HECMW_CPPFLAGS) $(MPI_CFLAGS) $(CPPFLAGS)
CPPLDFLAGS             = 
HECMW_CPPLDFLAGS       = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_CPPLDFLAGS         = $(HECMW_CPPLDFLAGS) $(MPI_LDFLAGS) $(CPPLDFLAGS)
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
TARGET                 = conv2mw3

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
	AssyModel.o \
	Beam.o \
	Beam2.o \
	ConPair.o \
	ConvMain.o \
	Egroup.o \
	Element.o \
	FileBase.o \
	FileReader.o \
	FileWriter.o \
	Group.o \
	GroupPair.o \
	Hexa.o \
	Hexa2.o \
	IFaceQuad.o \
	IFaceQuad2.o \
	Lgroup.o \
	Mesh.o \
	Message.o \
	MPCPair.o \
	Ngroup.o \
	Node.o \
	Prism.o \
	Prism2.o \
	Quad.o \
	Quad2.o \
	QuadShell.o \
	QuadShell2.o \
	Sgroup.o \
	Tetra.o \
	Tetra2.o \
	Triangle.o \
	Triangle2.o \
	TriShell.o \
	TriShell2.o \
	main.o

OBJSF =

HEADERS = \
	AssyModel.h \
	Beam.h \
	Beam2.h \
	ConPair.h \
	ConvHeader.h \
	ConvMain.h \
	Egroup.h \
	Element.h \
	FileBase.h \
	FileReader.h \
	FileWriter.h \
	FrontISTR_Type.h \
	Group.h \
	GroupPair.h \
	Hexa.h \
	Hexa2.h \
	IFaceQuad.h \
	IFaceQuad2.h \
	Lgroup.h \
	Mesh.h \
	Message.h \
	MPCPair.h \
	Ngroup.h \
	Node.h \
	Prism.h \
	Prism2.h \
	Quad.h \
	Quad2.h \
	QuadShell.h \
	QuadShell2.h \
	Sgroup.h \
	Tetra.h \
	Tetra2.h \
	Triangle.h \
	Triangle2.h \
	TriShell.h \
	TriShell2.h

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
