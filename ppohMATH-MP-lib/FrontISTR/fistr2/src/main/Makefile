MPIDIR                  = 
MPIBINDIR               = 
MPILIBDIR               = 
MPIINCDIR               = 
MPILIBS                 = 

HECMWDIR                = ../../../hecmw2
HECMWBINDIR             = $(HECMWDIR)/bin
HECMWLIBDIR             = $(HECMWDIR)/lib
HECMWINCDIR             = $(HECMWDIR)/include
HECMWLIBS               = -lfhecmw2 -lhecmw2 -lstdc++
HECMWDIR2               = ../../../hecmw1
HECMWBINDIR2            = $(HECMWDIR2)/bin
HECMWLIBDIR2            = $(HECMWDIR2)/lib
HECMWINCDIR2            = $(HECMWDIR2)/include
HECMWLIBS2              = -lfhecmw -lhecmw

FSTRBINDIR              = ../../bin
FSTRLIBDIR              = ../../lib
FSTRINCDIR              = ../../include
FSTRLIBS                = -lffistr -lfistr

REVOCAPDIR              = $(HOME)/REVOCAP_Coupler
REVOCAPINCDIR           = $(REVOCAPDIR)/librcap
REVOCAPLIBDIR           = $(REVOCAPDIR)/librcap
REVOCAPLIBS             = -lrcapf

REFINERDIR              = $(HOME)/REVOCAP_Refiner
REFINERINCDIR           = $(REFINERDIR)/Refiner
REFINERLIBDIR           = $(REFINERDIR)/lib
REFINERLIBS             = -lRcapRefiner

CC                      = mpifccpx
CFLAGS                  = -Kfast,openmp
BASE_CFLAGS             = -I.
MPI_CFLAGS              = 
HECMW_CFLAGS            = -I$(HECMWINCDIR)
HECMW_CFLAGS2           = -I$(HECMWINCDIR2)
FSTR_CFLAGS             = -I$(FSTRINCDIR)
ALL_CFLAGS              = $(BASE_CFLAGS) $(FSTR_CFLAGS) $(HECMW_CFLAGS) $(HECMW_CFLAGS2) $(MPI_CFLAGS) $(CFLAGS)
LDFLAGS                 = -lm
MPI_LDFLAGS             = 
HECMW_LDFLAGS           = -L$(HECMWLIBDIR) $(HECMWLIBS)
HECMW_LDFLAGS2          = -L$(HECMWLIBDIR2) $(HECMWLIBS2)
FSTR_LDFLAGS            = -L$(FSTRLIBDIR) $(FSTRLIBS)
ALL_LDFLAGS             = $(FSTR_LDFLAGS) $(HECMW_LDFLAGS) $(HECMW_LDFLAGS2) $(MPI_LDFLAGS) $(LDFLAGS)
OPTFLAGS                =  

REVOCAP_F90FLAGS        = 
REVOCAP_F90LDFLAGS      = 

F90                     = mpifrtpx
F90FLAGS                = -Kfast,openmp
BASE_F90FLAGS           = -I.
MPI_F90FLAGS            = 
HECMW_F90FLAGS          = -I$(HECMWINCDIR)
HECMW_F90FLAGS2         = -I$(HECMWINCDIR2)
FSTR_F90FLAGS           = -I$(FSTRINCDIR)
ALL_F90FLAGS            = $(BASE_F90FLAGS) $(FSTR_F90FLAGS) $(HECMW_F90FLAGS) $(HECMW_F90FLAGS2) $(MPI_F90FLAGS) $(F90FLAGS)
F90LDFLAGS              = -SCALAPACK -SSL2BLAMP
MPI_F90LDFLAGS          = 
HECMW_F90LDFLAGS        = -L$(HECMWLIBDIR) $(HECMWLIBS)
HECMW_F90LDFLAGS2       = -L$(HECMWLIBDIR2) $(HECMWLIBS2)
FSTR_F90LDFLAGS         = -L$(FSTRLIBDIR) $(FSTRLIBS)
ALL_F90LDFLAGS          = $(FSTR_F90LDFLAGS) $(HECMW_F90LDFLAGS) $(HECMW_F90LDFLAGS2) $(MPI_F90LDFLAGS) $(F90LDFLAGS)
F90OPTFLAGS             = 
F90LINKER               = mpifrtpx

AR                      = ar ruv
CP                      = cp -f
RM                      = rm -f
RANLIB                  = ranlib
MKDIR                   = mkdir -p

#------------------------------------------------------------------------------#
TARGET                  = fistr2

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
	istr.o

HEADERS =

#------------------------------------------------------------------------------#
all: build-default

build-default: $(OBJSF) $(HECMWLIBDIR) $(HECMWLIBDIR2) $(FSTRLIBDIR)
	$(F90LINKER) $(ALL_F90FLAGS) $(F90OPTFLAGS) \
	-o $(TARGET) \
	$(OBJSF) $(ALL_F90LDFLAGS)
	$(CP) $(TARGET) $(FSTRBINDIR)/.

build-with-rcap: $(OBJSF) $(HECMWLIBDIR) $(HECMWLIBDIR2) $(FSTRLIBDIR)
	$(F90LINKER)  $(REVOCAP_F90FLAGS) $(ALL_F90FLAGS) $(F90OPTFLAGS) \
	-o $(TARGET) \
	$(OBJSF) $(REVOCAP_F90LDFLAGS) $(ALL_F90LDFLAGS)
	$(CP) $(TARGET) $(FSTRBINDIR)/.

$(OBJS): $(HEADERS)

$(OBJSF):

install:

clean:
	$(RM) *.$(COBJFILEPOSTFIX) *.$(F90OBJFILEPOSTFIX) *.$(F90MODULEPOSTFIX)

distclean: clean
	$(RM) $(TARGET)
