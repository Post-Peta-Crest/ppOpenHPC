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

METISDIR               = $(HOME)/tools/metis-4.0.3/
METISLIBDIR            = $(METISDIR)
METISINCDIR            = $(METISDIR)/Lib
METISLIBS              = -lmetis
METIS_CFLAGS           = -DHECMW_WITH_METIS -I$(METISINCDIR)
METIS_LDFLAGS          = -L$(METISLIBDIR) $(METISLIBS)

REFINERDIR             = $(HOME)/REVOCAP_Refiner
REFINERINCDIR          = $(REFINERDIR)/Refiner
REFINERLIBDIR          = $(REFINERDIR)/lib
REFINERLIBS            = -lRcapRefiner

CC                     = mpifccpx
CFLAGS                 = -Kfast,openmp
BASE_CFLAGS            = -I.
MPI_CFLAGS             = 
HECMW_CFLAGS           = -I$(HECMWINCDIR)
METIS_CFLAGS           = -DHECMW_WITH_METIS -I$(METISINCDIR)
PARTITIONER_CFLAGS     = 
ALL_CFLAGS             = $(PARTITIONER_CFLAGS) $(METIS_CFLAGS) $(BASE_CFLAGS) $(HECMW_CFLAGS) $(MPI_CFLAGS) $(CFLAGS)
LDFLAGS                = -lm
MPI_LDFLAGS            = 
HECMW_LDFLAGS          = -L$(HECMWLIBDIR) $(HECMWLIBS)
METIS_LDFLAGS          = -L$(METISLIBDIR) $(METISLIBS)
PARTITIONER_LDFLAGS    = -lm
ALL_LDFLAGS            = $(PARTITIONER_LDFLAGS) $(METIS_LDFLAGS) $(HECMW_LDFLAGS) $(MPI_LDFLAGS) $(LDFLAGS)
OPTFLAGS               =  -DNDEBUG
PARTITIONER_OPTFLAGS   = -DHECMW_PART_WITH_METIS
ALL_OPTFLAGS           = $(OPTFLAGS) $(PARTITIONER_OPTFLAGS)
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
TARGET                 = hecmw_part1

COBJFILEPOSTFIX        = o

#-----------------------------------------------------------------------------#
.SUFFIXES:
.SUFFIXES: .o .c

.c.o:
	$(CC) -c $< $(ALL_CFLAGS) $(ALL_OPTFLAGS)

#-----------------------------------------------------------------------------#
OBJS = \
	hecmw_partlex.o \
	hecmw_part_log.o \
	hecmw_mesh_hash_sort.o \
	hecmw_mesh_edge_info.o \
	hecmw_part_get_control.o \
	hecmw_init_for_partition.o \
	hecmw_graph.o \
	hecmw_partition.o \
	hecmw_partitioner.o

HEADERS = \
	hecmw_part_define.h \
	hecmw_partlex.h \
	hecmw_part_log.h \
	hecmw_mesh_hash_sort.h \
	hecmw_mesh_edge_info.h \
	hecmw_part_get_control.h \
	hecmw_init_for_partition.h \
	hecmw_graph.h \
	hecmw_partition.h

LEXSRC = \
	hecmw_partlex.c

#-----------------------------------------------------------------------------#
all: build-default

build-default: $(TARGET)

$(TARGET): $(OBJS) $(HECMWLIBDIR)/*.*
	$(CLINKER) $(ALL_CFLAGS) $(ALL_OPTFLAGS) \
	-o $@ $(OBJS) $(ALL_LDFLAGS)
	$(CP) $(TARGET) $(HECMWBINDIR)/.

$(OBJS): $(HEADERS)

install:

clean:
	$(RM) *.$(COBJFILEPOSTFIX)

distclean: clean
	$(RM) $(TARGET)
