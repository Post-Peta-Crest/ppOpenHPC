include ../../../Makefile.in

HECMWBINDIR            = ../../bin
HECMWLIBDIR            = ../../lib
HECMWINCDIR            = ../../include
HECMWLIBS              = -lfppohFEM -lppohFEM

METISLIBS              = -lmetis
METIS_CFLAGS           = -DHECMW_WITH_METIS -I$(METISINCDIR)
METIS_LDFLAGS          = -L$(METISLIBDIR) $(METISLIBS)

REFINERDIR             = $(HOME)/REVOCAP_Refiner
REFINERINCDIR          = $(REFINERDIR)/Refiner
REFINERLIBDIR          = $(REFINERDIR)/lib
REFINERLIBS            = -lRcapRefiner

BASE_CFLAGS            = -I.
MPI_CFLAGS             = 
HECMW_CFLAGS           = -I$(HECMWINCDIR)
PARTITIONER_CFLAGS     = 
ALL_CFLAGS             = $(PARTITIONER_CFLAGS) $(METIS_CFLAGS) $(BASE_CFLAGS) $(HECMW_CFLAGS) $(MPI_CFLAGS) $(CFLAGS)
MPI_LDFLAGS            = 
HECMW_LDFLAGS          = -L$(HECMWLIBDIR) $(HECMWLIBS)
PARTITIONER_LDFLAGS    = -lm
ALL_LDFLAGS            = $(PARTITIONER_LDFLAGS) $(METIS_LDFLAGS) $(HECMW_LDFLAGS) $(MPI_LDFLAGS) $(LDFLAGS)
PARTITIONER_OPTFLAGS   = -DHECMW_PART_WITH_METIS
ALL_OPTFLAGS           = $(OPTFLAGS) $(PARTITIONER_OPTFLAGS)
CLINKER                = $(CC)

BASE_F90FLAGS          = -I.
MPI_F90FLAGS           = 
HECMW_F90FLAGS         = -I$(HECMWINCDIR)
ALL_F90FLAGS           = $(BASE_F90FLAGS) $(HECMW_F90FLAGS) $(MPI_F90FLAGS) $(F90FLAGS)
MPI_F90LDFLAGS         = 
HECMW_F90LDFLAGS       = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_F90LDFLAGS         = $(HECMW_F90LDLAGS) $(MPI_F90LDFLAGS) $(F90LDFLAGS)

RANLIB                 = ranlib

#-----------------------------------------------------------------------------#
TARGET                 = ppohFEM_part

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
	$(RM) *.$(COBJFILEPOSTFIX) $(TARGET)

distclean: clean
	$(RM) $(TARGET)
