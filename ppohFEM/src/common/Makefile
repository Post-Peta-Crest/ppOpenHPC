include ../../../Makefile.in

HECMWBINDIR       = ../../bin
HECMWLIBDIR       = ../../lib
HECMWINCDIR       = ../../include
HECMWLIBS         = -lfppohFEM -lppohFEM

REFINERDIR        = $(HOME)/REVOCAP_Refiner
REFINERINCDIR     = $(REFINERDIR)/Refiner
REFINERLIBDIR     = $(REFINERDIR)/lib
REFINERLIBS       = -lRcapRefiner

#CC                = mpicc
BASE_CFLAGS       = -I.
MPI_CFLAGS        = 
HECMW_CFLAGS      = -I$(HECMWINCDIR)
ALL_CFLAGS        = $(BASE_CFLAGS) $(HECMW_CFLAGS) $(MPI_CFLAGS) $(CFLAGS)
MPI_LDFLAGS       = 
HECMW_LDFLAGS     = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_LDFLAGS       = $(HECMW_LDFLAGS) $(MPI_LDFLAGS) $(LDFLAGS)

#F90               = mpif90
BASE_F90FLAGS     = -I.
MPI_F90FLAGS      = 
HECMW_F90FLAGS    = -I$(HECMWINCDIR)
ALL_F90FLAGS      = $(BASE_F90FLAGS) $(HECMW_F90FLAGS) $(MPI_F90FLAGS) $(F90FLAGS)
MPI_F90LDFLAGS    = 
HECMW_F90LDFLAGS  = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_F90LDFLAGS    = $(HECMW_F90LDFLAGS) $(MPI_F90LDFLAGS) $(F90LDFLAGS)

RANLIB            = ranlib

#-----------------------------------------------------------------------------#
LEX               = flex

TARGET            = libppohFEM.a
F90TARGET         = libfppohFEM.a

F90MODULEPOSTFIX  = mod
COBJFILEPOSTFIX   = o
F90OBJFILEPOSTFIX = o

#-----------------------------------------------------------------------------#
.SUFFIXES:
.SUFFIXES: .o .o .c .f90 .f

.c.o:
	$(CC) -c $< $(ALL_CFLAGS) $(OPTFLAGS)

.f90.o:
	$(F90) -c $< $(ALL_F90FLAGS) $(F90OPTFLAGS)

.f.o:
	$(F90) -c $< $(ALL_F90FLAGS) $(F90OPTFLAGS)

#-----------------------------------------------------------------------------#
LEXSRC = \
	hecmw_ctrllex.c \
	hecmw_gflex.c \
	hecmw_heclex.c \
	hecmw_ablex.c

MSGSRC = \
	hecmw_msgno.h \
	hecmw_msgno_f.f90 \
	hecmw_msg_table.c

OBJS = \
	hecmw_hash.o \
	hecmw_init.o \
	hecmw_finalize.o \
	hecmw_comm.o \
	hecmw_conn_conv.o \
	hecmw_error.o \
	hecmw_util.o \
	hecmw_malloc.o \
	hecmw_log.o \
	hecmw_msg.o \
	hecmw_msg_table.o \
	hecmw_lib_fc.o \
	hecmw_control.o \
	hecmw_ctrllex.o \
	hecmw_bit_array.o \
	hecmw_map_int.o \
	hecmw_set_int.o \
	hecmw_varray_int.o \
	hecmw_etype.o \
	hecmw_debug_write_dist.o \
	hecmw_reorder.o \
	hecmw_gflex.o \
	hecmw_heclex.o \
	hecmw_ablex.o \
	hecmw_io_hec.o \
	hecmw_io_abaqus.o \
	hecmw_io_nastran_dummy.o \
	hecmw_io_geofem.o \
	hecmw_io_dist.o \
	hecmw_io_mesh.o \
	hecmw_io_get_mesh.o \
	hecmw_io_get_mesh_if.o \
	hecmw_io_put_mesh.o \
	hecmw_dist.o \
	hecmw_dist_alloc.o \
	hecmw_dist_print.o \
	hecmw_dist_free.o \
	hecmw_dist_copy_f2c.o \
	hecmw_dist_copy_c2f.o \
	hecmw_dist_refine.o \
	hecmw_ucd_print.o \
	hecmw_geometric.o \
	hecmw_path.o \
	hecmw_put_mesh_if.o \
	hecmw_restart.o \
	hecmw_bin_io.o \
	hecmw_result.o \
	hecmw_result_copy_c2f.o \
	hecmw_result_copy_f2c.o \
	hecmw_visual_if.o \
	hecmw_system.o \
	hecmw_time.o

OBJSF = \
	hecmw_logging.o \
	hecmw_msgno_f.o \
	hecmw_msg_f.o \
	hecmw_control_f.o \
	hecmw_result_f.o \
	hecmw_restart_f.o \
	hecmw_dist_copy_f2c_f.o \
	hecmw_dist_copy_c2f_f.o \
	hecmw_dist_free_f.o \
	hecmw_dist_print_f.o \
	hecmw_visualizer_f.o \
	hecmw_etype_f.o \
	hecmw_io.o

OBJF_UTIL = hecmw_util_f.o
OBJF_UTIL_DUMM = hecmw_util_f_dumm.o

HEADERS = \
	hecmw_hash.h\
	hecmw_struct.h \
	hecmw_util.h \
	hecmw_comm.h \
	hecmw_conn_conv.h \
	hecmw_error.h \
	hecmw_malloc.h \
	hecmw_log.h \
	hecmw_msg.h \
	hecmw_msgno.h \
	hecmw_config.h \
	hecmw_init.h \
	hecmw_finalize.h \
	hecmw_lib_fc.h \
	hecmw_control.h \
	hecmw_ctrllex.h \
	hecmw_bit_array.h \
	hecmw_map_int.h \
	hecmw_set_int.h \
	hecmw_varray_int.h \
	hecmw_common.h \
	hecmw_common_define.h \
	hecmw_etype.h \
	hecmw_debug_write_dist.h \
	hecmw_reorder.h \
	hecmw_dist.h \
	hecmw_dist_alloc.h \
	hecmw_dist_print.h \
	hecmw_dist_copy_f2c.h \
	hecmw_dist_copy_c2f.h \
	hecmw_dist_free.h \
	hecmw_dist_refine.h \
	hecmw_geometric.h \
	hecmw_ablex.h \
	hecmw_gflex.h \
	hecmw_heclex.h \
	hecmw_io.h \
	hecmw_io_dist.h \
	hecmw_io_geofem.h \
	hecmw_io_nastran.h \
	hecmw_io_get_mesh.h \
	hecmw_io_hec.h \
	hecmw_io_abaqus.h \
	hecmw_io_mesh.h \
	hecmw_io_put_mesh.h \
	hecmw_io_struct.h \
	hecmw_path.h \
	hecmw_restart.h \
	hecmw_result.h \
	hecmw_ucd_print.h \
	hecmw_result_copy_c2f.h \
	hecmw_result_copy_f2c.h \
	hecmw_system.h \
	hecmw_time.h

#-----------------------------------------------------------------------------#
all: build-default

build-default: $(TARGET) with-mpi $(F90TARGET) install-module

build-serial: $(TARGET) without-mpi $(F90TARGET) install-module

build-without-f: $(TARGET)

$(TARGET): $(OBJS)
	$(AR) $(HECMWLIBDIR)/$@ $(OBJS)

$(F90TARGET): $(OBJSF)
	$(AR) $(HECMWLIBDIR)/$@ $(OBJSF)

with-mpi: $(OBJF_UTIL)
	$(AR) $(HECMWLIBDIR)/$(F90TARGET) $(OBJF_UTIL)

without-mpi: $(OBJF_UTIL_DUMM)
	$(AR) $(HECMWLIBDIR)/$(F90TARGET) $(OBJF_UTIL_DUMM)

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
