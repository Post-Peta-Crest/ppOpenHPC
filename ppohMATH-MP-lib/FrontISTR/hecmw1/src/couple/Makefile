MPIDIR            = 
MPIBINDIR         = 
MPILIBDIR         = 
MPIINCDIR         = 
MPILIBS           = 

PREFIX            = $(HOME)/FrontISTR
BINDIR            = $(PREFIX)/bin
LIBDIR            = $(PREFIX)/lib
INCLUDEDIR        = $(PREFIX)/include

HECMWBINDIR       = ../../bin
HECMWLIBDIR       = ../../lib
HECMWINCDIR       = ../../include
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
OBJS = \
	hecmw_couple_comm.o \
	hecmw_couple_copy_c2f.o \
	hecmw_couple_copy_f2c.o \
	hecmw_couple_control.o \
	hecmw_couple_info.o \
	hecmw_couple_info_if.o \
	hecmw_couple_get_mesh.o \
	hecmw_couple_boundary_info.o \
	hecmw_couple_bounding_box.o \
	hecmw_couple_background_cell.o \
	hecmw_couple_mapped_point.o \
	hecmw_couple_judge.o \
	hecmw_couple_inter_iftable.o \
	hecmw_couple_intra_iftable.o \
	hecmw_couple_weight.o \
	hecmw_couple_s2n_average.o \
	hecmw_couple_s2n_with_area.o \
	hecmw_couple_s2n_dist_node.o \
	hecmw_couple_s2n_dist_surf.o \
	hecmw_couple_n2s_average.o \
	hecmw_couple_n2s_with_area.o \
	hecmw_couple_interpolate_info.o \
	hecmw_couple_init.o \
	hecmw_couple_startup.o \
	hecmw_couple_finalize.o \
	hecmw_couple.o \
	hecmw_couple_init_if.o \
	hecmw_couple_startup_if.o \
	hecmw_couple_if.o \
	hecmw_couple_finalize_if.o

OBJSF = \
    hecmw_couple_define_f.o \
    hecmw_couple_struct_f.o \
	hecmw_couple_copy_c2f_f.o \
	hecmw_couple_copy_f2c_f.o \
	hecmw_couple_info_f.o \
	hecmw_couple_get_mesh_f.o \
	hecmw_couple_init_f.o \
	hecmw_couple_startup_f.o \
	hecmw_couple_cleanup_f.o \
	hecmw_couple_f.o \
	hecmw_couple_finalize_f.o

HEADERS = \
	hecmw_couple_define.h \
	hecmw_couple_struct.h \
	hecmw_couple_comm.h \
	hecmw_couple_control.h \
	hecmw_couple_info.h \
	hecmw_couple_boundary_info.h \
	hecmw_couple_bounding_box.h \
	hecmw_couple_background_cell.h \
	hecmw_couple_mapped_point.h \
	hecmw_couple_judge.h \
	hecmw_couple_inter_iftable.h \
	hecmw_couple_intra_iftable.h \
	hecmw_couple_weight.h \
	hecmw_couple_s2n_average.h \
	hecmw_couple_s2n_with_area.h \
	hecmw_couple_s2n_dist_node.h \
	hecmw_couple_s2n_dist_surf.h \
	hecmw_couple_n2s_average.h \
	hecmw_couple_n2s_with_area.h \
	hecmw_couple_interpolate_info.h \
	hecmw_couple_init.h \
	hecmw_couple_startup.h \
	hecmw_couple.h \
	hecmw_couple_finalize.h \
	hecmw_couple_copy_c2f.h \
	hecmw_couple_copy_f2c.h

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
