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
.SUFFIXES: .o .c

.c.o:
	$(CC) -c $< $(ALL_CFLAGS) $(OPTFLAGS)

#-----------------------------------------------------------------------------#
PSF_OBJS = \
	hecmw_vis_surface_main.o \
	hecmw_vis_comm_util.o \
	hecmw_vis_mem_util.o \
	hecmw_vis_connectivity_build.o \
	hecmw_vis_surface_compute.o \
	hecmw_vis_case_table.o \
	hecmw_vis_calc_attr.o \
	hecmw_vis_intersection_find.o \
	hecmw_vis_patch_const.o \
	hecmw_vis_tetra_intersect.o \
	hecmw_vis_combine.o \
	hecmw_vis_rendering.o \
	hecmw_vis_define_parameters.o \
	hecmw_vis_font_texture.o \
	hecmw_vis_color_mapping.o \
	hecmw_vis_color_composite_sf.o \
	hecmw_vis_generate_histogram_sf.o \
	hecmw_vis_subimage_composite_sf.o \
	hecmw_vis_endian.o \
	hecmw_fstr_output_femap.o
 
PVR_OBJS = \
	hecmw_vis_pvr_main.o \
	hecmw_vis_comm_util.o \
	hecmw_vis_mem_util.o \
	hecmw_vis_new_refine.o \
	hecmw_vis_ray_trace.o \
	hecmw_vis_define_parameters.o \
	hecmw_vis_font_texture.o \
	hecmw_vis_color_mapping.o \
	hecmw_vis_ucd_trans.o \
	hecmw_vis_voxel_gen.o \
	hecmw_vis_read_voxel.o \
	hecmw_vis_mesh_extent.o \
	hecmw_vis_color_composite_vr.o \
	hecmw_vis_generate_histogram_vr.o \
	hecmw_vis_subimage_composite_vr.o

OBJS = \
	hecmw_vis_read_control.o \
	hecmw_visualizer.o \
	$(PSF_OBJS) \
	$(PVR_OBJS) \

HEADERS = \
	hecmw_fstr_output_femap.h \
	hecmw_vis_SF_geom.h \
	hecmw_vis_bmp.h \
	hecmw_vis_calc_attr.h \
	hecmw_vis_case_table.h \
	hecmw_vis_color_composite_sf.h \
	hecmw_vis_color_composite_vr.h \
	hecmw_vis_color_mapping.h \
	hecmw_vis_combine.h \
	hecmw_vis_comm_util.h \
	hecmw_vis_connectivity_build.h \
	hecmw_vis_define_parameters.h \
	hecmw_vis_define_parameters_vr.h \
	hecmw_vis_endian.h \
	hecmw_vis_font_texture.h \
	hecmw_vis_generate_histogram_sf.h \
	hecmw_vis_generate_histogram_vr.h \
	hecmw_vis_intersection_find.h \
	hecmw_vis_mem_util.h \
	hecmw_vis_mesh_extent.h \
	hecmw_vis_new_refine.h \
	hecmw_vis_patch_const.h \
	hecmw_vis_psf_rendering.h \
	hecmw_vis_pvr_main.h \
	hecmw_vis_ray_trace.h \
	hecmw_vis_read_control.h \
	hecmw_vis_read_voxel.h \
	hecmw_vis_rendering.h \
	hecmw_vis_resampling.h \
	hecmw_vis_subimage_composite_sf.h \
	hecmw_vis_subimage_composite_vr.h \
	hecmw_vis_surface_compute.h \
	hecmw_vis_surface_main.h \
	hecmw_vis_tetra_intersect.h \
	hecmw_vis_ucd_trans.h \
	hecmw_vis_voxel_gen.h \
	hecmw_visualizer.h

INSTALLHEADERS = \
        hecmw_vis_mem_util.h \
        hecmw_vis_comm_util.h \
        hecmw_vis_read_control.h \
        hecmw_vis_SF_geom.h \
        hecmw_vis_ray_trace.h \
        hecmw_vis_psf_rendering.h \
        hecmw_visualizer.h

#-----------------------------------------------------------------------------#
all: build-default

build-default: $(TARGET)

build-serial: $(TARGET)

build-without-f: $(TARGET)

$(TARGET): $(OBJS)
	$(AR) $(HECMWLIBDIR)/$@ $(OBJS)

$(PSF_OBJS): $(HEADERS)

$(PVR_OBJS): $(HEADERS)

install-header: $(HEADERS)
	$(CP) $(INSTALLHEADERS) $(HECMWINCDIR)/.

install-module:
	$(CP) *.$(F90MODULEPOSTFIX) $(HECMWINCDIR)/.

install:

clean:
	$(RM) *.$(COBJFILEPOSTFIX) *.$(F90OBJFILEPOSTFIX) *.$(F90MODULEPOSTFIX)

distclean: clean
