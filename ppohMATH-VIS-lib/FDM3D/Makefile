include ../../Makefile.in

#------------------------------------------------------------------------------#
BASEDIR    = ../base

CTARGET    = libppohvisfdm3d.a
FTARGET    = libfppohvisfdm3d.a

.SUFFIXES:
.SUFFIXES: .o .c .f90

.c.o:
	$(CC) $(CFLAGS) -I../../ppohMATH-VIS-install/include -c $< -o $@
.f90.o:
	$(F90) $(F90FLAGS) -I../../ppohMATH-VIS-install/include -c $< -o $@

# BASECOBJS
include ../base/Makefile_cobj

COBJS = \
	ppohVIS_FDM3D_StrGrid.o \
	ppohVIS_FDM3D_StrGrid_F2C.o \
	ppohVIS_FDM3D_SetStrGrid_IF.o \
	ppohVIS_FDM3D_InitialVoxel.o \
	ppohVIS_FDM3D_VoxelRefine.o \
	ppohVIS_FDM3D_VoxelVisual.o \
	ppohVIS_FDM3D_Extern.o \
	ppohVIS_FDM3D_Init_IF.o \
	ppohVIS_FDM3D_Visualize_IF.o \
	ppohVIS_FDM3D_Finalize_IF.o

HEADERS = \
	ppohVIS_FDM3D_StrGrid.h \
	ppohVIS_FDM3D_StrGrid_F2C.h \
	ppohVIS_FDM3D_InitialVoxel.h \
	ppohVIS_FDM3D_VoxelRefine.h \
	ppohVIS_FDM3D_VoxelVisual.h \
	ppohVIS_FDM3D_Extern.h \
	ppohVIS_FDM3D_Util.h

# BASEF90OBJS
include ../base/Makefile_f90obj

F90OBJS = \
	ppohVIS_FDM3D_StrGrid_F.o \
	ppohVIS_FDM3D_Extern_F.o \
	ppohVIS_FDM3D_Util_F.o

F90MODULES = *.mod


all: $(CTARGET) $(FTARGET)

$(FTARGET): $(CTARGET) $(F90OBJS)
	$(AR) ../../ppohMATH-VIS-install/lib/$@ $(BASEF90OBJS) $(F90OBJS)
	$(CP) $(F90MODULES) ../../ppohMATH-VIS-install/include/.

$(CTARGET): $(CXXOBJS) $(COBJS)
	$(AR) ../../ppohMATH-VIS-install/lib/$@ $(BASECOBJS) $(CXXOBJS) $(COBJS)
	$(CP) $(HEADERS) ../../ppohMATH-VIS-install/include/.

clean:
	$(RM) *.o *.mod

distclean:
	$(RM) *.o *.mod ../../ppohMATH-VIS-install/lib/$(CTARGET) ../../ppohMATH-VIS-install/lib/$(FTARGET)

install:

uninstall:

