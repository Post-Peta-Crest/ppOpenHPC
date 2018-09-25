include ../../Makefile.in

#------------------------------------------------------------------------------#
#CTARGET = libppohvisbase.a
#FTARGET = libfppohvisbase.a

.SUFFIXES:
.SUFFIXES: .o .c .f90

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@
.f90.o:
	$(F90) $(F90FLAGS) -c $< -o $@

COBJS = \
	ppohVIS_BASE_Lib.o \
	ppohVIS_BASE_F2CUtil.o \
	ppohVIS_BASE_Error.o \
	ppohVIS_BASE_Parallel.o \
	ppohVIS_BASE_ParallelBoundingBox.o \
	ppohVIS_BASE_ParallelVoxel.o \
	ppohVIS_BASE_Control.o \
	ppohVIS_BASE_Control_F2C.o \
	ppohVIS_BASE_Control_C2F.o \
	ppohVIS_BASE_ControlFile.o \
	ppohVIS_BASE_GetControl_IF.o \
	ppohVIS_BASE_Mesh.o \
	ppohVIS_BASE_Geometry.o \
	ppohVIS_BASE_Edge.o \
	ppohVIS_BASE_TriangularSurface.o \
	ppohVIS_BASE_QuadrilateralSurface.o \
	ppohVIS_BASE_FreeSurface.o \
	ppohVIS_BASE_BoundingBox.o \
	ppohVIS_BASE_InnerComm.o \
	ppohVIS_BASE_Result.o \
	ppohVIS_BASE_Result_F2C.o \
	ppohVIS_BASE_UCD.o \
	ppohVIS_BASE_VoxelUtil.o \
	ppohVIS_BASE_VoxelItem.o \
	ppohVIS_BASE_InitialVoxel.o \
	ppohVIS_BASE_Voxel.o \
	ppohVIS_BASE_VoxelValue.o \
	ppohVIS_BASE_VoxelCost.o \
	ppohVIS_BASE_VoxelUtilGetCoord.o \
	ppohVIS_BASE_VoxelUtilSetValue.o \
	ppohVIS_BASE_VoxelUtilSearchInside.o \
	ppohVIS_BASE_VoxelUtilSetChildProp.o \
	ppohVIS_BASE_VoxelCount.o

HEADERS = \
	ppohVIS_BASE_mpi.h \
	ppohVIS_BASE_Config.h \
	ppohVIS_BASE_Lib.h \
	ppohVIS_BASE_F2CUtil.h \
	ppohVIS_BASE_Error.h \
	ppohVIS_BASE_Parallel.h \
	ppohVIS_BASE_ParallelBoundingBox.h \
	ppohVIS_BASE_ParallelVoxel.h \
	ppohVIS_BASE_Control.h \
	ppohVIS_BASE_Control_F2C.h \
	ppohVIS_BASE_Control_C2F.h \
	ppohVIS_BASE_ControlFile.h \
	ppohVIS_BASE_Mesh.h \
	ppohVIS_BASE_Geometry.h \
	ppohVIS_BASE_Edge.h \
	ppohVIS_BASE_TriangularSurface.h \
	ppohVIS_BASE_QuadrilateralSurface.h \
	ppohVIS_BASE_FreeSurface.h \
	ppohVIS_BASE_BoundingBox.h \
	ppohVIS_BASE_InnerComm.h \
	ppohVIS_BASE_Result.h \
	ppohVIS_BASE_Result_F2C.h \
	ppohVIS_BASE_UCD.h \
	ppohVIS_BASE_VoxelUtil.h \
	ppohVIS_BASE_VoxelItem.h \
	ppohVIS_BASE_InitialVoxel.h \
	ppohVIS_BASE_Voxel.h \
	ppohVIS_BASE_VoxelValue.h \
	ppohVIS_BASE_VoxelCost.h \
	ppohVIS_BASE_VoxelUtilGetCoord.h \
	ppohVIS_BASE_VoxelUtilSetValue.h \
	ppohVIS_BASE_VoxelUtilSearchInside.h \
	ppohVIS_BASE_VoxelUtilSetChildProp.h \
	ppohVIS_BASE_VoxelCount.h \
	ppohVIS_BASE_Util.h

F90OBJS = \
	ppohVIS_BASE_Config_F.o \
	ppohVIS_BASE_Lib_F.o \
	ppohVIS_BASE_Control_F.o \
	ppohVIS_BASE_ControlFile_F.o \
	ppohVIS_BASE_Mesh_F.o \
	ppohVIS_BASE_Result_F.o \
	ppohVIS_BASE_Util_F.o

F90MODULES = *.mod


all: c-compile f-compile

f-compile: $(F90OBJS)
	$(CP) $(F90MODULES) ../../ppohMATH-VIS-install/include/.

c-compile: $(COBJS)
	$(CP) $(HEADERS) ../../ppohMATH-VIS-install/include/.

clean:
	$(RM) *.o *.mod *~

distclean:
	$(RM) *.o *.mod

install:

uninstall:

