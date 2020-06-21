include ../Makefile.in
#
TARGET = ./sample_DEMutil
INC  =   ../include
LIB  =   ../lib/ppohDEM_Util_Lib.a

OBJS = \
ppohDEM_util.o ppohDEM_v_exit.o output_data.o ppohDEM_v_makevoxel.o ppohDEM_v_pre.o ppohDEM_loadfileinfo.o \
ppohDEM_v_inivar.o ppohDEM_v_makecrosslist_MPI.o ppohDEM_v_makegridout_MPI.o ppohDEM_v_stlreader.o ppohDEM_qsort.o \
ppohDEM_v_distancecalculate_MPI1.o ppohDEM_v_distancecalculate_MPI2.o ppohDEM_v_distanceprecise.o


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

default: $(LIB)

$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS)
	ranlib $(LIB)
	$(MPIF90) $(F90OMPFLAGS) -o $(TARGET) $(OBJS) ./src/sample_DEMutil.f90
	cp $(TARGET) ../bin/. 

run:
	mpirun -np 2 $(TARGET)
clean:
	/bin/rm -f *.o $(LIB) *~ *.mod core.* $(INC)/* $(TARGET) ../bin/sample_DEMutil log-* distance_*.dat object_mesh.stl

.f.o:
	$(MPIF90) $(F90OMPFLAGS) -c $*.f
.f90.o:
	$(MPIF90) $(F90OMPFLAGS) -c $*.f90
.SUFFIXES: .f90


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
