include ../Makefile.in
#
INC  =   ../include
LIB  =   ../lib/ppohDEMlib.a

default: $(LIB)

OBJS = \
ppohDEM_util.o ppohDEM_boundary.o ppohDEM_qsort.o ppohDEM_phys.o ppohDEM_math.o ppohDEM_makefilename.o \
ppohDEM_embedding.o ppohDEM_fileout.o ppohDEM_particleconfig.o ppohDEM_supp.o ppohDEM_calcforce.o \
ppohDEM_integrate.o ppohDEM_makeindex.o ppohDEM_exit.o ppohDEM_prepare_objects_fromfile.o \
ppohDEM_pre.o ppohDEM_loadfileinfo.o ppohDEM_check_objects.o ppohDEM_setparameters.o 

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS)
	ranlib $(LIB)
	cp *.mod $(INC)/.

clean:
	/bin/rm -f *.o $(LIB) *~ *.mod core.* $(INC)/* log-* distance_*.dat object_mesh.stl

.f.o:
	$(MPIF90) $(F90OMPFLAGS) -c $*.f
.f90.o:
	$(MPIF90) $(F90OMPFLAGS) -c $*.f90
.SUFFIXES: .f90

