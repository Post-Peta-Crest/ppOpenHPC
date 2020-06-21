include ../Makefile.in
#
INC  =   ../include
LIB  =   ../lib/ppohDEMlib.a

TARGET = ./sample_main01

OBJS = \
ppohDEM_util.o ppohDEM_boundary.o ppohDEM_qsort.o ppohDEM_phys.o ppohDEM_math.o ppohDEM_makefilename.o \
ppohDEM_embedding.o ppohDEM_fileout.o ppohDEM_particleconfig.o ppohDEM_supp.o ppohDEM_calcforce.o \
ppohDEM_integrate.o ppohDEM_makeindex.o ppohDEM_exit.o ppohDEM_prepare_objects_fromfile.o \
ppohDEM_pre.o ppohDEM_loadfileinfo.o ppohDEM_check_objects.o ppohDEM_setparameters.o 

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

default: $(LIB)

$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS)
	ranlib $(LIB)
	cp *.mod $(INC)/.
	$(MPIF90) $(F90OMPFLAGS) -o $(TARGET) $(OBJS) $(TARGET).f90
	cp ./$(TARGET) ../bin/. 

run:
	$(TARGET)
clean:
	/bin/rm -f *.o $(LIB) *~ *.mod core.* $(INC)/* $(TARGET) ../bin/sample_main01 log-* *.csv *.log
cleandata:
	rm ./data/out* ./pov/out* ./obj/obj* ./distance/objects_distance* 


.f.o:
	$(MPIF90) $(F90OMPFLAGS) -c $*.f
.f90.o:
	$(MPIF90) $(F90OMPFLAGS) -c $*.f90
.SUFFIXES: .f90


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

