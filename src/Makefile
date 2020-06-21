include ../Makefile.in

INC  =   ../include
LIB  =   ../lib/ppohAMRFDMlib.a

default: $(LIB)
OBJS = \
ppohAMRFDM_util.o \
ppohAMRFDM_init.o \
ppohAMRFDM_pre.o \
ppohAMRFDM_make_RMesh.o \
ppohAMRFDM_MrtN2Idxn.o \
ppohAMRFDM_MrtN_nghbrs.o \
ppohAMRFDM_make_GMesh.o \
ppohAMRFDM_refine_base.o \
ppohAMRFDM_setflag.o \
ppohAMRFDM_addRoct.o \
ppohAMRFDM_addGoct.o \
ppohAMRFDM_connect_oct.o \
ppohAMRFDM_set_buffer_ave.o \
ppohAMRFDM_findCPU.o \
ppohAMRFDM_abort.o \
ppohAMRFDM_barrier.o \
ppohAMRFDM_passing_fields.o \
ppohAMRFDM_allocate_Buf.o \
ppohAMRFDM_refine_init.o \
ppohAMRFDM_addRoct_init.o \
ppohAMRFDM_addGoct_init.o \
ppohAMRFDM_copy.o \
ppohAMRFDM_refine.o \
ppohAMRFDM_load_mesh.o \
ppohAMRFDM_load_index.o \
ppohAMRFDM_store_mesh.o \
ppohAMRFDM_resize_Mesh.o \
ppohAMRFDM_interpolate.o \
ppohAMRFDM_passing_iFLG.o \
ppohAMRFDM_sortoct.o \
ppohAMRFDM_get_ID_order.o \
ppohAMRFDM_load_parameters.o \
ppohAMRFDM_average.o \
ppohAMRFDM_passings.o \
ppohAMRFDM_DDD.o \
ppohAMRFDM_finalize.o \
ppohAMRFDM_input_data.o

$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS)
	ranlib $(LIB)
	cp *.mod $(INC)/.

clean:
	/bin/rm -f *.o $(LIB) *~ *.mod *__genmod* core.* $(INC)/*

.f90.o:
	$(MPIF90) $(pFFLAGS) -c $*.f90

.SUFFIXES: .f90
