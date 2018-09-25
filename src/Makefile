include ../Makefile.in
#
INC  =   ../include
LIB  =   ../lib/ppohFVMlib.a
default: $(LIB)
OBJS = \
ppohFVM_util.o\
ppohFVM_SR_r1.o ppohFVM_SR_r2.o ppohFVM_SR_rv2.o ppohFVM_SR_r5.o\
ppohFVM_comm.o ppohFVM_input_grid.o ppohFVM_input_grid_b.o ppohFVM_pre.o\
ppohFVM_edge_cre.o ppohFVM_error_exit.o ppohFVM_active.o \
ppohFVM_edge_metrics.o ppohFVM_edge_metrics_init.o ppohFVM_file_name.o \
ppohFVM_dist_file.o \
ppohFVM_edge_metrics_tetra_341.o ppohFVM_edge_metrics_prism_351.o ppohFVM_prism_351_metrics.o \
ppohFVM_edge_metrics_hexa_361.o ppohFVM_hexa_361_metrics.o ppohFVM_edge_color.o

$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS)
	ranlib $(LIB)
	cp *.mod $(INC)/.
clean:
	/bin/rm -f *.o $(LIB) *~ *.mod core.* $(INC)/*
.f.o:
	$(MPIF90) $(pFFLAGS) -c $*.f
.f90.o:
	$(MPIF90) $(pFFLAGS) -c $*.f90
.SUFFIXES: .f90
