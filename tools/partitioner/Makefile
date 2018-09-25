include ../../Makefile.in
#
FLIBS  = $(METISDIR)/libmetis.a
TARGET = ../../bin/ppohFVM_part
default: $(TARGET)
OBJS = \
ppohFVM_part_util.o \
ppohFVM_part_partitioner.o ppohFVM_part.o ppohFVM_part_input_grid.o\
ppohFVM_part_calc_edgcut.o ppohFVM_part_cre_local_data.o ppohFVM_part_define_file_name.o \
ppohFVM_part_interface_nodes.o ppohFVM_part_metis.o\
ppohFVM_part_neib_pe.o ppohFVM_part_paraset.o ppohFVM_part_proc_local.o ppohFVM_part_local_data.o\
ppohFVM_part_double_numbering.o ppohFVM_part_tool.o

$(TARGET):  $(OBJS)
	$(F90) $(sFFLAGS) -o $(TARGET) $(OBJS) $(FLIBS)
clean:
	/bin/rm -f *.o $(TARGET) *~ *.mod core.*
.f.o:
	$(F90) $(sFFLAGS) -c  $*.f
.f90.o:
	$(F90) $(sFFLAGS) -c $*.f90
.SUFFIXES: .f90
