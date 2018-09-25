include ../../../Makefile.in

FFLAGS = $(pFFLAGS) -I. -I $(INCDIR)
LFLAGS = $(pFFLAGS) -L $(LIBDIR)
FLIBS =  $(LIBDIR)/ppohFVMlib.a

.SUFFIXES:
.SUFFIXES: .o .f90 .f

.f.o:
	$(MPIF90) -c $(FFLAGS) $(FFLAGSL) $< -o $@
.f90.o:
	$(MPIF90) -c $(FFLAGS) $(FFLAGSL) $< -o $@

TARGET = ../../../bin/heat3D

OBJS = \
	ppohFVM_util_matrix.o ppohFVM_mat_ass_color.o\
	ppohFVM_mat_con.o ppohFVM_ucd_regular_hexa_1.o\
	ppohFVM_CG_2_1_00.o ppohFVM_solver11.o ppohFVM_sort.o\
	pfem_util.o test1.o \
	util.o \
	input_cntl.o \
	load_mesh.o\
	mat_ass_bc.o \
	mat_ass_main.o


all: $(TARGET)

$(TARGET): $(OBJS)
	$(MPIF90) -o $(TARGET) $(FFLAGS) $(FFLAGSL) $(OBJS) $(FLIBS); cp ../../../bin/heat3D ../run/heat3D 

clean:
	rm -f *.o *.mod $(TARGET) *~ ../run/heat3D

distclean:
	rm -f *.o *.mod $(TARGET)
