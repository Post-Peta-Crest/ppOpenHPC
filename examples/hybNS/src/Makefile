include ../../../Makefile.in

FFLAGS = $(pFFLAGS) -I. -I $(INCDIR)
LFLAGS = $(pFFLAGS) -L $(LIBDIR)
FLIBS =  $(LIBDIR)/ppohFVMlib.a
#
TARGET = ../../../bin/hybNS
default: $(TARGET)
OBJS = \
HYBRID.o bc_ffd.o bc_wal.o boundary.o \
calcsuf.o dynvisc.o \
extrapolate.o \
flow_init.o hist.o load_mesh.o\
hybNS.o input.o \
residual.o smooth.o solver.o \
edge_resid.o timstep.o var_free.o var_init.o rstart.o \
output_ucd.o

$(TARGET):  $(OBJS)
	$(MPIF90) $(LFLAGS) -o $(TARGET) $(OBJS) $(FLIBS); cp ../../../bin/hybNS ../run/hybNS 
clean:
	/bin/rm -f *.o $(TARGET) *~ *.mod core.* ../run/hybNS
.f.o:
	$(MPIF90) $(FFLAGS) -c  $*.f
.f90.o:
	$(MPIF90) $(FFLAGS) -c $*.f90
.SUFFIXES: .f90
