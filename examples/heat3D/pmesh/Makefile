include ../../../Makefile.in
#
#
TARGET  = ../../../bin/pmesh
default: $(TARGET)
OBJS = pmesh.o

$(TARGET):  $(OBJS)
	$(MPIF90) $(sMGFLAGS) -o $(TARGET) $(OBJS) ; cp ../../../bin/pmesh pmesh
clean:
	/bin/rm -f *.o $(TARGET) *~ *.mod core.*
.f.o:
	$(MPIF90) $(sMGFLAGS) -c  $*.f
.f90.o:
	$(MPIF90) $(sMGFLAGS) -c $*.f90
.SUFFIXES: .f90
