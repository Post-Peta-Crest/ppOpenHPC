include ../../../Makefile.in
#
#
TARGET = ../../../bin/hybNS_mg
default: $(TARGET)
OBJS = main.o grids.o partition.o tetgen.o

$(TARGET):  $(OBJS)
	$(F90) $(sMGFLAGS) -o $(TARGET) $(OBJS) 
clean:
	/bin/rm -f *.o $(TARGET) *~ *.mod core.*
.f.o:
	$(F90) $(sMGFLAGS) -c  $*.f
.f90.o:
	$(F90) $(sMGFLAGS) -c $*.f90
.SUFFIXES: .f90
