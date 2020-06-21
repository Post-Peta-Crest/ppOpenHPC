include ../../../Makefile.in

FFLAGS = $(sFFLAGS)

TARGET = ../../../bin/post_vtk

default: $(TARGET)
OBJS = post_vtk.f90

$(TARGET):  $(OBJS)
	$(F90) $(FFLAGS) -o $(TARGET) $(OBJS); cp ../../../bin/post_vtk ./post_vtk

clean:
	/bin/rm -f *.o $(TARGET) *~ *.mod core.* ./post_vtk
