include ../../../Makefile.in

FFLAGS = $(pFFLAGS) -I. -I $(INCDIR)
LFLAGS = $(pFFLAGS) -L $(LIBDIR)
FLIBS =  $(LIBDIR)/ppohAMRFDMlib.a

TARGET = ../../../bin/advAMR3D

default: $(TARGET)
OBJS = \
advAMR3D.o \
setup_model.o \
kernel_adv.o \
RCIP1D.o \
output_data.o

$(TARGET):  $(OBJS)
	$(MPIF90) $(LFLAGS) -o $(TARGET) $(OBJS) $(FLIBS); cp ../../../bin/advAMR3D ../run/advAMR3D

clean:
	/bin/rm -f *.o $(TARGET) *~ *.mod *__genmod* core.* ../run/advAMR3D

.f90.o:
	$(MPIF90) $(FFLAGS) -c $*.f90

.SUFFIXES: .f90
