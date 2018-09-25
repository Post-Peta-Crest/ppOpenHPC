include ../../../Makefile.in

FFLAGSL   = -I../../../include
FLDFLAGSL = -L../../../lib
LIBSL     = -lfppohvisfdm3d -lppohvisfdm3d


.SUFFIXES:
.SUFFIXES: .o .f

.f.o:
	$(FC) -c $(FFLAGS) $(FFLAGSL) $< -o $@

TARGET = ../run/fdm_test

OBJS = test.o

all: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) -o $(TARGET) $(FFLAGS) $(FFLAGSL) $(OBJS) $(LIBS) $(LIBSL) $(FLDFLAGSL)
clean:
	rm -f *.o *.mod $(TARGET)

distclean:
	rm -f *.o *.mod $(TARGET)
