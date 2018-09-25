# makefile for Seismic2D tools
# Intel compiler
FC = ifort
FFLAGS = -O3

all : pmxy2d rwav2d
pmxy2d: ../../src/seismic_2D/m_stdlib.f90 m_system.f90 m_number1.f90 m_getopt.f90 m_readprm.f90 m_ppm.f90 ppmxy2d.f90
	$(FC) $(FFLAGS) $^ -o $@

rwav2d: ../../src/seismic_2D/m_stdlib.f90 ../../src/seismic_2D/m_params.f90 rwav2d.f90
	$(FC) $(FFLAGS) $^ -o $@

clean:
	/bin/rm -f *.o *.mod *~ ppmxy2d rwav2d

