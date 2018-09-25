## Makefile of Seismic3D
# intel compiler
FC = ifort
FFLAGS = -O3

# FX10 compiler
# FC = frtpx
# FFLAGS = -O3

all: catsnap catwav ppmxy3d3 rwav3d

catsnap: m_stdio.f90 catsnap.f90
	$(FC) $(FFLAGS) $^ -o $@

catwav: m_stdio.f90 catwav.f90
	$(FC) $(FFLAGS) $^ -o $@

ppmxy3d3: m_stdio.f90 ppmxy3d3.f90
	$(FC) $(FFLAGS) $^ -o $@

rwav3d: m_stdio.f90 rwav3d.f90
	$(FC) $(FFLAGS) $^ -o $@

clean: 
	rm -f catsnap catwav rwav3d ppmxy3d3 *.o *.mod
