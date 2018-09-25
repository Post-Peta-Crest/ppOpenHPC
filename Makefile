# Makefile for the flat MPI and MPI/OpenMP hybrid parallel computing of ppOpen-APPPL/FDM
# Makefile for the ppOpen-APPL/FDM with the ppOpen-MATH/VIS-FDM3D

include Makefile.in

default:
	(cd ppohMATH-VIS-lib; make)

install:
	$(MKDIR) $(PREFIX)

	$(MKDIR) $(LIBDIR)
	$(CP) ./ppohMATH-VIS-install/lib/* $(LIBDIR)/
	$(MKDIR) $(INCDIR)
	$(CP) ./ppohMATH-VIS-install/include/* $(INCDIR)/

seism2d:
	(cd src/seismic_2D; make)

seism3d-ppohVIS:
	(cd src/seismic_3D/1.ppohFDM-ppohVIS; make)
seism3d-parallel:
	(cd src/seismic_3D/3.parallel; make)
seism3d-parallel-opt-fx10:
	(cd src/seismic_3D/4.parallel-opt/FX10; make)
seism3d-parallel-opt-intel:
	(cd src/seismic_3D/4.parallel-opt/Intel; make)

clean1:
	(cd src/seismic_2D; make clean)
clean2:
	(cd src/seismic_3D/1.pureMPI-ppohVIS; make clean)
clean4:
	(cd src/seismic_3D/3.parallel; make clean)
clean5:
	(cd src/seismic_3D/4.parallel-opt/FX10; make clean)
clean6:
	(cd src/seismic_3D/4.parallel-opt/Intel; make clean)

distclean:
	(cd src ; make distclean)
	(cd examples ; make distclean)
	$(RM) ./ppohMATH-VIS-install/lib*
	$(RM) ./ppohMATH-VIS-install/include/*

uninstall:
	$(RM) $(PREFIX)