# Makefile for ppohAMRFDM
include Makefile.in

default:
	(cd src ; make)

advAMR3D:
	(cd examples/advAMR3D/src ; make)
	(cd examples/advAMR3D/post_vtk ; make)

clean:
	(cd src ; make clean)
	(cd examples/advAMR3D/src ; make clean)
	(cd examples/advAMR3D/post_vtk ; make clean)

advAMR3D_clean:
	(cd examples/advAMR3D/src ; make clean)
	(cd examples/advAMR3D/post_vtk ; make clean)

install:
	$(MKDIR) $(PREFIX)
	$(MKDIR) $(LIBDIR)
	$(CP) ./lib/* $(LIBDIR)/

	$(MKDIR) $(INCDIR) 
	$(CP) ./include/* $(INCDIR)/

bin_install:
	$(MKDIR) $(PREFIX)
	$(MKDIR) $(BINDIR) 
	$(CP) ./bin/* $(BINDIR)/

uninstall:
	$(RM) $(PREFIX)