# Makefile for ppohFVM
include Makefile.in

default:
	(cd src ; make ) 
	(cd tools/partitioner ; make ) 

part:
	(cd tools/partitioner ; make ) 

ppohFVM:
	(cd src ; make ) 

hybNS:
	(cd examples/hybNS/src ; make ) 
	(cd examples/hybNS/mg ; make ) 

heat3D:
	(cd examples/heat3D/src ; make ) 
	(cd examples/heat3D/pmesh ; make ) 
	(cd examples/heat3D/pmesh ; make -f Makefile_bin) 

clean:
	(cd src ; make clean )
	(cd tools/partitioner ; make clean ) 
	(cd examples/hybNS/src ; make clean )
	(cd examples/hybNS/mg ; make clean )
	(cd examples/heat3D/src ; make clean )
	(cd examples/heat3D/pmesh ; make clean )
	(cd examples/heat3D/pmesh ; make -f Makefile_bin clean )

hybNS_clean:
	(cd examples/hybNS/src ; make clean) 
	(cd examples/hybNS/mg ; make clean) 

heat3D_clean:
	(cd examples/heat3D/src ; make clean) 
	(cd examples/heat3D/pmesh ; make clean) 
	(cd examples/heat3D/pmesh ; make -f Makefile_bin clean) 

install:
	$(MKDIR) $(PREFIX)
	$(MKDIR) $(LIBDIR)
	$(CP) ./lib/* $(LIBDIR)/

	$(MKDIR) $(INCDIR) 
	$(CP) ./include/* $(INCDIR)/
	$(MKDIR) $(BINDIR) 
	$(CP) ./bin/* $(BINDIR)/

ppohFVM_install:
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
