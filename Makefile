# Makefile for ppohVIS_FDM3D
include Makefile.in

default:
	(cd src ; make)

fdm1:
	(cd examples/FDM/src; make clean)
	(cd examples/FDM/src; make)

fdm3:
	(cd examples/FDM3/src; make clean)
	(cd examples/FDM3/src; make)

clean:
	(cd src ; make clean)
	(cd examples/FDM/src ; make clean)
	(cd examples/FDM3/src ; make clean)

fdm1_clean:
	(cd examples/FDM/src; make clean)

fdm3_clean:
	(cd examples/FDM3/src; make clean)


distclean:
	(cd src ; make distclean)
	$(RM) ./bin/*
	$(RM) ./lib/*
	$(RM) ./include/*

install:
	$(MKDIR) $(PREFIX)

	$(MKDIR) $(LIBDIR)
	$(CP) ./lib/* $(LIBDIR)/
	$(MKDIR) $(INCDIR)
	$(CP) ./include/* $(INCDIR)/

uninstall:
	$(RM) $(PREFIX)
