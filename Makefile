# Makefile for ppohDEM
include Makefile.in

default: 
	(cd utils ; make)
	(cd examples ; make) 

clean: 
	(cd utils ; make clean)
	(cd examples ; make clean)

install:
	$(MKDIR) $(PREFIX)
	$(MKDIR) $(LIBDIR)
	$(CP) ./lib/* $(LIBDIR)/
	$(MKDIR) $(INCDIR) 
	$(CP) ./include/* $(INCDIR)/

uninstall:
	$(RM) $(PREFIX)
