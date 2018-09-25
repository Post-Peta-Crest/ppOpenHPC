# Makefile for ppohAT

include ./Makefile.in
default:
	(cd src ; make ) 

src:
	(cd src ; make ) 

clean:
	(cd src ; make clean )

distclean:
	(cd src ; make distclean )
	$(RM) ./bin/*

install:
	$(MKDIR) $(PREFIX)
	$(MKDIR) $(BINDIR); $(CP) ./bin/* $(BINDIR)/
	$(MKDIR) $(INCDIR); $(CP) ./include/* $(INCDIR)/


uninstall:
	$(RM) $(PREFIX)
