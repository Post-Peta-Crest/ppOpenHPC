PREFIX     = $(HOME)/FrontISTR
BINDIR     = $(PREFIX)/bin
LIBDIR     = $(PREFIX)/lib
INCLUDEDIR = $(PREFIX)/include

MAKE       = make
AR         = ar ruv
CP         = cp -f
RM         = rm -f
RANLIB     = ranlib
MKDIR      = mkdir -p

all:
	@cd hecmw1 && $(MAKE)
	@cd fistr1 && $(MAKE)
#	@cd hecmw2 && $(MAKE)
#	@cd fistr2 && $(MAKE)
#	@cd fistr  && $(MAKE)

clean:
	@cd hecmw1 && $(MAKE) clean
	@cd fistr1 && $(MAKE) clean
	@cd hecmw2 && $(MAKE) clean
	@cd fistr2 && $(MAKE) clean
	@cd fistr  && $(MAKE) clean

distclean:
	@cd hecmw1 && $(MAKE) distclean
	@cd fistr1 && $(MAKE) distclean
	@cd hecmw2 && $(MAKE) distclean
	@cd fistr2 && $(MAKE) distclean
	@cd fistr  && $(MAKE) distclean
	- $(RM) ./bin/*

install:
	if [ ! -d $(PREFIX) ]; then \
		$(MKDIR) $(PREFIX); \
	fi
	if [ ! -d $(BINDIR) ]; then \
		$(MKDIR) $(BINDIR); \
	fi
	@cd hecmw1 && $(MAKE) install
	@cd fistr1 && $(MAKE) install
	@cd hecmw2 && $(MAKE) install
	@cd fistr2 && $(MAKE) install
	@cd fistr  && $(MAKE) install
