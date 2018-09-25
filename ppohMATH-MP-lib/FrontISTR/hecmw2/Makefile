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

all: create-include-dir create-lib-dir build-libs build-default

build-default: 

create-include-dir:
	if [ ! -d include ]; then \
		$(MKDIR) include; \
	fi

create-lib-dir:
	if [ ! -d lib ]; then \
		$(MKDIR) lib; \
	fi

create-bin-dir:
	if [ ! -d bin ]; then \
		$(MKDIR) bin; \
	fi

build-libs: clean-includes clean-libs
	@cd src && $(MAKE) build-default

build-tools:
	@cd tools && $(MAKE) build-default

clean-includes:
	$(RM) ./include/*

clean-libs:
	$(RM) ./lib/*

clean-bins:
	$(RM) ./bin/*

clean:
	@cd src && $(MAKE) clean
	@cd tools && $(MAKE) clean

distclean: clean-includes clean-libs clean-bins
	@cd src && $(MAKE) distclean
	@cd tools && $(MAKE) distclean

install:
	if [ ! -d $(PREFIX) ]; then \
		$(MKDIR) $(PREFIX); \
	fi
	if [ ! -d $(BINDIR) ]; then \
		$(MKDIR) $(BINDIR); \
	fi
	$(CP) ./bin/* $(BINDIR)
