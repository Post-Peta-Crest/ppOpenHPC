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

all: build-default

build-default:
	if [ ! -d include ]; then \
		$(MKDIR) include; \
	fi
	if [ ! -d lib ]; then \
		$(MKDIR) lib; \
	fi
	if [ ! -d bin ]; then \
		$(MKDIR) bin; \
	fi
	$(RM) ./include/*
	$(RM) ./lib/*
	@cd src && $(MAKE) build-default

build-tools:
	@cd tools && $(MAKE)

clean:
	@cd src && $(MAKE) clean
	@cd tools && $(MAKE) clean

distclean:
	@cd src && $(MAKE) distclean
	@cd tools && $(MAKE) distclean
	$(RM) ./include/*
	$(RM) ./bin/*
	$(RM) ./lib/*

install:
	if [ ! -d $(PREFIX) ]; then \
		$(MKDIR) $(PREFIX); \
	fi
	if [ ! -d $(BINDIR) ]; then \
		$(MKDIR) $(BINDIR); \
	fi
	$(CP) ./bin/* $(BINDIR)
	@cd tools && $(MAKE) install
