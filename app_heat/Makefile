include ../Makefile.in

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


clean:
	@cd src && $(MAKE) clean
	$(RM) ./include/*
	$(RM) ./bin/*
	$(RM) ./lib/*

distclean:
	@cd src && $(MAKE) distclean
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
