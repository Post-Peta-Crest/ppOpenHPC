include ../Makefile.in

all: build-default

build-default:
	if [ ! -d bin ]; then \
		$(MKDIR) bin; \
	fi
	@cd src && $(MAKE) build-default

clean:
	@cd src && $(MAKE) clean
	$(RM) ./bin/*

distclean:
	@cd src && $(MAKE) distclean
	$(RM) ./bin/*

install:
	$(CP) ./src/app_flow ./bin
