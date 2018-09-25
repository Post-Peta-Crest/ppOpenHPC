MAKE = make

all: build-default

build-default:
	@cd common   && $(MAKE) install-header
	@cd hecmw2   && $(MAKE) build-default
	@cd lib      && $(MAKE) build-default
	@cd common   && $(MAKE) build-default
	@cd analysis && $(MAKE) build-default
	@cd main     && $(MAKE) build-default

clean:
	@cd hecmw2   && $(MAKE) clean
	@cd lib      && $(MAKE) clean
	@cd common   && $(MAKE) clean
	@cd analysis && $(MAKE) clean
	@cd main     && $(MAKE) clean

distclean:
	@cd hecmw2   && $(MAKE) distclean
	@cd lib      && $(MAKE) distclean
	@cd common   && $(MAKE) distclean
	@cd analysis && $(MAKE) distclean
	@cd main     && $(MAKE) distclean

install:
