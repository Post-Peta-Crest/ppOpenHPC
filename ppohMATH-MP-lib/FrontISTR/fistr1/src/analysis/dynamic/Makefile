MAKE = make

all: build-default

build-default:
	@cd mode    && $(MAKE) build-default
	@cd freq    && $(MAKE) build-default
	@cd transit && $(MAKE) build-default

clean:
	@cd mode    && $(MAKE) clean
	@cd transit && $(MAKE) clean
	@cd freq    && $(MAKE) clean

distclean:
	@cd mode    && $(MAKE) distclean
	@cd transit && $(MAKE) distclean
	@cd freq    && $(MAKE) distclean

install:
