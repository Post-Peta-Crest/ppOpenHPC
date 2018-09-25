MAKE = make

all: build-default

build-default:
	@cd static  && $(MAKE) build-default
	@cd dynamic && $(MAKE) build-default
	@cd heat    && $(MAKE) build-default

clean:
	@cd static  && $(MAKE) clean
	@cd dynamic && $(MAKE) clean
	@cd heat    && $(MAKE) clean

distclean:
	@cd static  && $(MAKE) distclean
	@cd dynamic && $(MAKE) distclean
	@cd heat    && $(MAKE) distclean

install:
