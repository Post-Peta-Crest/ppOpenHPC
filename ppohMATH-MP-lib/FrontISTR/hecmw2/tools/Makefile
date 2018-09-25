MAKE = make

all: build-default

build-default:
	@cd conv2mw3    && $(MAKE) build-default
	@cd partitioner && $(MAKE) build-default
	@cd visualizer  && $(MAKE) build-default

clean:
	@cd conv2mw3    && $(MAKE) clean
	@cd partitioner && $(MAKE) clean
	@cd visualizer  && $(MAKE) clean

distclean:
	@cd conv2mw3    && $(MAKE) distclean
	@cd partitioner && $(MAKE) distclean
	@cd visualizer  && $(MAKE) distclean

install:
	@cd conv2mw3    && $(MAKE) install
	@cd partitioner && $(MAKE) install
	@cd visualizer  && $(MAKE) install
