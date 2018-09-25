MAKE = make

all: build-default

build-default:
	@cd partitioner           && $(MAKE) build-default
	@cd visualizer            && $(MAKE) build-default
	@cd hec2rcap              && $(MAKE) build-default
	@cd result_type_converter && $(MAKE) build-default
	@cd result_file_merger    && $(MAKE) build-default

clean:
	@cd partitioner           && $(MAKE) clean
	@cd visualizer            && $(MAKE) clean
	@cd hec2rcap              && $(MAKE) clean
	@cd result_type_converter && $(MAKE) clean
	@cd result_file_merger    && $(MAKE) clean

distclean:
	@cd partitioner           && $(MAKE) distclean
	@cd visualizer            && $(MAKE) distclean
	@cd hec2rcap              && $(MAKE) distclean
	@cd result_type_converter && $(MAKE) distclean
	@cd result_file_merger    && $(MAKE) distclean

install:
	@cd partitioner           && $(MAKE) install
	@cd visualizer            && $(MAKE) install
	@cd hec2rcap              && $(MAKE) install
	@cd result_type_converter && $(MAKE) install
	@cd result_file_merger    && $(MAKE) install
