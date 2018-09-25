MAKE = make

all: build-default

install-header:
	@cd dynamic_load_balancing && $(MAKE) install_header

build-default:
	@cd adaptation             && $(MAKE) build-default
	@cd dynamic_load_balancing && $(MAKE) build-default
	@cd jacobian               && $(MAKE) build-default

clean:
	@cd adaptation             && $(MAKE) clean
	@cd dynamic_load_balancing && $(MAKE) clean
	@cd jacobian               && $(MAKE) clean

distclean:
	@cd adaptation             && $(MAKE) distclean
	@cd dynamic_load_balancing && $(MAKE) distclean
	@cd jacobian               && $(MAKE) distclean

install:
	@cd adaptaion              && $(MAKE) install
	@cd dynamic_load_balancing && $(MAKE) install
	@cd jacobian               && $(MAKE) install
