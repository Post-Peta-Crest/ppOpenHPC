include ../../Makefile.in

all: build-default

build-default:
	@cd common     && $(MAKE) install-header
	@cd element    && $(MAKE) install-header
	@cd solver     && $(MAKE) install-header
	@cd visualizer && $(MAKE) install-header
	@cd common     && $(MAKE) build-default
	@cd element    && $(MAKE) build-default
	@cd solver     && $(MAKE) build-default
	@cd ppohFEM    && $(MAKE) build-default
	@cd visualizer && $(MAKE) build-default

msg-setup:
	@cd common     && $(MAKE) msg-setup

lex-setup:
	@cd common     && $(MAKE) lex-setup

clean:
	@cd common     && $(MAKE) clean
	@cd element    && $(MAKE) clean
	@cd solver     && $(MAKE) clean
	@cd ppohFEM    && $(MAKE) clean
	@cd visualizer && $(MAKE) clean

distclean:
	@cd common     && $(MAKE) distclean
	@cd element    && $(MAKE) distclean
	@cd solver     && $(MAKE) distclean
	@cd ppohFEM    && $(MAKE) distclean
	@cd visualizer && $(MAKE) distclean

install:
	@cd common     && $(MAKE) install
	@cd element    && $(MAKE) install
	@cd solver     && $(MAKE) install
	@cd ppohFEM    && $(MAKE) install
	@cd visualizer && $(MAKE) install
