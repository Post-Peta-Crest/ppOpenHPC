MAKE = make

all: build-default

build-default:
	@cd common     && $(MAKE) install-header
#	@cd operations && $(MAKE) install-header
	@cd solver     && $(MAKE) install-header
#	@cd couple     && $(MAKE) install-header
	@cd visualizer && $(MAKE) install-header
	@cd common     && $(MAKE) build-default
#	@cd operations && $(MAKE) build-default
	@cd solver     && $(MAKE) build-default
#	@cd couple     && $(MAKE) build-default
	@cd hecmw      && $(MAKE) build-default
	@cd visualizer && $(MAKE) build-default
#	@cd etc        && $(MAKE) build-default

msg-setup:
	@cd common     && $(MAKE) msg-setup

lex-setup:
	@cd common     && $(MAKE) lex-setup

clean:
	@cd common     && $(MAKE) clean
	@cd operations && $(MAKE) clean
	@cd solver     && $(MAKE) clean
	@cd couple     && $(MAKE) clean
	@cd hecmw      && $(MAKE) clean
	@cd visualizer && $(MAKE) clean
	@cd etc        && $(MAKE) clean

distclean:
	@cd common     && $(MAKE) distclean
	@cd operations && $(MAKE) distclean
	@cd solver     && $(MAKE) distclean
	@cd couple     && $(MAKE) distclean
	@cd hecmw      && $(MAKE) distclean
	@cd visualizer && $(MAKE) distclean
	@cd etc        && $(MAKE) distclean

install:
	@cd common     && $(MAKE) install
	@cd operations && $(MAKE) install
	@cd solver     && $(MAKE) install
	@cd couple     && $(MAKE) install
	@cd hecmw      && $(MAKE) install
	@cd visualizer && $(MAKE) install
	@cd etc        && $(MAKE) install
