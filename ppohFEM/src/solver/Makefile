include ../../../Makefile.in

all: build-default

install-header:

build-default:
	@cd communication && $(MAKE) build-default
	@cd matrix        && $(MAKE) build-default
	@cd init          && $(MAKE) build-default
	@cd sparse_matrix && $(MAKE) build-default
	@cd solver_11     && $(MAKE) build-default
	@cd solver_22     && $(MAKE) build-default
	@cd solver_33     && $(MAKE) build-default
	@cd solver_44     && $(MAKE) build-default
	@cd solver_66     && $(MAKE) build-default

clean:
	@cd communication && $(MAKE) clean
	@cd matrix        && $(MAKE) clean
	@cd init          && $(MAKE) clean
	@cd sparse_matrix && $(MAKE) clean
	@cd solver_11     && $(MAKE) clean
	@cd solver_22     && $(MAKE) clean
	@cd solver_33     && $(MAKE) clean
	@cd solver_44     && $(MAKE) clean
	@cd solver_66     && $(MAKE) clean

distclean:
	@cd communication && $(MAKE) distclean
	@cd matrix        && $(MAKE) distclean
	@cd init          && $(MAKE) distclean
	@cd sparse_matrix && $(MAKE) distclean
	@cd solver_11     && $(MAKE) distclean
	@cd solver_22     && $(MAKE) distclean
	@cd solver_33     && $(MAKE) distclean
	@cd solver_44     && $(MAKE) distclean
	@cd solver_66     && $(MAKE) distclean

maintainer-clean:
	@cd communication && $(MAKE) maintainer-clean
	@cd matrix        && $(MAKE) maintainer-clean
	@cd init          && $(MAKE) maintainer-clean
	@cd sparse_matrix && $(MAKE) maintainer_clean
	@cd solver_11     && $(MAKE) maintainer-clean
	@cd solver_22     && $(MAKE) maintainer-clean
	@cd solver_33     && $(MAKE) maintainer-clean
	@cd solver_44     && $(MAKE) maintainer-clean
	@cd solver_66     && $(MAKE) maintainer-clean

install:
	@cd communication && $(MAKE) install
	@cd matrix        && $(MAKE) install
	@cd init          && $(MAKE) install
	@cd sparse_matrix && $(MAKE) install
	@cd solver_11     && $(MAKE) install
	@cd solver_22     && $(MAKE) install
	@cd solver_33     && $(MAKE) install
	@cd solver_44     && $(MAKE) install
	@cd solver_66     && $(MAKE) install
