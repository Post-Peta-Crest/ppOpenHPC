include Makefile.in

all:
	@cd ppohFEM    && $(MAKE)
	@cd app_struct && $(MAKE)
	@cd app_heat   && $(MAKE)
	@cd app_flow   && $(MAKE)

clean:
	@cd ppohFEM    && $(MAKE) distclean
	@cd app_struct && $(MAKE) distclean
	@cd app_heat   && $(MAKE) distclean
	@cd app_flow   && $(MAKE) distclean
