include ../Makefile.in

####################################################
####################################################

COBJS	      = main.o pass1.o pass2.o pass3.o pass4.o pass5.o TuneRegion.o VCL_Lib.o Visual.o
CSRCS	      = $(COBJS:.o=.cpp)
CEXEC         = oat


all:		$(COBJS) $(CEXEC)
		$(CP) $(CEXEC) ../bin/

$(COBJS):	$(CSRCS)
		$(CC) $(CFLAGS) $(LDFLAGS) -c $(CSRCS)

$(CEXEC):	$(COBJS) 
		echo "Linking $(CEXEC) ..."
		$(CC) $(CFLAGS) $(COBJS) $(LDFLAGS) -o $(CEXEC)
		echo "done"




clean:;		@rm -f $(COBJS) core *~
distclean:;	@rm -f $(COBJS) core *~ $(CEXEC) 
