#include compile.mk
include ../Mkinclude
include files.mk

CMODS = $(COBJS:.o=.mod)

LIBJCUP = libjcup.a

all : ${LIBJCUP} install

${LIBJCUP}: ${COBJS}
	${AR} ${ARFLAGS} $@ $^ && ${RANLIB} $@


clean		:
		${RM} *.mod *.o ${LIBJCUP}


install : ${LIBJCUP}
	${CP} ${LIBJCUP} ${JCUPLIBDIR}
	${CP} ${CMODS} ${JCUPINCDIR}
