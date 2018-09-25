include ../Mkinclude
include files.mk

CMODS = $(COBJS:.o=.mod)

LIBJCF = libjcf.a

all : ${LIBJCF} install

${LIBJCF}: ${COBJS}
	${AR} ${ARFLAGS} $@ $^ && ${RANLIB} $@


clean		:
		${RM} *.mod *.o ${LIBJCF}


install : ${LIBJCF}
	${CP} ${LIBJCF} ${JCUPLIBDIR}
	${CP} ${CMODS} ${JCUPINCDIR}
