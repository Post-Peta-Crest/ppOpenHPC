PREFIX                  = $(HOME)/FrontISTR
BINDIR                  = $(PREFIX)/bin
LIBDIR                  = $(PREFIX)/lib
INCLUDEDIR              = $(PREFIX)/include

CC                      = mpifccpx
CFLAGS                  = -Kfast,openmp
BASE_CFLAGS             = -I.
LDFLAGS                 = -lm
OPTFLAGS                = 

AR                      = ar ruv
CP                      = cp -f
RM                      = rm -f
RANLIB                  = ranlib
MKDIR                   = mkdir -p

#------------------------------------------------------------------------------#
COBJFILEPOSTFIX         = o

TARGET                  = starter

#------------------------------------------------------------------------------#
.SUFFIXES:
.SUFFIXES: .o .c

.c.o:
	$(CC) -c $(BASE_CFLAGS) $(CFLAGS) $(OPTFLAGS) $< -o $@

#------------------------------------------------------------------------------#
OBJS = \
	starter.o

HEADERS =

#------------------------------------------------------------------------------#
all: build-default

build-default: $(OBJS)
	$(CC) -o $(TARGET) $(OBJS) $(LDFLAGS)

$(OBJS): $(HEADERS)

install:
	if [ ! -d $(PREFIX) ]; then \
		$(MKDIR) $(PREFIX); \
	fi
	if [ ! -d $(BINDIR) ]; then \
		$(MKDIR) $(BINDIR); \
	fi
#	$(CP) ./$(TARGET) $(BINDIR)/fistr
#	$(CP) ./$(TARGET) $(BINDIR)/hecmw_part
#	$(CP) ./$(TARGET) $(BINDIR)/hecmw_vis

clean:
	$(RM) *.$(COBJFILEPOSTFIX)

distclean: clean
	$(RM) $(TARGET)
