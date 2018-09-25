# Install directory
PREFIX        = ~/ppohAT_0.1.0
BINDIR        = $(PREFIX)/bin
INCDIR        = $(PREFIX)/include
LIBDIR        = $(PREFIX)/lib

# C compiler settings
CC            = gcc
CFLAGS        = $(CINCDIR) $(COPTFLAGS)
COPTFLAGS     = 
CINCDIR       = -I../include

# Linker settings
LD            = $(CC)
LIBS          = -lm -lstdc++
LDFLAGS       = $(LIBS)

# Archiver settings
AR            = ar rv

# ETC
CP            = cp -f
RM            = rm -rf
MKDIR         = mkdir
