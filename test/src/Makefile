include ./Mkinclude

.SUFFIXES: .f90 .o

OBJS = mod_common.o mod_field.o 
SRCS = $(OBJS:.o=.f90)
MODS = $(OBJS:.o=.mod)

TARGET1 = comp1
TARGET2 = comp2
TARGET3 = mapping

all : ${TARGET1} ${TARGET2} ${TARGET3}

${TARGET1} : $(OBJS) comp1.o
	$(FC) $(LDFLAGS) -o $@ $^ $(JOBJ) $(LIBS) $(LIBMATHMP) 

${TARGET2} : $(OBJS) comp2.o
	$(FC) $(LDFLAGS) -o $@ $^ $(JOBJ) $(LIBS) $(LIBMATHMP) 

${TARGET3} : $(OBJS) make_mapping.o
	$(FC) $(LDFLAGS) -o $@ $^ $(JOBJ) $(LIBS) $(LIBMATHMP) 

clean:
	-rm -f *mod *o

.f90.o:	
	$(FC) -I$(INCMATHMP) -c $< $(FFLAGS) 
