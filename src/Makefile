#FC_XXX = @FC_SINGLE@
#FC_XXX = sxf90
FC_XXX = fx10
#FC_XXX = frt
#FC_XXX = g95 
#FC_XXX = pgf90
#FC_XXX = xlf
#FC_XXX = sr16000

# NEC SX
FC_sxf90 = sxmpif90
#FC_sxf90 = sxmpif90 -ftrace
FFLAGS_sxf90 = -DVECTOR -DSX6_OPTION -dW -Wf "-pvctl inner fullmsg noassume noassoc noouterunroll loopcnt=4000000 vwork=stack -L fmtlist transform"
AR_sxf90 = sxar cr

# G95
FC_g95 = mpig95
FFLAGS_g95 = -g -i4 -fendian=big -fbounds-check -ftrace=full
#FFLAGS_g95 = -O2 -i4 -fendian=big
AR_g95 = ar cr
#AR_g95 = libtool -static -o  # for mac

# Intel Fortran
FC_ifort = mpif90
#FFLAGS_ifort = -assume byterecl -convert big_endian -traceback -g -fpe0 -CB -ftrapuv -auto -fpstkchk -warn -check all -zero-
#FFLAGS_ifort = -assume byterecl -convert big_endian -traceback -g -fpe0
FFLAGS_ifort = -check all -assume byterecl -convert big_endian
AR_ifort = ar cr

# PGI Fortran
FC_pgf90 = mpipgf90
FFLAGS_pgf90 = -O -Mpreprocess -byteswapio
AR_pgf90 = ar cr

# IBM XLF
FC_xlf = mpixlf -qsuffix=f=f90:cpp=F90 -qfree=f90
FFLAGS_xlf =
AR_xlf = libtool -static -o  # for mac

# HITACHI SR16000
FC_sr16000 = mpif90
FFLAGS_sr16000 = -nolimit -64 -Xpcomp
AR_sr16000 = ar -cr -X64

# Fujitsu FX10
FC_fx10 = mpifrtpx
FFLAGS_fx10 = -Cpp -Ccpp -O3
AR_fx10 = ar rv

# Default
FC_ = f90
FFLAGS_ =
AR_ = ar cr

FC = $(FC_$(FC_XXX))
FFLAGS = $(FFLAGS_$(FC_XXX))
AR = $(AR_$(FC_XXX))

OBJS_MATHMP = jcup_mpi_lib.o \
              jcup_constant.o \
              jcup_utils.o \
              jcup_time.o \
              jcup_buffer.o \
              jcup_comp.o \
              jcup_config.o \
              jcup_grid_base.o \
              jcup_interpolation.o \
              jcup_grid.o \
              jcup_data.o \
              jcup_exchange.o \
              jcup_io_base.o \
              jcup_interpolation_interface.o \
              jcup_interface.o \
              m_ppoh_MATHMP_base.o m_ppoh_MATHMP_namelist.o m_ppoh_MATHMP_mapping.o \
              m_ppoh_MATHMP_interpolation.o m_ppoh_MATHMP_rotation.o m_ppoh_MATHMP.o

all : libmathmp.a

libmathmp.a : $(OBJS_MATHMP)
	$(AR) libmathmp.a $(OBJS_MATHMP)
	cp *o ../include 
	cp *mod ../include
	cp libmathmp.a ../lib
clean :
	rm -f libmathmp.a i.*.L *.o *.mod *~

.SUFFIXES: 
.SUFFIXES: .f90 .F90 .o

.f90.o:
	$(FC) $(FFLAGS) -c $*.f90 -o $*.o 
.F90.o:
	$(FC) $(FFLAGS) -c $*.F90 -o $*.o 
  
