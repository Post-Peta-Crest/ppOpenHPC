SYSTEM = FX10
#SYSTEM = INTEL
#SYSTEM = XC30

#FX10
ifeq ($(SYSTEM),FX10)
OPTFLAGS = -fs
CC=mpifccpx
F90=mpifrtpx -Kfast,openmp
#F90=mpifrtpx -Kopenmp
CCFLAGS = $(OPTFLAGS)
F90FLAGS = $(OPTFLAGS) -Cfpp
LDFLAGS = -SSL2
endif

#intel
ifeq ($(SYSTEM),INTEL)
#OPTFLAGS = -O3 -traceback -ip -heap-arrays -qopenmp
OPTFLAGS = -qopenmp -O3 -ip
CC=mpiicc
F90=mpiifort
CCFLAGS = $(OPTFLAGS)
#F90FLAGS = $(OPTFLAGS) -fpp -assume nounderscore -names uppercase
F90FLAGS = $(OPTFLAGS) -fpp
#F90FLAGS = $(OPTFLAGS) -fpp -check all
#F90FLAGS = -fpe0 -traceback -g -CB -assume nounderscore -names lowercase -fpp -check all
#LDFLAGS = -mkl -trace
LDFLAGS = -mkl
endif

#XC30
ifeq ($(SYSTEM),XC30)
OPTFLAGS = -O2 -homp
CC=cc
F90=ftn
CCFLAGS = $(OPTFLAGS)
F90FLAGS = $(OPTFLAGS)
endif

LINK=$(F90)

OBJS= HACApK_lib.o m_ppohBEM_user_func.o m_ppohBEM_matrix_element_ij.o m_HACApK_calc_entry_ij.o \
	 m_HACApK_base.o m_HACApK_solve.o m_HACApK_use.o m_ppohBEM_bembb2hacapk.o bem-bb-fw-HACApK-0.100.4.o \


TARGET=bem-bb-SCM.out

.SUFFIXES: .o .c .f90

$(TARGET): $(OBJS)
			$(LINK) -o $@ $(OBJS) $(LDFLAGS)

.c.o: *.c
			$(CC) -c $(CCFLAGS) $<
.f90.o: *.f90
#			echo 'f90 complile'
			$(F90) -c $< $(F90FLAGS)
clean:
	rm -f *.o *.mod $(TARGET)

rmod:
	rm -f m_*.o *.mod

