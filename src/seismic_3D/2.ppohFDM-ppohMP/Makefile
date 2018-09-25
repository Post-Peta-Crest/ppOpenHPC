#for intel fortran compiler
#MFC = mpif90
#FFLAG = -O3 -xhost -warn all
#FFLAG = -ipo20 -O3 -xhost -warn all
#RM = rm -f

#for FX10 fujitsu compiler
MFC = mpifrtpx
FFLAG = -Kfast -Kopenmp
RM = rm -f
 JCUPINC = -I../../../ppohMATH-MP-lib/ppohMATHMP -I../../../ppohMATH-MP-lib/Jcup/include
 JCUPLIB = -L../../../ppohMATH-MP-lib/ppohMATHMP -lppohMATHMP -L../../../ppohMATH-MP-lib/Jcup/lib -ljcup

.SUFFIXES: 
.SUFFIXES: .f90 .f .o

#
seism3d3n : seism3d3n_real.o m_stdio.o m_coupling_prm.o m_param_real_mp.o m_pfd3d.o m_pssub.o m_boundary.o m_stress.o m_velocity.o m_source.o m_sponge_absorber.o m_set_condition_real.o m_io.o
	$(MFC) $(FFLAG) seism3d3n_real.f90 m_coupling_prm.o m_param_real_mp.o m_stdio.o m_pfd3d.o m_pssub.o m_boundary.o m_stress.o m_velocity.o m_source.o m_sponge_absorber.o m_set_condition_real.o m_io.o $(JCUPLIB) -o $@

seism3d3n_real.o: seism3d3n_real.f90 m_stdio.o m_coupling_prm.o m_param_real_mp.o m_pfd3d.o m_pssub.o m_boundary.o m_stress.o m_velocity.o m_source.o m_sponge_absorber.o m_set_condition_real.o m_io.o
	$(MFC) $(FFLAG) $(JCUPINC) -c seism3d3n_real.f90
m_stdio.o : m_stdio.f90
m_param_real_mp.o : m_param_real_mp.f90 m_stdio.o
m_pssub.o : m_pssub.f90 m_stdio.o
m_io.o	: m_io.f90 m_stdio.o 
m_pfd3d.o : m_pfd3d.f90 m_stdio.o
m_boundary.o : m_boundary.f90 m_stdio.o m_param_real_mp.o
m_stress.o : m_stress.f90 m_stdio.o m_param_real_mp.o
m_velocity.o : m_velocity.f90 m_stdio.o m_param_real_mp.o
m_source.o : m_source.f90 m_stdio.o m_param_real_mp.o
m_sponge_absorber.o : m_sponge_absorber.f90 m_stdio.o m_param_real_mp.o
m_set_condition_real.o : m_set_condition_real.f90 m_stdio.o m_param_real_mp.o
m_coupling_prm.o : m_coupling_prm.f90 m_param_real_mp.o

.f90.o:
	$(MFC) $(FFLAG) -c $< -o $@

clean:
	$(RM) *.o *.mod seism3d3n seism3d3n-*.dat seism3d3n-*.cod *.fld C.SEISM.coupling.log.* SEISM.log.* SEISM.coupling.log.* *~