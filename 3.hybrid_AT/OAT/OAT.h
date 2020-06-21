 
!     === common variables

      character*10000  OAT_AllRoutines
	  character*10000  OAT_InstallRoutines
	  character*10000  OAT_StaticRoutines
	  character*10000  OAT_DynamicRoutines
	  common /OAT_DRL/OAT_AllRoutines,                                  &
	 &      OAT_InstallRoutines,                                        &
	 &      OAT_StaticRoutines,                                         &
	 &      OAT_DynamicRoutines

	  integer  OAT_NUMPROCS
	  integer  OAT_STARTTUNESIZE
	  integer  OAT_ENDTUNESIZE
	  integer  OAT_SAMPDIST
	  integer  OAT_TUNESTATIC
	  integer  OAT_DEBUG
	  integer  OAT_MAXSAMPITER
          integer  OAT_ATEXEC_FLAG
	  common /OAT_SBP/OAT_NUMPROCS,                                     &
	 &         OAT_STARTTUNESIZE,                                       &
	 &         OAT_ENDTUNESIZE,                                         &
	 &         OAT_SAMPDIST,                                            &
	 &         OAT_TUNESTATIC,                                          &
	 &         OAT_DEBUG,                                               &
         &         OAT_MAXSAMPITER,                                         &
         &         OAT_ATEXEC_FLAG                   

	  integer  OAT_ALL
	  integer  OAT_INSTALL
	  integer  OAT_STATIC
	  integer  OAT_DYNAMIC
	  parameter (OAT_ALL = 0)
	  parameter (OAT_INSTALL = 1)
	  parameter (OAT_STATIC = 2)
	  parameter (OAT_DYNAMIC = 3)

	  integer  oat_myid
	  integer  oat_nprocs
          integer  oat_max_threads

          common  /OAT_pval/oat_myid,oat_nprocs,oat_max_threads

	  logical OAT_STATICTUNE
	  common /OAT_ST/OAT_STATICTUNE
	  logical OAT_DYNAMICTUNE
	  common /OAT_DT/OAT_DYNAMICTUNE


!       === for fitting
!         === data array defines
!         === upper bound for total sampling points
	  integer  OATLSM_MAX_N
	  parameter (OATLSM_MAX_N = 512)
	  integer  OATLSM_MAX_NPARM
	  parameter (OATLSM_MAX_NPARM = 512)

!        === upper bound for the order of linear polynonial equation
	  integer  OATLSM_MAX_M
	  parameter (OATLSM_MAX_M = 10)

!        === upper bound for the number of sampling points for matrix dimension
	  integer  OATLSM_MAX_SAMP
	  parameter (OATLSM_MAX_SAMP = 512)

!        === AT region variables 
         integer iusw1_ppohFDMupdate_stress_select
         integer iusw1_ppohFDMupdate_vel_select

         integer iusw1_ppohFDMupdate_stress
         integer iusw1_ppohFDMupdate_sponge
         integer iusw1_ppohFDMupdate_vel

         integer iusw1_ppohFDM_update_vel_sponge
         integer iusw1_ppohFDM_pdiffx3_p4
         integer iusw1_ppohFDM_pdiffx3_m4
         integer iusw1_ppohFDM_pdiffy3_p4
         integer iusw1_ppohFDM_pdiffy3_m4
         integer iusw1_ppohFDM_pdiffz3_p4
         integer iusw1_ppohFDM_pdiffz3_m4

         integer iusw1_ppohFDM_ps_bef
         integer iusw1_ppohFDM_ps_aft
         integer iusw1_ppohFDM_pv_bef
         integer iusw1_ppohFDM_pv_aft

          common /OAT_ATswitches/iusw1_ppohFDMupdate_stress_select,         &
         &                      iusw1_ppohFDMupdate_vel_select,             &
         &                      iusw1_ppohFDMupdate_stress,                 &
         &                      iusw1_ppohFDMupdate_sponge,                 &
         &                      iusw1_ppohFDMupdate_vel,                    &
         &                      iusw1_ppohFDM_update_vel_sponge,            &
         &                      iusw1_ppohFDM_pdiffx3_p4,                   &
         &                      iusw1_ppohFDM_pdiffx3_m4,                   &
         &                      iusw1_ppohFDM_pdiffy3_p4,                   &
         &                      iusw1_ppohFDM_pdiffy3_m4,                   &
         &                      iusw1_ppohFDM_pdiffz3_p4,                   &
         &                      iusw1_ppohFDM_pdiffz3_m4,                   &
         &                      iusw1_ppohFDM_ps_bef,                       &
         &                      iusw1_ppohFDM_ps_aft,                       &
         &                      iusw1_ppohFDM_pv_bef,                       &
         &                      iusw1_ppohFDM_pv_aft                      


!         === AT region variables for number of threads
	 integer iusw1_ppohFDMupdate_stress_select_nthreads
         integer iusw1_ppohFDMupdate_vel_select_nthreads

         integer iusw1_ppohFDMupdate_stress_nthreads
         integer iusw1_ppohFDMupdate_sponge_nthreads
         integer iusw1_ppohFDMupdate_vel_nthreads

         integer iusw1_ppohFDM_update_vel_sponge_nthreads
         integer iusw1_ppohFDM_pdiffx3_p4_nthreads
         integer iusw1_ppohFDM_pdiffx3_m4_nthreads
         integer iusw1_ppohFDM_pdiffy3_p4_nthreads
         integer iusw1_ppohFDM_pdiffy3_m4_nthreads
         integer iusw1_ppohFDM_pdiffz3_p4_nthreads
         integer iusw1_ppohFDM_pdiffz3_m4_nthreads

         integer iusw1_ppohFDM_ps_bef_nthreads
         integer iusw1_ppohFDM_ps_aft_nthreads
         integer iusw1_ppohFDM_pv_bef_nthreads
         integer iusw1_ppohFDM_pv_aft_nthreads

          common /OAT_ATswitchesNTh/iusw1_ppohFDMupdate_stress_select_nthreads, &
         &                      iusw1_ppohFDMupdate_vel_select_nthreads,    &
         &                      iusw1_ppohFDMupdate_stress_nthreads,        &
         &                      iusw1_ppohFDMupdate_sponge_nthreads,        &
         &                      iusw1_ppohFDMupdate_vel_nthreads,           &
         &                      iusw1_ppohFDM_update_vel_sponge_nthreads,   &
         &                      iusw1_ppohFDM_pdiffx3_p4_nthreads,          &
         &                      iusw1_ppohFDM_pdiffx3_m4_nthreads,          &
         &                      iusw1_ppohFDM_pdiffy3_p4_nthreads,          &
	 &                      iusw1_ppohFDM_pdiffy3_m4_nthreads,          &
         &                      iusw1_ppohFDM_pdiffz3_p4_nthreads,          &
         &                      iusw1_ppohFDM_pdiffz3_m4_nthreads,          &
         &                      iusw1_ppohFDM_ps_bef_nthreads,              &
         &                      iusw1_ppohFDM_ps_aft_nthreads,              &
         &                      iusw1_ppohFDM_pv_bef_nthreads,              &
         &                      iusw1_ppohFDM_pv_aft_nthreads                      


         integer iusw1_ppohFDMupdate_stress_select_flag
         integer iusw1_ppohFDMupdate_vel_select_flag

         integer iusw1_ppohFDMupdate_stress_flag
         integer iusw1_ppohFDMupdate_sponge_flag
         integer iusw1_ppohFDMupdate_vel_flag

         integer iusw1_ppohFDM_update_vel_sponge_flag
         integer iusw1_ppohFDM_pdiffx3_p4_flag
         integer iusw1_ppohFDM_pdiffx3_m4_flag
         integer iusw1_ppohFDM_pdiffy3_p4_flag
         integer iusw1_ppohFDM_pdiffy3_m4_flag
         integer iusw1_ppohFDM_pdiffz3_p4_flag
         integer iusw1_ppohFDM_pdiffz3_m4_flag

         integer iusw1_ppohFDM_ps_bef_flag
         integer iusw1_ppohFDM_ps_aft_flag
         integer iusw1_ppohFDM_pv_bef_flag
         integer iusw1_ppohFDM_pv_aft_flag


       
          common /OAT_ATswitchFlags/iusw1_ppohFDMupdate_stress_select_flag, &
         &                         iusw1_ppohFDMupdate_vel_select_flag,     &
         &                         iusw1_ppohFDMupdate_stress_flag,         &
         &                         iusw1_ppohFDMupdate_sponge_flag,         &
         &                         iusw1_ppohFDMupdate_vel_flag,            &
         &                         iusw1_ppohFDM_update_vel_sponge_flag,    &
         &                         iusw1_ppohFDM_pdiffx3_p4_flag,           &
         &                         iusw1_ppohFDM_pdiffx3_m4_flag,           &
         &                         iusw1_ppohFDM_pdiffy3_p4_flag,           &
         &                         iusw1_ppohFDM_pdiffy3_m4_flag,           &
         &                         iusw1_ppohFDM_pdiffz3_p4_flag,           &
	 &                         iusw1_ppohFDM_pdiffz3_m4_flag,           & 
	 &                         iusw1_ppohFDM_ps_bef_flag,               &
	 &                         iusw1_ppohFDM_ps_aft_flag,               &
	 &                         iusw1_ppohFDM_pv_bef_flag,               &
	 &                         iusw1_ppohFDM_pv_aft_flag



!      include 'mpif.h'
