!/*=====================================================================*
! *                                                                     *
! *   Software Name : ppohFEM                                           *
! *         Version : 1.0                                               *
! *                                                                     *
! *   License                                                           *
! *     This file is part of ppohFEM.                                   *
! *     ppohFEM is a free software, you can use it under the terms      *
! *     of The MIT License (MIT). See LICENSE file and User's guide     *
! *     for more details.                                               *
! *                                                                     *
! *   ppOpen-HPC project:                                               *
! *     Open Source Infrastructure for Development and Execution of     *
! *     Large-Scale Scientific Applications on Post-Peta-Scale          *
! *     Supercomputers with Automatic Tuning (AT).                      *
! *                                                                     *
! *   Organizations:                                                    *
! *     The University of Tokyo                                         *
! *       - Information Technology Center                               *
! *       - Atmosphere and Ocean Research Institute (AORI)              *
! *       - Interfaculty Initiative in Information Studies              *
! *         /Earthquake Research Institute (ERI)                        *
! *       - Graduate School of Frontier Science                         *
! *     Kyoto University                                                *
! *       - Academic Center for Computing and Media Studies             *
! *     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  *
! *                                                                     *
! *   Sponsorship:                                                      *
! *     Japan Science and Technology Agency (JST), Basic Research       *
! *     Programs: CREST, Development of System Software Technologies    *
! *     for post-Peta Scale High Performance Computing.                 *
! *                                                                     *
! *   Copyright (c) 2015 The University of Tokyo                        *
! *                       - Graduate School of Frontier Science         *
! *                                                                     *
! *=====================================================================*/


program app_heat

!use hecmw
use m_fstr
use m_ppohFEM2fstr_mesh_conv
use m_fstr_setup
use m_fstr_solve_heat
use m_heat_init
use m_heat_echo
use fstr_debug_dump

use ppohFEM

        implicit none
        type (hecmwST_local_mesh), pointer     :: hecMESH
        type (hecmwST_matrix ), pointer        :: hecMAT
        type (fstr_solid )                     :: fstrSOLID
        type (fstr_heat )                      :: fstrHEAT
        type (lczparam)                        :: fstrEIG
        type (fstr_dynamic )                   :: fstrDYNAMIC
        type ( hecmwST_result_data )           :: fstrRESULT
        type (fstr_couple )                    :: fstrCPL
        type (fstr_freqanalysis)               :: fstrFREQ
        character(len=HECMW_FILENAME_LEN)      :: name_ID

        real(kind=kreal) :: T1,T2,T3

        integer, parameter :: idx_mesh = 1
        integer, parameter :: idx_mat  = 1


        T1=0.0; T2=0.0; T3=0.0

        ! =============== INITIALIZE ===================


        call ppohFEM_init
        myrank = ppohFEM_comm_get_rank()
        nprocs = ppohFEM_comm_get_size()


        T1 = ppohFEM_Wtime()

        name_ID = 'fstrMSH'
        call ppohFEM_get_mesh( idx_mesh )


        paraContactFlag = .false.

        call ppohFEM_export_hecMESH(hecMESH, idx_mesh)
        call ppohFEM_export_hecMAT(hecMAT, idx_mat)

        call ppohFEM2fstr_mesh_conv( hecMESH )

        call fstr_init

        T2 = ppohFEM_Wtime()

        ! =============== ANALYSIS =====================


        call fstr_heat_analysis


        T3 = ppohFEM_Wtime()

!        if(hecMESH%my_rank==0) then
!          write(*,*)
!          write(*,*)           '===================================='
!          write(*,'(a,f10.2)') '    TOTAL TIME (sec) :', T3 - T1
!          write(*,'(a,f10.2)') '           pre (sec) :', T2 - T1
!          write(*,'(a,f10.2)') '         solve (sec) :', T3 - T2
!          write(*,*)           '===================================='
!        end if

        ! =============== FINALIZE =====================

        call fstr_finalize()
        call ppohFEM_finalize
        if(hecMESH%my_rank==0) write(*,*) 'app_heat Completed !!'

contains



!=============================================================================!
!> Initializer                                                                  !
!=============================================================================!

subroutine fstr_init
        implicit none

        integer, parameter :: idx_mesh = 1 ! index of mesh in ppohFEM module
        integer, parameter :: idx_mat = 1 ! index of hecMAT in ppohFEM module

        ! set pointer to null
        call ppohFEM_nullify_matrix( idx_mat )
        call ppohFEM_nullify_result_data( fstrRESULT )
        call fstr_nullify_fstr_param ( fstrPR     )
        call fstr_nullify_fstr_heat  ( fstrHEAT   )

        call fstr_init_file

		! ----  default setting of global params ---
        DT = 1
        ETIME = 1
        ITMAX = 20
        EPS = 1.0e-6

        ! -------  grobal pointer setting ----------
        REF_TEMP => fstrPR%ref_temp
        IECHO    => fstrPR%fg_echo
        IRESULT  => fstrPR%fg_result
        IVISUAL  => fstrPR%fg_visual

        ! for heat ...
        INEUTRAL => fstrPR%fg_neutral
        IRRES    => fstrPR%fg_irres
        IWRES    => fstrPR%fg_iwres
        NRRES    => fstrPR%nrres
        NPRINT   => fstrPR%nprint

        call ppohFEM_mat_con(idx_mesh, idx_mat)

        ! ------- initial value setting -------------
        call fstr_mat_init  ( hecMAT   )
        call fstr_param_init( fstrPR, hecMESH )

        call fstr_heat_init ( fstrHEAT  )

        call fstr_init_condition

        if( kstHEAT == fstrPR%solution_type ) then
          call heat_init_material (hecMESH,fstrHEAT)
          call heat_init_amplitude(hecMESH,fstrHEAT)
          call ppohFEM_set_mat_ndof(idx_mat, 1) ! set number of degree of freedom as 1 (temperture)
        endif
        call ppohFEM_mat_init( idx_mat )

end subroutine fstr_init

!------------------------------------------------------------------------------
!> Open all files preparing calculation
subroutine fstr_init_file
        implicit none
        character(len=HECMW_FILENAME_LEN) :: s, r
        character(len=HECMW_FILENAME_LEN) :: stafileNAME
        character(len=HECMW_FILENAME_LEN) :: logfileNAME
        character(len=HECMW_FILENAME_LEN) :: msgfileNAME
        character(len=HECMW_FILENAME_LEN) :: dbgfileNAME
        integer :: stat, flag, limit, irank

        ! set file name --------------------------------
        call hecmw_ctrl_is_subdir( flag, limit )
        write(s,*) myrank
        if( flag == 0 ) then
          write( logfileNAME, '(a,a)') trim(adjustl(s)), '.log'
          logfileNAME = adjustl(logfileNAME)
          write( dbgfileNAME, '(a,a)') 'heat.dbg.', trim(adjustl(s))
          dbgfileNAME = adjustl(dbgfileNAME)
        else
          if( nprocs > limit ) then
            irank = myrank / limit
            write(r,*) irank
            write( logfileNAME, '(a,a,a,a,a)') 'LOG/TRUNK', trim(adjustl(r)), '/', trim(adjustl(s)), '.log'
            logfileNAME = adjustl(logfileNAME)
            call hecmw_ctrl_make_subdir( logfileNAME, stat )
            if( stat /= 0 ) call fstr_setup_util_err_stop( '### Cannot create directory' )
            write( dbgfileNAME, '(a,a,a,a,a)') 'DBG/TRUNK', trim(adjustl(r)), '/', 'heat.dbg.', trim(adjustl(s))
            dbgfileNAME = adjustl(dbgfileNAME)
            call hecmw_ctrl_make_subdir( dbgfileNAME, stat )
            if( stat /= 0 ) call fstr_setup_util_err_stop( '### Cannot create directory' )
          else
            write( logfileNAME, '(a,a,a)') 'LOG/', trim(adjustl(s)), '.log'
            logfileNAME = adjustl(logfileNAME)
            call hecmw_ctrl_make_subdir( logfileNAME, stat )
            if( stat /= 0 ) call fstr_setup_util_err_stop( '### Cannot create directory' )
            write( dbgfileNAME, '(a,a,a)') 'DBG/', 'heat.dbg.', trim(adjustl(s))
            dbgfileNAME = adjustl(dbgfileNAME)
            call hecmw_ctrl_make_subdir( dbgfileNAME, stat )
            if( stat /= 0 ) call fstr_setup_util_err_stop( '### Cannot create directory' )
          end if
        end if
        stafileNAME = 'heat.sta'
        msgfileNAME = 'heat.msg'

        ! open & opening message out -------------------
        ! MSGFILE
        if( myrank == 0) then
                open(IMSG, file=msgfileNAME, status='replace', iostat=stat)
                if( stat /= 0 ) then
                        call fstr_setup_util_err_stop( '### Cannot open message file :'//msgfileNAME )
                endif
                write(IMSG,*) ':========================================:'
                write(IMSG,*) ':**   BEGIN app_heat                   **:'
                write(IMSG,*) ':========================================:'
                write(IMSG,*) '        Total no. of processors: ',nprocs
                write(IMSG,*)
                write(IMSG,*)
                write(IMSG,*)
                write(IMSG,*) ' *    STAGE Initialization and input   **'
        end if

        ! LOGFILE & STAFILE
        open (ILOG, file = logfileNAME, status = 'replace', iostat=stat )
        if( stat /= 0 ) then
                call fstr_setup_util_err_stop( '### Cannot open log file :'//logfileNAME )
        endif

        if( myrank == 0 ) then
                open (ISTA,file = stafileNAME, status = 'replace', iostat=stat )
                write(ISTA,'(''####''a80)') stafileNAME
                if( stat /= 0 ) then
                        call fstr_setup_util_err_stop( '### Cannot open status file :'//stafileNAME )
                endif
        endif

        open (IDBG,file = dbgfileNAME, status = 'replace')
        write(IDBG,'(''####''a80)') dbgfileNAME
        if( stat /= 0 ) then
                call fstr_setup_util_err_stop( '### Cannot open debug file :'//dbgfileNAME )
        endif

end subroutine fstr_init_file

!------------------------------------------------------------------------------
!> Read in control file and do all preparation
subroutine fstr_init_condition
        implicit none
        character(len=HECMW_FILENAME_LEN) :: cntfileNAME

        ! get fstr control & setup paramters -----------
        name_ID='fstrCNT'
        call ppohFEM_ctrl_get_control_file( name_ID, cntfileNAME )

        ! loading boundary conditions etc. from fstr control file or nastran mesh file
        ! and setup parameters ...
        svRarray(:) = hecMAT%Rarray(:)
        svIarray(:) = hecMAT%Iarray(:)
        call fstr_setup( cntfileNAME, hecMESH, fstrPR, fstrSOLID,   &
                           fstrEIG, fstrHEAT, fstrDYNAMIC, fstrCPL, fstrFREQ )
        hecMAT%Rarray(:) = svRarray(:)
        hecMAT%Iarray(:) = svIarray(:)

        write(*,*) 'app_heat: setup OK'; call flush(6)

        ! Timing and memory monitor initializations
         minit = .TRUE.
         tinit = .TRUE.
         tenditer = .FALSE.

         !call time_log( hecMESH, hecMAT, fstrEIG )
         !call memory_log( hecMESH, hecMAT, fstrEIG )

end subroutine fstr_init_condition


!=============================================================================!
!> Master subroutine of heat analysis                                         !
!=============================================================================!

subroutine fstr_heat_analysis

        if( IECHO.eq.1 ) call heat_echo(fstrPR,hecMESH,fstrHEAT)
        if(myrank .EQ. 0) THEN
                write(IMSG,*)
                write(IMSG,*)
                write(IMSG,*)
                write(IMSG,*) ' ***   STAGE Heat analysis    **'
        end if

        call fstr_solve_HEAT( hecMESH, hecMAT, fstrRESULT, fstrPR, fstrHEAT )

end subroutine fstr_heat_analysis


!=============================================================================!
!> Finalizer                                                                  !
!=============================================================================!

subroutine fstr_finalize

        close(ILOG)

        if( myrank==0 ) then
                close(ISTA)
        end if
        if( myrank .EQ. 0 ) then
                write(IMSG,*)
                write(IMSG,*)
                write(IMSG,*)
                write(IMSG,*) ':========================================:'
                write(IMSG,*) ':**            END app_heat            **:'
                write(IMSG,*) ':========================================:'
                close(IMSG)

        end if

        call fstr_solid_finalize( fstrSOLID )
        call ppohFEM_hecMAT_finalize( hecMAT )

        close(IDBG)

end subroutine fstr_finalize

!=============================================================================!
end program app_heat
