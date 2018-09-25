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


!> SOLVE STATIC LINEAR SOLID MECHANICS
module m_fstr_solve_LINEAR
   contains

   subroutine FSTR_SOLVE_LINEAR ( hecMESH,hecMAT,myEIG,fstrSOLID,fstrPARAM )

      use m_fstr
      use m_static_mat_ass
      use hecmw_solver_11
      use hecmw_solver_22
      use hecmw_solver_33
!      use hecmw_solver_direct
      use m_solve_lineq
      use m_fstr_Update
      use m_static_output
      use m_ppohFEM2fstr_mesh_conv

      implicit REAL(kind=kreal) (A-H,O-Z)
      type ( hecmwST_local_mesh  ) :: hecMESH
      type ( hecmwST_matrix      ) :: hecMAT
      type ( lczparam            ) :: myEIG
      type ( fstr_solid          ) :: fstrSOLID
      type ( hecmwST_result_data ) :: fstrRESULT
      type ( fstr_param          ) :: fstrPARAM

      integer(kind=kint) :: i

      integer, parameter :: idx_mesh = 1 ! index of mesh 
      integer, parameter :: idx_mat  = 1 ! index of stiffness matrix

      if( fstrSOLID%TEMP_ngrp_tot>0 .and. hecMESH%hecmw_flag_initcon==1 ) then
        do j=1, hecMESH%n_node
          fstrSOLID%last_temp(j) = hecMESH%node_init_val_item(j)
          fstrSOLID%temperature(j) = hecMESH%node_init_val_item(j)
        end do
      endif
!C
!C-- MATRIX ASSEMBLING
!C
      call fstr_mat_ass(idx_mesh, idx_mat, myEIG, fstrSOLID)

      IF(myrank .EQ. 0) THEN
        WRITE(IMSG,*)
        WRITE(IMSG,*)
        WRITE(IMSG,*) ' *****  STAGE Solve static problem    **'
        call flush(IDBG)
        call flush(IMSG)
      ENDIF
      call flush(ILOG)
!C
!C-- LINEAR SOLVER
!C

!      hecMAT%Iarray(98) = 1   !Assmebly complete
!      hecMAT%Iarray(97) = 1   !Need numerical factorization
      CALL solve_LINEQ(idx_mesh,idx_mat,imsg)

      IF(myrank .EQ. 0) THEN
        IF(ppohFEM_is_iterative_solver(idx_mat)) THEN
          write(IMSG,*) '*----------------------------*'
          write(IMSG,*) '## No.of ITER:',hecMAT%Iarray(1)
          write(IMSG,*) '*----------------------------*'
          call flush(IMSG)
        ENDIF
      ENDIF

!C
!C-- UPDATE DISPLACEMENT, STRAIN, STRESS
!C
      do i = 1, ppohFEM_get_n_node(idx_mesh)*ppohFEM_get_mat_ndof(idx_mat)
        fstrSOLID%unode(i) = ppohFEM_get_stiffMAT_X_direct_index(idx_mat, i)
      enddo
      if( ppohFEM_get_mat_ndof(idx_mat)==3 ) then
        call ppohFEM_update_3_R ( idx_mesh, fstrSOLID%unode, ppohFEM_get_np(idx_mat) )
        call fstr_Update3D ( idx_mesh, fstrSOLID )
      else if( ppohFEM_get_mat_ndof(idx_mat)==2 ) then
!write(*,*) 'ndof=2 update is not supported'
!call ppohFEM_abort
        call ppohFEM_update_2_R ( idx_mesh, fstrSOLID%unode, ppohFEM_get_np(idx_mat) ) !DEBUG later
        call fstr_Update2D ( hecMESH, fstrSOLID )                                      !DEBUG later
      else if( ppohFEM_get_mat_ndof(idx_mat)==6) THEN
!write(*,*) 'ndof=6 update is not supported'
!call ppohFEM_abort
        call ppohFEM_update_6_R ( idx_mesh, fstrSOLID%unode, ppohFEM_get_np(idx_mat) ) !DEBUG later
        call fstr_Update6D ( hecMESH, fstrSOLID )                                      !DEBUG later
      endif

      IF(myrank .EQ. 0) THEN
        WRITE(IMSG,*)
        WRITE(IMSG,*) ' *     STAGE Output and postprocessing    **'
        call flush(IDBG)
        call flush(IMSG)
      ENDIF
      call flush(ILOG)
!C
!C-- POST PROCESSING
!C
      call fstr_static_Output( 1, 1, hecMESH, fstrSOLID, fstrPR%solution_type )

      end subroutine FSTR_SOLVE_LINEAR
end module m_fstr_solve_LINEAR
