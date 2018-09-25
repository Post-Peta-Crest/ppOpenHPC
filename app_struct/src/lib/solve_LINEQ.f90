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

module m_solve_LINEQ
   implicit none

   private
   public :: solve_LINEQ

   contains

   SUBROUTINE solve_LINEQ(idx_mesh, idx_mat, imsg)
      USE hecmw
      USE hecmw_solver_11
      USE hecmw_solver_22
      USE hecmw_solver_33
      USE hecmw_solver_44
      USE hecmw_solver_66
!      USE hecmw_solver_direct
!      USE hecmw_solver_direct_parallel
!      USE hecmw_solver_direct_MUMPS

      use ppohFEM

      integer(kind=kint), intent(IN) :: idx_mesh
      integer(kind=kint), intent(IN) :: idx_mat
      INTEGER(kind=kint) imsg, i, myrank
      real(kind=kreal) :: resid

 !     type (hecmwST_local_mesh), pointer :: hecMESH
 !     type (hecmwST_matrix    ), pointer :: hecMAT
!
!      call ppohFEM_export_hecMESH(hecMESH, idx_mesh)
!      call ppohFEM_export_hecMAT(hecMAT, idx_mat)

!C
!! iterative solver only
!      SELECT CASE(hecMAT%Iarray(99))
!C
!* Call Iterative Solver
!      CASE (1)
!C
        SELECT CASE(ppohFEM_get_mat_ndof(idx_mat))
        CASE(1)
!          WRITE(*,*) "Calling 1x1 Iterative Solver..."
          CALL ppohFEM_solve_11(idx_mesh,idx_mat)
        CASE(2)
!          WRITE(*,*) "Calling 2x2 Iterative Solver..."
          CALL ppohFEM_solve_22(idx_mesh,idx_mat)
        CASE(3)
!          WRITE(*,*) "Calling 3x3 Iterative Solver..."
          CALL ppohFEM_solve_33(idx_mesh,idx_mat)
        CASE(4)
!          WRITE(*,*) "Calling 4x4 Iterative Solver..."
          CALL ppohFEM_solve_44(idx_mesh,idx_mat)
        CASE(5)
          !CALL hecmw_solve_mm(hecMESH,hecMAT)
!          WRITE(*,*) "FATAL: Solve_mm not yet available..."
          call ppohFEM_abort()
        CASE(6)
!          WRITE(*,*) "Calling 6x6 Iterative Solver..."
          CALL ppohFEM_solve_66(idx_mesh,idx_mat)
        CASE(7:)
          !CALL hecmw_solve_mm(hecMESH,hecMAT)
!          WRITE(*,*) "FATAL: Solve_mm not yet available..."
          call ppohFEM_abort()
        END SELECT
! direct solver is not supported in ppohFEM
!!C
!!* Call Direct Solver
!      CASE(2:)
!!C
!!* Please note the following:
!!* Flag to activate symbolic factorization: 1(yes) 0(no)  hecMESH%Iarray(98)
!!* Flag to activate numeric  factorization: 1(yes) 0(no)  hecMESH%Iarray(97)
!
!        if (hecMAT%Iarray(2) .eq. 104) then
!          call hecmw_solve_direct_MUMPS(hecMESH, hecMAT)
!        else
!          IF(hecMESH%PETOT.GT.1) THEN
!            CALL hecmw_solve_direct_parallel(hecMESH,hecMAT,imsg)
!          ELSE
!            CALL hecmw_solve_direct(hecMESH,hecMAT,imsg)
!          ENDIF
!!!!       hecMAT%X = hecMAT%B -- leading stack overflow (intel9)
!          do i=1,hecMAT%NP*hecMESH%n_dof
!              hecMAT%X(i) = hecMAT%B(i)
!          end do
!        endif
!
!        SELECT CASE(hecMESH%n_dof)
!        CASE(1)
!          resid=hecmw_rel_resid_L2_11(hecMESH,hecMAT)
!        CASE(2)
!          resid=hecmw_rel_resid_L2_22(hecMESH,hecMAT)
!        CASE(3)
!          resid=hecmw_rel_resid_L2_33(hecMESH,hecMAT)
!        CASE(4:)
!          !resid=hecmw_rel_resid_L2_mm(hecMESH,hecMAT)
!          resid=0.d0 !! TEMPORARY
!        END SELECT
!        myrank=hecmw_comm_get_rank()
!        if (myrank==0) then
!          write(*,"(a,1pe12.5)")'### Relative residual =', resid
!          if( resid >= 1.0d-8) then
!            write(*,"(a)")'### Relative residual exceeded 1.0d-8---Direct Solver### '
!!            stop
!          endif
!        endif
!!C
!      END SELECT
!C
      RETURN

   end subroutine solve_LINEQ

end module m_solve_LINEQ
