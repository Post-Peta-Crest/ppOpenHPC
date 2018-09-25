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


module m_hecmw_solve_error
      use hecmw_util

      integer(kind=kint), parameter :: &
           HECMW_SOLVER_ERROR_INCONS_PC = 1001, &
           HECMW_SOLVER_ERROR_ZERO_DIAG = 2001, &
           HECMW_SOLVER_ERROR_ZERO_RHS = 2002, &
           HECMW_SOLVER_ERROR_NOCONV_MAXIT = 3001, &
           HECMW_SOLVER_ERROR_DIVERGE_MAT = 3002, &
           HECMW_SOLVER_ERROR_DIVERGE_PC = 3003

contains

      subroutine hecmw_solve_error (hecMESH, IFLAG)
      use hecmw_util
      implicit none
      type (hecmwST_local_mesh) :: hecMESH
      integer(kind=kint) :: IFLAG

      if ( hecMESH%zero.eq.1 .and. &
           (IFLAG.eq.HECMW_SOLVER_ERROR_INCONS_PC .or. IFLAG.eq.HECMW_SOLVER_ERROR_ZERO_DIAG) ) then
        write (*,'(/a )')                                               &
     &           '###############################################'
        write (*,'( a )')                                               &
     &           '######## ERROR MESSAGE : LINEAR SOLVER ########'
        write (*,'( a/)')                                               &
     &           '###############################################'
      endif

      if (IFLAG.eq.HECMW_SOLVER_ERROR_INCONS_PC) then
        if (hecMESH%zero.eq.1) then
          write (*,'(/a )')'  #### HEC-MW-SOLVER-E-1001'
          write (*,'( a/)')'    inconsistent solver/preconditioning'
        endif
!        call MPI_ABORT (hecMESH%MPI_COMM, ierr)
        call hecmw_abort( hecmw_comm_get_comm())
      endif

      if (IFLAG.eq.HECMW_SOLVER_ERROR_ZERO_DIAG) then
        if (hecMESH%zero.eq.1) then
          write (*,'(/a )')'  #### HEC-MW-SOLVER-E-2001: '
          write (*,'( a/)')'    ZERO component in diagonal block'
        endif
!        call MPI_ABORT (hecMESH%MPI_COMM, ierr)
        call hecmw_abort( hecmw_comm_get_comm())
      endif

      if (IFLAG.eq.HECMW_SOLVER_ERROR_ZERO_RHS) then
        if (hecMESH%zero.eq.1) then
          write (*,'(/a )')'  #### HEC-MW-SOLVER-W-2002: '
          write (*,'( a/)')'    ZERO RHS norm'
        endif
      endif

      if (IFLAG.eq.HECMW_SOLVER_ERROR_NOCONV_MAXIT) then
        if (hecMESH%zero.eq.1) then
          write (*,'(/a )')'  #### HEC-MW-SOLVER-W-3001: '
          write (*,'( a/)')'    not converged within ceratin iterations'
        endif
      endif

      if (IFLAG.eq.HECMW_SOLVER_ERROR_DIVERGE_MAT) then
        if (hecMESH%zero.eq.1) then
          write (*,'(/a )')'  #### HEC-MW-SOLVER-W-3002: '
          write (*,'( a/)')'    diverged due to indefinite or negative definite matrix'
        endif
      endif

      if (IFLAG.eq.HECMW_SOLVER_ERROR_DIVERGE_PC) then
        if (hecMESH%zero.eq.1) then
          write (*,'(/a )')'  #### HEC-MW-SOLVER-W-3003: '
          write (*,'( a/)')'    diverged due to indefinite preconditioner'
        endif
      endif

      !stop " PROGRAM STOP:"

      end subroutine hecmw_solve_error

end module m_hecmw_solve_error

