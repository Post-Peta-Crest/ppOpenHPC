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

module m_heat_mat_ass_bc_FIXT
   contains
!C
!C***
!C*** MAT_ASS_BC_FIXT
!C***
!C
   subroutine heat_mat_ass_bc_FIXT( hecMAT, fstrHEAT, CTIME )

      use m_fstr
      use m_heat_get_amplitude

      implicit none
      integer(kind=kint) ib,ii,id
      real(kind=kreal)   CTIME,QQ
      type (fstr_heat         ) :: fstrHEAT
      type (hecmwST_matrix    ) :: hecMAT
      logical :: OutOfRange

      do ib= 1, fstrHEAT%T_FIX_tot
        ii= fstrHEAT%T_FIX_node(ib)

        ID = fstrHEAT%T_FIX_ampl(ib)
        call heat_get_amplitude( fstrHEAT, ID, CTIME, QQ, OutOfRange )

        if (OutOfRange) cycle

        call hecmw_mat_ass_bc(hecMAT, ii, 1, fstrHEAT%T_FIX_VAL(ib) * QQ)

      enddo

      return

   end subroutine heat_mat_ass_bc_FIXT
end module m_heat_mat_ass_bc_FIXT
