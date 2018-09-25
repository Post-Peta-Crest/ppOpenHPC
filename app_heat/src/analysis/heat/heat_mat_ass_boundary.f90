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

module m_heat_mat_ass_boundary
   contains
!C***
!C*** MAT_ASS_BOUNDARY
!C***
   subroutine heat_mat_ass_boundary ( hecMESH,hecMAT,fstrHEAT,ATIME, BTIME, DTIME )

      use m_fstr
      use m_heat_mat_ass_bc_CFLUX
      use m_heat_mat_ass_bc_DFLUX
      use m_heat_mat_ass_bc_FIXT
      use m_heat_mat_ass_bc_FILM
      use m_heat_mat_ass_bc_RADIATE

      implicit none
      real(kind=kreal)   ATIME,BTIME,CTIME, DTIME
      type (fstr_heat         ) :: fstrHEAT
      type (hecmwST_matrix    ) :: hecMAT
      type (hecmwST_local_mesh) :: hecMESH

      CTIME = ATIME + BTIME

!C
!C +---------+
!C | !CFLUX  |
!C +---------+
!C===
      call heat_mat_ass_bc_CFLUX ( hecMAT, fstrHEAT, CTIME )
!C
!C +---------+
!C | !DFLUX  |
!C +---------+
!C===
      call heat_mat_ass_bc_DFLUX ( hecMESH, hecMAT, fstrHEAT, CTIME, DTIME )
!C
!C +--------+
!C | !FILM  |
!C +--------+
!C===
      call heat_mat_ass_bc_FILM ( hecMESH, hecMAT, fstrHEAT, CTIME )
!C
!C +-----------+
!C | !RADIATE  |
!C +-----------+
!C===
      call heat_mat_ass_bc_RADIATE ( hecMESH, hecMAT, fstrHEAT, CTIME )
!C
!C +------------+
!C | !BOUNDARY  |
!C +------------+
!C===
      call heat_mat_ass_bc_FIXT ( hecMAT, fstrHEAT, CTIME )
      return

   end subroutine heat_mat_ass_boundary
end module m_heat_mat_ass_boundary
