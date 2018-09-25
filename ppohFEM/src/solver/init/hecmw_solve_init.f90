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


!
!C***
!C*** hecmw_solve_init
!C***
!

module m_hecmw_solve_init

contains

      subroutine hecmw_solve_init (hecMAT)
      use hecmw_util
      implicit REAL*8 (A-H,O-Z)
      type (hecmwST_matrix)     :: hecMAT

      hecMAT%Iarray= 0
      hecMAT%Rarray= 0.d0

      hecMAT%Iarray(1)= 100
      hecMAT%Iarray(6)=  10

      hecMAT%Rarray(1)= 1.d-8
      hecMAT%Rarray(2)= 1.d0
      hecMAT%Rarray(3)= 0.d0
      hecMAT%Rarray(4)= 0.10d0
      hecMAT%Rarray(5)= 0.10d0

      end subroutine hecmw_solve_init

end module m_hecmw_solve_init


