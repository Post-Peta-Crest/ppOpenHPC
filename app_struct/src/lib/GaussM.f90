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


!-----------------------------------------------------------------------*
!     Definitions of module
!-----------------------------------------------------------------------*
      module gauss_integration
      use hecmw
      implicit none
      REAL(kind=kreal) XG(3,3)     !< abscissa of gauss points
      REAL(kind=kreal) WGT(3,3)    !< wieght of gauss points
!****************************
!* Gauss Integration Table **
!****************************
!** 1st ***
      data XG (1,1)/0.0/
      data WGT(1,1)/2.0/
!** 2nd ***
      data XG(2,1),XG(2,2)/-0.577350269189626,0.577350269189626/
      data WGT(2,1),WGT(2,2)/1.0,1.0/
!** 3rd ***
      data XG(3,1),XG(3,2),XG(3,3)/  &
      -0.7745966692,                 &
      0.0,                           &
      0.7745966692/
      data WGT(3,1),WGT(3,2),WGT(3,3)/ &
      0.5555555555,                    &
      0.8888888888,                    &
      0.5555555555/
! end of this module
      end module gauss_integration
