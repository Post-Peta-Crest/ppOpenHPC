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


!> This subroutine calculate isotropic hardening tangent
function uhardening( matl, pstrain )
    use hecmw
    implicit none
    real( KIND=kreal ), INTENT(IN) :: matl(:) !< material proerties
    real( KIND=kreal ), INTENT(IN) :: pstrain !< plastic strain
	real(kind=kreal) :: uhardening

    uhardening = 0.d0
end function

!> This subroutine calculate kinematic hardening tangent
function ukhardening( matl, pstrain )
    use hecmw
    implicit none
    real( KIND=kreal ), INTENT(IN) :: matl(:) !< material proerties
    real( KIND=kreal ), INTENT(IN) :: pstrain !< plastic strain
	real(kind=kreal) :: ukhardening

    ukhardening = 0.d0
end function

!> This subroutine calculate current yield value
function uCurrYield( matl, pstrain )
    use hecmw
    implicit none
    real( KIND=kreal ), INTENT(IN) :: matl(:) !< material proerties
    real( KIND=kreal ), INTENT(IN) :: pstrain !< plastic strain
	real(kind=kreal) :: uCurrYield

    uCurrYield = 0.d0
end function
