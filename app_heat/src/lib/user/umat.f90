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

module mUmat
implicit none

INTEGER, PARAMETER, PRIVATE :: kreal = kind(0.0d0)

contains

!> This subroutine calculates constitutive matrix
subroutine uMatlMatrix( mname, matl, strain, stress, fstat, D  &
    , dtime, ttime, temperature )
     CHARACTER(len=*), INTENT(IN)  :: mname     !< material name
     REAL(KIND=kreal), INTENT(IN)  :: matl(:)   !< material properties
     real(kind=kreal), intent(in)  :: strain(6) !< Green-Lagrangen strain
     REAL(KIND=kreal), INTENT(IN)  :: stress(6) !< 2nd Piola-Kirchhiff stress tensor
     REAL(KIND=kreal), INTENT(IN)  :: fstat(:)  !< state variables
     REAL(KIND=kreal), INTENT(OUT) :: D(:,:)    !< strain-stress relation
     REAL(KIND=kreal), INTENT(IN)  :: dtime     !< time increment
     REAL(KIND=kreal), INTENT(IN)  :: ttime     !< total time at the start of the current increment
	 REAL(KIND=kreal), optional    :: temperature !< temprature

end subroutine

!> This subroutine calculate strain and stress increment
subroutine uUpdate(  mname, matl, strain, stress, fstat, dtime, ttime, temperature )
      character(len=*), intent(in)    :: mname      !< material name
      real(KIND=kreal), intent(in)    :: matl(:)    !< material properties
      real(kind=kreal), intent(in)    :: strain(6)  !< strain
      real(kind=kreal), intent(inout) :: stress(6)  !< 2nd Piola-Kirchhiff stress tensor
      real(kind=kreal), intent(inout) :: fstat(:)   !< state variables
      REAL(KIND=kreal), INTENT(IN)    :: dtime     !< time increment
      REAL(KIND=kreal), INTENT(IN)    :: ttime     !< total time at the start of the current increment
      real(KIND=kreal), optional      :: temperature !< temperature

end subroutine

end module
