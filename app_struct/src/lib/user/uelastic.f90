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

module mUElastic

implicit none

contains

!> This subroutine calculates constitutive relation
subroutine uElasticMatrix( matl, strain, D )
    use hecmw
    real(kind=kreal), intent(in)  :: matl(:)   !< material properties
    real(kind=kreal), intent(in)  :: strain(6) !< Green-Lagrangen strain
    real(kind=kreal), intent(out) :: D(6,6)    !< constitutive matrix

! following examples of linear elasticicty
    real(kind=kreal) :: EE, PP

    D(:,:)=0.d0

    EE = matl(1)
    PP = matl(2)
    D(1,1)=EE*(1.d0-PP)/(1.d0-2.d0*PP)/(1.d0+PP)
    D(1,2)=EE*PP/(1.d0-2.d0*PP)/(1.d0+PP)
    D(1,3)=D(1,2)
    D(2,1)=D(1,2)
    D(2,2)=D(1,1)
    D(2,3)=D(1,2)
    D(3,1)=D(1,3)
    D(3,2)=D(2,3)
    D(3,3)=D(1,1)
    D(4,4)=EE/(1.d0+PP)*0.5d0
    D(5,5)=EE/(1.d0+PP)*0.5d0
    D(6,6)=EE/(1.d0+PP)*0.5d0
end subroutine

!> This subroutine calculate updated strain and stress
subroutine uElasticUpdate( matl, strain, stress )
    use hecmw
    real(kind=kreal), intent(in)  :: matl(:)   !< material properties
	real(kind=kreal), intent(in)  :: strain(6) !< strain
    real(kind=kreal), intent(out) :: stress(6) !< stress

! following examples of linear elasticicty
    real(kind=kreal) :: D(6,6)
	call uElasticMatrix( matl(:), strain, D )
	stress = matmul( D, strain )
end subroutine

end module

