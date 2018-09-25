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


!> This subroutine calculates elastoplastic constitutive relation
subroutine uElastoPlasticMatrix( matl, stress, istat, fstat, D  )
     use hecmw
     implicit none
     REAL(KIND=kreal), INTENT(IN)  :: matl(:)   !< material properties
     REAL(KIND=kreal), INTENT(IN)  :: stress(6) !< stress
     INTEGER, INTENT(IN)           :: istat     !< plastic state
     REAL(KIND=kreal), INTENT(IN)  :: fstat(:)  !< plastic strain, back stress
     REAL(KIND=kreal), INTENT(OUT) :: D(:,:)    !< strain-stress relation

end subroutine

!> This subroutine does backward-Euler return calculation
subroutine uBackwardEuler( matl, stress, istat, fstat )
      use hecmw
      implicit none
      REAL(KIND=kreal), intent(in)    :: matl       !< material properties
      real(kind=kreal), intent(inout) :: stress(6)  !< stress
      integer, intent(inout)          :: istat      !< plastic state
      real(kind=kreal), intent(inout) :: fstat(:)   !< plastic strain, back stress
end subroutine
