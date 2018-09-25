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


module shape_tri3n
  integer, parameter, private :: kreal = kind(0.0d0)

  contains
    subroutine ShapeFunc_tri3n(areacoord,func)
      real(kind=kreal), intent(in) :: areacoord(2)
      real(kind=kreal) :: func(3)
      func(1:2) = areacoord(1:2)
      func(3)   = 1.d0-areacoord(1)-areacoord(2)
    end subroutine

    subroutine ShapeDeriv_tri3n(func)
      real(kind=kreal) :: func(3,2)
      func(1,1) = 1.d0
      func(2,1) = 0.d0
      func(3,1) = -1.d0

      func(1,2) = 0.d0
      func(2,2) = 1.d0
      func(3,2) = -1.d0
    end subroutine

    subroutine Shape2ndDeriv_tri3n(func)
      real(kind=kreal) :: func(3,2,2)
      func(:,:,:) = 0.d0
    end subroutine


      ! (Gaku Hashimoto, The University of Tokyo, 2012/11/15) <
!####################################################################
      SUBROUTINE NodalNaturalCoord_tri3n(nncoord)
!####################################################################

      IMPLICIT NONE

!--------------------------------------------------------------------

      REAL(KIND = kreal), INTENT(OUT) :: nncoord(3, 2)

!--------------------------------------------------------------------

      ! xi-coordinate at a node in a local element
      nncoord(1, 1) =  1.0D0
      nncoord(2, 1) =  0.0D0
      nncoord(3, 1) =  0.0D0
      ! eta-coordinate at a node in a local element
      nncoord(1, 2) =  0.0D0
      nncoord(2, 2) =  1.0D0
      nncoord(3, 2) =  0.0D0

!--------------------------------------------------------------------

      RETURN

!####################################################################
      END SUBROUTINE NodalNaturalCoord_tri3n
!####################################################################
      ! > (Gaku Hashimoto, The University of Tokyo, 2012/11/15)


end module
