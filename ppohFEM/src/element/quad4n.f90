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


MODULE shape_quad4n
  integer, parameter, private :: kreal = kind(0.0d0)

  contains
    subroutine ShapeFunc_quad4n(lcoord,func)
      real(kind=kreal), intent(in) :: lcoord(2)
      real(kind=kreal) :: func(4)
      func(1) = 0.25d0*(1.d0-lcoord(1))*(1.d0-lcoord(2))
      func(2) = 0.25d0*(1.d0+lcoord(1))*(1.d0-lcoord(2))
      func(3) = 0.25d0*(1.d0+lcoord(1))*(1.d0+lcoord(2))
      func(4) = 0.25d0*(1.d0-lcoord(1))*(1.d0+lcoord(2))
    end subroutine

    subroutine ShapeDeriv_quad4n(lcoord,func)
      real(kind=kreal), intent(in) :: lcoord(2)
      real(kind=kreal) :: func(4,2)
      func(1,1) = -0.25d0*(1.d0-lcoord(2))
      func(2,1) =  0.25d0*(1.d0-lcoord(2))
      func(3,1) =  0.25d0*(1.d0+lcoord(2))
      func(4,1) = -0.25d0*(1.d0+lcoord(2))

      func(1,2) = -0.25d0*(1.d0-lcoord(1))
      func(2,2) = -0.25d0*(1.d0+lcoord(1))
      func(3,2) =  0.25d0*(1.d0+lcoord(1))
      func(4,2) =  0.25d0*(1.d0-lcoord(1))
    end subroutine

    subroutine Shape2ndDeriv_quad4n(func)
      real(kind=kreal) :: func(4,2,2)
      func(:,1,1) = 0.d0
      func(1,1,2) = 0.25d0
      func(2,1,2) = -0.25d0
      func(3,1,2) = 0.25d0
      func(4,1,2) = -0.25d0

      func(1,2,1) = 0.25d0
      func(2,2,1) = -0.25d0
      func(3,2,1) = 0.25d0
      func(4,2,1) = -0.25d0
      func(:,2,2) = 0.d0
    end subroutine


      ! (Gaku Hashimoto, The University of Tokyo, 2012/11/15) <
!####################################################################
      SUBROUTINE NodalNaturalCoord_quad4n(nncoord)
!####################################################################

      IMPLICIT NONE

!--------------------------------------------------------------------

      REAL(KIND = kreal), INTENT(OUT) :: nncoord(4, 2)

!--------------------------------------------------------------------

      ! xi-coordinate at a node in a local element
      nncoord(1, 1) = -1.0D0
      nncoord(2, 1) =  1.0D0
      nncoord(3, 1) =  1.0D0
      nncoord(4, 1) = -1.0D0
      ! eta-coordinate at a node in a local element
      nncoord(1, 2) = -1.0D0
      nncoord(2, 2) = -1.0D0
      nncoord(3, 2) =  1.0D0
      nncoord(4, 2) =  1.0D0

!--------------------------------------------------------------------

      RETURN

!####################################################################
      END SUBROUTINE NodalNaturalCoord_quad4n
!####################################################################
      ! > (Gaku Hashimoto, The University of Tokyo, 2012/11/15)


END MODULE
