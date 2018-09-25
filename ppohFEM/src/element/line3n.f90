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


module shape_line3n
  integer, parameter, private :: kreal = kind(0.0d0)

  contains
    subroutine ShapeFunc_line3n(lcoord,func)
      real(kind=kreal), intent(in) :: lcoord(1)
      real(kind=kreal) :: func(3)
      func(1) =-0.5d0*(1.d0-lcoord(1))*lcoord(1)
      func(2) = 0.5d0*(1.d0+lcoord(1))*lcoord(1)
      func(3) = 1.0d0-lcoord(1)*lcoord(1)
    end subroutine

    subroutine ShapeDeriv_line3n(lcoord,func)
      real(kind=kreal), intent(in) :: lcoord(1)
      real(kind=kreal) :: func(3,1)
      func(1,1) = lcoord(1)-0.5d0
      func(2,1) = lcoord(1)+0.5d0
      func(3,1) =-2.d0*lcoord(1)
    end subroutine

    subroutine Shape2ndDeriv_line3n(func)
      real(kind=kreal) :: func(3,1,1)
      func(1,1,1) = 1.d0
      func(2,1,1) = 1.d0
      func(3,1,1) = -2.d0
    end subroutine

end module
