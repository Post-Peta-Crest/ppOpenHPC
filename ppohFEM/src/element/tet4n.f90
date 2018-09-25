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


module shape_tet4n
  integer, parameter, private :: kreal = kind(0.0d0)

  contains
    subroutine ShapeFunc_tet4n(volcoord,func)
      real(kind=kreal), intent(in) :: volcoord(3)
      real(kind=kreal) :: func(4)
      func(2:4) = volcoord(1:3)
      func(1)   = 1.d0-volcoord(1)-volcoord(2)-volcoord(3)
    end subroutine

    subroutine ShapeDeriv_tet4n(func)
      real(kind=kreal), intent(out) :: func(4,3)
      func(1,1) = -1.d0
      func(2,1) = 1.d0
      func(3,1) = 0.d0
      func(4,1) = 0.d0

      func(1,2) = -1.d0
      func(2,2) = 0.d0
      func(3,2) = 1.d0
      func(4,2) = 0.d0

      func(1,3) = -1.d0
      func(2,3) = 0.d0
      func(3,3) = 0.d0
      func(4,3) = 1.d0
    end subroutine

end module
