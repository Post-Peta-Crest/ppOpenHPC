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


module shape_hex8n
  integer, parameter, private :: kreal = kind(0.0d0)

  contains
    subroutine ShapeFunc_hex8n(localcoord,func)
      real(kind=kreal) :: localcoord(3)
      real(kind=kreal) :: func(8)
      func(1) = 0.125d0*(1.d0-localcoord(1))*(1.d0-localcoord(2))*(1.d0-localcoord(3))
      func(2) = 0.125d0*(1.d0+localcoord(1))*(1.d0-localcoord(2))*(1.d0-localcoord(3))
      func(3) = 0.125d0*(1.d0+localcoord(1))*(1.d0+localcoord(2))*(1.d0-localcoord(3))
      func(4) = 0.125d0*(1.d0-localcoord(1))*(1.d0+localcoord(2))*(1.d0-localcoord(3))
      func(5) = 0.125d0*(1.d0-localcoord(1))*(1.d0-localcoord(2))*(1.d0+localcoord(3))
      func(6) = 0.125d0*(1.d0+localcoord(1))*(1.d0-localcoord(2))*(1.d0+localcoord(3))
      func(7) = 0.125d0*(1.d0+localcoord(1))*(1.d0+localcoord(2))*(1.d0+localcoord(3))
      func(8) = 0.125d0*(1.d0-localcoord(1))*(1.d0+localcoord(2))*(1.d0+localcoord(3))
    end subroutine

    subroutine ShapeDeriv_hex8n(localcoord, func)
      real(kind=kreal) :: localcoord(3)
      real(kind=kreal) :: func(8,3)
      func(1,1) = -0.125d0*(1.d0-localcoord(2))*(1.d0-localcoord(3))
      func(2,1) =  0.125d0*(1.d0-localcoord(2))*(1.d0-localcoord(3))
      func(3,1) =  0.125d0*(1.d0+localcoord(2))*(1.d0-localcoord(3))
      func(4,1) = -0.125d0*(1.d0+localcoord(2))*(1.d0-localcoord(3))
      func(5,1) = -0.125d0*(1.d0-localcoord(2))*(1.d0+localcoord(3))
      func(6,1) =  0.125d0*(1.d0-localcoord(2))*(1.d0+localcoord(3))
      func(7,1) =  0.125d0*(1.d0+localcoord(2))*(1.d0+localcoord(3))
      func(8,1) = -0.125d0*(1.d0+localcoord(2))*(1.d0+localcoord(3))

      func(1,2) = -0.125d0*(1.d0-localcoord(1))*(1.d0-localcoord(3))
      func(2,2) = -0.125d0*(1.d0+localcoord(1))*(1.d0-localcoord(3))
      func(3,2) =  0.125d0*(1.d0+localcoord(1))*(1.d0-localcoord(3))
      func(4,2) =  0.125d0*(1.d0-localcoord(1))*(1.d0-localcoord(3))
      func(5,2) = -0.125d0*(1.d0-localcoord(1))*(1.d0+localcoord(3))
      func(6,2) = -0.125d0*(1.d0+localcoord(1))*(1.d0+localcoord(3))
      func(7,2) =  0.125d0*(1.d0+localcoord(1))*(1.d0+localcoord(3))
      func(8,2) =  0.125d0*(1.d0-localcoord(1))*(1.d0+localcoord(3))

      func(1,3) = -0.125d0*(1.d0-localcoord(1))*(1.d0-localcoord(2))
      func(2,3) = -0.125d0*(1.d0+localcoord(1))*(1.d0-localcoord(2))
      func(3,3) = -0.125d0*(1.d0+localcoord(1))*(1.d0+localcoord(2))
      func(4,3) = -0.125d0*(1.d0-localcoord(1))*(1.d0+localcoord(2))
      func(5,3) =  0.125d0*(1.d0-localcoord(1))*(1.d0-localcoord(2))
      func(6,3) =  0.125d0*(1.d0+localcoord(1))*(1.d0-localcoord(2))
      func(7,3) =  0.125d0*(1.d0+localcoord(1))*(1.d0+localcoord(2))
      func(8,3) =  0.125d0*(1.d0-localcoord(1))*(1.d0+localcoord(2))
    end subroutine

end module
