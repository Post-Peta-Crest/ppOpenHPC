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


module shape_tri6n
  integer, parameter, private :: kreal = kind(0.0d0)

  contains
    subroutine ShapeFunc_tri6n(areacoord,func)
      real(kind=kreal), intent(in) :: areacoord(2)
      real(kind=kreal) :: func(6)
      real(kind=kreal) :: xi,et,st
      xi=areacoord(1);  et=areacoord(2); st=1.d0-xi-et

      func(1)=st*(2.d0*st-1.d0)
      func(2)=xi*(2.d0*xi-1.d0)
      func(3)=et*(2.d0*et-1.d0)
      func(4)=4.d0*xi*st
      func(5)=4.d0*xi*et
      func(6)=4.d0*et*st
    end subroutine

    subroutine ShapeDeriv_tri6n(areacoord,func)
      real(kind=kreal), intent(in) :: areacoord(2)
      real(kind=kreal) :: func(6,2)
      real(kind=kreal) :: xi,et,st
      xi=areacoord(1);  et=areacoord(2); st=1.d0-xi-et

      func(1,1)=1.d0-4.d0*st
      func(2,1)=4.d0*xi-1.d0
      func(3,1)=0.d0
      func(4,1)=4.d0*(st-xi)
      func(5,1)=4.d0*et
      func(6,1)=-4.d0*et

      func(1,2)=1.d0-4.d0*st
      func(2,2)=0.d0
      func(3,2)=4.d0*et-1.d0
      func(4,2)=-4.d0*xi
      func(5,2)=4.d0*xi
      func(6,2)=4.d0*(st-et)

    end subroutine

    subroutine Shape2ndDeriv_tri6n(func)
      real(kind=kreal) :: func(6,2,2)
      func(1,1,1) = 4.d0;  func(1,1,2) = 4.d0
      func(2,1,1) = 4.d0;  func(2,1,2) = 0.d0
      func(3,1,1) = 0.d0;  func(3,1,2) = 0.d0
      func(4,1,1) =-8.d0;  func(4,1,2) = -4.d0
      func(5,1,1) = 0.d0;  func(5,1,2) = 4.d0
      func(6,1,1) = 0.d0;  func(6,1,2) = -4.d0

      func(1,2,1) = 4.d0;  func(1,2,2) = 4.d0
      func(2,2,1) = 0.d0;  func(2,2,2) = 0.d0
      func(3,2,1) = 0.d0;  func(3,2,2) = 4.d0
      func(4,2,1) =-4.d0;  func(4,2,2) = 0.d0
      func(5,2,1) = 4.d0;  func(5,2,2) = 0.d0
      func(6,2,1) =-4.d0;  func(6,2,2) =-8.d0
    end subroutine

end module
