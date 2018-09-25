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


module shape_prism6n
  integer, parameter, private :: kreal = kind(0.0d0)

  contains
    subroutine ShapeFunc_prism6n(ncoord,func)
      use shape_tri3n
      real(kind=kreal), intent(in)  :: ncoord(3)
      real(kind=kreal) :: func(6)
      real(kind=kreal) :: xi, et, a, ze
      xi=ncoord(1); et=ncoord(2); ze=ncoord(3)
      a=1.d0-xi-et
      func(1)=0.5d0*a *(1.d0-ze)
      func(2)=0.5d0*xi*(1.d0-ze)
      func(3)=0.5d0*et*(1.d0-ze)
      func(4)=0.5d0*a *(1.d0+ze)
      func(5)=0.5d0*xi*(1.d0+ze)
      func(6)=0.5d0*et*(1.d0+ze)
    end subroutine

    subroutine ShapeDeriv_prism6n(ncoord,func)
      real(kind=kreal), intent(in)  :: ncoord(3)
      real(kind=kreal) :: func(6,3)
      real(kind=kreal) :: xi, et, a, ze
      xi=ncoord(1); et=ncoord(2); ze=ncoord(3)
      a=1.d0-xi-et

!      func(1,1) = 0.5d0*(1.d0-ncoord(3))
 !     func(2,1) = 0.d0
  !    func(3,1) = -0.5d0*(1.d0-ncoord(3))
   !   func(4,1) = 0.5d0*(1.d0+ncoord(3))
  !    func(5,1) = 0.d0
  !    func(6,1) = -0.5d0*(1.d0+ncoord(3))

  !    func(1,2) = 0.d0
  !    func(2,2) = 0.5d0*(1.d0-ncoord(3))
  !    func(3,2) = -0.5d0*(1.d0-ncoord(3))
  !    func(4,2) = 0.d0
  !    func(5,2) = 0.5d0*(1.d0+ncoord(3))
  !    func(6,2) = -0.5d0*(1.d0+ncoord(3))

  !    func(1,3) = -0.5d0*ncoord(1)
  !    func(2,3) = -0.5d0*ncoord(2)
  !    func(3,3) = -0.5d0*(1.d0-ncoord(1)-ncoord(2))
  !    func(4,3) = 0.5d0*ncoord(1)
  !    func(5,3) = 0.5d0*ncoord(2)
  !    func(6,3) = 0.5d0*(1.d0-ncoord(1)-ncoord(2))
!
!     local derivatives of the shape functions: xi-derivative
!
      func(1,1)=-0.5d0*(1.d0-ze)
      func(2,1)= 0.5d0*(1.d0-ze)
      func(3,1)= 0.d0
      func(4,1)=-0.5d0*(1.d0+ze)
      func(5,1)= 0.5d0*(1.d0+ze)
      func(6,1)= 0.d0
!
!     local derivatives of the shape functions: eta-derivative
!
      func(1,2)=-0.5d0*(1.d0-ze)
      func(2,2)= 0.d0
      func(3,2)= 0.5d0*(1.d0-ze)
      func(4,2)=-0.5d0*(1.d0+ze)
      func(5,2)= 0.d0
      func(6,2)= 0.5d0*(1.d0+ze)

!
!     local derivatives of the shape functions: zeta-derivative
!
      func(1,3)=-0.5d0*a
      func(2,3)=-0.5d0*xi
      func(3,3)=-0.5d0*et
      func(4,3)= 0.5d0*a
      func(5,3)= 0.5d0*xi
      func(6,3)= 0.5d0*et
    end subroutine

end module
