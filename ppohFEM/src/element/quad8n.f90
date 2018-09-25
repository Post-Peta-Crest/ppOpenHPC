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


MODULE shape_quad8n
  implicit none

  integer, parameter, private :: kreal = kind(0.0d0)

  contains
    subroutine ShapeFunc_quad8n(lcoord,func)
      real(kind=kreal), intent(in) :: lcoord(2)
      real(kind=kreal) :: func(8)
      real(kind=kreal) :: RI,SI,RP,SP,RM,SM
      RI=lcoord(1);  SI=lcoord(2)
      RP=1.d0+lcoord(1)
      SP=1.d0+lcoord(2)
      RM=1.d0-lcoord(1)
      SM=1.d0-lcoord(2)
      func(1)=0.25d0*RM*SM*(-1.d0-RI-SI)
      func(2)=0.25d0*RP*SM*(-1.d0+RI-SI)
      func(3)=0.25d0*RP*SP*(-1.d0+RI+SI)
      func(4)=0.25d0*RM*SP*(-1.d0-RI+SI)
      func(5)=0.5d0*(1.d0-RI*RI)*(1.d0-SI)
      func(6)=0.5d0*(1.d0-SI*SI)*(1.d0+RI)
      func(7)=0.5d0*(1.d0-RI*RI)*(1.d0+SI)
      func(8)=0.5d0*(1.d0-SI*SI)*(1.d0-RI)
    end subroutine

    subroutine ShapeDeriv_quad8n(lcoord,func)
      real(kind=kreal), intent(in) :: lcoord(2)
      real(kind=kreal) :: func(8,2)
      func(1,1)= .25d0*(1.d0-lcoord(2))*(2.d0*lcoord(1)+lcoord(2))
      func(2,1)= .25d0*(1.d0-lcoord(2))*(2.d0*lcoord(1)-lcoord(2))
      func(3,1)= .25d0*(1.d0+lcoord(2))*(2.d0*lcoord(1)+lcoord(2))
      func(4,1)= .25d0*(1.d0+lcoord(2))*(2.d0*lcoord(1)-lcoord(2))
      func(5,1)=-lcoord(1)*(1.d0-lcoord(2))
      func(6,1)= 0.5d0*(1.d0-lcoord(2)*lcoord(2))
      func(7,1)=-lcoord(1)*(1.d0+lcoord(2))
      func(8,1)=-0.5d0*(1.d0-lcoord(2)*lcoord(2))


      func(1,2)= .25d0*(1.d0-lcoord(1))*(lcoord(1)+2.d0*lcoord(2))
      func(2,2)=-.25d0*(1.d0+lcoord(1))*(lcoord(1)-2.d0*lcoord(2))
      func(3,2)= .25d0*(1.d0+lcoord(1))*(lcoord(1)+2.d0*lcoord(2))
      func(4,2)=-.25d0*(1.d0-lcoord(1))*(lcoord(1)-2.d0*lcoord(2))
      func(5,2)=-0.5d0*(1.d0-lcoord(1)*lcoord(1))
      func(6,2)=-lcoord(2)*(1.d0+lcoord(1))
      func(7,2)= 0.5d0*(1.d0-lcoord(1)*lcoord(1))
      func(8,2)=-lcoord(2)*(1.d0-lcoord(1))
    end subroutine

    subroutine Shape2ndDeriv_quad8n(lcoord,func)
      real(kind=kreal), intent(in) :: lcoord(2)
      real(kind=kreal) :: func(8,2,2)
      func(1,1,1) = 0.5d0*(1.d0-lcoord(2))
      func(2,1,1) = 0.5d0*(1.d0-lcoord(2))
      func(3,1,1) = 0.5d0*(1.d0+lcoord(2))
      func(4,1,1) = 0.5d0*(1.d0+lcoord(2))
      func(5,1,1) = lcoord(2)-1.d0
      func(6,1,1) = 0.d0
      func(7,1,1) =-lcoord(2)-1.d0
      func(8,1,1) = 0.d0

      func(1,1,2) = 0.25d0-0.5d0*(lcoord(1)+lcoord(2))
      func(2,1,2) =-0.25d0-0.5d0*(lcoord(1)-lcoord(2))
      func(3,1,2) = 0.25d0+0.5d0*(lcoord(1)+lcoord(2))
      func(4,1,2) =-0.25d0+0.5d0*(lcoord(1)-lcoord(2))
      func(5,1,2) = lcoord(1)
      func(6,1,2) =-lcoord(2)
      func(7,1,2) =-lcoord(1)
      func(8,1,2) = lcoord(2)

      func(1,2,1) = 0.25d0-0.5d0*(lcoord(1)+lcoord(2))
      func(2,2,1) =-0.25d0-0.5d0*(lcoord(1)-lcoord(2))
      func(3,2,1) = 0.25d0+0.5d0*(lcoord(1)+lcoord(2))
      func(4,2,1) =-0.25d0+0.5d0*(lcoord(1)-lcoord(2))
      func(5,2,1) = lcoord(1)
      func(6,2,1) =-lcoord(2)
      func(7,2,1) =-lcoord(1)
      func(8,2,1) = lcoord(2)

      func(1,2,2) = 0.5d0*(1.d0-lcoord(1))
      func(2,2,2) = 0.5d0*(1.d0+lcoord(1))
      func(3,2,2) = 0.5d0*(1.d0+lcoord(1))
      func(4,2,2) = 0.5d0*(1.d0-lcoord(1))
      func(5,2,2) = 0.d0
      func(6,2,2) =-lcoord(1)-1.d0
      func(7,2,2) = 0.d0
      func(8,2,2) = lcoord(1)-1.d0
    end subroutine

END MODULE
