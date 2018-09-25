!=====================================================================!
!                                                                     !
!   Software Name : ppOpen-APPL/FDM                                   !
!         Version : 0.2                                               !
!                                                                     !
!   License                                                           !
!     This file is part of ppOpen-APPL/FDM.                           !
!     ppOpen-APPL/FDM is a free software, you can use it under the    !
!     terms of The MIT License (MIT). See LICENSE file and User's     !
!     guide for more details.                                         !
!                                                                     !
!   ppOpen-HPC project:                                               !
!     Open Source Infrastructure for Development and Execution of     !
!     Large-Scale Scientific Applications on Post-Peta-Scale          !
!     Supercomputers with Automatic Tuning (AT).                      !
!                                                                     !
!   Organizations:                                                    !
!     The University of Tokyo                                         !
!       - Information Technology Center                               !
!       - Atmosphere and Ocean Research Institute (AORI)              !
!       - Interfaculty Initiative in Information Studies              !
!         /Earthquake Research Institute (ERI)                        !
!       - Graduate School of Frontier Science                         !
!     Kyoto University                                                !
!       - Academic Center for Computing and Media Studies             !
!     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  !
!                                                                     !
!   Sponsorship:                                                      !
!     Japan Science and Technology Agency (JST), Basic Research       !
!     Programs: CREST, Development of System Software Technologies    !
!     for post-Peta Scale High Performance Computing.                 !
!                                                                     !
!                 Copyright (c) 2013 T.Furumura                       !
!                                                                     !
!=====================================================================!
module ppohFDM_boundary
!
! This module applies a zero-stress boundary condition on a free surface
!
  use ppohFDM_stdio
  use ppohFDM_param
  implicit none
  public

contains


  subroutine ppohFDM_bc_zero_stress( KFSZ, NIFS, NJFS, &
                             IFSX, IFSY, IFSZ, JFSX, JFSY, JFSZ)
    !
    ! Applies a zero stress value to the stress components (S_pz, p=x,y,z) on a free surface
    !
    integer, intent(in) :: KFSZ(NXP0:NXP1,NYP0:NYP1)
    integer, intent(in) :: NIFS, NJFS
    integer, intent(in) :: IFSX(NFSMAX), IFSY(NFSMAX), IFSZ(NFSMAX)
    integer, intent(in) :: JFSX(NFSMAX), JFSY(NFSMAX), JFSZ(NFSMAX)

    integer :: i, j

    
    !! Topography Condition: Zero Shear Stress

    !$acc  kernels pcopy(SXZ,SYZ,SXY) &
    !$acc& pcopyin(KFSZ,IFSX,IFSY,IFSZ,JFSX,JFSY,JFSZ) async(0)

    !$acc loop gang vector(8)
    do J=NYP00, NYP10
       !$acc loop gang vector(32)
       do I=NXP00, NXP10
          SXZ(I,J,KFSZ(I,J)) = 0.0_PN
          SYZ(I,J,KFSZ(I,J)) = 0.0_PN
       end do
    end do

    !$acc loop independent gang vector(128)
    do I=1, NIFS
       SXZ( IFSX(I),IFSY(I),IFSZ(I) ) = 0.0_PN
       SXY( IFSX(I),IFSY(I),IFSZ(I) ) = 0.0_PN
    end do

    !$acc loop independent gang vector(128)
    do I=1, NJFS
       SXY( JFSX(I),JFSY(I),JFSZ(I) ) = 0.0_PN
       SYZ( JFSX(I),JFSY(I),JFSZ(I) ) = 0.0_PN
    end do

    !$acc end kernels

  end subroutine ppohFDM_bc_zero_stress


  subroutine ppohFDM_bc_stress_deriv( KFSZ, NIFS, NJFS, &
                              IFSX, IFSY, IFSZ, JFSX, JFSY, JFSZ)
    !
    ! One-side differentiation scheme used for calculating spatial derivatives
    ! of the velocity components just below and above a free surface. 
    !
    integer, intent(in) :: KFSZ(NXP0:NXP1,NYP0:NYP1)
    integer, intent(in) :: NIFS, NJFS
    integer, intent(in) :: IFSX(NFSMAX), IFSY(NFSMAX), IFSZ(NFSMAX)
    integer, intent(in) :: JFSX(NFSMAX), JFSY(NFSMAX), JFSZ(NFSMAX)

    integer :: i, j
    
    
    ! Overwrite derivertives among boundary

    !$acc  kernels&
    !$acc& pcopy(DZSXZ,DZSYZ,DZSZZ,DXSXZ,DXSXY,DXSXX,DYSYZ,DYSXY,DYSYY)&
    !$acc& pcopyin(KFSZ,IFSX,IFSY,IFSZ,JFSX,JFSY,JFSZ)&
    !$acc& async(0)

    ! z-direction
    !$acc loop gang vector(8)
    do J=1, NYP
       !$acc loop gang vector(32)
       do I=1, NXP
          
          DZSXZ(I,J,KFSZ(I,J)  ) = -SXZ(I,J,KFSZ(I,J)-1) * DZI
          DZSXZ(I,J,KFSZ(I,J)+1) =  SXZ(I,J,KFSZ(I,J)+1) * DZI
          DZSYZ(I,J,KFSZ(I,J)  ) = -SYZ(I,J,KFSZ(I,J)-1) * DZI
          DZSYZ(I,J,KFSZ(I,J)+1) =  SYZ(I,J,KFSZ(I,J)+1) * DZI
          DZSZZ(I,J,KFSZ(I,J)+1) = ( SZZ(I,J,KFSZ(I,J)+2) - SZZ(I,J,KFSZ(I,J)+1) ) * DZI
          DZSZZ(I,J,KFSZ(I,J)  ) = ( SZZ(I,J,KFSZ(I,J)+1) - SZZ(I,J,KFSZ(I,J)  ) ) * DZI
          DZSZZ(I,J,KFSZ(I,J)-1) = ( SZZ(I,J,KFSZ(I,J)  ) - SZZ(I,J,KFSZ(I,J)-1) ) * DZI
          
       end do
    end do
    
    ! x-direction
    !$acc loop independent gang vector(128)
    do I=1, NIFS
       DXSXZ(IFSX(I)  ,IFSY(I),IFSZ(I)) = - SXZ(IFSX(I)-1,IFSY(I),IFSZ(I)) * DXI
       DXSXZ(IFSX(I)+1,IFSY(I),IFSZ(I)) =   SXZ(IFSX(I)+1,IFSY(I),IFSZ(I)) * DXI
       DXSXY(IFSX(I)  ,IFSY(I),IFSZ(I)) = - SXY(IFSX(I)-1,IFSY(I),IFSZ(I)) * DXI
       DXSXY(IFSX(I)+1,IFSY(I),IFSZ(I)) =   SXY(IFSX(I)+1,IFSY(I),IFSZ(I)) * DXI
       
       DXSXX(IFSX(I)-1,IFSY(I),IFSZ(I)) = ( SXX(IFSX(I)  ,IFSY(I),IFSZ(I)) &
                                          - SXX(IFSX(I)-1,IFSY(I),IFSZ(I)) ) * DXI
       DXSXX(IFSX(I)  ,IFSY(I),IFSZ(I)) = ( SXX(IFSX(I)+1,IFSY(I),IFSZ(I)) &
                                          - SXX(IFSX(I)  ,IFSY(I),IFSZ(I)) ) * DXI
       DXSXX(IFSX(I)+1,IFSY(I),IFSZ(I)) = ( SXX(IFSX(I)+2,IFSY(I),IFSZ(I)) &
                                          - SXX(IFSX(I)+1,IFSY(I),IFSZ(I)) ) * DXI
       
    end do
    ! y-direction
    !$acc loop independent gang vector(128)
    do J=1, NJFS

       DYSYZ(JFSX(J),JFSY(J),  JFSZ(J)) = - SYZ(JFSX(J),JFSY(J)-1,JFSZ(J)) * DYI
       DYSYZ(JFSX(J),JFSY(J)+1,JFSZ(J)) =   SYZ(JFSX(J),JFSY(J)+1,JFSZ(J)) * DYI
       DYSXY(JFSX(J),JFSY(J),  JFSZ(J)) = - SXY(JFSX(J),JFSY(J)-1,JFSZ(J)) * DYI
       DYSXY(JFSX(J),JFSY(J)+1,JFSZ(J)) =   SXY(JFSX(J),JFSY(J)+1,JFSZ(J)) * DYI
       
       DYSYY(JFSX(J),JFSY(J)-1,JFSZ(J)) = ( SYY(JFSX(J),JFSY(J)  ,JFSZ(J)) &
                                        - SYY(JFSX(J),JFSY(J)-1,JFSZ(J)) ) * DYI
       DYSYY(JFSX(J),JFSY(J)  ,JFSZ(J)) = ( SYY(JFSX(J),JFSY(J)+1,JFSZ(J)) &
                                        - SYY(JFSX(J),JFSY(J)  ,JFSZ(J)) ) * DYI
       DYSYY(JFSX(J),JFSY(J)+1,JFSZ(J)) = ( SYY(JFSX(J),JFSY(J)+2,JFSZ(J)) &
                                        - SYY(JFSX(J),JFSY(J)+1,JFSZ(J)) ) * DYI


    end do

    !$acc end kernels

  end subroutine ppohFDM_bc_stress_deriv


  subroutine ppohFDM_bc_vel_deriv( KFSZ, NIFS, NJFS, &
                           IFSX, IFSY, IFSZ, JFSX, JFSY, JFSZ)
    integer, intent(in) :: KFSZ(NXP0:NXP1,NYP0:NYP1)
    integer, intent(in) :: NIFS, NJFS
    integer, intent(in) :: IFSX(NFSMAX), IFSY(NFSMAX), IFSZ(NFSMAX)
    integer, intent(in) :: JFSX(NFSMAX), JFSY(NFSMAX), JFSZ(NFSMAX)

    integer :: i, j 

    
    DXI = 1.0_PN / DX
    DYI = 1.0_PN / DY
    DZI = 1.0_PN / DZ

    !$acc  kernels&
    !$acc& pcopy(DZVX,DZVY,DXVY,DXVZ,DYVX,DYVZ) &
    !$acc& pcopyin(KFSZ,IFSX,IFSY,IFSZ,JFSX,JFSY,JFSZ) &
    !$acc& async(0)
    
    !!-- Topography
    !$acc loop gang vector(8)
    do J=1, NYP
       !$acc loop gang vector(32)
       do I=1,NXP

          DZVX(I,J,KFSZ(I,J)-1) = ( VX(I,J,KFSZ(I,J)  )-VX(I,J,KFSZ(I,J)-1) ) * DZI
          DZVX(I,J,KFSZ(I,J)+1) = ( VX(I,J,KFSZ(I,J)+2)-VX(I,J,KFSZ(I,J)+1) ) * DZI
          DZVY(I,J,KFSZ(I,J)-1) = ( VY(I,J,KFSZ(I,J)  )-VY(I,J,KFSZ(I,J)-1) ) * DZI
          DZVY(I,J,KFSZ(I,J)+1) = ( VY(I,J,KFSZ(I,J)+2)-VY(I,J,KFSZ(I,J)+1) ) * DZI
          
       end do
    end do
    
    !$acc loop independent gang vector(128)
    do I=1, NIFS
       DXVY(IFSX(I)-1,IFSY(I),IFSZ(I)) = ( VY(IFSX(I)  ,IFSY(I),IFSZ(I)) &
                                         - VY(IFSX(I)-1,IFSY(I),IFSZ(I)) ) * DXI
       DXVY(IFSX(I)+1,IFSY(I),IFSZ(I)) = ( VY(IFSX(I)+2,IFSY(I),IFSZ(I)) &
                                         - VY(IFSX(I)+1,IFSY(I),IFSZ(I)) ) * DXI
       DXVZ(IFSX(I)-1,IFSY(I),IFSZ(I)) = ( VZ(IFSX(I)  ,IFSY(I),IFSZ(I)) &
                                         - VZ(IFSX(I)-1,IFSY(I),IFSZ(I)) ) * DXI
       DXVZ(IFSX(I)+1,IFSY(I),IFSZ(I)) = ( VZ(IFSX(I)+2,IFSY(I),IFSZ(I)) &
                                         - VZ(IFSX(I)+1,IFSY(I),IFSZ(I)) ) * DXI

    end do
    
    !$acc loop independent gang vector(128)
    do J=1, NJFS
       DYVX(JFSX(J),JFSY(J)-1,JFSZ(J)) = ( VX(JFSX(J),JFSY(J)  ,JFSZ(J)) &
                                         - VX(JFSX(J),JFSY(J)-1,JFSZ(J)) ) * DYI
       DYVX(JFSX(J),JFSY(J)+1,JFSZ(J)) = ( VX(JFSX(J),JFSY(J)+2,JFSZ(J)) &
                                         - VX(JFSX(J),JFSY(J)+1,JFSZ(J)) ) * DYI
       DYVZ(JFSX(J),JFSY(J)-1,JFSZ(J)) = ( VZ(JFSX(J),JFSY(J)  ,JFSZ(J)) &
                                         - VZ(JFSX(J),JFSY(J)-1,JFSZ(J)) ) * DYI
       DYVZ(JFSX(J),JFSY(J)+1,JFSZ(J)) = ( VZ(JFSX(J),JFSY(J)+2,JFSZ(J)) &
                                         - VZ(JFSX(J),JFSY(J)+1,JFSZ(J)) ) * DYI
    end do

    !$acc end kernels

  end subroutine ppohFDM_bc_vel_deriv

end module ppohFDM_boundary
