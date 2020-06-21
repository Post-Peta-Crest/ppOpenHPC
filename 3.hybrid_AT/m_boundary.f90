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
!=Declarations
  use ppohFDM_stdio
  use ppohFDM_param
  implicit none
  public
!+
!--
contains
  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_bc_zero_stress( KFSZ, NIFS, NJFS, &
                             IFSX, IFSY, IFSZ, JFSX, JFSY, JFSZ)
  !
  !=Arguments
    integer, intent(in) :: KFSZ(NXP0:NXP1,NYP0:NYP1)
    integer, intent(in) :: NIFS, NJFS
    integer, intent(in) :: IFSX(NFSMAX), IFSY(NFSMAX), IFSZ(NFSMAX)
    integer, intent(in) :: JFSX(NFSMAX), JFSY(NFSMAX), JFSZ(NFSMAX)
  !+
    integer :: i, j
  !--
    
    !! Topography Condition: Zero Shear Stress
    do J=NYP00, NYP10
       do I=NXP00, NXP10
          SXZ(I,J,KFSZ(I,J)) = 0.0_PN
          SYZ(I,J,KFSZ(I,J)) = 0.0_PN
       end do
    end do

    do I=1, NIFS
       SXZ( IFSX(I),IFSY(I),IFSZ(I) ) = 0.0_PN
       SXY( IFSX(I),IFSY(I),IFSZ(I) ) = 0.0_PN
    end do

    do I=1, NJFS
       SXY( JFSX(I),JFSY(I),JFSZ(I) ) = 0.0_PN
       SYZ( JFSX(I),JFSY(I),JFSZ(I) ) = 0.0_PN
    end do

  end subroutine ppohFDM_bc_zero_stress
  !----------------------------------------------------------------------------!
  
  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_bc_stress_deriv( KFSZ, NIFS, NJFS, &
                              IFSX, IFSY, IFSZ, JFSX, JFSY, JFSZ)
  !
  !=Arguments
    integer, intent(in) :: KFSZ(NXP0:NXP1,NYP0:NYP1)
    integer, intent(in) :: NIFS, NJFS
    integer, intent(in) :: IFSX(NFSMAX), IFSY(NFSMAX), IFSZ(NFSMAX)
    integer, intent(in) :: JFSX(NFSMAX), JFSY(NFSMAX), JFSZ(NFSMAX)
  !+
    integer :: i, j
  !--
    
    
    ! Overwrite derivertives among boundary
    ! z-direction
    do J=1, NYP
       do I=1, NXP
          KK = KFSZ(I,J)
          
          DZSXZ(I,J,KK  ) = -SXZ(I,J,KK-1) * DZI
          DZSXZ(I,J,KK+1) =  SXZ(I,J,KK+1) * DZI
          DZSYZ(I,J,KK  ) = -SYZ(I,J,KK-1) * DZI
          DZSYZ(I,J,KK+1) =  SYZ(I,J,KK+1) * DZI
          DZSZZ(I,J,KK+1) = ( SZZ(I,J,KK+2) - SZZ(I,J,KK+1) ) * DZI
          DZSZZ(I,J,KK  ) = ( SZZ(I,J,KK+1) - SZZ(I,J,KK  ) ) * DZI
          DZSZZ(I,J,KK-1) = ( SZZ(I,J,KK  ) - SZZ(I,J,KK-1) ) * DZI
          
       end do
    end do

    ! x-direction
    do I=1, NIFS
       II = IFSX(I)
       JJ = IFSY(I)
       KK = IFSZ(I)
       DXSXZ(II  ,JJ,KK) = - SXZ(II-1,JJ,KK) * DXI
       DXSXZ(II+1,JJ,KK) =   SXZ(II+1,JJ,KK) * DXI
       DXSXY(II  ,JJ,KK) = - SXY(II-1,JJ,KK) * DXI
       DXSXY(II+1,JJ,KK) =   SXY(II+1,JJ,KK) * DXI
       
       DXSXX(II-1,JJ,KK) = ( SXX(II  ,JJ,KK) - SXX(II-1,JJ,KK) ) * DXI
       DXSXX(II  ,JJ,KK) = ( SXX(II+1,JJ,KK) - SXX(II  ,JJ,KK) ) * DXI
       DXSXX(II+1,JJ,KK) = ( SXX(II+2,JJ,KK) - SXX(II+1,JJ,KK) ) * DXI
    end do
    ! y-direction
    do J=1, NJFS
       II = JFSX(J)
       JJ = JFSY(J)
       KK = JFSZ(J)
       
       DYSYZ(II,JJ,  KK) = - SYZ(II,JJ-1,KK) * DYI
       DYSYZ(II,JJ+1,KK) =   SYZ(II,JJ+1,KK) * DYI
       DYSXY(II,JJ,  KK) = - SXY(II,JJ-1,KK) * DYI
       DYSXY(II,JJ+1,KK) =   SXY(II,JJ+1,KK) * DYI
       
       DYSYY(II,JJ-1,KK) = ( SYY(II,JJ  ,KK) - SYY(II,JJ-1,KK) ) * DYI
       DYSYY(II,JJ  ,KK) = ( SYY(II,JJ+1,KK) - SYY(II,JJ  ,KK) ) * DYI
       DYSYY(II,JJ+1,KK) = ( SYY(II,JJ+2,KK) - SYY(II,JJ+1,KK) ) * DYI
    end do

  end subroutine ppohFDM_bc_stress_deriv
  !----------------------------------------------------------------------------!

  !+---------------------------------------------------------------------------!
  subroutine ppohFDM_bc_vel_deriv( KFSZ, NIFS, NJFS, &
                           IFSX, IFSY, IFSZ, JFSX, JFSY, JFSZ)
  !
  !=Arguments
    integer, intent(in) :: KFSZ(NXP0:NXP1,NYP0:NYP1)
    integer, intent(in) :: NIFS, NJFS
    integer, intent(in) :: IFSX(NFSMAX), IFSY(NFSMAX), IFSZ(NFSMAX)
    integer, intent(in) :: JFSX(NFSMAX), JFSY(NFSMAX), JFSZ(NFSMAX)
  !+
    integer :: i, j 
  !--
    
    DXI = 1.0_PN / DX
    DYI = 1.0_PN / DY
    DZI = 1.0_PN / DZ
    
    !!-- Topography
    do J=1, NYP
       do I=1,NXP
          KK = KFSZ(I,J)
          
          DZVX(I,J,KK-1) = ( VX(I,J,KK  )-VX(I,J,KK-1) ) * DZI
          DZVX(I,J,KK+1) = ( VX(I,J,KK+2)-VX(I,J,KK+1) ) * DZI
          DZVY(I,J,KK-1) = ( VY(I,J,KK  )-VY(I,J,KK-1) ) * DZI
          DZVY(I,J,KK+1) = ( VY(I,J,KK+2)-VY(I,J,KK+1) ) * DZI
          
       end do
    end do
    
    do I=1, NIFS
       II = IFSX(I)
       JJ = IFSY(I)
       KK = IFSZ(I)
       DXVY(II-1,JJ,KK) = ( VY(II  ,JJ,KK) - VY(II-1,JJ,KK) ) * DXI
       DXVY(II+1,JJ,KK) = ( VY(II+2,JJ,KK) - VY(II+1,JJ,KK) ) * DXI
       DXVZ(II-1,JJ,KK) = ( VZ(II  ,JJ,KK) - VZ(II-1,JJ,KK) ) * DXI
       DXVZ(II+1,JJ,KK) = ( VZ(II+2,JJ,KK) - VZ(II+1,JJ,KK) ) * DXI
    end do

    do J=1, NJFS
       II = JFSX(J)
       JJ = JFSY(J)
       KK = JFSZ(J)
       DYVX(II,JJ-1,KK) = ( VX(II,JJ  ,KK) - VX(II,JJ-1,KK) ) * DYI
       DYVX(II,JJ+1,KK) = ( VX(II,JJ+2,KK) - VX(II,JJ+1,KK) ) * DYI
       DYVZ(II,JJ-1,KK) = ( VZ(II,JJ  ,KK) - VZ(II,JJ-1,KK) ) * DYI
       DYVZ(II,JJ+1,KK) = ( VZ(II,JJ+2,KK) - VZ(II,JJ+1,KK) ) * DYI
    end do

  end subroutine ppohFDM_bc_vel_deriv
  !----------------------------------------------------------------------------!
end module ppohFDM_boundary
