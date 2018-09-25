!=====================================================================!
!                                                                     !
!   Software Name : ppOpen-APPL/FDM                                   !
!         Version : 0.3                                               !
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
!                 Copyright (c) 2015 T.Furumura                       !
!                                                                     !
!=====================================================================!
module ppohFDM_m_surfbc
  !
  ! This module applies free surface boundary conditions on the solid/air interface
  !
  use ppohFDM_m_stdlib
  use ppohFDM_m_params, only : Nx1, Nx, Dx, Dz
  use ppohFDM_m_comvar
  use ppohFDM_m_medium, only : nifs, kfsz, ifsx, ifsz, is_fs, &
       niob, kobz, iobx, iobz, is_ob
  implicit none
  private

  !=Public procedures
  public :: ppohFDM_surfbc__zerostress
  public :: ppohFDM_surfbc__stressderiv
  public :: ppohFDM_surfbc__velderiv
  
contains
  

  subroutine ppohFDM_surfbc__zerostress()
    !
    ! Applies zero to the shear stress tensor
    !
    
    if( is_fs ) call ppohFDM_zerostress( kfsz, nifs, ifsx, ifsz )
    if( is_ob ) call ppohFDM_zerostress( kobz, niob, iobx, iobz )
    
  end subroutine ppohFDM_surfbc__zerostress


  subroutine ppohFDM_zerostress( kfsz, nifs, ifsx, ifsz )
    
    integer, intent(in) :: kfsz(:)
    integer, intent(in) :: nifs
    integer, intent(in) :: ifsx(:)
    integer, intent(in) :: ifsz(:)
    
    integer :: i
    
    do i=1, Nx
       Sxz(I,kfsz(i)) = 0.0_PN
    end do
    do i=1, nifs
       Sxz( ifsx(i),ifsz(i) ) = 0.0_PN
    end do
    
  end subroutine ppohFDM_zerostress


  subroutine ppohFDM_surfbc__stressderiv()
    !
    ! Calculates spatial derivatives of the stress component on a free surface, assuming zero share stress
    !
    
    if( is_fs ) call ppohFDM_stressderiv( kfsz, nifs, ifsx, ifsz )
    if( is_ob ) call ppohFDM_stressderiv( kobz, niob, iobx, iobz )
    
  end subroutine ppohFDM_surfbc__stressderiv


  subroutine ppohFDM_stressderiv( kfsz, nifs, ifsx, ifsz )
    
    integer, intent(in) :: kfsz(:)
    integer, intent(in) :: nifs
    integer, intent(in) :: ifsx(:)
    integer, intent(in) :: ifsz(:)
    
    integer :: i
    integer :: ii, kk
    
    do i=1, Nx
       
       kk = kfsz(i)
       
       dzSxz(I,kk  ) = -Sxz(I,kk-1) / Dz
       dzSxz(I,kk+1) =  Sxz(I,kk+1) / Dz
       dzSzz(I,kk+1) = ( Szz(I,kk+2) - Szz(I,kk+1) ) / Dz
       dzSzz(I,kk  ) = ( Szz(I,kk+1) - Szz(I,kk  ) ) / Dz
       dzSzz(I,kk-1) = ( Szz(I,kk  ) - Szz(I,kk-1) ) / Dz
       
    end do
    
    ! x-direction
    do i=1, nifs
       ii = ifsx(i)
       kk = ifsz(i)
       dxSxz(ii  ,kk) = - Sxz(ii-1,kk) / Dx
       dxSxz(ii+1,kk) =   Sxz(ii+1,kk) / Dx
       
       dxSxx(ii-1,kk) = ( Sxx(ii  ,kk) - Sxx(ii-1,kk) ) / Dx
       dxSxx(ii  ,kk) = ( Sxx(ii+1,kk) - Sxx(ii  ,kk) ) / Dx
       dxSxx(ii+1,kk) = ( Sxx(ii+2,kk) - Sxx(ii+1,kk) ) / Dx
    end do
    
    
  end subroutine ppohFDM_stressderiv


  subroutine ppohFDM_surfbc__velderiv()
    !
    ! Calculates spatial derivatives of the velocity component on a free surface, assuming zero share stress
    !
    
    if( is_fs ) call ppohFDM_velderiv( kfsz, nifs, ifsx, ifsz )
    if( is_ob ) call ppohFDM_velderiv( kobz, niob, iobx, iobz )
    
  end subroutine ppohFDM_surfbc__velderiv


  subroutine ppohFDM_velderiv( kfsz, nifs, ifsx, ifsz )
    
    integer, intent(in) :: kfsz(:)
    integer, intent(in) :: nifs
    integer, intent(in) :: ifsx(:)
    integer, intent(in) :: ifsz(:)
    
    integer :: i
    integer :: ii, kk
    
    do i=1,Nx
       kk = kfsz(i)
       dzVx(I,kk-1) = ( Vx(I,kk  )-Vx(I,kk-1) ) / Dz
       dzVx(I,kk+1) = ( Vx(I,kk+2)-Vx(I,kk+1) ) / Dz
    end do
    do i=1, nifs
       ii = ifsx(i)
       kk = ifsz(i)
       dxVz(ii-1,kk) = ( Vz(ii  ,kk) - Vz(ii-1,kk) ) / Dx
       dxVz(ii+1,kk) = ( Vz(ii+2,kk) - Vz(ii+1,kk) ) / Dx
    end do
  end subroutine ppohFDM_velderiv

  
end module ppohFDM_m_surfbc
