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
module ppohFDM_m_kernel
  !
  ! The kernel for FDM simulation of a seismic wave in 2D, including spatial
  ! differentiation of variables and updating (time integration) of the velocity
  ! vector and the stress tensor
  !
  use ppohFDM_m_stdlib
  use ppohFDM_m_comvar
  use ppohFDM_m_params, only : Nx, Nz, Nx1, Nz1, Dx, Dz, Dt, NPM
  implicit none
  private 

  !=Constants for finite difference calcluation
  real(PN), parameter :: C20 = 1.0_PN
  real(PN), parameter :: C40 = 1.125_PN
  real(PN), parameter :: C41 = 1.0_PN / 24.0_PN

  !=Public Procedures
  public :: ppohFDM_kernel__setup
  public :: ppohFDM_kernel__velderiv
  public :: ppohFDM_kernel__stressderiv
  public :: ppohFDM_kernel__update_vel
  public :: ppohFDM_kernel__update_stress


contains
  
  !!---------------------------------------------------------------------------!
  !!                              Public Procedures
  !!---------------------------------------------------------------------------!
  subroutine ppohFDM_kernel__setup()
    !
    ! Initialization of local variables
    !
    
    Vx (:,:) = 0.0_PN
    Vz (:,:) = 0.0_PN
    Sxx(:,:) = 0.0_PN
    Sxz(:,:) = 0.0_PN
    Szz(:,:) = 0.0_PN
    
  end subroutine ppohFDM_kernel__setup


  subroutine ppohFDM_kernel__stressderiv()
    !
    ! Spatial differentiation (dp Spq, pq=xz) of the stress tensor (Spq)
    !
    
    call ppohFDM_fdiffx2_p4 ( Sxx, DxSxx, Nx, Nz, Nx1, Nz1, Dx )
    call ppohFDM_fdiffx2_m4 ( Sxz, DxSxz, Nx, Nz, Nx1, Nz1, Dx )
    call ppohFDM_fdiffz2_p4 ( Szz, DzSzz, Nx, Nz, Nx1, Nz1, Dz )
    call ppohFDM_fdiffz2_m4 ( Sxz, DzSxz, Nx, Nz, Nx1, Nz1, Dz )
    
  end subroutine ppohFDM_kernel__stressderiv


  subroutine ppohFDM_kernel__velderiv()
    !
    ! Spatial differentiation (dp Vq, pq=xz) of the velocity vector (Vp)
    !
    
    call ppohFDM_fdiffx2_m4 ( Vx, DxVx, Nx, Nz, Nx1, Nz1, Dx )
    call ppohFDM_fdiffx2_p4 ( Vz, DxVz, Nx, Nz, Nx1, Nz1, Dx )
    call ppohFDM_fdiffz2_m4 ( Vz, DzVz, Nx, Nz, Nx1, Nz1, Dz )
    call ppohFDM_fdiffz2_p4 ( Vx, DzVx, Nx, Nz, Nx1, Nz1, Dz )
    
  end subroutine ppohFDM_kernel__velderiv


  subroutine ppohFDM_kernel__update_vel(i0, i1, k0, k1)
    !
    ! Update of the velocity vector in the area (i0:i1,k0:k1), 
    ! excluding the absorbing buffer zone
    !
    integer, intent(in), optional :: i0, i1, k0, k1

    !=Internal Variables
    integer :: i, k
    real(PN) :: rox, roz
    integer :: ii0, ii1, kk0, kk1

    
    if( present(i0) .and. present(i1) .and. present(k0) .and. present(k1) ) then
       ii0 = i0
       ii1 = i1
       kk0 = k0
       kk1 = k1
    else
       ii0 = NPM+1
       ii1 = Nx-NPM
       kk0 = NPM+1
       kk1 = Nz-NPM
    end if
    
    do k = kk0, kk1
       do i = ii0, ii1
          
          ! Effective Density
          rox = 2.0_PN/( DEN(i,k) + DEN(i+1,k) )
          roz = 2.0_PN/( DEN(i,k) + DEN(i,k+1) )
          
          Vx(i,k) = Vx(i,k) + ( DxSxx(i,k)+DzSxz(i,k) )*rox*DT
          Vz(i,k) = Vz(i,k) + ( DxSxz(i,k)+DzSzz(i,k) )*roz*DT
          
       end do
    end do
    
  end subroutine ppohFDM_kernel__update_vel


  subroutine ppohFDM_kernel__update_stress(i0,i1,k0,k1)
    !
    ! Update of the stress tensor in the area (i0:i1,k0:k1), 
    ! excluding the absorbing buffer zone
    !
    integer, intent(in), optional :: i0, i1, k0, k1

    !=Internal Variables
    integer  :: ii0, ii1, kk0, kk1
    integer  :: i, k
    real(PN) :: lam1, rig1, rig2, lam2rig
    real(PN) :: DxVx1, DzVz1
    real(PN) :: d2V2                                       ! DxVx+DzVz
    real(PN) :: DxVzDzVx1
    real(PN) :: r1, r2, r3, r4
    real(PN) :: eps = epsilon(1.0_PN)
    real(PN) :: rigxz
    
    
    if( present(i0) .and. present(i1) .and. present(k0) .and. present(k1) ) then
       ii0 = i0
       ii1 = i1
       kk0 = k0
       kk1 = k1
    else
       ii0 = NPM+1
       ii1 = Nx-NPM
       kk0 = NPM+1
       kk1 = Nz-NPM
    end if
    
    
    do k = kk0, kk1
       do i = ii0, ii1
          
          lam1    = LAM (i,k)
          rig1    = RIG (i,k)
          rig2    = rig1 + rig1
          lam2rig = lam1+rig2
          
          r1 = rig1
          r2 = rig(i+1,k  )
          r3 = rig(i,  k+1)
          r4 = rig(i+1,k+1)
          rigxz = 4*r1*r2*r3*r4 &
               / (r1*r2*(r3+r4) + r3*r4*(r1+r2) + eps)
          DxVx1 = DxVx(i,k)
          DzVz1 = DzVz(i,k)
          d2V2  = DxVx1 +  DzVz1
          
          DxVzDzVx1 = DxVz(i,k)+DzVx(i,k)
          
          Sxx (i,k) = Sxx (i,k) + ( lam2rig*(d2V2) - rig2*DzVz1 )*DT
          Szz (i,k) = Szz (i,k) + ( lam2rig*(d2V2) - rig2*DxVx1 )*DT
          Sxz (i,k) = Sxz (i,k) + ( rigxz*DxVzDzVx1             )*DT
          
       end do
    end do
    
  end subroutine ppohFDM_kernel__update_stress
  
  !!---------------------------------------------------------------------------!
  !!                              Private Procedures
  !!
  !! Spatial differentiation of the velocity vector using a 4th-order
  !! staggered-grid FDM
  !!---------------------------------------------------------------------------!
  
  subroutine ppohFDM_fdiffz2_m4( V, DzV, Nx, Nz, Nx1, Nz1, Dz )
    integer,  intent(in)  :: Nx
    integer,  intent(in)  :: Nz
    integer,  intent(in)  :: Nx1
    integer,  intent(in)  :: Nz1
    real(PN), intent(in)  :: V(Nx1, Nz1)
    real(PN), intent(in)  :: Dz
    real(PN), intent(out) :: DzV(Nx1,Nz1)

    real(PN) :: R40, R41, R20
    integer :: i,j

    R40 = C40 / Dz
    R41 = C41 / Dz
    R20 = C20 / Dz
    do i=1, Nx
       do j=3, Nz-1
          DzV(i,j) = ( V(i,j)   - V(i,j-1) ) * R40 &
               - ( V(i,j+1) - V(i,j-2) ) * R41
       end do
    end do
    do i=1, Nx
       DzV(i, 1) = ( V(i, 1) - 0.0_PN    ) * R20
       DzV(i, 2) = ( V(i, 2) - V(i,   1) ) * R20
       DzV(i,Nz) = ( V(i,Nz) - V(i,Nz-1) ) * R20 
    end do
    
    
  end subroutine ppohFDM_fdiffz2_m4


  subroutine ppohFDM_fdiffz2_p4( V, DzV, Nx, Nz, Nx1, Nz1, Dz )
    integer,  intent(in)  :: Nx
    integer,  intent(in)  :: Nz
    integer,  intent(in)  :: Nx1
    integer,  intent(in)  :: Nz1
    real(PN), intent(in)  :: V(Nx1, Nz1)
    real(PN), intent(in)  :: Dz
    real(PN), intent(out) :: DzV(Nx1,Nz1)

    real(PN) :: R40, R41, R20
    integer :: i,j

    R40 = C40 / Dz
    R41 = C41 / Dz
    R20 = C20 / Dz
    do i=1, Nx
       do j=2, Nz-2
          DzV(i,j) = ( V(i,j+1) - V(i,j) ) * R40 &
               - ( V(i,j+2) - V(i,j-1) ) * R41
       end do
    end do
    
    do i=1, Nx
       DzV(i,1) = ( V(i,2) - V(i,1) ) * R20
       DzV(i,Nz-1) = ( V(i,Nz) - V(i,Nz-1) ) * R20
       DzV(i,Nz)   = - V(i,Nz) * R20
    end do
    
  end subroutine ppohFDM_fdiffz2_p4


  subroutine  ppohFDM_fdiffx2_p4 ( V, DxV, Nx, Nz, Nx1, Nz1, Dx )
    integer,  intent(in)  :: Nx
    integer,  intent(in)  :: Nz
    integer,  intent(in)  :: Nx1
    integer,  intent(in)  :: Nz1
    real(PN), intent(in)  :: V(Nx1, Nz1)
    real(PN), intent(in)  :: Dx
    real(PN), intent(out) :: DxV(Nx1,Nz1)

    real(PN) ::R40, R41, R20
    integer :: i,j

    R40 = C40/Dx
    R41 = C41/Dx
    R20 = C20/Dx
    
    do J = 1, Nz
       do I = 2, Nx-2
          DxV (i,j) = ( V(i+1,j) - V(i,j  ) ) * R40 &
               - ( V(i+2,j) - V(i-1,j) ) * R41
       end do
    end do
    
    do J = 1, Nz
       DxV(   1,j) = ( V( 2,j) - V(   1,j) ) * R20
       DxV(Nx-1,j) = ( V(Nx,j) - V(Nx-1,j) ) * R20
       DxV(Nx  ,j) = ( 0.0_PN  - V(Nx  ,j) ) * R20
    end do
    
  end subroutine ppohFDM_fdiffx2_p4


  subroutine  ppohFDM_fdiffx2_m4 ( V, DxV, Nx, Nz, Nx1, Nz1, Dx )
    integer,  intent(in)  :: Nx
    integer,  intent(in)  :: Nz
    integer,  intent(in)  :: Nx1
    integer,  intent(in)  :: Nz1
    real(PN), intent(in)  :: V(Nx1, Nz1)
    real(PN), intent(in)  :: Dx
    real(PN), intent(out) :: DxV(Nx1,Nz1)

    real(PN) ::R40, R41, R20
    integer :: i,j

    
    R40 = C40/Dx
    R41 = C41/Dx
    R20 = C20 / Dx
    
    do j=1, Nz
       do I = 3, Nx-1
          DxV (i,j) = ( V(i,j)  -V(i-1,j) ) * R40 &
               - ( V(i+1,j) - V(i-2,j) ) * R41
       end do
    end do
    
    do j=1, Nz
       DxV( 1,j) = ( V( 1,j) - 0.0_PN    ) * R20
       DxV( 2,j) = ( V( 2,j) - V(   1,j) ) * R20
       DxV(Nx,j) = ( V(Nx,j) - V(Nx-1,j) ) * R20 
    end do
    
  end subroutine ppohFDM_fdiffx2_m4
  
end module ppohFDM_m_kernel
