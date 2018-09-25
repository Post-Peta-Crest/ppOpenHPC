!=====================================================================!
!                                                                     !
!   Software Name : ppOpen-APPL/FDM                                   !
!         Version : 0.3                                     !
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
module ppohFDM_m_comvar
  !
  ! Common variables used in the 2D FDM simulation of seismic wave
  ! propagation
  !
  use ppohFDM_m_stdlib
  use ppohFDM_m_params, only: Nx1, Nz1
  implicit none 
  public


  !!-- << Independent Variables >>
  real(PN), public, save :: Sxx(Nx1,Nz1), Szz(Nx1,Nz1)
  real(PN), public, save :: Sxz(Nx1,Nz1)
  real(PN), public, save :: Vx (Nx1,Nz1), Vz (Nx1,Nz1) 
  
  !!-- << Derivertives wrt Space >>
  real(PN), public, save :: dxVx (Nx1,Nz1), dxVz (Nx1,Nz1)
  real(PN), public, save :: dzVx (Nx1,Nz1), dzVz (Nx1,Nz1)
  real(PN), public, save :: dxSxx(Nx1,Nz1), dxSxz(Nx1,Nz1)
  real(PN), public, save :: dzSzz(Nx1,Nz1), dzSxz(Nx1,Nz1)
  
  real(PN) :: den(Nx1,Nz1) ! Density 
  real(PN) :: rig(Nx1,Nz1) ! Rigidity
  real(PN) :: lam(Nx1,Nz1) ! Lame's coefficient
  real(PN) :: RO, VP, VS
  real(PN) :: RO1(100), VP1(100), VS1(100)
contains


  subroutine ppohFDM_comvar__setup()
    !
    ! Initialization of common variables
    !
    
    Sxx  (:,:) = 0.0_PN
    Sxz  (:,:) = 0.0_PN
    Szz  (:,:) = 0.0_PN
    Vx   (:,:) = 0.0_PN
    Vz   (:,:) = 0.0_PN
    
    dxVx (:,:) = 0.0_PN
    dxVz (:,:) = 0.0_PN
    dzVx (:,:) = 0.0_PN
    dzVz (:,:) = 0.0_PN
    
    dxSxx(:,:) = 0.0_PN
    dxSxz(:,:) = 0.0_PN
    dzSxz(:,:) = 0.0_PN
    dzSzz(:,:) = 0.0_PN
    
    den  (:,:) = 0.0_PN
    rig  (:,:) = 0.0_PN
    lam  (:,:) = 0.0_PN
    
  end subroutine ppohFDM_comvar__setup

end module ppohFDM_m_comvar
