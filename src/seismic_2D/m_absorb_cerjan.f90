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
module ppohFDM_m_absorb
  !
  ! Sponge-type absorbing boundary based on Cerjan, et al. (1985) to
  ! eliminate artificial reflection from the edges of bounded media.
  !
  use ppohFDM_m_stdlib
  use ppohFDM_m_params, only : Nx1, Nz1, Nx, Nz, NPM, Dt
  use ppohFDM_m_comvar
  use ppohFDM_m_kernel
  implicit none
  private

  !=Public Procedures
  public :: ppohFDM_absorb__setup
  public :: ppohFDM_absorb__update_vel
  public :: ppohFDM_absorb__update_stress

  !=Public Variables
  real(PN), save :: gg(Nx1,Nz1)

contains
  
  !!--------------------------------------------------------------------------!!
  !!                             PUBLIC PROCEDURES                            !!
  !!--------------------------------------------------------------------------!!
  subroutine ppohFDM_absorb__setup()
    !
    ! Sets up absorbing conditions (attenuation coefficients) in the sponge buffer
    ! zone surrounding the medium.
    !
    real(PN), parameter :: APARA = 0.015_PN
    integer :: i, k
    
    gg = 1.0
    do k=1, Nz
       do i=1, Nx
          if( i <= NPM ) then
             gg(i,k) = exp( -( ( APARA * (NPM - i     ) )**2 ) )
          else if ( k <= NPM ) then
             gg(i,k) = exp( -( ( APARA * (NPM - k    ) )**2 ) )
          else if ( i >= Nx - NPM + 1 ) then
             gg(i,k) = exp( -( ( APARA * ( i - NX+NPM-1) )**2 ) )
          else if ( k >= Nz - NPM + 1 ) then
             gg(i,k) = exp( -( ( APARA  * ( k - NZ+NPM-1) )**2 ))
          end if
       end do
    end do
    
    
  end subroutine ppohFDM_absorb__setup


  subroutine ppohFDM_absorb__update_vel()
    !
    ! Updates the velocity vector in the absorbing area
    !
    
    call ppohFDM_kernel__update_vel( 1,        NPM,    1,         Nz  ) ! x-left
    call ppohFDM_kernel__update_vel( Nx-NPM+1, Nx,     1,         Nz  ) ! x-right
    call ppohFDM_kernel__update_vel( NPM+1,    Nx-NPM, 1,         NPM ) ! z-top
    call ppohFDM_kernel__update_vel( NPM+1,    Nx-NPM, Nz -NPM+1, Nz  ) ! z-down
    
    Vx(:,:) = Vx(:,:) * gg(:,:)
    Vz(:,:) = Vz(:,:) * gg(:,:)
    
  end subroutine ppohFDM_absorb__update_vel


  subroutine ppohFDM_absorb__update_stress()
    !
    ! Updates the stress tensor in the absorbing area.
    !
    call ppohFDM_kernel__update_stress( 1,        NPM,    1,         Nz )
    call ppohFDM_kernel__update_stress( Nx-NPM+1, Nx,     1,         Nz )
    call ppohFDM_kernel__update_stress( NPM+1,    Nx-NPM, 1,         NPM )
    call ppohFDM_kernel__update_stress( NPM+1,    Nx-NPM, Nz-NPM+1, Nz )
    
    Sxx(:,:) = Sxx(:,:) * gg(:,:)
    Szz(:,:) = Szz(:,:) * gg(:,:)
    Sxz(:,:) = Sxz(:,:) * gg(:,:)
    
  end subroutine ppohFDM_absorb__update_stress
  
end module ppohFDM_m_absorb
