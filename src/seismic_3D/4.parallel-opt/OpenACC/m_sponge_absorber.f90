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
module ppohFDM_sponge_absorber
!
! A sponge-type absorbing buffer to suppress reflection of a signal from
! artificial boundaries in the bounded domain
!
  use ppohFDM_stdio
  use ppohFDM_param
  implicit none
  public

  real(PN), parameter :: ALPHA = 0.015_PN
contains

  subroutine ppohFDM_set_sponge_absorber( )
  !
  ! Sets absorption parameters for absorbing a buffer zone
  !
    integer :: i, j, k

    do i=1, NXP
       ii = IA(i)
       if( ii <= NPM ) then
          gx(i) = exp( - ( ( ALPHA * (NPM-ii))**2 ))
       else if ( ii >= NX-NPM+1 ) then
          gx(i) = exp( -( ( ALPHA * ( ii - NX+NPM-1) )**2 ) )
       else
          gx(i) = 1.0_PN
       end if
    end do

    do j=1, NYP
       jj = JA(j)
       if( jj <= NPM ) then
          gy(j) = exp( - ( ( ALPHA * (NPM-jj))**2 ))
       else if ( jj >= NY-NPM+1 ) then
          gy(j) = exp( -( ( ALPHA * ( jj - NY+NPM-1) )**2 ) )
       else
          gy(j) = 1.0_PN
       end if
    end do

    do k=1, NZP
       kk = KA(k)
       if( kk <= NPM ) then
          gz(k) = exp( - ( ( ALPHA * (NPM-kk))**2 ))
       else if ( kk >= NZ-NPM+1 ) then
          gz(k) = exp( -( ( ALPHA * ( kk - NZ+NPM-1) )**2 ) )
       else
          gz(k) = 1.0_PN
       end if
    end do

    
  end subroutine ppohFDM_set_sponge_absorber
  
end module ppohFDM_sponge_absorber
