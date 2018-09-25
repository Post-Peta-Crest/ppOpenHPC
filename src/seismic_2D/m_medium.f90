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
module ppohFDM_m_medium
  !
  ! Sets up elastic parameters in the simulation model
  !
  use ppohFDM_m_stdlib
  use ppohFDM_m_comvar, only: den, rig, lam, RO, VP, VS, RO1, VP1, VS1
  use ppohFDM_m_params, only: Nx1, Nz1, Nx, NPM, KFS, Dx, Dz, ZDEP, MST
  implicit none
  private

  !=Public Procedures
  public :: ppohFDM_medium__setup

  !=Public Variables
  !!-- << Medium Parameters >>
  
  !!-- << Free Surface >>
  integer, public, save :: kfsz(NX1)            ! Z-dir
  integer, public, save :: ifsx(NX1), ifsz(NX1) ! X-dir
  integer, public, save :: nifs                 ! # of discontinuities
  logical, public, save :: is_fs
  
  !!-- << Ocean Bottom >>
  integer, public, save :: kobz(NX1)            ! Z-dir
  integer, public, save :: iobx(NX1), iobz(NX1) ! X-dir
  integer, public, save :: niob                 ! # of discontinuities
  logical, public, save :: is_ob


contains
  
  !!--------------------------------------------------------------------------!!
  !!                             PRIVATE ROUTINES                             !!
  !!--------------------------------------------------------------------------!!
  
  subroutine ppohFDM_medium__setup() 
    !
    ! Sets up user defined elastic parameters (Den, Rig, Ram)
    !
    
    integer :: i
    
    !! 1. Free surface boundary on the absolute grid 
    is_fs = .true.
    is_ob = .false.
    
    do i=NPM+1, NX-NPM
       kfsz(i) = KFS 
    end do
    
    
    do i=NPM+1, NX-NPM
       kobz(i) = kfsz(i)
    end do
    
    ! trim kfsz; detect horizontal boundary
    
    if( is_fs ) then
       call ppohFDM_set_free_surface( kfsz, NIFS, IFSX, IFSZ )
    end if
    if( is_ob ) then
       call ppohFDM_set_free_surface( kobz, niob, iobx, iobz )
    end if
    
    ! No ocean
    if( ( .not. is_ob ) .and. is_fs ) then
       kobz = kfsz
       niob = 0
    end if
    
    
    call ppohFDM_set_medium( DEN, RIG, LAM  )
    
   end subroutine ppohFDM_medium__setup


   !!--------------------------------------------------------------------------!!
   !!                             PRIVATE ROUTINES                             !!
   !!--------------------------------------------------------------------------!!

   subroutine ppohFDM_set_free_surface( kfsz, NIFS, IFSX, IFSZ )
     integer, intent(inout) :: kfsz(NX1)
     integer, intent(out)   :: NIFS
     integer, intent(out)   :: IFSX(NX1), IFSZ(NX1)

     integer :: i, k

     !! 1. Smoothing too-small scale topographic change
     do I=NPM+1, NX-NPM, 2
        kfsz(I ) = int( ( kfsz(I)+kfsz(I+1) ) / 2.0_PN + 0.5 )
        kfsz(I+1) = kfsz(I)
     end do

     !! 2. ABSORBING REGION HAVE SAME STRUCTURE WITH LAYER BOUNDARY
     kfsz(1:NPM) = kfsz(NPM+1)
     kfsz(NX-NPM+1:NX1) = kfsz(NX-NPM)

     !! 3. Horizontal Boundary Scan: X-dir
     NIFS = 0
     do I=2, NX1
        if( kfsz(I) > kfsz(I-1) ) then
           do K=kfsz(I-1), kfsz(I)-1
              NIFS = NIFS+1
              IFSX(NIFS) = I-1 
              IFSZ(NIFS) = K+1
           end do
        else if( kfsz(I) < kfsz(I-1) ) then
           do K=kfsz(I), kfsz(I-1)-1
              NIFS = NIFS+1
              IFSX(NIFS) = I-1
              IFSZ(NIFS) = K+1
           end do
        end if
     end do

   end subroutine ppohFDM_set_free_surface


   subroutine ppohFDM_set_medium_param
     integer :: i
     character(len=80) filename
     character(len=80) tmp1


     filename="medium.dat"
     open(7,file=filename,status='old')
     read(7,*) tmp1
     read(7,*) tmp1
     read(7,*) tmp1
     read(7,*) tmp1
     print *,'Number of medium layer'
     read(7,*) MST
     print *, MST
     print *,'Medium parameter [ZDEP RO VP VS]'
     do i=1, MST
        read(7, *) ZDEP(i), RO1(i), VP1(i), VS1(i)
        print *, ZDEP(i), RO1(i), VP1(i), VS1(i)
     end do
     close(7)
   end subroutine ppohFDM_set_medium_param


   subroutine ppohFDM_set_medium( DEN, RIG, LAM )
     real(PN), intent(out), dimension(NX1,NZ1) :: DEN, RIG, LAM

     integer  :: I, K, M, kz
     integer  :: kza(MST+1)

     !! Set velocity, density and attenuation
     call ppohFDM_set_medium_param
     kza(1)=KFS
     do K = 1, NZ1
        do I = 1, NX1
           do M = 1, MST
              kz = int (ZDEP(M)/Dz+0.5)+kfsz(I)
              kza(M+1)=kz
              if(K <= kfsz(I)) then
                 RO = 0.001_PN
                 VP = 0.0_PN
                 VS = 0.0_PN
                 DEN  (I,K) = RO
                 RIG  (I,K) = RO*VS**2
                 LAM  (I,K) = RO*VP**2 - 2*RO*VS**2
              else if(kza(M) < K .and. K <= kza(M+1)) then
                 RO = RO1(M)
                 VP = VP1(M)
                 VS = VS1(M)
                 DEN  (I,K) = RO
                 RIG  (I,K) = RO*VS**2
                 LAM  (I,K) = RO*VP**2 - 2*RO*VS**2
              else if(kza(M+1) < K) then
                 RO = RO1(MST)
                 VP = VP1(MST)
                 VS = VS1(MST)
                 DEN  (I,K) = RO
                 RIG  (I,K) = RO*VS**2
                 LAM  (I,K) = RO*VP**2 - 2*RO*VS**2
              end if
           end do
       end do
    end do


  end subroutine ppohFDM_set_medium

end module ppohFDM_m_medium
