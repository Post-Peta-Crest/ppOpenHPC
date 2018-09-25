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
module ppohFDM_m_output
  !
  ! This module describes output of the simulation results
  !
  use ppohFDM_m_stdlib
  use ppohFDM_m_params   !, only : NX, NZ, TITLE, DT, NTMAX, dx, dz, I0, K0
  use ppohFDM_m_source   !, only : I0, K0
  use ppohFDM_m_medium   !, only : is_fs, kfsz
  use ppohFDM_m_comvar
  implicit none
  private

  !=Public Procedures
  public :: ppohFDM_output__setup
  public :: ppohFDM_output__write_snap
  public :: ppohFDM_output__close_files

  !!parameters
  
  character(99), parameter, public :: ODIR = './out'
  
  !! output switch
  logical, save :: is_output_sur = .false.
  logical, save :: is_output_sps = .true.
  logical, save :: is_output_wav = .true.
  
  integer,  parameter, public :: NSKIP   = 10           ! Waveform Decimation
  integer,  parameter, public :: NTSKP   = NTMAX/NSKIP  ! Waveform samples
  integer,  parameter, public :: NWRITE  = 100           ! Snapshot decimation
  
  !-- << Stations >>
  !integer,  parameter, public :: NST     =  8 ! Nx/8
  
  !-- << Snapshots >>
  integer,  parameter, public :: NXD     = 1      ! Decimation in Snapshot
  integer,  parameter, public :: NZD     = 1      ! Decimation in Snapshot
  integer,  parameter, public :: NXS     = NX/NXD ! Snapshot Array Size
  integer,  parameter, public :: NZS     = NZ/NZD ! Snapshot Array Size 
  
  !-- << I/O Numbers: BIG ENDIAN: Assumes 900-999 >>
  integer, save :: iosps, iosnp, iowav
  
  !-- << Output Filename >>
  character(80), parameter, public :: WNAME  = trim(TITLE)//".WAV" 
  character(80), parameter, public :: ONAME  = trim(TITLE)//".SPS" 
  character(80), parameter, public :: SNAME  = trim(TITLE)//".SUR"
  
  !-- << Snapshot Array >>
  real(PN), save :: vpsnap (NXS,NZS), vssnap (NXS,NZS)
  real(PN), save :: vxsnap (NXS,NZS), vzsnap (NXS,NZS)
  
  !!-- << Velocity/Displacement at Stations >>
  integer,  save :: istx(100), isty(100), istz(100)
  real(PN), save :: vxall(100), vzall(100)
  real(PN), save :: vxall0, vzall0
  real(PN), save :: uxall(100), uzall(100)
  
contains


  subroutine ppohFDM_set_station()
    real :: Xtmp, Ztmp
    integer :: i
    character(len=80) filename
    character(len=80) tmp1
    filename="station.dat"
    open(7,file=filename,status='old')
    read(7,*) tmp1
    read(7,*) tmp1
    read(7,*) tmp1
    read(7,*) tmp1
    read(7,*) NST
    print *,'station points'
    do i=1, NST
       read(7,*) Xtmp, Ztmp
       istx (i) = int(Xtmp/Dx+0.5)
       istz (i) = int(Ztmp/Dz+0.5)+KFS+1
       isty (i) = 0 ! dummy for 2D simu
       print *,istx(i),istz(i)
    end do
    close(7)
  end subroutine ppohFDM_set_station
  

  subroutine ppohFDM_output__setup()
    !
    ! Opens an output file
    !
    real(PN), parameter :: DY = 0.0 ! dummy for output
    integer :: i, k, ii, kk
    integer :: ierr
    real(PN) :: stlo(nst), stla(nst), stdp(nst)
    character(8) :: stnm(1:nst)
    character(8) :: cmpnm1(1:nst), cmpnm2(1:nst), cmpnm3(1:nst), cmpnm4(1:nst)
    integer :: j0 = 0
    
    do i=1, nst
       write(stnm(i),'(A,I5.5)') 'st_',i 
       cmpnm1(i) = 'VX'
       cmpnm2(i) = 'VZ'
       cmpnm3(i) = 'UX'
       cmpnm4(i) = 'UZ'
    end do
    call ppohFDM_set_station()
    !! station locaiton
!    do i = 1, NST
!       istx (i) = int( nx/nst * (i-0.5) )
!       istz (i) = kfsz(i)+1
!       isty (i) = 0 ! dummy for 2D simu
!    end do
    
    ! Open snapshot binary files
    if( is_output_sps ) then
       call stdlib__getio( iosps, is_big=.true. )
       open( IOSPS, file=trim(trim(ODIR)//'/'//ONAME), &
            status='replace', access='stream', iostat=ierr, action='write' )
    end if
    
    if( is_output_sur ) then
       call stdlib__getio( iosnp, is_big=.true. )
       open( IOSNP, file=trim(trim(ODIR)//'/'//SNAME), &
            status='replace', access='stream', iostat=ierr, action='write' )
    end if
    
    if( is_output_wav ) then
       call stdlib__getio( iowav, is_big=.true. )
       open( IOWAV, file=trim(trim(ODIR)//'/'//WNAME), &
            status='replace', access='stream', iostat=ierr, action='write' )
       
       stlo = -12345.0
       stla = -12345.0
       stdp = 0.0_PN
       
       ! set-up the header part of the wave file
       write(iowav) len_trim(title)
       write(iowav) trim(title)
       write(iowav) ntmax/nskip + 1 ! including t = 0
       write(iowav) DT * nskip      ! decimated sample
       write(iowav) nst * 4         ! vx, vz, ux, uz
       write(iowav) istx(1:nst), istx(1:nst), istx(1:nst), istx(1:nst)
       write(iowav) isty(1:nst), isty(1:nst), isty(1:nst), isty(1:nst)
       write(iowav) istz(1:nst)-kfsz(1:nst), istz(1:nst)-kfsz(1:nst), &
            istz(1:nst)-kfsz(1:nst), istz(1:nst)-kfsz(1:nst)
       write(iowav) dx, dy, dz
       write(iowav) I0, j0, K0 
       write(iowav) stlo(1:nst), stlo(1:nst), stlo(1:nst), stlo(1:nst)
       write(iowav) stla(1:nst), stla(1:nst), stla(1:nst), stla(1:nst)
       write(iowav) stdp(1:nst), stdp(1:nst), stdp(1:nst), stdp(1:nst)
       write(iowav) stnm(1:nst), stnm(1:nst), stnm(1:nst), stnm(1:nst)
       write(iowav) cmpnm1(1:nst),cmpnm2(1:nst),cmpnm3(1:nst),cmpnm4(1:nst)
       
       vxall = 0.0_PN
       vzall = 0.0_PN
    end if
    
    ! Ground Surface Snap
    do k = 1, NZS
       do i = 1, NXS
          ii = I*NXD
          kk = K*NZD
          vpsnap (i,k) = RIG (ii,kk)
          vssnap (i,k) = DEN (ii,kk)
       end do
    end do
    
    if( is_output_sps ) then
       write ( IOSPS ) vpsnap
       write ( IOSPS ) vssnap
    end if
    
    if( is_output_sur ) then
       write ( IOSNP ) vpsnap
       write ( IOSNP ) vssnap
    end if

    
  end subroutine ppohFDM_output__setup


  subroutine ppohFDM_output__close_files()
    !
    ! Closes an output file.
    !
    
    close( IOSPS )
    close( IOSNP )
    close( IOWAV )
    
  end subroutine ppohFDM_output__close_files


  subroutine ppohFDM_output__write_snap(it)
    !
    ! Dumps snapshots of a seismic wavefield at time step "it"
    !
    integer, intent(in) :: it

    integer :: ns, isx, isz
    integer :: i, k, ii, kk, j
    character(len=80) filename

    
    !! Timeseries
    if( is_output_wav ) then
       
       do ns = 1, NST                                                 
          isx  =  istx ( ns )                                           
          isz  =  istz ( ns ) 
          
          vxall0 = vxall( ns ) ! store previous value
          vzall0 = vzall( ns ) ! store previous value
          vxall  ( ns ) =  vx (isx,isz)
          vzall  ( ns ) =  vz (isx,isz) 
          
          if( it == 1 ) then
             uxall ( ns ) = vxall( ns ) * 0.5_PN * DT
             uzall ( ns ) = vzall( ns ) * 0.5_PN * DT 
          else
             uxall (NS) = uxall(NS) + ( vxall(NS)+vxall0 )/2*DT
             uzall (NS) = uzall(NS) + ( vzall(NS)+vzall0 )/2*DT
          end if
          
       end do

       
       if( mod(it-1, NSKIP) == 0 ) then
          write(IOWAV) real( vxall(1:NST) )
          write(IOWAV) real( vzall(1:NST) )
          write(IOWAV) real( uxall(1:NST) )
          write(IOWAV) real( uzall(1:NST) )
       end if

       
       if( mod( it-1, NSKIP ) == 0 ) then
          !       write(filename,'("seismic_2d-result",I4.4".dat")')it
          write(filename,'("./out/Seism2D-result.dat")')
          !       open(7,file=filename,status='replace')
          if(it==1) then
             open(7,file=filename,status='unknown')
             write(7, '("#station number station_x station_z Dt Vx Vz")')
             write(7, '("Time="F10.6)')(it-1)*dt
          else
             open(7,file=filename,access='append')
             write(7, '("Time="F10.6)')(it-1)*dt
          end if
          do j=1, NST
             !             write(7,'(I0" "I0" "I0" "E10.2" "E10.2" "E10.2)') j, istx(j), istz(j), (i-1)*dt, vxall(j), vzall(j)
             write(7,'(3I5,F15.6,2ES17.8)') j, istx(j), istz(j), (it-1)*dt, vxall(j), vzall(j)
          end do
          close(7)
       end if
       
    end if

    
    if( is_output_sur ) then
       !! Displacement: Numerical Integration for Surface Snapshot
       do k = 1, NZS
          do i = 1, NXS
             ii = I*NXD
             kk = K*NZD
             vxsnap(i,k) = vxsnap(i,k) + VX(ii,kk)*DT
             vzsnap(i,k) = vzsnap(i,k) + VZ(ii,kk)*DT
          end do
       end do
    end if
    
    !! Prepares VP, VS snaps
    if ( mod(it-1,NWRITE) .eq. 0 ) then
       
       !! Snapshot at the ground surface
       if( is_output_sps ) then
          do k = 1, NZS
             do i = 1, NXS
                ii = i * NXD
                kk = k * NZD
                
                ! divergence( v )
                vpsnap (i,k) = abs( dxvx(ii,kk) + dzvz(ii,kk) )
                ! abs(rotx)+abs(roty)+abs(rotz)
                vssnap (i,k) = abs( dzvx(ii,kk) - dxvz(ii,kk) )
             end do
          end do
          
          !! Write Snapshot
          write( IOSPS ) real(vpsnap)
          write( IOSPS ) real(vssnap)
       end if
       
       if( is_output_sur ) then
          write( IOSNP ) real(vxsnap)
          write( IOSNP ) real(vzsnap)
       end if

    end if
    
  end subroutine ppohFDM_output__write_snap


end module ppohFDM_m_output
