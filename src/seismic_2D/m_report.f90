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
module ppohFDM_m_report
  !
  ! This module displays simulation parameters and computational time on the screen
  !
  use ppohFDM_m_stdlib
  use ppohFDM_m_params
  !, only : NX1, NZ1, NX, NZ, DT, DX, DZ, NTMAX, NPM, KFS
  use ppohFDM_m_medium
  !, only : kfsz
  use ppohFDM_m_output, only : NWRITE
  use ppohFDM_m_comvar
  implicit none
  private

  !=Public Procedures
  public :: ppohFDM_report__welcome
  public :: ppohFDM_report__goodby
  public :: ppohFDM_report__stblcnd
  public :: ppohFDM_report__progress
  public :: ppohFDM_report__init_counter
  public :: ppohFDM_report__output_prm

  !=Internal variables
  !<< Lapse Time Measurement >>
  integer,  save :: timcount, timcount0, timprev, crate
  real(PN), save :: tstep, ttotal
  real(PN), save :: etas
  integer,  save :: etah, etam, etasi
  
  
contains
  
  !!--------------------------------------------------------------------------!!
  !!                             PUBLIC PROCEDURES                            !!
  !!--------------------------------------------------------------------------!!
  
  subroutine ppohFDM_report__output_prm
    !
    ! Displays simulation parameters
    !
    use ppohFDM_m_params
    !, only: title, nx, nz, dx, dz, dt, ntmax, npm
    use ppohFDM_m_source
    !, only: i0, k0, strike, dip, rake, rmo, at, t0
    use ppohFDM_m_output
    !, only: nskip, ntskp, nwrite, nst, nxd, nzd, nxs ,nzs, &
    !     wname, oname, sname, odir
    
    integer, parameter :: io = 86
    
    open( io, file = trim(odir)//'/'//trim(title)//'.prm', action='write')
    
    write(io,*) '<TITLE>    ', trim(title)
    write(io,*) '<NX>       ', NX
    write(io,*) '<NZ>       ', NZ
    write(io,*) '<DX>       ', DX
    write(io,*) '<DZ>       ', DZ
    write(io,*) '<DT>       ', DT
    write(io,*) '<NTMAX>    ', NTMAX
    write(io,*) '<NPM>      ', NPM
    write(io,*) '<I0>       ', i0
    write(io,*) '<K0>       ', k0
    write(io,*) '<STRIKE>   ', STRIKE
    write(io,*) '<DIP>      ', DIP
    write(io,*) '<RAKE>     ', RAKE
    write(io,*) '<RMO>      ', rmo
    write(io,*) '<AT>       ', AT
    write(io,*) '<T0>       ', T0
    write(io,*) '<NSKIP>    ', nskip
    write(io,*) '<NTSKP>    ', ntskp
    write(io,*) '<NWRITE>   ', nwrite
    write(io,*) '<NST>      ', nst
    write(io,*) '<NXD>      ', nxd
    write(io,*) '<NZD>      ', nzd
    write(io,*) '<NXS>      ', nxs
    write(io,*) '<NZS>      ', nzs
    write(io,*) '<WNAME>    ', trim(wname)
    write(io,*) '<ONAME>    ', trim(oname)
    write(io,*) '<SNAME>    ', trim(sname)
    
    close( io )
    
  end subroutine ppohFDM_report__output_prm


  subroutine ppohFDM_report__welcome()
    !
    ! Displays the simulation start time
    !
    
    write(STDERR,'(A)') "------------------------------------------------------"
    write(STDERR,'(A)') "           PROGRAM Seism2D Serial Elastic"
    write(STDERR,'(A)') "------------------------------------------------------"
    write(STDERR,*)
    write(STDERR,'(A,I5,A,I5,A,I5)') "  MODEL  SIZE: ", NX, "x", NZ
    write(STDERR,*)
    
  end subroutine ppohFDM_report__welcome


  subroutine ppohFDM_report__goodby()
    !
    ! Displays the simulation end time
    !
    call system_clock( timcount, crate )
    ttotal = ttotal + tstep
    write(STDERR,'(A,I15,A)') &
         "Finished Computation. Total Time = ", int(ttotal), " sec."
  end subroutine ppohFDM_report__goodby


  subroutine ppohFDM_report__stblcnd
    !
    ! Examines availability of simulation parameters to satisfy computational stability conditions
    !
    
    write(STDERR,'(A,F10.5)') "STABLE CONDITION (SHOULD BE SMALLER THAN 1)", &
         DT/( 0.45*max(DX,DZ)/maxval(sqrt((LAM+2*RIG)/DEN)))
    if( DT > 0.45*max(DX,DZ)/maxval(sqrt((LAM+2*RIG)/DEN)) ) then
       write(STDERR,*) "Enlarge Spatial Grid and/or Shorten Time Grid!"
    end if
    write(STDERR,*) 
    
  end subroutine ppohFDM_report__stblcnd


  subroutine ppohFDM_report__init_counter()
    !
    ! Starts measurement of time
    !
    
    call system_clock( timcount, crate )
    timcount0 = timcount
    timprev   = timcount
    ttotal = 0
    
  end subroutine ppohFDM_report__init_counter


  subroutine ppohFDM_report__progress( it )
    !
    ! Displays the progress of calculation
    !
    
    integer, intent(in) :: it
    real(PN), save :: xmax, zmax
    
    !! elapsed time measurement
    if( mod(IT, NWRITE) == 0 ) then
       
       call system_clock( timcount, crate )
       tstep = real( timcount - timprev ) / real( crate )
       ttotal = ttotal + tstep
       etas   = (ntmax -it+1)/ real(it) &
            * (timcount-timcount0)/real( crate )
       etah = int( etas/(   60*60) ); etas = etas - etah   *60*60
       etam = int( etas/(      60) ); etas = etas - etam      *60
       etasi = int(etas)
       timprev = timcount
       
       call get_max( xmax, zmax )
       
       write(STDERR,'(A,I6,A,I6,A,I2.2,A,I2.2,A,I2.2,A,F9.4,A,2ES9.2,A)') &
            "it=(", it, "/",NTMAX,"), ETA=", &
            etah,":",etam,":",etasi, &
            ", Time/Step=", ttotal / (it), "[s] , MAX = ( ", &
            xmax, zmax, " )"
       
    end if
    
  end subroutine ppohFDM_report__progress
  
  
  !!--------------------------------------------------------------------------!!
  !!                            PRIVATE PROCEDURES                            !!
  !!--------------------------------------------------------------------------!!
  subroutine get_max( xmax,  zmax )
    !
    ! Getting the maximum amplitude for the confirmation screen display
    !
    real(PN), intent(inout) :: xmax, zmax
    logical, save :: is_first = .true.
    integer :: i
    
    if( is_first ) then
       xmax = 0.0
       zmax = 0.0
       is_first = .false.
    end if
    
    do i=NPM+1, NX-NPM
       xmax = max(  abs(vx(i,kfsz(i)+1)) , xmax )
       zmax = max(  abs(vz(i,kfsz(i)+1)) , zmax )
    end do
    
  end subroutine get_max
  
end module ppohFDM_m_report
