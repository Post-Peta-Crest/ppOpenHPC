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
module ppohFDM_m_swatch
  !
  ! This module measures the computational time of each module
  !
  use ppohFDM_m_stdlib
  implicit none
  private

  !=Public Procedures
  public :: ppohFDM_swatch__setup
  public :: ppohFDM_swatch__setnm
  public :: ppohFDM_swatch__on
  public :: ppohFDM_swatch__off
  public :: ppohFDM_swatch__report  

  !=Private Variables
  !<< Total elapsed time measurement >>
  integer  :: timcount, timcount0, timprev, tstep
  real(PN) :: etas
  integer  :: etah, etam, etasi
  
  !<< Elapsed time for each routine >>
  type ttim
     character(20) :: title
     integer       :: c0
     integer       :: c1
     real          :: ttotal  = 0.0_PN
     logical       :: is_used = .false.
  end type ttim
  
  logical, save :: measure_time ! switch
  integer, save :: cpu
  integer, save :: crate
  integer, save :: cmax
  integer, parameter :: NBLOCK_MAX =999
  type(ttim), save  :: t(NBLOCK_MAX)
  
contains
  

  subroutine ppohFDM_swatch__setup( sw )
    !
    ! Initialize elapsed time counter
    !
    logical, intent(in) :: sw 

    integer :: i

    call system_clock( timcount, crate )
    timcount0 = timcount
    timprev = timcount
    
    ! keep mtime flag
    measure_time = sw
    
    do i=1, NBLOCK_MAX
       t(i)%ttotal = 0.0_PN
       t(i)%is_used = .false.
    end do
    
    
  end subroutine ppohFDM_swatch__setup


  subroutine ppohFDM_swatch__setnm( i, nm )
    !
    ! Set user-defined name to i-th block
    !
    integer, intent(in) :: i
    character(*), intent(in) :: nm

    t(i)%title = trim(adjustl(nm))
  end subroutine ppohFDM_swatch__setnm


  subroutine ppohFDM_swatch__on( i, nm )
    !
    ! Start time measurement for i-th block
    !
    integer, intent(in) :: i
    character(*), intent(in), optional :: nm
    
    if( .not. measure_time ) return
    
    if( present(nm) ) call ppohFDM_swatch__setnm(i,nm)
    call system_clock( t(i)%c0, crate, cmax )
    t(i)%is_used = .true.
    
  end subroutine ppohFDM_swatch__on


  subroutine ppohFDM_swatch__off( i )
    !
    ! Stop measurement for i-th block
    ! Computation time from the previous call of swatch_on is accumlated 
    !
    integer, intent(in) :: i

    if( .not. measure_time ) return
    call system_clock( t(i)%c1, crate, cmax )
    if( t(i)%c1 < t(i)%c0 ) t(i)%c1 = t(i)%c1 + cmax
    t(i)%ttotal = t(i)%ttotal + ( t(i)%c1-t(i)%c0 )/real(crate)
  end subroutine ppohFDM_swatch__off


  subroutine ppohFDM_swatch__report( io, ishead )
    !
    ! Report the total computation time and their occupation rate to unit io
    ! Two lines of header is added if ishead = .true.
    !
    integer, optional, intent(in) :: io
    logical, optional, intent(in) :: ishead

    real :: tsum
    real :: trate(NBLOCK_MAX)
    integer :: i
    integer :: io_out


    if( present(io) ) then
       io_out = io
    else
       io_out = STDERR
    end if
    
    
    if( .not. measure_time ) return
    tsum = sum( t(:)%ttotal )
    trate(:) = t(:)%ttotal / tsum * 100.0
    
    if( present( ishead ) ) then
       if( ishead ) then
          write(io,'(A)') '  CPU      #ID         Procedure Name      Real Time[s]  Total Time[s]  Occupation [%]   Total Occp [%] '
          write(io,'(A)') '-------+-------+-----------------------+--------------+--------------+----------------+----------------'
       end if
    end if
    
    do i=1, NBLOCK_MAX
       if( t(i)%is_used ) then
          write(io_out,'(2I7.5,A22,4F15.3)') cpu, i, t(i)%title, &
               t(i)%ttotal, sum(t(1:i)%ttotal), trate(i), sum(trate(1:i))
       end if
       
    end do
    
  end subroutine ppohFDM_swatch__report
  
end module ppohFDM_m_swatch
