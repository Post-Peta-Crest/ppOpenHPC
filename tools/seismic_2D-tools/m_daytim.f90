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
module m_daytim
  use ppohFDM_m_stdlib
  implicit none
  private

  !=Public Routines
  public :: daytim__isLeapYear
  public :: daytim__ymd2jul
  public :: daytim__jul2md
  public :: daytim__dayweek
  public :: daytim__timelocal
  public :: daytim__localtime
  public :: daytim__getDate

  !=Private Variables
  integer :: timeCount_0                    ! initial  timecount
  integer :: timeCount_p                    ! previous timecount

  !-----------------------------------------------------------------[ public ]-!
  interface daytim__getdate
     
     module procedure  getdate_c, getdate_i1, getdate_i2
     
  end interface daytim__getdate
  !----------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------[ public ]-!
  interface daytim__dayweek
     
     module procedure  dayweek_i, dayweek_a
     
  end interface daytim__dayweek
  !----------------------------------------------------------------------------!
  
contains
  
  !-----------------------------------------------------------------[ public ]-!
  subroutine daytim__isLeapYear( year, isLeap )
    integer, intent(in)  :: year
    logical, intent(out) :: isLeap
    
    if( mod( year, 100 ) == 0 .and.  mod( year, 400 ) == 0 ) then
       isLeap = .true.
    else if( mod(year, 100 ) /= 0 .and. mod( year, 4 ) == 0 ) then
       isLeap = .true.
    else
       isLeap = .false.
    end if
    
  end subroutine daytim__isLeapYear
  !----------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------[ public ]-!
  subroutine daytim__ymd2jul( year, month, day, julday )
    integer, intent(in)  :: year
    integer, intent(in)  :: month
    integer, intent(in)  :: day
    integer, intent(out) :: julday
 
    !=Variables
    integer :: dom(12)  ! day of month
    logical :: is_leap  ! leap year ?
    integer :: i


    call daytim__isLeapYear( year, is_leap )
    
    if( is_leap ) then
       dom = (/31,29,31,30,31,30,31,31,30,31,30,31/)
    else
       dom = (/31,28,31,30,31,30,31,31,30,31,30,31/)
    end if
    
    ! initialize
    julday = 0
    
    ! error handling
    if( day > dom(month) ) then
       write(STDERR,*) 'ymd2jul: input day excees the last day of the month'
       julday = 0
       return
    end if
    if( month > 12 ) then
       write(STDERR,*) 'ymd2jul: month excees 12 !'
       julday = 0
       return
    end if
    
    ! month
    do i=1,month-1
       julday = julday + dom(i)
    end do
    
    ! day
    julday = julday + day
    
  end subroutine daytim__ymd2jul
  !----------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------[ public ]-!
  subroutine daytim__jul2md( julday, year, month, day )
    integer, intent(in)  :: julday
    integer, intent(in)  :: year
    integer, intent(out) :: month
    integer, intent(out) :: day

    !=Variables
    integer :: dom(12)  ! day of month
    logical :: is_leap   ! check for uruu doshi


    call daytim__isLeapYear( year, is_leap )
    
    if( is_leap ) then
       dom = (/31,29,31,30,31,30,31,31,30,31,30,31/)
    else
       dom = (/31,28,31,30,31,30,31,31,30,31,30,31/)
    end if
    
    if( is_leap .and. julday > 366 ) then
       write(STDERR,*) 'jul2md: day of year excees Dec. 31'
       month = 0
       day   = 0
       return
    else if ( .not. is_leap ) then
       if( julday > 365 ) then
          write(STDERR,*) 'jul2md: day of year excees Dec. 31'
          month = 0
          day   = 0
          return
       end if
       
    end if
    
    month = 1
    
    do  while( sum( dom(1:month) ) < julday )
       month = month + 1
    end do
    
    day = julday - sum(dom(1:month-1))
    
  end subroutine daytim__jul2md
  !----------------------------------------------------------------------------!
    
  !----------------------------------------------------------------[ private ]-!
  subroutine dayweek_i( year, month, day, dw )
    integer, intent(in)  :: year
    integer, intent(in)  :: month
    integer, intent(in)  :: day
    integer, intent(out) :: dw

    integer :: y, m, d

    if( month<=0 .or. month>=14 ) then
       write(STDERR,'(A)') 'subroutine dayweek: invalied argument'
       dw = -1
       return
    end if
    if( month==1 .or. month==2 ) then
       y = year-1
       m = month + 12
       d = day
    else
       y = year
       m = month
       d = day
    end if
    
    dw = y + int(y/4.) - int(y/100.) + int(y/400.)  + &
         int( (26*m+16)/10.) + d
    dw = mod( dw, 7 )
    
  end subroutine dayweek_i
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine dayweek_a( year, month, day, dw )
    integer, intent(in)  :: year
    integer, intent(in)  :: month
    integer, intent(in)  :: day
    character(*), intent(out) :: dw

    integer :: idw
    character(9) :: dwname(0:6) = (/'Sunday   ', &
                                    'Monday   ', &
                                    'Tuesday  ', &
                                    'Wednesday', &
                                    'Thursday ', &
                                    'Friday   ', &
                                    'Saturday ' /)

    call dayweek_i( year, month, day, idw )
    dw = dwname(idw )
  end subroutine dayweek_a
  !----------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------[ public ]-!
  subroutine daytim__timelocal( year, month, day, hour, min, sec, tim )
    integer, intent(in)  :: year 
    integer, intent(in)  :: month
    integer, intent(in)  :: day
    integer, intent(in)  :: hour
    integer, intent(in)  :: min
    integer, intent(in)  :: sec
    integer, intent(out) :: tim

    !=Variables
    integer :: doy
    integer :: jday
    integer :: i
 
    tim = 0
    if( year > 1970 ) then
       do i = 1970, year-1
          call daytim__ymd2jul( i, 12, 31, doy )
          tim = tim + doy * 24 * 60 * 60
       end do
    end if
    call daytim__ymd2jul( year, month, day, jday )
    tim = tim + ( (jday-1) * 24 * 60 + hour*60 + min )*60 + sec
    
  end subroutine daytim__timelocal
  !----------------------------------------------------------------------------!
  
  !-----------------------------------------------------------------[ public ]-!
  subroutine daytim__localtime( tim, year, month, day, hour, min, sec )
    integer, intent(in)  :: tim
    integer, intent(out) :: year 
    integer, intent(out) :: month
    integer, intent(out) :: day
    integer, intent(out) :: hour
    integer, intent(out) :: min
    integer, intent(out) :: sec

    !=Variables
    integer :: doy, soy, ttim, jday

    ttim = tim
    year = 1970
    
    do 
       call daytim__ymd2jul(year,12,31,doy)
       soy = doy*24*60*60
       if(ttim<soy) then
          exit
       else
          year = year + 1
          ttim  = ttim - soy
       end if
       
    end do
    
    jday = floor(ttim / 86400.) +1
    call daytim__jul2md( jday, year, month, day )
    
    ttim = ttim - (jday-1) * 86400
    hour = floor(ttim / 3600.)
    ttim = ttim - hour * 3600
    min =  floor( ttim / 60. )
    sec = ttim - min * 60
    
  end subroutine daytim__localtime
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine getDate_c( date )
    ! 
    ! Return date and time in formatted characters
    ! example: 
    ! 2005/07/31 15:32:53 
    !

    character(20), intent(out) :: date

    character(8)  :: ymd
    character(10) :: hms
    call date_and_time( ymd, hms )
    date = ymd(1:4)//'/'//ymd(5:6)//'/'//ymd(7:8)//' '//&
         hms(1:2)//':'//hms(3:4)//':'//hms(5:6)
  end subroutine getDate_c
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine getDate_i1( yr, mo, dy, hr, mi, sc )
    ! 
    ! Return date and time as intergers in the following order: 
    ! year, month, day, hour, min, sec
    !
    integer, intent(out) :: yr, mo, dy, hr, mi, sc

    character(8)  :: ymd
    character(10) :: hms

    call date_and_time( ymd, hms )
    read(ymd(1:4),*) yr
    read(ymd(5:6),*) mo
    read(ymd(7:8),*) dy
    read(hms(1:2),*) hr
    read(hms(3:4),*) mi
    read(hms(5:6),*) sc
    
  end subroutine getDate_i1
  !----------------------------------------------------------------------------!
  
  !----------------------------------------------------------------[ private ]-!
  subroutine getDate_i2( itim )
    ! 
    ! Return date and time as intergers in the localtime integer
    !
    integer, intent(out) :: itim

    integer :: yr, mo, dy, hr, mi, sc
    character(8)  :: ymd
    character(10) :: hms

    call date_and_time( ymd, hms )
    read(ymd(1:4),*) yr
    read(ymd(5:6),*) mo
    read(ymd(7:8),*) dy
    read(hms(1:2),*) hr
    read(hms(3:4),*) mi
    read(hms(5:6),*) sc
    
    call daytim__timelocal( yr, mo, dy, hr, mi, sc, itim )
    
  end subroutine getDate_i2
  
end module m_daytim
