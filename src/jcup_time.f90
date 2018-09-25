!=======+=========+=========+=========+=========+=========+=========+=========+

!
!Copyright (c) 2011, arakawa@rist.jp
!All rights reserved.
!
! 2014/06/27 add milli_sec and micro_sec 
!
module jcup_time
  private

!--------------------------------   public  ----------------------------------!

  public :: TU_SEC          ! time unit second 2014/07/03 [ADD]
  public :: TU_MIL          ! time unit milli second 2014/07/03 [ADD]
  public :: TU_MCR          ! time unit micro second 2014/07/03 [ADD]

  public :: time_type       ! type 
  public :: set_time_unit   ! subroutine (default_time_unit)
  public :: get_time_unit   ! integer function ()

  public :: set_time_data   ! subroutine (tm, yyyy, mo, dd, hh, mm, ss, milli_sec, micro_sec) ! 2014/06/27 [MOD]
  public :: get_time_data   ! subroutine (tm, yyyy, mo, dd, hh, mm, ss, milli_sec, micro_sec) ! 2014/06/27 [MOD]
  public :: operator(==)    ! logical function (t1, t2)
  public :: operator(/=)    ! logical function (t1, t2)
  public :: operator(>=)    ! logical function (t1, t2)
  public :: operator(>)     ! logical function (t1, t2)
  public :: operator(<=)    ! logical function (t1, t2)
  public :: operator(<)     ! logical function (t1, t2)
  public :: TimeToSecond ! integer(kind=8) function (time)
  public :: DateToTimeStr
  public :: inc_calendar ! subroutine (now_time, delta_t) ! 2014/10/29 [ADD]
  public :: inc_time     ! subroutine (now_time, delta_t) ! 2014/07/04 [ADD] now_time%ss = now_time%ss + delta_t
  public :: cal_time_diff ! subroutine (time1, time2, diff_sec, diff_mil, diff_mcr) ! 2014/07/10 [ADD]


  public :: init_all_time   ! subroutine (comp_id)
  public :: init_each_time  ! subroutine (comp_id, domain_id)
  public :: set_start_time ! subroutine (component_id, domain_id, time)
  public :: get_start_time ! subroutine (component_id, domain_id, time)
  public :: set_current_time ! subroutine (component_id, domain_id, time, delta_t)
  public :: get_current_time ! subroutine (component_id, domain_id, time)
  public :: get_before_time  ! subroutine (component_id, domain_id, time)
  public :: get_delta_t      ! subroutine (component_id, domain_id, delta_t)
  public :: is_before_exchange_step ! logical function (component_id, domain_id, interval)
  public :: is_exchange_step ! logical functiuon (component_id, domain_id, interval)
  public :: is_exchange_step_from_c_time ! logical function (component_id, domain_id, intervar, current_time)
  public :: is_next_exchange_step
  public :: cal_next_exchange_time ! subroutine (component_id, domain_id, interval, time)
  public :: destruct_all_time
  public :: write_time ! subroutine (file_id, comp_id) 2013.06.10 [ADD]
  public :: read_time  ! subroutine (file_id, comp_id) 2013.06.11 [ADD]

!--------------------------------   private  ---------------------------------!

  interface set_start_time
    module procedure set_start_time_str, set_start_time_date
  end interface

  interface get_start_time
    module procedure get_start_time_str, get_start_time_date, get_start_time_type
  end interface

  interface SetEndTime
    module procedure set_end_time_str, set_end_time_date
  end interface

  interface GetEndTime
    module procedure get_end_time_str, get_end_time_date
  end interface

  interface set_current_time
    module procedure set_current_time_str, set_current_time_date, set_current_time_type
  end interface

  interface get_current_time
    module procedure get_current_time_str, get_current_time_date, get_current_time_type
  end interface

  interface get_before_time
    module procedure get_before_time_str, get_before_time_date, get_before_time_type
  end interface

  interface operator(==)
    module procedure EqualTime
  end interface

  interface operator(/=)
    module procedure NotEqualTime
  end interface

  interface operator(>=) 
    module procedure GETime
  end interface

  interface operator(>) 
    module procedure GTTime
  end interface

  interface operator(<=) 
    module procedure LETime
  end interface

  interface operator(<) 
    module procedure LTTime
  end interface

  ! comment out 2014/06/27
  interface TimeStrToDate
    module procedure time_str_to_date1, time_str_to_date2
  end interface

  interface DateToTimeStr
    module procedure date_to_time_str1, date_to_time_str2
  end interface

  integer, parameter :: TU_SEC = 0 ! delta_t second
  integer, parameter :: TU_MIL = 1 ! delta_t milli second
  integer, parameter :: TU_MCR = 2 ! delat_t micro second

  integer :: time_unit = -1 !

  type time_type
    integer :: yyyy = 0 ! 2015/06/12 [MOD]
    integer :: mo = 0
    integer :: dd = 0
    integer :: hh = 0
    integer :: mm = 0
    integer(kind=8) :: ss = 0! 2014/10/29 [MOD]
    integer :: milli_sec = 0
    integer :: micro_sec = 0
    integer :: delta_t = 0
  end type

  type model_time_type
    type(time_type) :: start_time
    type(time_type) :: end_time
    type(time_type) :: current_time
    type(time_type) :: before_time
  end type

  type model_time
    type(model_time_type),pointer,dimension(:) :: tm ! array size: number of component
  end type
  
  type(model_time),pointer,dimension(:),private :: time

  integer,parameter,private :: BASIC_YEAR = 1000

  private :: set_time
  private :: set_start_time_str
  private :: set_start_time_date
  private :: get_start_time_str
  private :: get_start_time_date
  private :: set_end_time_str
  private :: set_end_time_date
  private :: get_end_time_str
  private :: get_end_time_date
  private :: set_current_time_str
  private :: set_current_time_date
  private :: get_current_time_str
  private :: get_current_time_date
  private :: time_str_to_date1
  private :: time_str_to_date2
  private :: date_to_time_str1
  private :: date_to_time_str2
  private :: GetYearDate
  private :: GetMonthDate

contains

!---------------------   time_type related procedures  -----------------------!

!=======+=========+=========+=========+=========+=========+=========+=========+
! add 2014/07/03
subroutine set_time_unit(default_time_unit)
  implicit none
  integer, intent(IN) :: default_time_unit

  time_unit = default_time_unit

end subroutine set_time_unit

!=======+=========+=========+=========+=========+=========+=========+=========+
! add 2014/07/03
integer function get_time_unit()
  implicit none
  
  get_time_unit = time_unit

end function get_time_unit


!=======+=========+=========+=========+=========+=========+=========+=========+
! mod 2014/06/27
subroutine set_time_data(tm, yyyy, mo, dd, hh, mm, ss, milli_sec, micro_sec)
  implicit none
  type(time_type), intent(INOUT) :: tm
  integer, intent(IN) :: yyyy, mo, dd, hh, mm
  integer(kind=8) :: ss
  integer, optional, intent(IN) :: milli_sec, micro_sec

  tm%yyyy = yyyy
  tm%mo   = mo
  tm%dd   = dd
  tm%hh   = hh
  tm%mm   = mm
  tm%ss   = ss
  
  tm%milli_sec = 0
  tm%micro_sec = 0

  if (present(milli_sec)) tm%milli_sec = milli_sec
  if (present(micro_sec)) tm%micro_sec = micro_sec

end subroutine set_time_data

!=======+=========+=========+=========+=========+=========+=========+=========+
! mod 2014/06/27
subroutine set_time(t1, t2)
  implicit none
  type(time_type), intent(INOUT) :: t1
  type(time_type), intent(IN) :: t2

  t1%yyyy = t2%yyyy
  t1%mo   = t2%mo
  t1%dd   = t2%dd
  t1%hh   = t2%hh
  t1%mm   = t2%mm
  t1%ss   = t2%ss
  t1%milli_sec = t2%milli_sec
  t1%micro_sec = t2%micro_sec

  t1%delta_t = t2%delta_t

end subroutine set_time

!=======+=========+=========+=========+=========+=========+=========+=========+
! mod 2014/06/27
subroutine get_time_data(tm, yyyy, mo, dd, hh, mm, ss, milli_sec, micro_sec)
  implicit none
  type(time_type), intent(IN) :: tm
  integer, intent(INOUT) :: yyyy, mo, dd, hh, mm
  integer(kind=8) :: ss
  integer, optional, intent(INOUT) :: milli_sec, micro_sec

  yyyy = tm%yyyy
  mo   = tm%mo
  dd   = tm%dd
  hh   = tm%hh
  mm   = tm%mm
  ss   = tm%ss

  if (present(milli_sec)) milli_sec = tm%milli_sec
  if (present(micro_sec)) micro_sec = tm%micro_sec

end subroutine get_time_data

!=======+=========+=========+=========+=========+=========+=========+=========+
! mod 2014/06/27
logical function EqualTime(t1,t2)
  implicit none
  type(time_type),intent(IN) :: t1, t2

  EqualTime = .false.
  if (t1%yyyy /= t2%yyyy) return
  if (t1%mo   /= t2%mo  ) return
  if (t1%dd   /= t2%dd  ) return
  if (t1%hh   /= t2%hh  ) return
  if (t1%mm   /= t2%mm  ) return
  if (t1%ss   /= t2%ss  ) return
  if (t1%milli_sec /= t2%milli_sec) return
  if (t1%micro_sec /= t2%micro_sec) return
  
  EqualTime = .true.

end function EqualTime

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function NotEqualTime(t1,t2)
  implicit none
  type(time_type),intent(IN) :: t1, t2

  NotEqualTime = .not.EqualTime(t1,t2)

end function NotEqualTime

!=======+=========+=========+=========+=========+=========+=========+=========+
! mod 2014/06/27
logical function GETime(t1,t2)
  implicit none
  type(time_type), intent(IN) :: t1, t2
 
  if (t1%yyyy /= t2%yyyy) then
    GETime = (t1%yyyy > t2%yyyy)
    return
  end if
  if (t1%mo /= t2%mo) then
    GETime = (t1%mo > t2%mo)
    return
  end if
  if (t1%dd /= t2%dd) then
    GETime = (t1%dd > t2%dd)
    return
  end if
  if (t1%hh /= t2%hh) then
    GETime = (t1%hh > t2%hh)
    return
  end if
  if (t1%mm /= t2%mm) then
    GETime = (t1%mm > t2%mm)
    return
  end if
  if (t1%ss /= t2%ss) then
    GETime = (t1%ss > t2%ss)
    return
  end if

  if (t1%milli_sec /= t2%milli_sec) then
    GETime = (t1%milli_sec > t2%milli_sec)
    return
  end if

  if (t1%micro_sec /= t2%micro_sec) then
    GETime = (t1%micro_sec > t2%micro_sec)
    return
  end if

  GETime = .true.

end function GETime

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function LTTime(t1,t2)
  implicit none
  type(time_type), intent(IN) :: t1, t2

  LTTime = .not.GETime(t1,t2)

end function LTTime

!=======+=========+=========+=========+=========+=========+=========+=========+
! mod 2014/06/27
logical function LETime(t1,t2)
  implicit none
  type(time_type), intent(IN) :: t1, t2

  if (t1%yyyy /= t2%yyyy) then
    LETime = (t1%yyyy < t2%yyyy)
    return
  end if
  if (t1%mo /= t2%mo) then
    LETime = (t1%mo < t2%mo)
    return
  end if
  if (t1%dd /= t2%dd) then
    LETime = (t1%dd < t2%dd)
    return
  end if
  if (t1%hh /= t2%hh) then
    LETime = (t1%hh < t2%hh)
    return
  end if
  if (t1%mm /= t2%mm) then
    LETime = (t1%mm < t2%mm)
    return
  end if
  if (t1%ss /= t2%ss) then
    LETime = (t1%ss < t2%ss)
    return
  end if

  if (t1%milli_sec /= t2%milli_sec) then
    LETime = (t1%milli_sec < t2%milli_sec)
    return
  end if

  if (t1%micro_sec /= t2%micro_sec) then
    LETime = (t1%micro_sec < t2%micro_sec)
    return
  end if
  
  LETime = .true.

end function LETime

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function GTTime(t1,t2)
  implicit none
  type(time_type), intent(IN) :: t1, t2

  GTTime = .not.LETime(t1,t2)

end function GTTime

!=======+=========+=========+=========+=========+=========+=========+=========+
! mod 2014/07/03
! 2014/10/22 [MOD]
subroutine time_str_to_date1(time_str,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)
  use jcup_utils, only : error
  implicit none
  character(len=*),intent(IN) :: time_str
  integer,intent(INOUT) :: yyyy,mo,dd,hh,mm
  integer(kind=8), intent(INOUT) :: ss
  integer, intent(INOUT) :: milli_sec, micro_sec

  milli_sec = 0
  micro_sec = 0

  select case(len_trim(time_str))
  case(14) ! second
    !read(time_str(1:4),*) yyyy
    !read(time_str(5:6),*) mo
    !read(time_str(7:8),*) dd
    !read(time_str(9:10),*) hh
    !read(time_str(11:12),*) mm
    read(time_str(1:14),*) ss
  case(17) ! milli second
    !read(time_str(1:4),*) yyyy
    !read(time_str(5:6),*) mo
    !read(time_str(7:8),*) dd
    !read(time_str(9:10),*) hh
    !read(time_str(11:12),*) mm
    read(time_str(1:14),*) ss
    read(time_str(15:17),*) milli_sec
  case(20)
    !read(time_str(1:4),*) yyyy
    !read(time_str(5:6),*) mo
    !read(time_str(7:8),*) dd
    !read(time_str(9:10),*) hh
    !read(time_str(11:12),*) mm
    read(time_str(1:14),*) ss
    read(time_str(15:17),*) milli_sec
    read(time_str(18:20),*) micro_sec
  case default
    call error("time_tr_to_date1", "time_str length error")
  end select

end subroutine time_str_to_date1

!=======+=========+=========+=========+=========+=========+=========+=========+
! mod 2014/07/03
! 2014/10/22 [MOD]
subroutine time_str_to_date2(time_str,date)
  use jcup_utils, only : error
  implicit none
  character(len=*),intent(IN)    :: time_str
  type(time_type) ,intent(INOUT) :: date

  date%milli_sec = 0 
  date%micro_sec = 0

  select case(len_trim(time_str))
  case(14)
    !read(time_str(1:4),*) date%yyyy
    !read(time_str(5:6),*) date%mo
    !read(time_str(7:8),*) date%dd
    !read(time_str(9:10),*) date%hh
    !read(time_str(11:12),*) date%mm
    read(time_str(1:14),*) date%ss
  case(17)
    !read(time_str(1:4),*) date%yyyy
    !read(time_str(5:6),*) date%mo
    !read(time_str(7:8),*) date%dd
    !read(time_str(9:10),*) date%hh
    !read(time_str(11:12),*) date%mm
    read(time_str(1:14),*) date%ss
    read(time_str(15:17),*) date%milli_sec
  case(20)
    !read(time_str(1:4),*) date%yyyy
    !read(time_str(5:6),*) date%mo
    !read(time_str(7:8),*) date%dd
    !read(time_str(9:10),*) date%hh
    !read(time_str(11:12),*) date%mm
    read(time_str(1:14),*) date%ss
    read(time_str(15:17),*) date%milli_sec
    read(time_str(18:20),*) date%micro_sec
  case default
    call error("time_tr_to_date2", "time_str length error")
  end select

end subroutine time_str_to_date2

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/04  [MOD]
! 2014/10/22 [MOD]
subroutine date_to_time_str1(time_str,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)
  implicit none
  character(len=*),intent(INOUT) :: time_str
  integer,intent(IN) :: yyyy,mo,dd,hh,mm
  integer(kind=8), intent(IN) :: ss
  integer, optional, intent(IN) :: milli_sec, micro_sec

  !write(time_str,'(i4.4,5(i2.2))') yyyy,mo,dd,hh,mm,ss
  write(time_str,'(i14.14)') ss

  if (present(milli_sec)) then
    write(time_str(15:17), '(I3.3)') milli_sec
  end if

  if (present(micro_sec)) then
    write(time_str(18:20),'(I3.3)') micro_sec
  end if

end subroutine date_to_time_str1

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/08 [MOD]
! 2014/10/22 [MOD]
subroutine date_to_time_str2(time_str,date)
  use jcup_utils, only : error
  implicit none
  character(len=*),intent(INOUT) :: time_str
  type(time_type) ,intent(IN)    :: date

  time_str(:) = " "

  !write(time_str,'(I4.4,5(I2.2))') date%yyyy,date%mo,date%dd,date%hh,date%mm,date%ss
  write(time_str,'(I14.14)') date%ss

  select case(time_unit)
  case(TU_SEC)
  case(TU_MIL)
    write(time_str(15:17),'(I3.3)') date%milli_sec
  case(TU_MCR)
    write(time_str(15:17),'(I3.3)') date%milli_sec
    write(time_str(18:20),'(I3.3)') date%micro_sec
  case default
    call error("date_to_time_str2","time_unit parameter error")
  end select

end subroutine date_to_time_str2

!=======+=========+=========+=========+=========+=========+=========+=========+

integer(kind=8) function TimeToSecond(time)
  implicit none
  type(time_type),intent(IN) :: time

  integer(kind=8) :: d_sec

  d_sec = 60*60*24

  TimeToSecond = (GetYearDate(time%yyyy)+GetMonthDate(time%yyyy,time%mo)+time%dd-1)*d_sec &
                 +time%hh*3600+time%mm*60+time%ss

end function TimeToSecond

!=======+=========+=========+=========+=========+=========+=========+=========+

integer(kind=8) function GetYearDate(yyyy)
  use jcup_utils, only : error, IntToStr
  implicit none
  integer,intent(IN) :: yyyy

  if (yyyy<BASIC_YEAR) call error("GetYearDate","year : "//trim(IntToStr(yyyy))//" should be GE BASE_YEAR")

  GetYearDate = 365*(yyyy-BASIC_YEAR)+int((yyyy-BASIC_YEAR+3)/4)

end function GetYearDate

!=======+=========+=========+=========+=========+=========+=========+=========+

integer(kind=8) function GetMonthDate(yyyy,mo)
  use jcup_utils, only : error
  implicit none
  integer,intent(IN) :: yyyy, mo

  integer :: md

  if ((mo<1).or.(mo>12)) call error("GetMonthDate","month should be 1<= <=12")

  md = 0
  if (mo> 1) md = 31
  if (mo> 2) md = md+28+int((4-mod(yyyy,4))/4)
  if (mo> 3) md = md+31
  if (mo> 4) md = md+30
  if (mo> 5) md = md+31
  if (mo> 6) md = md+30
  if (mo> 7) md = md+31
  if (mo> 8) md = md+31
  if (mo> 9) md = md+30
  if (mo>10) md = md+31
  if (mo>11) md = md+30

  GetMonthDate = md

end function GetMonthDate

!=======+=========+=========+=========+=========+=========+=========+=========+

function SecondToTime(scnd) result(data_time)
  implicit none
  integer(kind=8),intent(IN) :: scnd

   type(time_type) :: data_time
   integer :: yyyy,mo,dd,hh,mm,ss
   integer(kind=8) :: y_mod, d_mod
   integer(kind=8) :: d_sec, day

   d_sec =60*60*24

   day = int(scnd/d_sec)+1 ; d_mod = mod(scnd,d_sec) ; hh = int(d_mod/3600) 
   mm = int((d_mod-hh*3600)/60) ; ss = d_mod-hh*3600-mm*60

   yyyy = BASIC_YEAR+1
   do 
     if (day<=GetYearDate(yyyy)) exit
     yyyy = yyyy+1
   end do
    
   yyyy = yyyy - 1
  
   y_mod = day - GetYearDate(yyyy)

   do mo = 1, 12
     if (y_mod<=GetMonthDate(yyyy,mo)) exit
   end do

   mo = mo-1

   if (mo>1) then 
     dd = y_mod-GetMonthDate(yyyy,mo) ! 2004/01/19
   else  
     dd = y_mod
   end if

   data_time%yyyy = yyyy ; data_time%mo = mo ; data_time%dd = dd 
   data_time%hh   = hh ; data_time%mm = mm ; data_time%ss = ss
   data_time%delta_t = 0

end function SecondToTime

!=======+=========+=========+=========+=========+=========+=========+=========+
! calculate now_time + delta_t -> now_time
! 2014/10/29 [NEW]
subroutine inc_calendar(now_time, delta_t)
  use jcup_utils, only : error
  implicit none
  type(time_type), intent(INOUT) :: now_time
  integer, intent(IN) :: delta_t
  integer(kind=8) :: time_sec, del_t_mil
  integer :: now_milli_sec, now_micro_sec

  select case (time_unit)
  case(TU_SEC)
    time_sec = TimeToSecond(now_time)
    time_sec = time_sec + delta_t
    now_time = SecondToTime(time_sec)
  case(TU_MIL)
    time_sec = TimeToSecond(now_time)
    now_milli_sec = now_time%milli_sec
    time_sec = time_sec + int((now_milli_sec+delta_t)/1000)
    now_time = SecondToTime(time_sec)
    now_time%milli_sec = mod((now_milli_sec+delta_t), 1000)
  case(TU_MCR)
    time_sec = TimeToSecond(now_time)
    now_milli_sec = now_time%milli_sec
    now_micro_sec = now_time%micro_sec
    time_sec = time_sec + int((now_milli_sec*1000+now_micro_sec+delta_t)/1000000)
    now_time = SecondToTime(time_sec)
    now_time%micro_sec = mod(now_micro_sec+delta_t, 1000)
    now_time%milli_sec = mod(now_milli_sec + int((now_micro_sec+delta_t)/1000), 1000)
  case default
    call error("inc_time","time_unit parameter error")
  end select
  
end subroutine inc_calendar

!=======+=========+=========+=========+=========+=========+=========+=========+
! calculate now_time + delta_t -> now_time
! 2014/07/09 [NEW]
subroutine inc_time(now_time, delta_t)
  use jcup_utils, only : error
  implicit none
  type(time_type), intent(INOUT) :: now_time
  integer, intent(IN) :: delta_t
  integer(kind=8) :: time_sec, del_t_mil
  integer :: now_milli_sec, now_micro_sec

  select case (time_unit)
  case(TU_SEC)
    time_sec = now_time%ss !TimeToSecond(now_time)
    time_sec = time_sec + delta_t
    now_time%ss  = time_sec !SecondToTime(time_sec)
  case(TU_MIL)
    time_sec = now_time%ss !TimeToSecond(now_time)
    now_milli_sec = now_time%milli_sec
    time_sec = time_sec + int((now_milli_sec+delta_t)/1000)
    now_time%ss = time_sec !SecondToTime(time_sec)
    now_time%milli_sec = mod((now_milli_sec+delta_t), 1000)
  case(TU_MCR)
    time_sec = now_time%ss !TimeToSecond(now_time)
    now_milli_sec = now_time%milli_sec
    now_micro_sec = now_time%micro_sec
    time_sec = time_sec + int((now_milli_sec*1000+now_micro_sec+delta_t)/1000000)
    now_time%ss = time_sec !SecondToTime(time_sec)
    now_time%micro_sec = mod(now_micro_sec+delta_t, 1000)
    now_time%milli_sec = mod(now_milli_sec + int((now_micro_sec+delta_t)/1000), 1000)
  case default
    call error("inc_time","time_unit parameter error")
  end select
  
end subroutine inc_time

!=======+=========+=========+=========+=========+=========+=========+=========+
! calculate time1-time2 
! 2014/07/10 [NEW]
! 2014/10/17 [MOD]
subroutine cal_time_diff(time1, time2, diff_sec, diff_mil, diff_mcr) 
  use jcup_utils, only : error
  implicit none
  type(time_type), intent(IN) :: time1
  type(time_type), intent(IN) :: time2
  integer(kind=8), intent(OUT) :: diff_sec
  integer, intent(OUT)         :: diff_mil, diff_mcr
  integer(kind=8) :: sec1, sec2
  integer(kind=8) :: sec_diff
  integer         :: mcr_sec1, mcr_sec2
  integer         :: mcr_sec_diff

  !if (time2>time1) then
  !  call error("cal_time_diff", "time1 must >= time2") 
  !end if

  sec1 = time1%ss !TimeToSecond(time1)
  sec2 = time2%ss !TimeToSecond(time2)
  mcr_sec1 = time1%milli_sec*1000+time1%micro_sec
  mcr_sec2 = time2%milli_sec*1000+time2%micro_sec

  sec_diff = sec1 - sec2

  if (mcr_sec2 > mcr_sec1) sec_diff = sec_diff - 1

  diff_sec = sec_diff

  mcr_sec_diff = mcr_sec1 - mcr_sec2
  if (mcr_sec_diff < 0) mcr_sec_diff = 1000000+mcr_sec_diff

  diff_mil = int(mcr_sec_diff/1000)
  diff_mcr = mod(mcr_sec_diff, 1000)

end subroutine cal_time_diff

!=======+=========+=========+=========+=========+=========+=========+=========+

!---------------------   model_time related procedures  ----------------------!

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_all_time(mdl)
  use jcup_utils, only : error
  implicit none
  integer,intent(IN) :: mdl

  integer :: status

  allocate(time(mdl),STAT=status)

  if (status/=0) call error("InitTime","data allocation error")

end subroutine init_all_time

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_each_time(mdl,dmn)
  use jcup_utils, only : error
  implicit none
  integer,intent(IN) :: mdl,dmn

  integer :: status

  allocate(time(mdl)%tm(dmn),STAT=status)
  if (status/=0) call error("InitTime","data allocation error")
  if (.not.associated(time(mdl)%tm)) call error("InitTime","data allocation error")

end subroutine init_each_time

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine destruct_all_time()
  implicit none
  integer :: mdl

  do mdl = 1, size(time,1)
    if (associated(time(mdl)%tm)) deallocate(time(mdl)%tm)
  end do
  if (associated(time)) deallocate(time)

end subroutine destruct_all_time

!=======+=========+=========+=========+=========+=========+=========+=========+
! mod 2014/07/03
subroutine set_start_time_str(mdl,dmn,time_str)
  implicit none
  integer,intent(IN) :: mdl,dmn
  character(len=*),intent(IN) :: time_str

  integer :: yyyy,mo,dd,hh,mm
  integer(kind=8) :: ss
  integer ::  milli_sec, micro_sec

  call TimeStrToDate(time_str,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)

  call set_time_data(time(mdl)%tm(dmn)%start_time,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)

end subroutine set_start_time_str

!=======+=========+=========+=========+=========+=========+=========+=========+
! mod 2014/07/03
subroutine set_start_time_date(mdl,dmn,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)
  implicit none
  integer,intent(IN) :: mdl,dmn
  integer,intent(IN) :: yyyy,mo,dd,hh,mm
  integer(kind=8), intent(IN) :: ss
  integer, optional, intent(IN) :: milli_sec, micro_sec

  call set_time_data(time(mdl)%tm(dmn)%start_time,yyyy,mo,dd,hh,mm,ss, MILLI_SEC = milli_sec, MICRO_SEC = micro_sec)

end subroutine set_start_time_date

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_start_time_str(mdl,dmn,time_str)
  implicit none
  integer,intent(IN) :: mdl,dmn
  character(len=*),intent(INOUT) :: time_str

  call DateToTimeStr(time_str,time(mdl)%tm(dmn)%start_time)

end subroutine get_start_time_str

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/04 [MOD]
subroutine get_start_time_date(mdl,dmn,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)
  implicit none
  integer,intent(IN) :: mdl,dmn
  integer,intent(INOUT) :: yyyy,mo,dd,hh,mm
  integer(kind=8), intent(INOUT) :: ss
  integer, optional, intent(INOUT) :: milli_sec, micro_sec

  call get_time_data(time(mdl)%tm(dmn)%start_time,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)

end subroutine get_start_time_date

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_start_time_type(mdl,dmn,data_time)
  implicit none
  integer,intent(IN) :: mdl,dmn
  type(time_type),intent(INOUT) :: data_time

  data_time = time(mdl)%tm(dmn)%start_time

end subroutine get_start_time_type

!=======+=========+=========+=========+=========+=========+=========+=========+
! mod 2014/07/03
subroutine set_end_time_str(mdl,dmn,time_str)
  implicit none
  integer,intent(IN) :: mdl,dmn
  character(len=*),intent(IN) :: time_str

  integer :: yyyy,mo,dd,hh,mm
  integer(kind=8) :: ss
  integer :: milli_sec, micro_sec

  call TimeStrToDate(time_str,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)
  call set_time_data(time(mdl)%tm(dmn)%end_time,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)

end subroutine set_end_time_str

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/04 [MOD]
subroutine set_end_time_date(mdl,dmn,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)
  implicit none
  integer,intent(IN) :: mdl,dmn
  integer,intent(IN) :: yyyy,mo,dd,hh,mm
  integer(kind=8), intent(IN) :: ss
  integer, optional, intent(IN) :: milli_sec, micro_sec

  call set_time_data(time(mdl)%tm(dmn)%end_time,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)

end subroutine set_end_time_date

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_end_time_str(mdl,dmn,time_str)
  implicit none
  integer,intent(IN) :: mdl,dmn
  character(len=*),intent(INOUT) :: time_str

  call DateToTimeStr(time_str, time(mdl)%tm(dmn)%end_time)

end subroutine get_end_time_str

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/04 [MOD]
subroutine get_end_time_date(mdl,dmn,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)
  implicit none
  integer,intent(IN) :: mdl,dmn
  integer,intent(INOUT) :: yyyy,mo,dd,hh,mm
  integer(kind=8), intent(INOUT) :: ss
  integer, optional, intent(INOUT) :: milli_sec, micro_sec

  call get_time_data(time(mdl)%tm(dmn)%end_time,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)

end subroutine get_end_time_date

!=======+=========+=========+=========+=========+=========+=========+=========+
! mod 2014/07/03
subroutine set_current_time_str(mdl,dmn,time_str, delta_t)
  implicit none
  integer,intent(IN) :: mdl,dmn
  character(len=*),intent(IN) :: time_str
  integer, optional, intent(IN) :: delta_t
  type(time_type) :: c_time
  integer :: yyyy,mo,dd,hh,mm
  integer(kind=8) :: ss
  integer :: milli_sec, micro_sec

  !!call TimeStrToDate(time_str,yyyy,mo,dd,hh,mm,ss)
  !!call set_time_data(c_time,yyyy,mo,dd,hh,mm,ss)

  !!if (EqualTime(time(mdl)%tm(dmn)%current_time, c_time)) return ! if same time, nothing todo

  call set_time(time(mdl)%tm(dmn)%before_time,time(mdl)%tm(dmn)%current_time)

  call TimeStrToDate(time_str,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)
  call set_time_data(time(mdl)%tm(dmn)%current_time,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)

  if (present(delta_t)) time(mdl)%tm(dmn)%current_time%delta_t = delta_t


end subroutine set_current_time_str

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_current_time_date(mdl, dmn, yyyy, mo, dd, hh, mm, ss, milli_sec, micro_sec, delta_t)
  implicit none
  integer, intent(IN) :: mdl,dmn
  integer, intent(IN) :: yyyy,mo,dd,hh,mm
  integer(kind=8), intent(IN) :: ss
  integer, optional, intent(IN) :: milli_sec
  integer, optional, intent(IN) :: micro_sec
  integer, optional, intent(IN) :: delta_t
  type(time_type) :: c_time

  !!call set_time_data(c_time,yyyy,mo,dd,hh,mm,ss)

  !!if (EqualTime(time(mdl)%tm(dmn)%current_time, c_time)) return ! if same time, nothing todo

  call set_time(time(mdl)%tm(dmn)%before_time,time(mdl)%tm(dmn)%current_time)

  call set_time_data(time(mdl)%tm(dmn)%current_time,yyyy,mo,dd,hh,mm,ss,milli_sec, micro_sec)

  if (present(delta_t)) time(mdl)%tm(dmn)%current_time%delta_t = delta_t

end subroutine set_current_time_date

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_current_time_type(mdl,dmn,data_time, delta_t)
  implicit none
  integer,intent(IN) :: mdl,dmn
  type(time_type),intent(IN) :: data_time
  integer, optional, intent(IN) :: delta_t

  !!if (EqualTime(time(mdl)%tm(dmn)%current_time, data_time)) return ! if same time, nothing todo

  call set_time(time(mdl)%tm(dmn)%before_time,time(mdl)%tm(dmn)%current_time)

  time(mdl)%tm(dmn)%current_time = data_time

  if (present(delta_t)) time(mdl)%tm(dmn)%current_time%delta_t = delta_t
 
end subroutine set_current_time_type

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_current_time_str(mdl,dmn,time_str, delta_t)
  implicit none
  integer,intent(IN) :: mdl,dmn
  character(len=*),intent(INOUT)   :: time_str
  integer, optional, intent(INOUT) :: delta_t

  call DateToTimeStr(time_str,time(mdl)%tm(dmn)%current_time)

  if (present(delta_t)) delta_t = time(mdl)%tm(dmn)%current_time%delta_t 

end subroutine get_current_time_str

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/04 [MOD]
subroutine get_current_time_date(mdl,dmn,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec, delta_t)
  implicit none
  integer,intent(IN) :: mdl,dmn
  integer,intent(INOUT) :: yyyy,mo,dd,hh,mm
  integer(kind=8), intent(INOUT) :: ss
  integer, optional, intent(INOUT) :: milli_sec, micro_sec
  integer, optional, intent(INOUT) :: delta_t

  call get_time_data(time(mdl)%tm(dmn)%current_time,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)
  
  if (present(delta_t)) delta_t = time(mdl)%tm(dmn)%current_time%delta_t

end subroutine get_current_time_date

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_current_time_type(mdl,dmn,data_time, delta_t)
  implicit none
  integer,intent(IN) :: mdl,dmn
  type(time_type),intent(INOUT) :: data_time
  integer, optional, intent(INOUT) :: delta_t


  data_time = time(mdl)%tm(dmn)%current_time

  if (present(delta_t)) delta_t = time(mdl)%tm(dmn)%current_time%delta_t

end subroutine get_current_time_type

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_before_time_str(mdl,dmn,time_str, delta_t)
  implicit none
  integer,intent(IN) :: mdl,dmn
  character(len=*),intent(INOUT)   :: time_str
  integer, optional, intent(INOUT) :: delta_t

  call DateToTimeStr(time_str,time(mdl)%tm(dmn)%before_time)

  if (present(delta_t)) delta_t = time(mdl)%tm(dmn)%before_time%delta_t 

end subroutine get_before_time_str

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/04 [MOD]
subroutine get_before_time_date(mdl,dmn,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec, delta_t)
  implicit none
  integer,intent(IN) :: mdl,dmn
  integer,intent(INOUT) :: yyyy,mo,dd,hh,mm
  integer(kind=8), intent(INOUT) :: ss
  integer, optional, intent(INOUT) :: milli_sec, micro_sec
  integer, optional, intent(INOUT) :: delta_t

  call get_time_data(time(mdl)%tm(dmn)%before_time,yyyy,mo,dd,hh,mm,ss, milli_sec, micro_sec)
  
  if (present(delta_t)) delta_t = time(mdl)%tm(dmn)%before_time%delta_t

end subroutine get_before_time_date

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_before_time_type(mdl,dmn,data_time, delta_t)
  implicit none
  integer,intent(IN) :: mdl,dmn
  type(time_type),intent(INOUT) :: data_time
  integer, optional, intent(INOUT) :: delta_t

  data_time = time(mdl)%tm(dmn)%before_time

  if (present(delta_t)) delta_t = time(mdl)%tm(dmn)%before_time%delta_t

end subroutine get_before_time_type

!=======+=========+=========+=========+=========+=========+=========+=========+
subroutine get_delta_t(mdl,dmn,delta_t)
  implicit none
  integer,intent(IN) :: mdl,dmn
  integer,intent(INOUT) :: delta_t

  delta_t = time(mdl)%tm(dmn)%current_time%delta_t

end subroutine get_delta_t

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/10 [ADD]
!subroutine cal_time_diff_from_start(mdl,dmn, diff_sec, diff_mil, diff_mcr)
!1  implicit none
!  integer,intent(IN) :: mdl,dmn
!  integer(kind=8), intent(OUT) :: diff_sec
!1  integer, intent(OUT) :: diff_mil, diff_mcr

!1  call cal_time_diff(time(mdl)%tm(dmn)%current_time, time(mdl)%tm(dmn)%start_time, diff_sec, diff_mil, diff_mcr)
 
!end subroutine cal_time_diff_from_start

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/10/22 [NEW]
logical function is_before_exchange_step(mdl, dmn, interval)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: mdl, dmn, interval ! my model, domain, exchange interval
  integer(kind=8) :: diff_sec
  integer         :: diff_mil, diff_mcr
  integer(kind=8) :: time_diff

  call cal_time_diff(time(mdl)%tm(dmn)%before_time, time(mdl)%tm(dmn)%start_time, diff_sec, diff_mil, diff_mcr)
  select case (time_unit)
  case (TU_SEC)
    is_before_exchange_step = (Mod(diff_sec, interval)==0)
  case (TU_MIL)
    time_diff = diff_sec*1000+diff_mil
    is_before_exchange_step = (Mod(time_diff, interval) == 0)
  case(TU_MCR)
    time_diff = diff_sec*1000000 + diff_mil*1000 + diff_mcr
    is_before_exchange_step = (Mod(time_diff, interval) == 0)
  case default
    call error("is_exchange_step","time_unit parameter error")
  end select

end function is_before_exchange_step

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/10 [NOD]
logical function is_exchange_step(mdl, dmn, interval)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: mdl, dmn, interval ! my model, domain, exchange interval
  integer(kind=8) :: diff_sec
  integer         :: diff_mil, diff_mcr
  integer(kind=8) :: time_diff

  call cal_time_diff(time(mdl)%tm(dmn)%current_time, time(mdl)%tm(dmn)%start_time, diff_sec, diff_mil, diff_mcr)
  select case (time_unit)
  case (TU_SEC)
    is_exchange_step = (Mod(diff_sec, interval)==0)
  case (TU_MIL)
    time_diff = diff_sec*1000+diff_mil
    is_exchange_step = (Mod(time_diff, interval) == 0)
  case(TU_MCR)
    time_diff = diff_sec*1000000 + diff_mil*1000 + diff_mcr
    is_exchange_step = (Mod(time_diff, interval) == 0)
  case default
    call error("is_exchange_step","time_unit parameter error")
  end select

end function is_exchange_step

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/10 [MOD]
logical function is_exchange_step_from_c_time(my_mdl, my_dmn, interval, current_time)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: my_mdl, my_dmn, interval ! my model, domain, exchange interval
  type(time_type) :: current_time

  integer(kind=8) :: diff_sec
  integer         :: diff_mil, diff_mcr
  integer(kind=8) :: time_diff


  call cal_time_diff(current_time, time(my_mdl)%tm(my_dmn)%start_time, diff_sec, diff_mil, diff_mcr)
  select case(time_unit)
  case(TU_SEC)
    is_exchange_step_from_c_time = (Mod(diff_sec, interval)==0)
  case(TU_MIL)
    time_diff = diff_sec*1000 + diff_mil
    is_exchange_step_from_c_time = (Mod(time_diff, interval)==0)
  case(TU_MCR)
    time_diff = diff_sec*1000000 + diff_mil*1000 + diff_mcr
    is_exchange_step_from_c_time = (Mod(time_diff, interval)==0)
  case default
    call error("is_exchange_step_from_c_time","time_unit parameter error")
  end select
   
end function is_exchange_step_from_c_time

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/10 [MOD]
logical function is_next_exchange_step(mdl, dmn, interval)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: mdl, dmn, interval
  integer(kind=8) :: diff_sec 
  integer ::  diff_mil, diff_mcr
  integer(kind=8) :: time_diff

  call cal_time_diff(time(mdl)%tm(dmn)%current_time, time(mdl)%tm(dmn)%start_time, diff_sec, diff_mil, diff_mcr)
  select case (time_unit)
  case(TU_SEC)
    time_diff = diff_sec + time(mdl)%tm(dmn)%current_time%delta_t
    is_next_exchange_step = (Mod(time_diff, interval)==0)
  case(TU_MIL)
    time_diff = diff_sec*1000 + diff_mil + time(mdl)%tm(dmn)%current_time%delta_t
    is_next_exchange_step = (Mod(time_diff, interval)==0)
  case(TU_MCR)
    time_diff = diff_sec*1000000 + diff_mil*1000 + diff_mcr + time(mdl)%tm(dmn)%current_time%delta_t
    is_next_exchange_step = (Mod(time_diff, interval)==0)
  case default
    call error("is_next_exchange_step","time_unit parameter error")
  end select

end function is_next_exchange_step

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/07/04 [MOD]
! 2014/10/28 [MOD] cal_time_diff(time%current_time -> cal_time_diff(time%before_time
subroutine cal_next_exchange_time(mdl, dmn, interval, next_time)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: mdl, dmn
  integer, intent(IN) :: interval
  type(time_type), intent(INOUT) :: next_time
  integer :: num_of_interval
  integer(kind=8) :: diff_sec
  integer         :: diff_mil, diff_mcr
  integer(kind=8) :: time_diff

  !!!call cal_time_diff(time(mdl)%tm(dmn)%current_time, time(mdl)%tm(dmn)%start_time, diff_sec, diff_mil, diff_mcr)
  call cal_time_diff(time(mdl)%tm(dmn)%before_time, time(mdl)%tm(dmn)%start_time, diff_sec, diff_mil, diff_mcr)

  select case(time_unit)
  case(TU_SEC)
    time_diff = diff_sec 
    num_of_interval = int(time_diff/interval)
    next_time = time(mdl)%tm(dmn)%start_time
    call inc_time(next_time, (num_of_interval+1)*interval)
  case(TU_MIL)
    time_diff = diff_sec*1000 + diff_mil 
    num_of_interval = int(time_diff/interval)
    next_time = time(mdl)%tm(dmn)%start_time
    call inc_time(next_time, (num_of_interval+1)*interval)
  case(TU_MCR)
    time_diff = diff_sec*1000000 + diff_mil*1000 + diff_mcr 
    num_of_interval = int(time_diff/interval)
    next_time = time(mdl)%tm(dmn)%start_time
    call inc_time(next_time, (num_of_interval+1)*interval)
  case default
    call error("cal_next_exchange_time","time_unit parameter error")
  end select
 
end subroutine cal_next_exchange_time

!=======+=========+=========+=========+=========+=========+=========+=========+
! 2014/12/12 [MOD] modify to write the target component time only 
subroutine write_time(fid, comp_id)
  use jcup_mpi_lib, only : jml_isLocalLeader
  implicit none
  integer, intent(IN) :: fid, comp_id
  integer :: i, j
  
  if (.not.jml_isLocalLeader(comp_id)) return

  !do i = 1, size(time)
    do j = 1, size(time(comp_id)%tm)
      write(fid, *) time(comp_id)%tm(j)%start_time
      write(fid, *) time(comp_id)%tm(j)%end_time
      write(fid, *) time(comp_id)%tm(j)%current_time
      write(fid, *) time(comp_id)%tm(j)%before_time
    end do
  !end do

end subroutine write_time

!=======+=========+=========+=========+=========+=========+=========+=========+
! mod 2014/07/03
! 2014/11/05 [MOD} integer :: time_buffer -> integer(kind=8) :: time_buffer
! 2014/12/12 [MOD] modify to read target component time only 
subroutine read_time(fid, comp_id)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_BcastLocal, jml_GetMyrankGlobal
  implicit none
  integer, intent(IN) :: fid
  integer, intent(IN) :: comp_id
  integer :: i, j
  integer(kind=8) :: time_buffer(9*4) ! yyyy, mo, dd, hh, mm, ss, milli, micro, delta_t
  
  if (jml_isLocalLeader(comp_id)) then
    !do i = 1, size(time)   
      do j = 1, size(time(comp_id)%tm)
        read(fid, *) time(comp_id)%tm(j)%start_time
        read(fid, *) time(comp_id)%tm(j)%end_time
        read(fid, *) time(comp_id)%tm(j)%current_time
        read(fid, *) time(comp_id)%tm(j)%before_time

        call set_buffer(time(comp_id)%tm(j)%start_time,   time_buffer, 1)
        call set_buffer(time(comp_id)%tm(j)%end_time,     time_buffer, 10)
        call set_buffer(time(comp_id)%tm(j)%current_time, time_buffer, 19)
        call set_buffer(time(comp_id)%tm(j)%before_time,  time_buffer, 28)

        call jml_BcastLocal(comp_id, time_buffer, 1, 9*4)

      end do

    !end do
  else
    !do i = 1, size(time)
      do j = 1, size(time(comp_id)%tm)
        call jml_BcastLocal(comp_id, time_buffer, 1, 9*4)
    
        call get_buffer(time(comp_id)%tm(j)%start_time,   time_buffer, 1)
        call get_buffer(time(comp_id)%tm(j)%end_time,     time_buffer, 10)
        call get_buffer(time(comp_id)%tm(j)%current_time, time_buffer, 19)
        call get_buffer(time(comp_id)%tm(j)%before_time,  time_buffer, 28)
      end do
    !end do
  end if

end subroutine read_time

!=======+=========+=========+=========+=========+=========+=========+=========+
! mod 2014/07/03
! 2014/11/05 [MOD] integer :: buffer -> integer(kind=8) :: buffer
subroutine set_buffer(time, buffer, is)
  implicit none
  type(time_type), intent(IN) :: time
  integer(kind=8), intent(OUT) :: buffer(:)
  integer, intent(IN) :: is

  buffer(is+0) = time%yyyy
  buffer(is+1) = time%mo
  buffer(is+2) = time%dd
  buffer(is+3) = time%hh
  buffer(is+4) = time%mm
  buffer(is+5) = time%ss
  buffer(is+6) = time%milli_sec
  buffer(is+7) = time%micro_sec
  buffer(is+8) = time%delta_t

end subroutine set_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+
! mod 2014/07/03
! 2014/11/05 [MOD] integer :: buffer -> integer(kind=8) :: buffer
subroutine get_buffer(time, buffer, is)
  implicit none
  type(time_type), intent(OUT) :: time
  integer(kind=8), intent(IN) :: buffer(:)
  integer, intent(IN) :: is

  time%yyyy = buffer(is+0)
  time%mo   = buffer(is+1) 
  time%dd   = buffer(is+2) 
  time%hh   = buffer(is+3)
  time%mm   = buffer(is+4)
  time%ss   = buffer(is+5)
  time%milli_sec = buffer(is+6)
  time%micro_sec = buffer(is+7)
  time%delta_t = buffer(is+8) 

end subroutine get_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcup_time











