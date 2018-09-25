!=======+=========+=========+=========+=========+=========+=========+=========+

!
!Copyright (c) 2011, arakawa@rist.jp
!All rights reserved.
!
module jcup_time
  private

!--------------------------------   public  ----------------------------------!

  public :: time_type
  public :: set_time_data
  public :: get_time_data
  public :: operator(==)
  public :: operator(/=)
  public :: operator(>=)
  public :: operator(>)
  public :: operator(<=)
  public :: operator(<)
  public :: init_all_time
  public :: init_each_time
  public :: set_start_time ! subroutine (component_id, domain_id, time)
  public :: get_start_time ! subroutine (component_id, domain_id, time)
  public :: set_current_time ! subroutine (component_id, domain_id, time, delta_t)
  public :: get_current_time ! subroutine (component_id, domain_id, time)
  public :: get_before_time  ! subroutine (component_id, domain_id, time)
  public :: set_delta_t
  public :: get_delta_t
  public :: DateToStr
  public :: TimeStrToDate
  public :: DateToTimeStr
  public :: TimeToSecond ! integer function (time)
  public :: SecondToTime ! time function (second)
  public :: is_exchange_step ! logical functiuon (component_id, domain_id, interval)
  public :: is_exchange_step_from_c_time ! logical function (component_id, domain_id, intervar, current_time)
  public :: is_next_exchange_step
  public :: cal_next_exchange_time ! subroutine (component_id, domain_id, interval, time)
  public :: isSendRecvStep
  public :: cal_near_time
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

  interface TimeStrToDate
    module procedure time_str_to_date1, time_str_to_date2
  end interface

  interface DateToTimeStr
    module procedure date_to_time_str1, date_to_time_str2
  end interface

  type time_type
    integer :: yyyy,mo,dd,hh,mm,ss
    integer :: delta_t
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

subroutine set_time_data(tm, yyyy, mo, dd, hh, mm, ss)
  implicit none
  type(time_type), intent(INOUT) :: tm
  integer, intent(IN) :: yyyy, mo, dd, hh, mm, ss

  tm%yyyy = yyyy
  tm%mo   = mo
  tm%dd   = dd
  tm%hh   = hh
  tm%mm   = mm
  tm%ss   = ss

end subroutine set_time_data

!=======+=========+=========+=========+=========+=========+=========+=========+

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
  t1%delta_t = t2%delta_t
end subroutine set_time

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_time_data(tm, yyyy, mo, dd, hh, mm, ss)
  implicit none
  type(time_type), intent(IN) :: tm
  integer, intent(INOUT) :: yyyy, mo, dd, hh, mm, ss

  yyyy = tm%yyyy
  mo   = tm%mo
  dd   = tm%dd
  hh   = tm%hh
  mm   = tm%mm
  ss   = tm%ss

end subroutine get_time_data

!=======+=========+=========+=========+=========+=========+=========+=========+

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
  
  EqualTime = .true.
end function EqualTime

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function NotEqualTime(t1,t2)
  implicit none
  type(time_type),intent(IN) :: t1, t2

  NotEqualTime = .not.EqualTime(t1,t2)

end function NotEqualTime

!=======+=========+=========+=========+=========+=========+=========+=========+

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

  GETime = .true.

end function GETime

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function LTTime(t1,t2)
  implicit none
  type(time_type), intent(IN) :: t1, t2

  LTTime = .not.GETime(t1,t2)

end function LTTime

!=======+=========+=========+=========+=========+=========+=========+=========+

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
  
  LETime = .true.

end function LETime

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function GTTime(t1,t2)
  implicit none
  type(time_type), intent(IN) :: t1, t2

  GTTime = .not.LETime(t1,t2)

end function GTTime

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=14) function DateToStr(data_time) 
  implicit none
  type(time_type),intent(IN) :: data_time

  character(len=14) :: cdate

  write(cdate,'(i4.4,5(i2.2))') data_time%yyyy,data_time%mo,data_time%dd,data_time%hh,data_time%mm,data_time%ss

  DateToStr = cdate

end function DateToStr

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine time_str_to_date1(time_str,yyyy,mo,dd,hh,mm,ss)
  implicit none
  character(len=*),intent(IN) :: time_str
  integer,intent(INOUT) :: yyyy,mo,dd,hh,mm,ss

  read(time_str(1:4),*) yyyy
  read(time_str(5:6),*) mo
  read(time_str(7:8),*) dd
  read(time_str(9:10),*) hh
  read(time_str(11:12),*) mm
  read(time_str(13:14),*) ss

end subroutine time_str_to_date1

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine time_str_to_date2(time_str,date)
  implicit none
  character(len=*),intent(IN)    :: time_str
  type(time_type) ,intent(INOUT) :: date

  read(time_str(1:4),*) date%yyyy
  read(time_str(5:6),*) date%mo
  read(time_str(7:8),*) date%dd
  read(time_str(9:10),*) date%hh
  read(time_str(11:12),*) date%mm
  read(time_str(13:14),*) date%ss

end subroutine time_str_to_date2

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine date_to_time_str1(time_str,yyyy,mo,dd,hh,mm,ss)
  implicit none
  character(len=*),intent(INOUT) :: time_str
  integer,intent(IN) :: yyyy,mo,dd,hh,mm,ss

  write(time_str,'(i4.4,5(i2.2))') yyyy,mo,dd,hh,mm,ss

end subroutine date_to_time_str1

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine date_to_time_str2(time_str,date)
  implicit none
  character(len=*),intent(INOUT) :: time_str
  type(time_type) ,intent(IN)    :: date

  write(time_str,'(I4.4,5(I2.2))') date%yyyy,date%mo,date%dd,date%hh,date%mm,date%ss

end subroutine date_to_time_str2

!=======+=========+=========+=========+=========+=========+=========+=========+

integer(kind=8) function CalTimeDiff(t1,t2)
  implicit none
  type(time_type),intent(IN) :: t1,t2

  CalTimeDiff = TimeToSecond(t2)-TimeToSecond(t1)

end function CalTimeDiff

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

subroutine set_start_time_str(mdl,dmn,time_str)
  implicit none
  integer,intent(IN) :: mdl,dmn
  character(len=*),intent(IN) :: time_str

  integer :: yyyy,mo,dd,hh,mm,ss

  call TimeStrToDate(time_str,yyyy,mo,dd,hh,mm,ss)

  call set_time_data(time(mdl)%tm(dmn)%start_time,yyyy,mo,dd,hh,mm,ss)

end subroutine set_start_time_str

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_start_time_date(mdl,dmn,yyyy,mo,dd,hh,mm,ss)
  implicit none
  integer,intent(IN) :: mdl,dmn
  integer,intent(IN) :: yyyy,mo,dd,hh,mm,ss

  call set_time_data(time(mdl)%tm(dmn)%start_time,yyyy,mo,dd,hh,mm,ss)

end subroutine set_start_time_date

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_start_time_str(mdl,dmn,time_str)
  implicit none
  integer,intent(IN) :: mdl,dmn
  character(len=*),intent(INOUT) :: time_str

  call DateToTimeStr(time_str,time(mdl)%tm(dmn)%start_time)

end subroutine get_start_time_str

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_start_time_date(mdl,dmn,yyyy,mo,dd,hh,mm,ss)
  implicit none
  integer,intent(IN) :: mdl,dmn
  integer,intent(INOUT) :: yyyy,mo,dd,hh,mm,ss

  call get_time_data(time(mdl)%tm(dmn)%start_time,yyyy,mo,dd,hh,mm,ss)

end subroutine get_start_time_date

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_start_time_type(mdl,dmn,data_time)
  implicit none
  integer,intent(IN) :: mdl,dmn
  type(time_type),intent(INOUT) :: data_time

  data_time = time(mdl)%tm(dmn)%start_time

end subroutine get_start_time_type

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_end_time_str(mdl,dmn,time_str)
  implicit none
  integer,intent(IN) :: mdl,dmn
  character(len=*),intent(IN) :: time_str

  integer :: yyyy,mo,dd,hh,mm,ss

  call TimeStrToDate(time_str,yyyy,mo,dd,hh,mm,ss)
  call set_time_data(time(mdl)%tm(dmn)%end_time,yyyy,mo,dd,hh,mm,ss)

end subroutine set_end_time_str

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_end_time_date(mdl,dmn,yyyy,mo,dd,hh,mm,ss)
  implicit none
  integer,intent(IN) :: mdl,dmn
  integer,intent(IN) :: yyyy,mo,dd,hh,mm,ss

  call set_time_data(time(mdl)%tm(dmn)%end_time,yyyy,mo,dd,hh,mm,ss)

end subroutine set_end_time_date

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_end_time_str(mdl,dmn,time_str)
  implicit none
  integer,intent(IN) :: mdl,dmn
  character(len=*),intent(INOUT) :: time_str

  call DateToTimeStr(time_str, time(mdl)%tm(dmn)%end_time)

end subroutine get_end_time_str

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_end_time_date(mdl,dmn,yyyy,mo,dd,hh,mm,ss)
  implicit none
  integer,intent(IN) :: mdl,dmn
  integer,intent(INOUT) :: yyyy,mo,dd,hh,mm,ss

  call get_time_data(time(mdl)%tm(dmn)%end_time,yyyy,mo,dd,hh,mm,ss)

end subroutine get_end_time_date

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_current_time_str(mdl,dmn,time_str, delta_t)
  implicit none
  integer,intent(IN) :: mdl,dmn
  character(len=*),intent(IN) :: time_str
  integer, optional, intent(IN) :: delta_t
  type(time_type) :: c_time
  integer :: yyyy,mo,dd,hh,mm,ss

  !!call TimeStrToDate(time_str,yyyy,mo,dd,hh,mm,ss)
  !!call set_time_data(c_time,yyyy,mo,dd,hh,mm,ss)

  !!if (EqualTime(time(mdl)%tm(dmn)%current_time, c_time)) return ! if same time, nothing todo

  call set_time(time(mdl)%tm(dmn)%before_time,time(mdl)%tm(dmn)%current_time)

  call TimeStrToDate(time_str,yyyy,mo,dd,hh,mm,ss)
  call set_time_data(time(mdl)%tm(dmn)%current_time,yyyy,mo,dd,hh,mm,ss)

  if (present(delta_t)) time(mdl)%tm(dmn)%current_time%delta_t = delta_t


end subroutine set_current_time_str

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_current_time_date(mdl,dmn,yyyy,mo,dd,hh,mm,ss, delta_t)
  implicit none
  integer, intent(IN) :: mdl,dmn
  integer, intent(IN) :: yyyy,mo,dd,hh,mm,ss
  integer, optional, intent(IN) :: delta_t
  type(time_type) :: c_time

  !!call set_time_data(c_time,yyyy,mo,dd,hh,mm,ss)

  !!if (EqualTime(time(mdl)%tm(dmn)%current_time, c_time)) return ! if same time, nothing todo

  call set_time(time(mdl)%tm(dmn)%before_time,time(mdl)%tm(dmn)%current_time)

  call set_time_data(time(mdl)%tm(dmn)%current_time,yyyy,mo,dd,hh,mm,ss)
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

subroutine get_current_time_date(mdl,dmn,yyyy,mo,dd,hh,mm,ss, delta_t)
  implicit none
  integer,intent(IN) :: mdl,dmn
  integer,intent(INOUT) :: yyyy,mo,dd,hh,mm,ss
  integer, optional, intent(INOUT) :: delta_t

  call get_time_data(time(mdl)%tm(dmn)%current_time,yyyy,mo,dd,hh,mm,ss)
  
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

subroutine get_before_time_date(mdl,dmn,yyyy,mo,dd,hh,mm,ss, delta_t)
  implicit none
  integer,intent(IN) :: mdl,dmn
  integer,intent(INOUT) :: yyyy,mo,dd,hh,mm,ss
  integer, optional, intent(INOUT) :: delta_t

  call get_time_data(time(mdl)%tm(dmn)%before_time,yyyy,mo,dd,hh,mm,ss)
  
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

subroutine set_delta_t(mdl,dmn,delta_t)
  implicit none
  integer,intent(IN) :: mdl,dmn,delta_t

  time(mdl)%tm(dmn)%current_time%delta_t = delta_t

end subroutine set_delta_t

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_delta_t(mdl,dmn,delta_t)
  implicit none
  integer,intent(IN) :: mdl,dmn
  integer,intent(INOUT) :: delta_t

  delta_t = time(mdl)%tm(dmn)%current_time%delta_t

end subroutine get_delta_t

!=======+=========+=========+=========+=========+=========+=========+=========+

integer(kind=8) function CalSecondFromStart(mdl,dmn)
  implicit none
  integer,intent(IN) :: mdl,dmn

  CalSecondFromStart = CalTimeDiff(time(mdl)%tm(dmn)%start_time,time(mdl)%tm(dmn)%current_time)
 
end function CalSecondFromStart

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_exchange_step(mdl, dmn, interval)
  implicit none
  integer, intent(IN) :: mdl, dmn, interval ! my model, domain, exchange interval

  is_exchange_step = (Mod(int(CalSecondFromStart(mdl,dmn)), interval)==0)

end function is_exchange_step

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_exchange_step_from_c_time(my_mdl, my_dmn, interval, current_time)
  implicit none
  integer, intent(IN) :: my_mdl, my_dmn, interval ! my model, domain, exchange interval
  type(time_type) :: current_time

  integer(kind=8) :: time_diff

  time_diff = CalTimeDiff(time(my_mdl)%tm(my_dmn)%start_time, current_time)

  is_exchange_step_from_c_time = (Mod(int(time_diff), interval)==0)

end function is_exchange_step_from_c_time

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_next_exchange_step(mdl, dmn, interval)
  implicit none
  integer, intent(IN) :: mdl, dmn, interval

  is_next_exchange_step = (Mod(int(CalSecondFromStart(mdl,dmn)+time(mdl)%tm(dmn)%current_time%delta_t), interval)==0)
 
end function is_next_exchange_step

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_next_exchange_time(mdl, dmn, interval, next_time)
  implicit none
  integer, intent(IN) :: mdl, dmn
  integer, intent(IN) :: interval
  type(time_type), intent(INOUT) :: next_time
  integer :: num_of_interval
  type(time_type) :: time1, time2

  num_of_interval = CalSecondFromStart(mdl,dmn)/interval

  next_time = SecondToTime((num_of_interval+1)*interval+TimeToSecond(time(mdl)%tm(dmn)%start_time))
  
end subroutine cal_next_exchange_time

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function isSendRecvStep(mdl,dmn,step_int)

  implicit none
  integer,intent(IN) :: mdl,dmn,step_int ! send model, send domain, send step int.

  isSendRecvStep = .true.

  select case(step_int)
  case(0)
    isSendRecvStep = .false.
  case(1)
    isSendRecvStep = .true.
  case(-1)
    isSendRecvStep = (time(mdl)%tm(dmn)%current_time == time(mdl)%tm(dmn)%start_time)
  case default
    isSendRecvStep = (Mod(int(CalSecondFromStart(mdl,dmn)/time(mdl)%tm(dmn)%current_time%delta_t), step_int) == 0)
  end select

end function isSendRecvStep

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_near_time(s_mdl, s_dmn, data_time, send_step_int, time1, time2)
  implicit none
  integer,intent(IN) :: s_mdl, s_dmn ! send model and domain
  type(time_type),intent(IN) :: data_time ! recv data time
  integer,intent(IN) :: send_step_int ! send data time step interval
  type(time_type),intent(INOUT) :: time1, time2 ! send time

  integer(kind=8) :: time_diff, t1_sec, t2_sec, t_step

  time_diff = CalTimeDiff(time(s_mdl)%tm(s_dmn)%start_time,data_time)
  t_step = time(s_mdl)%tm(s_dmn)%current_time%delta_t*send_step_int
  t1_sec = t_step*int(time_diff/t_step)+TimeToSecond(time(s_mdl)%tm(s_dmn)%start_time)
  t2_sec = t1_sec+t_step

  time1 = SecondToTime(t1_sec)
  time2 = SecondToTime(t2_sec)

end subroutine cal_near_time

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine write_time(fid, comp_id)
  use jcup_mpi_lib, only : jml_isLocalLeader
  implicit none
  integer, intent(IN) :: fid, comp_id
  integer :: i, j
  
  if (.not.jml_isLocalLeader(comp_id)) return

  do i = 1, size(time)
    do j = 1, size(time(i)%tm)
      write(fid, *) time(i)%tm(j)%start_time
      write(fid, *) time(i)%tm(j)%end_time
      write(fid, *) time(i)%tm(j)%current_time
      write(fid, *) time(i)%tm(j)%before_time
    end do
  end do

end subroutine write_time

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine read_time(fid, comp_id)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_BcastLocal, jml_GetMyrankGlobal
  implicit none
  integer, intent(IN) :: fid
  integer, intent(IN) :: comp_id
  integer :: i, j
  integer :: time_buffer(7*4)
  
  if (jml_isLocalLeader(comp_id)) then
    do i = 1, size(time)   
      do j = 1, size(time(i)%tm)
        read(fid, *) time(i)%tm(j)%start_time
        read(fid, *) time(i)%tm(j)%end_time
        read(fid, *) time(i)%tm(j)%current_time
        read(fid, *) time(i)%tm(j)%before_time

        call set_buffer(time(i)%tm(j)%start_time,   time_buffer, 1)
        call set_buffer(time(i)%tm(j)%end_time,     time_buffer, 8)
        call set_buffer(time(i)%tm(j)%current_time, time_buffer, 15)
        call set_buffer(time(i)%tm(j)%before_time,  time_buffer, 22)

        call jml_BcastLocal(comp_id, time_buffer, 1, 7*4)

      end do

    end do
  else
    do i = 1, size(time)
      do j = 1, size(time(i)%tm)
        call jml_BcastLocal(comp_id, time_buffer, 1, 7*4)
    
        call get_buffer(time(i)%tm(j)%start_time,   time_buffer, 1)
        call get_buffer(time(i)%tm(j)%end_time,     time_buffer, 8)
        call get_buffer(time(i)%tm(j)%current_time, time_buffer, 15)
        call get_buffer(time(i)%tm(j)%before_time,  time_buffer, 22)
      end do
    end do
  end if

end subroutine read_time

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_buffer(time, buffer, is)
  implicit none
  type(time_type), intent(IN) :: time
  integer, intent(OUT) :: buffer(:)
  integer, intent(IN) :: is

  buffer(is+0) = time%yyyy
  buffer(is+1) = time%mo
  buffer(is+2) = time%dd
  buffer(is+3) = time%hh
  buffer(is+4) = time%mm
  buffer(is+5) = time%ss
  buffer(is+6) = time%delta_t

end subroutine set_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_buffer(time, buffer, is)
  implicit none
  type(time_type), intent(OUT) :: time
  integer, intent(IN) :: buffer(:)
  integer, intent(IN) :: is

  time%yyyy = buffer(is+0)
  time%mo   = buffer(is+1) 
  time%dd   = buffer(is+2) 
  time%hh   = buffer(is+3)
  time%mm   = buffer(is+4)
  time%ss   = buffer(is+5)
  time%delta_t = buffer(is+6) 

end subroutine get_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcup_time











