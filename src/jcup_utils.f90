!=======+=========+=========+=========+=========+=========+=========+=========+

!
!Copyright (c) 2011, arakawa@rist.jp
!All rights reserved.
!
module jcup_utils
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  ! public constants
  integer,public,parameter :: MAX_STRING_LENGTH = 255 

  ! public procedures
  public :: init_utils
  public :: set_log_level ! subroutine (log_level, log_stderr)
  public :: get_log_level ! integer function ()
  public :: is_output_stderr ! logical function ()
  public :: set_log_unit_id
  public :: init_log
  public :: finalize_log
  public :: put_log ! subroutine (log_str, log_level)
  public :: open_log_file
  public :: close_log_file
  public :: error
  public :: check_argument
  public :: IntToStr
  public :: LongIntToStr
  public :: StrToInt
  public :: IDate2CDate
  public :: cdate_2_idate
  public :: sort_int_1d
  public :: split_string
  public :: TrimString
  public :: startsWith
  public :: lw2up
  public :: up2lw
  public :: is_comment_line
  public :: cut_comment

  integer, parameter, public :: NO_OUTPUT_LOG = 0
  integer, parameter, public :: STANDARD_LOG = 1
  integer, parameter, public :: DETAIL_LOG = 2
  logical, parameter, public :: NO_OUTPUT_STDERR = .false.
  logical, parameter, public :: OUTPUT_STDERR    = .true.

!--------------------------------   private  ---------------------------------!

  interface IntToStr
    module procedure IntegerToStr, LongIntToStr
  end interface 

  ! private constants
  integer,private,parameter :: TIME_STR_LEN = 19
  integer,private,parameter :: STD_IN=5,STD_OUT=6,STD_ERR=0
  integer,private           :: LogUnitID = STD_ERR ! Log file unit id
  integer,private           :: LogFileID = 690
  integer,private,parameter :: FILE_STRING_LEN = 127
  integer,private           :: current_record  = 0
  integer,private           :: my_rank_global
  integer,private           :: log_level = NO_OUTPUT_LOG  ! (NO_OUTPUT_LOG or STANDARD_LOG or DETAIL_LOG)
  integer,private           :: max_log_level = NO_OUTPUT_LOG ! (NO_OUTPUT_LOG or STANDARD_LOG or DETAIL_LOG)
  logical,private           :: log_stderr = NO_OUTPUT_STDERR ! (NO_OUTPUT_STDERR or OUTPUT_STDERR)
  character(len=TIME_STR_LEN),private :: timestr ! time string
  character(len=FILE_STRING_LEN), private :: model_name
  character(len=2), private :: comment_char ="!#"

  ! private procedures
  private :: GetDateTimeString
  private :: set_date_time_string
  private :: write_error_message
  private :: is_trim_char

contains


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_utils()
  LogUnitID=STD_OUT
end subroutine init_utils


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_log_level(log_l, log_std)
  implicit none
  integer, intent(IN) :: log_l
  logical, intent(IN) :: log_std

  max_log_level = max(log_l, log_level)
  
  log_level = log_l
  log_stderr = (log_std .eqv. OUTPUT_STDERR)

  select case(log_level)
  case (NO_OUTPUT_LOG, STANDARD_LOG, DETAIL_LOG)
  case default
    write(0,*) "log_level setting error!!, check coupler_config"
    write(0,*) "log level is set to STANDARD_LOG"
    log_level = STANDARD_LOG
  end select

!  select case (log_std)
!  case(NO_OUTPUT_STDERR, OUTPUT_STDERR)
!  case default
!    write(0,*) "log_stderr setting error!!, check coupler_config"
!    write(0,*) "log stderr is set to OUTPUT_STDERR"
!    log_stderr = .true.
!  end select

end subroutine set_log_level
  
!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_log_level()
  implicit none

  get_log_level = log_level

end function get_log_level

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_output_stderr()
  implicit none

  is_output_stderr = log_stderr

end function is_output_stderr

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_log_unit_id(luid)
  implicit none
  integer,intent(IN)::luid ! log unit id
  LogUnitID=luid
end subroutine set_log_unit_id

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=TIME_STR_LEN) function GetDateTimeString()
  implicit none
  integer,dimension(8) :: datetime

  call DATE_AND_TIME(VALUES=datetime)
  write(timestr,'(I4,"-",I2,"-",I2,"/",I2,":",I2,":",I2)') &
        datetime(1),datetime(2),datetime(3),datetime(5),datetime(6),datetime(7)

  GetDateTimeString = timestr

end function GetDateTimeString


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_date_time_string()
  implicit none
  integer,dimension(8) :: datetime

  call DATE_AND_TIME(VALUES=datetime)
  write(timestr,'(I4,"-",I2.2,"-",I2.2,"/",I2.2,":",I2.2,":",I2.2)') &
        datetime(1),datetime(2),datetime(3),datetime(5),datetime(6),datetime(7)

end subroutine set_date_time_string

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_log(my_model_name, log_dir)
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  use jcup_constant, only : STRING_LEN 
  implicit none
  character(len=*), intent(IN) :: my_model_name
  character(len=*), optional, intent(IN) :: log_dir

  character(len=STRING_LEN) :: file_name
  character(len=STRING_LEN) :: dir_name
  character(len=4) :: pe_num

  model_name = my_model_name

  my_rank_global = jml_GetMyrankGlobal()

  if (get_log_level() /= NO_OUTPUT_LOG) then

    pe_num = '0000'
    write(pe_num,'(I4.4)') jml_GetMyrankGlobal()

    file_name = trim(model_name)//".coupling.log.PE"//pe_num

    dir_name = "."
    if (present(log_dir)) dir_name = trim(log_dir)

    call open_log_file(trim(dir_name)//"/"//trim(file_name), LogFileID)
    call open_log_file(trim(dir_name)//"/C."//trim(file_name), LogFileID+1, "DIRECT")

  end if

end subroutine init_log

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine open_log_file(file_name, log_file_unit, access_mode)
  use jcup_constant, only : STRING_LEN
  implicit none
  character(len=*), intent(IN) :: file_name
  integer, intent(IN) :: log_file_unit
  character(len=*), optional, intent(IN) :: access_mode

  logical :: isopened

  inquire(log_file_unit, opened = isopened)
  if (isopened) close(log_file_unit)

  if (present(access_mode)) then
    if (trim(access_mode)=="DIRECT") then
      open(log_file_unit, file=trim(file_name),form = 'formatted', &
           access = 'direct', recl=FILE_STRING_LEN, action = 'write', err = 200)
    else
      open(log_file_unit, file=trim(file_name),form = 'formatted', &
           access = 'sequential', action = 'write', err = 200)
    end if
  else
    open(log_file_unit, file=trim(file_name),form = 'formatted', &
           access = 'sequential', action = 'write', err = 200)
  end if

  return

200 call error('open_log_file','cannot create log file: '//trim(file_name))
    return

end subroutine open_log_file

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine finalize_log()
  implicit none


  if (max_log_level /= NO_OUTPUT_LOG) then
    call close_log_file(LogFileID)
    call close_log_file(LogFileID+1)
  end if

end subroutine finalize_log

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine close_log_file(log_file_unit)
  implicit none
  integer, intent(IN) :: log_file_unit

  close(log_file_unit)

end subroutine close_log_file

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_log(log_str, log_l)
  implicit none
  character(len=*), intent(IN) :: log_str
  integer, intent(IN), optional :: log_l

  if (is_output_stderr()) then
 
   if (present(log_l)) then
      call put_log_to_stderr(log_l, log_str)
    else
      call put_log_to_stderr(2, log_str)
    end if

  end if

  if (get_log_level() /= NO_OUTPUT_LOG) then
    if (present(log_l)) then
      call put_log_to_file(log_l, log_str)
    else
      call put_log_to_file(2, log_str)
    end if
  end if

end subroutine put_log
  
!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_log_to_stderr(log_l, LogStr)
  use jcup_mpi_lib, only : jml_isLocalLeader
  implicit none
  integer, optional, intent(IN) :: log_l
  character(len=*),intent(IN)::LogStr ! log string

  if (get_log_level() == DETAIL_LOG) then
  !if (jml_isLocalLeader()) then
    call set_date_time_string()
    write(LogUnitID,'(A19," : ",A," :: ",A)') timestr,trim(model_name),LogStr 
  !end if
  else
    if (log_l==1) then
    !if (jml_isLocalLeader()) then
      call set_date_time_string()
      write(LogUnitID,'(A19," : ",A," :: ",A)') timestr,trim(model_name),LogStr 
    !end if
    end if
  end if

end subroutine put_log_to_stderr


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine put_log_to_file(log_l, LogStr)
  implicit none
  integer, intent(IN) :: log_l
  character(len=*),intent(IN) :: LogStr ! log string
  character(len=FILE_STRING_LEN) :: log_str
  integer :: i

    call set_date_time_string()


    if (get_log_level() == DETAIL_LOG) then
      write(LogFileID,'(A19," :: ",A)') timestr,LogStr ! write message
    else
      if (log_l==1) then
        write(LogFileID,'(A19," :: ",A)') timestr,LogStr ! write message
      end if
    end if

    write(log_str,'(A19," :: ")') timestr ! write message
    do i = 24,min(len(LogStr)+23,FILE_STRING_LEN) 
      log_str(i:i) = LogStr(i-23:i-23)     
    end do
    do i=min(FILE_STRING_LEN,min(len(LogStr),FILE_STRING_LEN)+24),FILE_STRING_LEN-1
      log_str(i:i) = ' '
    end do
    log_str(FILE_STRING_LEN:FILE_STRING_LEN) = char(10)
    write(LogFileID+1,'(A)',rec=current_record+1) log_str ! write message
    current_record = mod(current_record+1,1000)

end subroutine put_log_to_file

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine write_error_message(ErrorStr)
  use jcup_mpi_lib, only : jml_isRoot
  implicit none
  character(len=*),intent(IN)::ErrorStr ! error message string

  call put_log_to_file(1,errorStr)

  if (jml_isRoot()) then
    call set_date_time_string()
    write(STD_ERR,'(A19," :: ",A)') &
       timestr,errorStr ! write message
  end if

end subroutine Write_error_message

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine error(routine_name,message)
  use jcup_mpi_lib, only : jml_abort
  implicit none 
  character(len=*),intent(IN) :: routine_name,message

  character(len=MAX_STRING_LENGTH) :: message_str
  character(len=MAX_STRING_LENGTH) :: istr
  integer :: i

  write(istr, *) my_rank_global

  write(message_str, &
      '("!!! error !!! [RANK=",A,"][",A,"] : ",A,", program terminated")') &
      trim(adjustl(istr)),routine_name,message

  write(0,*) message_str

  call write_error_message(trim(message_str))
  call close_log_file(LogFileID)
  call close_log_file(LogFileID+1)
  call jml_abort()
  stop

end subroutine error

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_argument(routine_name, my_value, l_value, u_value)
  implicit none
  character(len=*),intent(IN) :: routine_name ! routine name
  integer,intent(IN) :: my_value, l_value, u_value

  if ((l_value<=my_value).and.(my_value<=u_value)) return

  call error(routine_name,"argument error")

end subroutine check_argument


!=======+=========+=========+=========+=========+=========+=========+=========+
!  2014/10/30 [MOD] IntToStr -> IntegerToStr
character(len=MAX_STRING_LENGTH) function IntegerToStr(idata) 
  implicit none
  integer,intent(IN) :: idata

  character(len=MAX_STRING_LENGTH) :: istr

  write(istr, *, err=100) idata
  IntegerToStr = trim(adjustl(istr))

  return

100 continue

  call error("IntToStr","Integer argument data format error")

end function IntegerToStr

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=MAX_STRING_LENGTH) function LongIntToStr(idata) 
  implicit none
  integer(kind=8),intent(IN) :: idata

  character(len=MAX_STRING_LENGTH) :: istr

  write(istr, *, err=100) idata
  LongIntToStr = trim(adjustl(istr))

  return

100 continue

  call error("IntToStr","Integer argument data format error")

end function LongIntToStr

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function StrToInt(istr) 
  implicit none
  character(len=*),intent(IN) :: istr
  integer :: idata

  read(istr, *, err=100) idata
  StrToInt = idata
  
  return

100 continue

    call error("StrToInt", 'String argument "'//trim(istr)//'" format error')
    
end function StrToInt

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=14) function IDate2CDate(yyyy,mo,dd,hh,mm,ss)
  implicit none
  integer,intent(IN) :: yyyy,mo,dd,hh,mm,ss

  character(len=14) :: cdate

  write(cdate, '(I4.4,I2.2,I2.2,I2.2,I2.2,I2.2)') yyyy, mo, dd, hh, mm, ss
  !write(cdate(1:4),'(i4)', err=100) yyyy
  !write(cdate(5:5),'(i1)', err=100) int(mo/10)
  !write(cdate(6:6),'(i1)', err=100) mod(mo,10)
  !write(cdate(7:7),'(i1)', err=100) int(dd/10)
  !write(cdate(8:8),'(i1)', err=100) mod(dd,10)
  !write(cdate(9:9),'(i1)', err=100) int(hh/10)
  !write(cdate(10:10),'(i1)', err=100) mod(hh,10)
  !write(cdate(11:11),'(i1)', err=100) int(mm/10)
  !write(cdate(12:12),'(i1)', err=100) mod(mm,10)
  !write(cdate(13:13),'(i1)', err=100) int(ss/10)
  !write(cdate(14:14),'(i1)', err=100) mod(ss,10)
    
  IDate2CDate = cdate

  return

100 continue

    call error("IDate2CDate","Integer arguments format error")

end function IDate2CDate

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cdate_2_idate(cdate,yyyy,mo,dd,hh,mm,ss)
  implicit none
  character(len=*),intent(IN) :: cdate
  integer,intent(INOUT)       :: yyyy,mo,dd,hh,mm,ss

  read(cdate(1:4),*, err=100) yyyy
  read(cdate(5:6),*, err=100) mo
  read(cdate(7:8),*, err=100) dd
  read(cdate(9:10),*, err=100) hh
  read(cdate(11:12),*, err=100) mm
  read(cdate(13:14),*, err=100) ss

  return

100 continue

    call error("cdate_2_idate", 'String argument "'//trim(cdate)//'" format error')

end subroutine cdate_2_idate


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine sort_int_1d(num_of_data, data)
  implicit none
  integer, intent(IN) :: num_of_data
  integer, intent(INOUT) :: data(:)
  
  integer :: temp_data
  integer :: i,j

  do i = num_of_data, 1, -1
    do j = 1, i-1
      if (data(j) > data(j+1)) then
        temp_data = data(j+1)
        data(j+1) = data(j)
        data(j)   = temp_data
      end if
    end do
  end do

end subroutine sort_int_1d

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine split_string(source_str, reg_chr, splited_str1, splited_str2)
  implicit none
  character(len=*), intent(IN) :: source_str
  character, intent(IN) :: reg_chr
  character(len=*), intent(INOUT) :: splited_str1, splited_str2

  integer :: str_counter, char_counter
  character :: sc, rc
  integer :: i, j
  logical :: is_substring
  character(len=2) :: substr_chr 

  splited_str1 = ""
  splited_str2 = ""

  is_substring = .false.
  str_counter = 0
  do i = 1, len_trim(source_str)
    str_counter = str_counter+1
    if (source_str(i:i)=="=") then
      is_substring = .true.
      str_counter = 0
      cycle
    end if
    if (is_substring) then
      splited_str2(str_counter:str_counter) = source_str(i:i)
    else
      splited_str1(str_counter:str_counter) = source_str(i:i)
    end if
  end do

  splited_str1 = trim(adjustl(splited_str1))
  splited_str2 = trim(adjustl(splited_str2))

end subroutine split_string

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=MAX_STRING_LENGTH) function TrimString(source_str, trim_str)
  implicit none
  character(len=*), intent(INOUT) :: source_str
  character(len=*), intent(IN) :: trim_str

  integer :: start_counter, end_counter
  integer :: i

  start_counter = 1
  do while(is_trim_char(source_str(start_counter:start_counter), trim_str))
   start_counter = start_counter+1
  end do

  end_counter = len_trim(source_str)
  do while(is_trim_char(source_str(end_counter:end_counter), trim_str))
    end_counter = end_counter-1
  end do

  TrimString = trim(adjustl(source_str(start_counter:end_counter)))

end function TrimString

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_trim_char(s, trim_str)
  implicit none
  character, intent(IN) :: s
  character(len=*), intent(IN) :: trim_str
  integer :: i

  is_trim_char = .false.
  do i = 1, len(trim_str)
    if (s==trim_str(i:i)) then
      is_trim_char = .true.
      return
    end if
  end do
end function is_trim_char

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function startsWith(source_str, prefix)
  implicit none
  character(len=*), intent(IN) :: source_str
  character(len=*), intent(IN) :: prefix

  if (index(trim(source_str), prefix)==1) then
    startsWith = .true.
  else
    startsWith = .false.
  end if

end function startsWith

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=MAX_STRING_LENGTH) function lw2up(buf)
  implicit none
  character(len=*), intent(IN) :: buf
  integer :: i
  character(len=MAX_STRING_LENGTH) :: ret_str

  do i = 1, len(buf)
    if ((ichar(buf(i:i))>=97).and.(ichar(buf(i:i))<=122)) then
      ret_str(i:i) = char(ichar(buf(i:i))-32)
    else
      ret_str(i:i) = buf(i:i)  
    end if
  end do

  lw2up = trim(ret_str)

end function lw2up

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=MAX_STRING_LENGTH) function up2lw(buf)
  implicit none
  character(len=*), intent(IN) :: buf
  integer :: i
  character(len=MAX_STRING_LENGTH) :: ret_str
  ret_str=""

  do i = 1, len_trim(buf)
    if ((ichar(buf(i:i))>=65).and.(ichar(buf(i:i))<=90)) then
      ret_str(i:i) = char(ichar(buf(i:i))+32)
    else
      ret_str(i:i) = buf(i:i)
    end if
  end do

  up2lw = trim(ret_str)

end function up2lw

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_comment_line(data_str)
  implicit none
  character(len=*), intent(IN) :: data_str

  integer :: i

  is_comment_line = .false.

  do i = 1, len_trim(comment_char)
    if (startsWith(data_str, comment_char(i:i))) is_comment_line = .true.
  end do

end function is_comment_line

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=MAX_STRING_LENGTH) function cut_comment(source_str)
  implicit none
  character(len=*), intent(IN) :: source_str
  integer :: i

  cut_comment = ""

  do i = 1, len_trim(source_str)
    if (is_trim_char(source_str(i:i), comment_char)) exit
    cut_comment(i:i) = source_str(i:i)
  end do

end function cut_comment

!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcup_utils









