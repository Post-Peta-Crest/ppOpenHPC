module ppoh_MATHMP_namelist
  use jcup_interface, only : jcup_varp_type, jcup_varg_type
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  integer, parameter :: RUN_MODE_DEBUG = 2
  integer, parameter :: RUN_MODE_NORMAL = 0
  character(len=3), parameter :: EX_MODE_SNP = "SNP"
  character(len=3), parameter :: EX_MODE_AVR = "AVR"

  public :: exchange_config_type
  public :: vector_config_type
  public :: read_coupling_config     ! subroutine (namelist_file_name)
  public :: get_run_mode             ! integer function ()
  public :: get_num_of_comp_config   ! integer function ()
  public :: get_comp_config          ! subroutine (comp_conf_num, send_comp, send_grid, recv_comp, recv_grid, remap_file_name)
  public :: get_comp_config_ptr      ! exchange_config_type, pointer, function (comp_conf_num) 
  public :: get_num_of_data_config   ! integer function (comp_conf_num)
  public :: get_data_config          ! subroutine (comp_conf_num, data_conf_num, send_data, recv_data, interval, exchange_mode)
  public :: get_num_of_vector_config ! integer function ()
  public :: get_vector_config_ptr    ! vector_config_type, pointer function (vector_conf_num)
  
!--------------------------------  private  ----------------------------------!

  integer, parameter :: NAME_LEN = 32
  integer, parameter :: STR_LEN  = 128

  type coupling_config_type
    integer :: run_mode = RUN_MODE_NORMAL
  end type

  type (coupling_config_type), save :: coupling_config

  type exchange_data_type
    character(len=NAME_LEN) :: send_data = ""
    character(len=NAME_LEN) :: recv_data = ""
    integer :: interval
    character(len=3) :: exchange_mode = EX_MODE_SNP
    type (jcup_varp_type), pointer :: varp_ptr => null()
    type (jcup_varg_type), pointer :: varg_ptr => null()
  end type

  type exchange_config_type
    character(len=NAME_LEN) :: send_comp = ""
    character(len=NAME_LEN) :: send_grid = ""
    character(len=NAME_LEN) :: recv_comp = ""
    character(len=NAME_LEN) :: recv_grid = ""
    character(len=STR_LEN)  :: map_file_name = ""
    character(len=1)        :: map_file_type = ""
    integer :: num_of_data = 0
    type (exchange_data_type), pointer :: ed(:)
  end type

  integer :: num_of_config = 0
  type (exchange_config_type), pointer :: conf(:)

  type vector_config_type
    character(len=NAME_LEN) :: data_name
    character(len=NAME_LEN) :: comp_name
    character(len=NAME_LEN) :: vector_data(3)
    character(len=STR_LEN)  :: rot_file_name
    character(len=1)        :: rot_file_type
  end type

  integer :: num_of_vector = 0
  type (vector_config_type), pointer :: vconf(:)
  
contains

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> read namelist file
subroutine read_coupling_config(namelist_file_name)
  use ppoh_MATHMP_base, only : LOG_FID, error
  implicit none
  character(len=*), intent(IN) :: namelist_file_name
  character(len=6) :: run_mode
  character(len=NAME_LEN) :: send_comp, send_grid, recv_comp, recv_grid
  character(len=STR_LEN) :: map_file_name
  character(len=1      ) :: map_file_type
  character(len=NAME_LEN) :: send_data, recv_data
  integer :: interval
  character(len=3) :: flag
  character(len=NAME_LEN) :: data_name
  character(len=NAME_LEN) :: comp_name
  character(len=NAME_LEN) :: vector_data(3)
  character(len=STR_LEN)  :: rot_file_name
  character(len=1)        :: rot_file_type

  integer, parameter :: FID = 68
  integer :: istat
  namelist / nmcoupling / run_mode, send_comp, send_grid, recv_comp, recv_grid, map_file_name, map_file_type, &
                          send_data, recv_data, interval, flag, &
                          data_name, comp_name, vector_data, rot_file_name, rot_file_type

  open(FID, file=trim(namelist_file_name), action='read', status="OLD", iostat = istat)

  if (istat /= 0) then
     call error("read_coupling_config, File Open error ! file name = "//trim(namelist_file_name))
  end if

  call read_coupling_base()
  call count_comp_config()
  call read_comp_config()
  call count_data_config()
  call read_data_config()
  call count_vector_config()
  call read_vector_config()
 
  close(FID)


contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine read_coupling_base()
  implicit none

  rewind(FID)
  
  do 
    run_mode = ""
    read(FID, nml=nmcoupling, iostat = istat, end = 7000)
    if (trim(run_mode) /= "") then
      if (trim(run_mode) == "DEBUG") then
        coupling_config%run_mode = RUN_MODE_DEBUG
      end if
      return
    end if

  end do

  7000 continue

  return

end subroutine read_coupling_base

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine count_comp_config()
  implicit none

  rewind(FID)

  num_of_config = 0
  do 
    send_comp = ""
    read(FID, nml=nmcoupling, iostat = istat, end = 8000)
    if (trim(send_comp) /= "") num_of_config = num_of_config + 1
  end do

  8000 continue

  allocate(conf(num_of_config))

  return
  
end subroutine count_comp_config

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine read_comp_config()
  implicit none

  rewind(FID)

  num_of_config = 0

  do 

    send_comp = ""
    read(FID, nml=nmcoupling, iostat = istat, end = 8000)
    if (trim(send_comp) /= "") then
      num_of_config = num_of_config + 1
      conf(num_of_config)%send_comp = send_comp
      conf(num_of_config)%send_grid = send_grid
      conf(num_of_config)%recv_comp = recv_comp
      conf(num_of_config)%recv_grid = recv_grid
      conf(num_of_config)%map_file_name = map_file_name
      conf(num_of_config)%map_file_type = map_file_type
    end if

  end do

  8000 continue

  return
  
end subroutine read_comp_config

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine count_data_config()
  implicit none
  integer :: i

  rewind(FID)

  num_of_config = 0
  do 
    send_comp = ""
    send_data = ""
    read(FID, nml=nmcoupling, iostat = istat, end = 8000)
    if (trim(send_comp) /= "") then
      num_of_config = num_of_config + 1
      conf(num_of_config)%num_of_data = 0
    end if
    if (trim(send_data) /= "") then
      conf(num_of_config)%num_of_data = conf(num_of_config)%num_of_data + 1
    end if
  end do

  8000 continue

  do i = 1, num_of_config
    allocate(conf(i)%ed(conf(i)%num_of_data))
  end do

  return
  
end subroutine count_data_config

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine read_data_config()
  implicit none
  integer :: num_of_data
  integer :: i

  rewind(FID)

  num_of_config = 0
  do 
    send_comp = ""
    send_data = ""
    read(FID, nml=nmcoupling, iostat = istat, end = 8000)
    if (trim(send_comp) /= "") then
      num_of_config = num_of_config + 1
      num_of_data = 0
    end if
    if (trim(send_data) /= "") then
      num_of_data = num_of_data + 1
      conf(num_of_config)%ed(num_of_data)%send_data = send_data
      conf(num_of_config)%ed(num_of_data)%recv_data = recv_data
      conf(num_of_config)%ed(num_of_data)%interval  = interval
      if (flag == "AVR")  conf(num_of_config)%ed(num_of_data)%exchange_mode = EX_MODE_AVR
    end if
  end do

  8000 continue

  return
  
end subroutine read_data_config

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine count_vector_config()
  implicit none

  rewind(FID)

  num_of_vector = 0
  do 
    data_name = ""
    read(FID, nml=nmcoupling, iostat = istat, end = 8000)
    if (trim(data_name) /= "") num_of_vector = num_of_vector + 1
  end do

  8000 continue

  allocate(vconf(num_of_vector))

  return
  
end subroutine count_vector_config

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine read_vector_config()
  implicit none

  rewind(FID)

  num_of_vector = 0

  do 

    data_name = ""
    read(FID, nml=nmcoupling, iostat = istat, end = 8000)
    if (trim(data_name) /= "") then
      num_of_vector = num_of_vector + 1
      vconf(num_of_vector)%data_name = data_name
      vconf(num_of_vector)%comp_name = comp_name
      vconf(num_of_vector)%vector_data(:) = vector_data(:)
      vconf(num_of_vector)%rot_file_name = rot_file_name
      vconf(num_of_vector)%rot_file_type = rot_file_type
    end if

  end do

  8000 continue

  return
  
end subroutine read_vector_config

!=======+=========+=========+=========+=========+=========+=========+=========+

end subroutine read_coupling_config

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_run_mode() result(res)
  implicit none
  integer :: res

  res = coupling_config%run_mode

end function get_run_mode

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_comp_config() result(res)
  implicit none

  res = num_of_config

end function get_num_of_comp_config

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_comp_config(comp_conf_num, send_comp, send_grid, recv_comp, recv_grid, map_file_name, map_file_type)
  implicit none
  integer, intent(IN) :: comp_conf_num
  character(len=*), intent(OUT) :: send_comp, send_grid, recv_comp, recv_grid, map_file_name, map_file_type

  send_comp = conf(comp_conf_num)%send_comp
  send_grid = conf(comp_conf_num)%send_grid
  recv_comp = conf(comp_conf_num)%recv_comp
  recv_grid = conf(comp_conf_num)%recv_grid
  map_file_name = conf(comp_conf_num)%map_file_name
  map_file_type = conf(comp_conf_num)%map_file_type

end subroutine get_comp_config

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_comp_config_ptr(comp_conf_num) result(res)
  implicit none
  integer, intent(IN) :: comp_conf_num
  type (exchange_config_type), pointer :: res

  res => conf(comp_conf_num)

end function get_comp_config_ptr

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_data_config(comp_conf_num)  result(res)
  implicit none
  integer, intent(IN) :: comp_conf_num

  res = conf(comp_conf_num)%num_of_data

end function get_num_of_data_config

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_data_config(comp_conf_num, data_conf_num, send_data, recv_data, interval, flag)
  implicit none
  integer, intent(IN) :: comp_conf_num, data_conf_num
  character(len=*), intent(OUT) :: send_data, recv_data
  integer, intent(OUT) :: interval
  character(len=3), intent(OUT) :: flag

  send_data = conf(comp_conf_num)%ed(data_conf_num)%send_data
  recv_data = conf(comp_conf_num)%ed(data_conf_num)%recv_data
  interval  = conf(comp_conf_num)%ed(data_conf_num)%interval

  if (conf(comp_conf_num)%ed(data_conf_num)%exchange_mode == EX_MODE_AVR) then
     flag = "AVR"
  else
     flag = "SNP"
  end if
  
end subroutine get_data_config

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_vector_config() result(res)
  implicit none

  res = num_of_vector

end function get_num_of_vector_config

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_vector_config_ptr(vector_conf_num) result(res)
  implicit none
  integer, intent(IN) :: vector_conf_num
  type (vector_config_type), pointer :: res

  res => vconf(vector_conf_num)

end function get_vector_config_ptr

end module ppoh_MATHMP_namelist
