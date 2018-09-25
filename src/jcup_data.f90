!
!Copyright (c) 2011, arakawa@rist.jp
!All rights reserved.
!
module jcup_data
  use jcup_constant, only : NAME_LEN
  use jcup_grid_base, only : local_area_type
  use jcup_config, only : send_data_conf_type, recv_data_conf_type
  use jcup_time, only : time_type
  implicit none
  private

!--------------------------------   public  ----------------------------------!
  public :: varp_type
  public :: varg_type
  public :: init_data_def
  public :: set_default_config ! subroutine (my_comp_name, send_comp_name, recv_mode, interval, time_lag, mapping_tag, exchange_tag)
  public :: def_varp ! subroutine (data_type_ptr, comp_name, data_name, grid_index, num_of_data)
  public :: end_def_varp ! subroutine (comp_id)
  public :: def_varg ! subroutine (data_type_ptr, comp_name, data_name, grid_index, num_of_data, 
                     ! send_model_name, send_data_name, recv_mode, interval, time_lag, mapping_tag, exchange_tag)
  public :: end_def_varg ! subroutine (comp_id)
  public :: check_data_definition
  public :: get_send_data_dimension
  public :: get_recv_data_dimension
  public :: get_num_of_exchange_send_data ! integer function (comp_id, data_name) ! return num_of_data ( = num_of_vgrid) of the data
  public :: get_num_of_exchange_recv_data ! integer function (comp_id, data_name) ! return num_of_data ( = num_of_vgrid) of the data
  public :: is_data_defined
  public :: get_comp_id   ! integer function (varp_type or varg_type)
  public :: get_data_name ! character(len=NAME_LEN) function (varp_type or varg_type)
  public :: set_time ! subroutine (varp_type or varg_type, current_time)
  public :: get_time ! type(time_type) function (varp_type or varg_type)
  public :: get_varp_data_dim ! integer function (varp_type) ! 2015/02/23 [ADD]
  public :: get_varg_data_dim ! integer function (varg_type) ! 2015/02/23 [ADD]
  public :: get_varp_num_of_data ! integer function (varp_type) ! 2015/02/23 [ADD]
  public :: get_varg_num_of_data ! integer function (varg_type) ! 2015/02/23 [ADD]
  public :: data_array_size_ok ! logical function (varp_type or varg_type, s1, s2, s3)
 
!--------------------------------   private  ---------------------------------!

  ! default setting
  type default_setting_type
    character(len=3) :: recv_mode = "SNP"
    integer :: time_lag = -1
    integer :: interval = 3600
    integer :: mapping_tag = 1
    integer :: exchange_tag = 1
  end type

  type(default_setting_type), pointer :: default_setting(:,:) ! (num_of_component, num_of_component)


  type varp_type
    type(varp_type), pointer :: next_ptr
    character(len=NAME_LEN) :: name
    integer :: grid_index
    type(local_area_type), pointer :: my_grid
    type(send_data_conf_type), pointer :: sd => null()
    integer :: data_dimension_type
    integer :: num_of_data ! number of data (for 2.5D)
    type(time_type) :: current_time
  end type

  type(varp_type), pointer :: sd_ptr(:) ! (num_of_total_component)
  type(varp_type), pointer :: current_sd_ptr

  type varg_type
    type(varg_type), pointer :: next_ptr
    character(len=NAME_LEN) :: name
    character(len=3) :: recv_mode ! "SNP" or "AVR"
    integer :: interval  ! exchange interval
    integer :: time_lag
    integer :: mapping_tag
    integer :: exchange_tag
    character(len=NAME_LEN) :: send_model_name
    character(len=NAME_LEN) :: send_data_name
    integer :: grid_index
    type(local_area_type), pointer :: my_grid
    type(recv_data_conf_type), pointer :: rd => null()
    integer :: data_dimension_type
    integer :: num_of_data ! number of data (for 2.5D)
    type(time_type) :: current_time
  end type

  type(varg_type), pointer :: rd_ptr(:) ! (num_of_total_component)
  type(varg_type), pointer :: current_rd_ptr

  !type(varp_type), pointer, private :: sd_def(:) ! array of my send data information
  !type(varg_type), pointer, private :: rd_def(:) ! array of my recv data information

  interface get_comp_id
    module procedure get_varp_comp_id, get_varg_comp_id
  end interface

  interface get_data_name
    module procedure get_varp_data_name, get_varg_data_name
  end interface

  interface set_time
    module procedure set_varp_time, set_varg_time
  end interface

  interface get_time
    module procedure get_varp_time, get_varg_time
  end interface

  interface data_array_size_ok
    module procedure put_data_array_size_ok, get_data_array_size_ok
  end interface

  integer, private :: send_counter
  integer, private :: recv_counter

contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_data_def()
  use jcup_config, only : get_num_of_send_data, get_num_of_recv_data, set_current_conf
  use jcup_comp, only : get_num_of_total_component, is_my_component
  implicit none
  integer :: data_counter
  integer :: num_of_model
  integer :: i

  num_of_model = get_num_of_total_component()

  send_counter = 0
  recv_counter = 0

  allocate(sd_ptr(num_of_model))
  do i = 1, size(sd_ptr)
    nullify(sd_ptr(i)%next_ptr)
  end do

  allocate(rd_ptr(num_of_model))
  do i = 1, size(rd_ptr)
    nullify(rd_ptr(i)%next_ptr)
  end do

  allocate(default_setting(num_of_model, num_of_model))

end subroutine init_data_def

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_default_config(my_comp, send_comp, recv_mode, interval, time_lag, mapping_tag, exchange_tag)
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: my_comp, send_comp
  character(len=3), optional, intent(IN) :: recv_mode
  integer, optional, intent(IN) :: interval
  integer, optional, intent(IN) :: time_lag
  integer, optional, intent(IN) :: mapping_tag
  integer, optional, intent(IN) :: exchange_tag

  integer :: my_comp_id, send_comp_id

  my_comp_id = get_comp_id_from_name(trim(my_comp))
  send_comp_id = get_comp_id_from_name(trim(send_comp))

  if (present(recv_mode)) default_setting(my_comp_id, send_comp_id)%recv_mode = recv_mode
  if (present(time_lag)) default_setting(my_comp_id, send_comp_id)%time_lag = time_lag
  if (present(interval)) default_setting(my_comp_id, send_comp_id)%interval = interval
  if (present(mapping_tag)) default_setting(my_comp_id, send_comp_id)%mapping_tag = mapping_tag
  if (present(exchange_tag)) default_setting(my_comp_id, send_comp_id)%exchange_tag = exchange_tag
  
end subroutine set_default_config

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine def_varp(data_type_ptr, comp_name, data_name, grid_index, num_of_data)
  use jcup_constant, only : DATA_1D, DATA_2D, DATA_25D, DATA_3D
  use jcup_utils, only : error, put_log, IntToStr
  use jcup_config, only : is_my_send_data, get_send_data_conf_ptr, &
                          is_my_recv_data, get_recv_data_conf_ptr, &
                          set_current_conf
  use jcup_comp, only : get_comp_id_from_name
  use jcup_grid_base, only : get_my_local_area_ptr
  implicit none
  type(varp_type), pointer :: data_type_ptr
  character(len=*), intent(IN) :: comp_name
  character(len=*), intent(IN) :: data_name
  integer, intent(IN) :: grid_index
  integer, optional, intent(IN) :: num_of_data ! for 2.5 D data
  logical :: is_25D_data
  integer :: comp_id

  is_25D_data = .false.

  if (present(num_of_data)) then
    if (num_of_data > 1) is_25D_data = .true.
  end if

  comp_id = get_comp_id_from_name(trim(comp_name))

  call set_current_conf(comp_id)

  current_sd_ptr => sd_ptr(comp_id)

  do 
    if (.not.associated(current_sd_ptr%next_ptr)) then
      allocate(current_sd_ptr%next_ptr)
      current_sd_ptr => current_sd_ptr%next_ptr
      nullify(current_sd_ptr%next_ptr)
      exit
    end if
    current_sd_ptr => current_sd_ptr%next_ptr
  end do

  current_sd_ptr%grid_index = grid_index
  current_sd_ptr%my_grid => get_my_local_area_ptr(comp_id, grid_index)
  current_sd_ptr%name = data_name

  if (is_25D_data) then
    current_sd_ptr%data_dimension_type = DATA_25D
    current_sd_ptr%num_of_data = num_of_data
  else
    current_sd_ptr%data_dimension_type = DATA_1D !DATA_2D
    current_sd_ptr%num_of_data = 1
  end if

  data_type_ptr => current_sd_ptr
  call put_log("jcup_def_varp : Send data definition : "//trim(data_name)//", defined data dimension : " &
             !//trim(IntToStr(current_sd_ptr%data_dimension_type)), 2)
             //trim(IntToStr(current_sd_ptr%num_of_data)), 2)
      
  return

end subroutine def_varp

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine end_def_varp(comp_id)
  use jcup_config, only : set_current_conf, init_send_config, set_send_config, get_send_data_conf_ptr
  implicit none
  integer, intent(IN) :: comp_id
  integer :: data_counter

  call set_current_conf(comp_id)

  current_sd_ptr => sd_ptr(comp_id)
  data_counter = 0
  do
    if (.not.associated(current_sd_ptr%next_ptr)) exit
    data_counter = data_counter + 1
    current_sd_ptr => current_sd_ptr%next_ptr
  end do

  call init_send_config(comp_id, data_counter)

  current_sd_ptr => sd_ptr(comp_id)
  data_counter = 0
  do
    if (.not.associated(current_sd_ptr%next_ptr)) exit
    data_counter = data_counter + 1
    current_sd_ptr => current_sd_ptr%next_ptr
    call set_send_config(comp_id, data_counter, current_sd_ptr%name, current_sd_ptr%grid_index, current_sd_ptr%num_of_data)
    current_sd_ptr%sd => get_send_data_conf_ptr(current_sd_ptr%name)
  end do


end subroutine end_def_varp

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine def_varg(data_type_ptr, comp_name, data_name, grid_index, num_of_data, send_model_name, send_data_name, &
                    recv_mode, &
                    interval, time_lag, mapping_tag, exchange_tag)
  use jcup_constant, only : DATA_1D, DATA_2D, DATA_25D, DATA_3D
  use jcup_utils, only : error, put_log, IntToStr
  use jcup_config, only : is_my_send_data, get_send_data_conf_ptr, &
                          is_my_recv_data, get_recv_data_conf_ptr, &
                          set_current_conf
  use jcup_comp, only : get_comp_id_from_name
  use jcup_grid_base, only : get_my_local_area_ptr
  implicit none
  type(varg_type), pointer :: data_type_ptr
  character(len=*), intent(IN) :: comp_name
  character(len=*), intent(IN) :: data_name
  integer, intent(IN) :: num_of_data ! for 2.5 D data
  character(len=*), intent(IN) :: send_model_name
  character(len=*), intent(IN) :: send_data_name
  integer, intent(IN) :: grid_index
  character(len=3), optional, intent(IN) :: recv_mode
  integer, optional, intent(IN) :: interval
  integer, optional, intent(IN) :: time_lag
  integer, optional, intent(IN) :: mapping_tag
  integer, optional, intent(IN) :: exchange_tag
  logical :: is_25D_data
  integer :: comp_id
  integer :: send_comp_id

  is_25D_data = .false.
  if (num_of_data > 1) is_25D_data = .true.

  comp_id = get_comp_id_from_name(trim(comp_name))

  call set_current_conf(comp_id)

  send_comp_id = get_comp_id_from_name(trim(send_model_name))

  current_rd_ptr => rd_ptr(comp_id)

  do 
    if (.not.associated(current_rd_ptr%next_ptr)) then
      allocate(current_rd_ptr%next_ptr)
      current_rd_ptr => current_rd_ptr%next_ptr
      nullify(current_rd_ptr%next_ptr)
      exit
    end if
    current_rd_ptr => current_rd_ptr%next_ptr
  end do

  current_rd_ptr%grid_index = grid_index
  current_rd_ptr%my_grid => get_my_local_area_ptr(comp_id, grid_index)
  current_rd_ptr%name = data_name
  current_rd_ptr%send_model_name = send_model_name
  current_rd_ptr%send_data_name  = send_data_name

  if (present(recv_mode)) then
    current_rd_ptr%recv_mode = recv_mode
  else
    current_rd_ptr%recv_mode = default_setting(comp_id, send_comp_id)%recv_mode
  end if

  if (present(interval)) then
    current_rd_ptr%interval = interval
  else
    current_rd_ptr%interval = default_setting(comp_id, send_comp_id)%interval
  end if

  if (present(time_lag)) then
    current_rd_ptr%time_lag = time_lag
  else
    current_rd_ptr%time_lag = default_setting(comp_id, send_comp_id)%time_lag
  end if

  if (present(mapping_tag)) then
    current_rd_ptr%mapping_tag = mapping_tag
  else
    current_rd_ptr%mapping_tag = default_setting(comp_id, send_comp_id)%mapping_tag
  end if

  if (present(exchange_tag)) then
    current_rd_ptr%exchange_tag = exchange_tag
  else
    current_rd_ptr%exchange_tag = default_setting(comp_id, send_comp_id)%exchange_tag
  end if


  if (is_25D_data) then
    current_rd_ptr%data_dimension_type = DATA_25D
    current_rd_ptr%num_of_data = num_of_data
  else
    current_rd_ptr%data_dimension_type = DATA_1D !DATA_2D
    current_rd_ptr%num_of_data = 1
  end if

  data_type_ptr => current_rd_ptr

  call put_log("jcup_def_varg : Recv data definition : "//trim(comp_name)//" : "//trim(data_name)//", data dim : " &
             //trim(IntToStr(current_rd_ptr%num_of_data))//&
              ", exchange data tag : "//trim(IntToStr(current_rd_ptr%exchange_tag))//&
              ", time lag : "//trim(IntToStr(current_rd_ptr%time_lag)), 2)

end subroutine def_varg

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine end_def_varg(comp_id)
  use jcup_config, only : set_current_conf, init_recv_config, set_recv_config, get_recv_data_conf_ptr
  implicit none
  integer, intent(IN) :: comp_id
  integer :: data_counter

  call set_current_conf(comp_id)

  current_rd_ptr => rd_ptr(comp_id)
  data_counter = 0
  do
    if (.not.associated(current_rd_ptr%next_ptr)) exit
    data_counter = data_counter + 1
    current_rd_ptr => current_rd_ptr%next_ptr
  end do

  call init_recv_config(comp_id, data_counter)

  current_rd_ptr => rd_ptr(comp_id)
  data_counter = 0
  do
    if (.not.associated(current_rd_ptr%next_ptr)) exit
    data_counter = data_counter + 1
    current_rd_ptr => current_rd_ptr%next_ptr
    call set_recv_config(comp_id, data_counter, current_rd_ptr%name, current_rd_ptr%grid_index, &
                         current_rd_ptr%num_of_data, &
                         current_rd_ptr%recv_mode, current_rd_ptr%interval, current_rd_ptr%time_lag, &
                         current_rd_ptr%mapping_tag, current_rd_ptr%exchange_tag, &
                         current_rd_ptr%send_model_name, current_rd_ptr%send_data_name)
    current_rd_ptr%rd => get_recv_data_conf_ptr(current_rd_ptr%name)
  end do

end subroutine end_def_varg

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_data_definition()
  use jcup_config, only : get_num_of_send_data, get_num_of_recv_data, set_current_conf
  use jcup_comp, only : get_num_of_total_component, is_my_component
  use jcup_utils, only : error
  implicit none
  integer :: i, j

  do i = 1, get_num_of_total_component()
    if (is_my_component(i)) then
      call set_current_conf(i)
      do j = 1, get_num_of_send_data()
        call check_send_data_definition(i, j)
      end do
    end if
  end do

  do i = 1, get_num_of_total_component()
    if (is_my_component(i)) then
      call set_current_conf(i)
      do j = 1, get_num_of_recv_data()
        call check_recv_data_definition(i, j)
      end do
    end if
  end do

end subroutine check_data_definition


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_send_data_definition(comp_id, data_num)
  use jcup_config, only : send_data_conf_type, get_send_data_conf_ptr_from_id
  use jcup_comp, only : get_component_name
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: comp_id, data_num
  type(send_data_conf_type), pointer :: sd
  integer :: i

  return

  !sd => get_send_data_conf_ptr_from_id(comp_id, data_num)

  !if (.not.sd%is_send) return

  !do i = 1, size(sd_def)
  !  if (.not.associated(sd_def(i)%sd)) cycle
  !  if ((sd%model_id == sd_def(i)%sd%model_id).and.(trim(sd%name) == trim(sd_def(i)%sd%name))) return
  !end do

  !call error("check_send_data_definition", "model : "//trim(get_component_name(comp_id))//", send data "//trim(sd%name)//" is not defined")

end subroutine check_send_data_definition

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_recv_data_definition(comp_id, data_num)
  use jcup_config, only : recv_data_conf_type, get_recv_data_conf_ptr_from_id
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: comp_id, data_num
  type(recv_data_conf_type), pointer :: rd
  integer :: i
 
  return

  !rd => get_recv_data_conf_ptr_from_id(comp_id, data_num)

  !if (.not.rd%is_recv) return

  !do i = 1, size(rd_def)
  !  if (.not.associated(rd_def(i)%rd)) cycle
  !  if ((rd%model_id == rd_def(i)%rd%model_id).and.(trim(rd%name) == trim(rd_def(i)%rd%name))) return
  !end do

  !call error("check_recv_data_definition", "recv data "//trim(rd%name)//" is not defined")
  
end subroutine check_recv_data_definition

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_send_data_dimension(comp_id, data_name)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: comp_id
  character(len=*), intent(IN) :: data_name
  integer :: i

  get_send_data_dimension = 0

  current_sd_ptr => sd_ptr(comp_id)

  do
    if (.not.associated(current_sd_ptr%next_ptr)) exit
    current_sd_ptr => current_sd_ptr%next_ptr
    if (trim(data_name)==trim(current_sd_ptr%sd%name)) then
      get_send_data_dimension = current_sd_ptr%data_dimension_type
      return
    end if
  end do

  call error("get_send_data_dimension", "No such data defined : "//trim(data_name))

end function get_send_data_dimension

  
!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_recv_data_dimension(comp_id, data_name)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: comp_id
  character(len=*), intent(IN) :: data_name
  integer :: i

  get_recv_data_dimension = 0

  current_rd_ptr => rd_ptr(comp_id)

  do
    if (.not.associated(current_rd_ptr%next_ptr)) exit
    current_rd_ptr => current_rd_ptr%next_ptr
    if (trim(data_name)==trim(current_rd_ptr%rd%name)) then
      get_recv_data_dimension = current_rd_ptr%data_dimension_type
      return
    end if
  end do

  call error("get_recv_data_dimension", "No such data defined : "//trim(data_name))

end function get_recv_data_dimension

  
!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_exchange_send_data(comp_id, data_name)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: comp_id
  character(len=*), intent(IN) :: data_name
  integer :: i

  get_num_of_exchange_send_data = 0

  current_sd_ptr => sd_ptr(comp_id)

  do
    if (.not.associated(current_sd_ptr%next_ptr)) exit
    current_sd_ptr => current_sd_ptr%next_ptr
    if (trim(data_name)==trim(current_sd_ptr%sd%name)) then
      get_num_of_exchange_send_data = current_sd_ptr%num_of_data
      return
    end if
  end do

  call error("get_num_of_exchange_send_data", "No such data defined : "//trim(data_name))

end function get_num_of_exchange_send_data

  
!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_exchange_recv_data(comp_id, data_name)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: comp_id
  character(len=*), intent(IN) :: data_name
  integer :: i

  get_num_of_exchange_recv_data = 0

  current_rd_ptr => rd_ptr(comp_id)

  do
    if (.not.associated(current_rd_ptr%next_ptr)) exit
    current_rd_ptr => current_rd_ptr%next_ptr
    if (trim(data_name)==trim(current_rd_ptr%rd%name)) then
      get_num_of_exchange_recv_data = current_rd_ptr%num_of_data
      return
    end if
  end do

  call error("get_num_of_exchange_recv_data", "No such data defined : "//trim(data_name))

end function get_num_of_exchange_recv_data

  
!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_data_defined(comp_id, data_name)
  implicit none
  integer, intent(IN) :: comp_id
  character(len=*), intent(IN) :: data_name
  integer :: i

  current_sd_ptr => sd_ptr(comp_id)

  do
    if (.not.associated(current_sd_ptr%next_ptr)) exit
    current_sd_ptr => current_sd_ptr%next_ptr
    if (trim(data_name)==trim(current_sd_ptr%sd%name)) then
      is_data_defined = .true.
      return
    end if
  end do


  current_rd_ptr => rd_ptr(comp_id)

  do
    if (.not.associated(current_rd_ptr%next_ptr)) exit
    current_rd_ptr => current_rd_ptr%next_ptr
    if (trim(data_name)==trim(current_rd_ptr%rd%name)) then
      is_data_defined = .true.
      return
    end if
  end do

  is_data_defined = .false.

  return

end function is_data_defined

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_varp_comp_id(data_type)
  use jcup_utils, only : error
  implicit none
  type(varp_type), pointer :: data_type

  if (.not.associated(data_type)) call error("get_varp_comp_id", "data_type is not assosiated")

  get_varp_comp_id = data_type%sd%model_id

end function get_varp_comp_id

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_varg_comp_id(data_type)
  use jcup_utils, only : error
  implicit none
  type(varg_type), pointer :: data_type

  if (.not.associated(data_type)) call error("get_varg_comp_id", "data_type is not assosiated")

  get_varg_comp_id = data_type%rd%model_id

end function get_varg_comp_id

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=NAME_LEN) function get_varp_data_name(data_type)
  use jcup_utils, only : error
  implicit none
  type(varp_type), pointer :: data_type

  if (.not.associated(data_type)) call error("get_varp_data_name", "data_type is not assosiated")

  get_varp_data_name = data_type%sd%name

end function get_varp_data_name

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=NAME_LEN) function get_varg_data_name(data_type)
  use jcup_utils, only : error
  implicit none
  type(varg_type), pointer :: data_type

  if (.not.associated(data_type)) call error("get_varg_data_name", "data_type is not assosiated")

  get_varg_data_name = data_type%rd%name

end function get_varg_data_name

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_varp_time(data_type, current_time)
  use jcup_utils, only : error
  implicit none
  type(varp_type), pointer :: data_type
  type(time_type), intent(IN) :: current_time

  if (.not.associated(data_type)) call error("set_varp_time", "data_type is not assosiated")

  data_type%current_time = current_time

end subroutine set_varp_time

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_varp_time(data_type) result(current_time)
  use jcup_utils, only : error
  implicit none
  type(varp_type), pointer :: data_type
  type(time_type) :: current_time

  if (.not.associated(data_type)) call error("get_varp_time", "data_type is not assosiated")

  current_time = data_type%current_time

end function get_varp_time

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_varg_time(data_type, current_time)
  use jcup_utils, only : error
  implicit none
  type(varg_type), pointer :: data_type
  type(time_type), intent(IN) :: current_time

  if (.not.associated(data_type)) call error("set_varg_time", "data_type is not assosiated")

  data_type%current_time = current_time

end subroutine set_varg_time

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_varg_time(data_type) result(current_time)
  use jcup_utils, only : error
  implicit none
  type(varg_type), pointer :: data_type
  type(time_type) :: current_time

  if (.not.associated(data_type)) call error("get_varg_time", "data_type is not assosiated")

  current_time = data_type%current_time

end function get_varg_time

!=======+=========+=========+=========+=========+=========+=========+=========+
!> get data dimension type of varp
!! @param data_type pointer of data type
!! @protected
!  2015/02/23 [ADD]
integer function get_varp_data_dim(data_type) 
  implicit none
  type(varp_type), pointer :: data_type

  get_varp_data_dim = data_type%data_dimension_type

end function get_varp_data_dim

!=======+=========+=========+=========+=========+=========+=========+=========+
!> get data dimension type of varg
!! @param data_type pointer of data type
!! @protected
!  2015/02/23 [ADD]
integer function get_varg_data_dim(data_type) 
  implicit none
  type(varg_type), pointer :: data_type

  get_varg_data_dim = data_type%data_dimension_type

end function get_varg_data_dim

!=======+=========+=========+=========+=========+=========+=========+=========+
!> get the number of data or vertical layer of varp
!! @param data_type pointer of data type
!! @protected
!  2015/02/23 [ADD]
integer function get_varp_num_of_data(data_type) 
  implicit none
  type(varp_type), pointer :: data_type

  get_varp_num_of_data = data_type%num_of_data

end function get_varp_num_of_data

!=======+=========+=========+=========+=========+=========+=========+=========+
!> get the number of data or vertical layer of varg
!! @param data_type pointer of data type
!! @protected
!  2015/02/23 [ADD]
integer function get_varg_num_of_data(data_type) 
  implicit none
  type(varg_type), pointer :: data_type

  get_varg_num_of_data = data_type%num_of_data

end function get_varg_num_of_data

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function put_data_array_size_ok(data_type, s1, s2, s3)
  implicit none
  type(varp_type), pointer :: data_type
  integer, intent(IN) :: s1, s2, s3
  integer :: i_size, j_size

  i_size = data_type%my_grid%num_of_point !data_type%my_grid%ie-data_type%my_grid%is+1
  !j_size = data_type%my_grid%je-data_type%my_grid%js+1

  put_data_array_size_ok = .false.
  
  if (i_size /= s1) return
  !if (j_size /= s2) return

  put_data_array_size_ok = .true.

end function put_data_array_size_ok


!=======+=========+=========+=========+=========+=========+=========+=========+

logical function get_data_array_size_ok(data_type, s1, s2, s3)
  implicit none
  type(varg_type), pointer :: data_type
  integer, intent(IN) :: s1, s2, s3
  integer :: i_size, j_size

  i_size = data_type%my_grid%num_of_point !data_type%my_grid%ie-data_type%my_grid%is+1
  !j_size = data_type%my_grid%je-data_type%my_grid%js+1

  get_data_array_size_ok = .false.
  
  if (i_size /= s1) return
  !if (j_size /= s2) return

  get_data_array_size_ok = .true.

end function get_data_array_size_ok


!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcup_data
