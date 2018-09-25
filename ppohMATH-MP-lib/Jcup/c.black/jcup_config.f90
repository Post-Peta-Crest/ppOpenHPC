!
!Copyright (c) 2011, arakawa@rist.jp
!All rights reserved.
!
module jcup_config
  use jcup_constant, only : MAX_DOMAIN, NAME_LEN, NO_NAME, NO_DATA, DEBUG_FILE_ID, STRING_LEN
  private

!--------------------------------   public  ----------------------------------!

  public :: init_conf ! subroutine (num_of_model, conf_file_name)
  public :: destruct_conf
  public :: init_send_config ! subroutine (comp_id, num_of_send_data)
  public :: set_send_config ! subroutine (comp_id, data_num, data_name)
  public :: init_recv_config ! subroutine (comp_id, num_of_send_data)
  public :: set_recv_config ! subroutine (comp_id, data_num, data_name, recv_mode, interval, time_lag, mapping_tag, 
                            !             exchange_tag, send_model_name, send_data_name)
  public :: exchange_send_config_info ! subroutine (comp_id)
  public :: exchange_recv_config_info ! subroutine (comp_id)
  public :: set_configuration ! subroutine ()


  public :: set_current_conf ! subroutine (component_id) set current my component id
  public :: get_current_comp_id ! integer function (NONE) 
!  public :: SendDataConf
!  public :: RecvDataConf
!  public :: SetRecvFlag
!  public :: SendRecvFlag
!  public :: RecvRecvFlag
!  public :: GetRecvFlag
!  public :: SetCurrentModel
!  public :: GetCurrentModel
  public :: GetSendDataIndex
  public :: GetRecvDataIndex
  public :: is_source_model
  public :: is_my_send_data ! logical function (data_name)
  public :: is_my_recv_data ! logical function (data_name)
  public :: isSendData
  public :: isRecvData
  public :: isRecvData2
  public :: is_mean_data    ! logical function (send_comp_id, data_name)
  public :: is_average_data ! logical function (recv_comp_id, data_name)
  public :: GetMappingTag
  public :: GetRecvMappingTag
  public :: GetExchangeTag
  public :: get_num_of_remapping_table ! integer function (component_id or component_name or current_component)
  public :: get_num_of_send_data ! integer function (component_id or component_name or current_component)
  public :: get_num_of_recv_data ! integer function (component_id or component_name or current_component)
!  public :: GetGridIndex
  public :: GetTimeStepInterval
  public :: isOnlyFirstStep
  public :: GetSendModelName
  public :: get_send_data_name
  public :: get_my_send_data_name ! character(len=NAME_LEN) function (component_name, data_id)
  public :: get_my_recv_data_name ! character(len=NAME_LEN) function (component_name, data_id)
  !!!!!!!!public :: write_configure
  public :: get_send_data_conf_ptr_from_id ! type(send_data_conf_type), pointer function (component_id, data_num)
  public :: get_recv_data_conf_ptr_from_id ! type(recv_data_conf_type), pointer function (component_id, data_num)
  public :: get_send_data_conf_ptr ! type(send_data_conf_type), pointer function (comp_name, data_num or data_num or data_name)
  public :: get_recv_data_conf_ptr ! type(recv_data_conf_type), pointer function (comp_name, data_num or data_num or data_name)
  public :: send_data_conf_type
  public :: recv_data_conf_type
  public :: is_my_exchange_step !
  public :: is_exchange_step ! logical function (my_comp_id, target_comp_id, current_time)
  public :: is_put_step_data
  public :: is_send_step_data
  public :: is_recv_step_data
  public :: get_comp_id_from_comp_name ! integer function (componend_name)
  public :: get_comp_name_from_comp_id ! character(len=NAME_LEN) function (componend_id)
  public :: get_comp_exchange_type ! integer function (my_comp_id, target_comp_id) 
  public :: get_send_comp_id_from_data_name   ! integer function (data_name)
  public :: get_recv_comp_id_from_data_name   ! integer function (data_name)
  public :: get_send_data_id_from_data_name   ! integer function (data_name)
  public :: get_recv_data_id_from_data_name   ! integer funciton (data_name)


!--------------------------------   private  ---------------------------------!

  ! configuretion module
  ! configuration id = component id
  ! configuration number = configuration id

  integer, parameter, private :: WRITE_CONF_UNIT = 200

  interface set_current_conf
    module procedure set_current_conf_name, set_current_conf_id
  end interface

  interface get_num_of_remapping_table
    module procedure get_num_of_remapping_table_id, get_num_of_remapping_table_name, get_num_of_remapping_table_current
  end interface

  interface get_num_of_send_data
    module procedure get_num_of_send_data_id, GetNumofModelSendData, GetNumofMySendData
  end interface

  interface get_num_of_recv_data
    module procedure get_num_of_recv_data_id, GetNumOfModelRecvData, GetNumOfMyRecvData
  end interface

  interface get_send_data_ptr
    module procedure GetSendDataPtr1, GetSendDataPtr2
  end interface

  interface get_recv_data_ptr
    module procedure GetRecvDataPtr1, GetRecvDataPtr2
  end interface

  interface GetSendModelName
    module procedure GetSendModelName1, GetSendModelName2
  end interface

  interface get_send_data_name
    module procedure GetSendDataName1, GetSendDataName2, GetSendDataName3
  end interface
  
  interface is_my_exchange_step
    module procedure is_exchange_step_all_models, is_exchange_step_some_model
    module procedure is_exchange_step_all_from_ct
  end interface

  interface is_exchange_step
    module procedure is_exchange_step_send_recv_model
  end interface

  interface get_send_data_conf_ptr
    module procedure get_send_data_conf_ptr_name, get_send_data_conf_ptr_id, get_other_send_data_conf_ptr
  end interface

  interface get_recv_data_conf_ptr
    module procedure get_recv_data_conf_ptr_name, get_recv_data_conf_ptr_id, get_other_recv_data_conf_ptr
  end interface


  type recv_data_conf_type
    integer                 :: data_id ! data id number
    integer                 :: model_id ! my model ID (component id)
    character(len=NAME_LEN) :: name ! recv data name
    logical                 :: is_recv = .false.
    logical                 :: is_average = .false.
    integer                 :: interval ! exchange interval (sec)
    integer                 :: time_lag ! exchange time lag
    integer                 :: mapping_tag
    integer                 :: exchange_tag
    character(len=NAME_LEN) :: send_model
    integer                 :: send_model_id
    character(len=NAME_LEN) :: send_data
    integer                 :: data_dimension
    integer(kind=8)         :: recv_tag ! model_id+interval+mode+mapping_tag+exchange_tag
    integer                 :: grid_id ! id of assigned grid
    integer                 :: num_of_data ! number of multi categoly data or vertical grid
  end type

  type send_data_conf_type
    integer                 :: data_id ! data id number
    integer                 :: model_id ! my model id
    character(len=NAME_LEN) :: name     
    logical                 :: is_send  = .false.
    logical                 :: is_average = .false.
    integer                 :: data_dimension
    integer                 :: num_of_my_recv_data
    integer                 :: grid_id ! id of assigned grid
    type(recv_data_conf_type), pointer :: my_recv_conf(:) ! recv conf information of this send data
    integer                 :: num_of_data ! number of multi categoly data or vertical grid
  end type

  type remapping_table_conf_type
    integer :: send_model_id
    character(len=NAME_LEN) :: grid_put
    character(len=NAME_LEN) :: grid_get
    character(len=STRING_LEN) :: table_file_name
  end type

  type model_data_conf_type
    character(len=NAME_LEN) :: model_name 
    integer :: model_id ! component id number
    integer :: num_of_remapping_table
    type(remapping_table_conf_type), pointer, dimension(:) :: fl 
    integer :: num_of_send_data
    integer :: num_of_recv_data
    integer,pointer :: exchange_interval(:)  ! data exchange interval (sec)
    type(send_data_conf_type),pointer,dimension(:) :: sd
    type(recv_data_conf_type),pointer,dimension(:) :: rd
  end type

  type(model_data_conf_type), private, pointer :: mdc(:) ! array size : number of component
  type(model_data_conf_type), private, pointer :: mdc_tmp(:)
  type(model_data_conf_type), private, pointer :: current_conf
  integer, private :: current_conf_id

  integer, private, allocatable :: comp_exchange_type(:,:) ! concurrent, advance, behind, no_send_recv 
                                                           ! (num_of_comp x num_of_comp)
  integer, parameter, private :: NO_EXCHANGE=99999999

  character(len=STRING_LEN) :: conf_file_name = "coupling.conf"

  character(len=7), parameter :: NO_FILE = "NO_FILE"

contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_conf(num_of_model)
  use jcup_comp, only : get_component_name
  implicit none
  integer,intent(IN) :: num_of_model
  integer :: m

  allocate(mdc(num_of_model))

  do m = 1, num_of_model
    mdc(m)%model_name = get_component_name(m)
    mdc(m)%model_id   = m
    nullify(mdc(m)%sd)
    nullify(mdc(m)%rd)
    nullify(mdc(m)%fl)
    allocate(mdc(m)%exchange_interval(num_of_model))
    mdc(m)%exchange_interval(:) = NO_EXCHANGE
  end do  

end subroutine init_conf

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine destruct_conf
  implicit none

  integer :: mdl

  do mdl = 1, size(mdc,1)
    if (associated(mdc(mdl)%sd)) deallocate(mdc(mdl)%sd)
    if (associated(mdc(mdl)%rd)) deallocate(mdc(mdl)%rd)
    if (associated(mdc(mdl)%exchange_interval)) deallocate(mdc(mdl)%exchange_interval)
  end do
 
  if (associated(mdc)) deallocate(mdc)

end subroutine destruct_conf

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_send_config(comp_id, num_of_send_data)
  implicit none
  integer, intent(IN) :: comp_id
  integer, intent(IN) :: num_of_send_data
  integer :: m

  mdc(comp_id)%num_of_send_data = num_of_send_data

  allocate(mdc(comp_id)%sd(num_of_send_data))

  do m = 1, num_of_send_data
    mdc(comp_id)%sd(m)%model_id = comp_id
  end do
  
end subroutine init_send_config

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_send_config(comp_id, data_num, data_name, grid_id, num_of_data)
  implicit none
  integer, intent(IN) :: comp_id, data_num
  character(len=*), intent(IN) :: data_name
  integer, intent(IN) :: grid_id
  integer, intent(IN) :: num_of_data

  mdc(comp_id)%sd(data_num)%name = data_name
  mdc(comp_id)%sd(data_num)%is_send = .true.
  mdc(comp_id)%sd(data_num)%grid_id = grid_id
  mdc(comp_id)%sd(data_num)%num_of_data = num_of_data

end subroutine set_send_config

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_recv_config(comp_id, num_of_recv_data)
  implicit none
  integer, intent(IN) :: comp_id
  integer, intent(IN) :: num_of_recv_data
  integer :: m

  mdc(comp_id)%num_of_recv_data = num_of_recv_data

  allocate(mdc(comp_id)%rd(num_of_recv_data))

  do m = 1, num_of_recv_data
    mdc(comp_id)%rd(m)%model_id = comp_id
  end do

end subroutine init_recv_config

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_recv_config(comp_id, data_num, data_name, grid_id, num_of_data, recv_mode, interval, &
                           time_lag, mapping_tag, exchange_tag, send_model, send_data_name)
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  integer, intent(IN) :: comp_id, data_num
  character(len=*), intent(IN) :: data_name
  integer, intent(IN) :: grid_id ! grid_index
  integer, intent(IN) :: num_of_data 
  character(len=3), intent(IN) :: recv_mode ! "SNP" or "AVR"
  integer, intent(IN) :: interval ! exchange interval
  integer, intent(IN) :: time_lag ! -1, 0, 1
  integer, intent(IN) :: mapping_tag ! id number of mapping table
  integer, intent(IN) :: exchange_tag ! tag for interpolation 
  character(len=*), intent(IN) :: send_model ! send component name
  character(len=*), intent(IN) :: send_data_name ! send data name

  mdc(comp_id)%rd(data_num)%name = data_name
  mdc(comp_id)%rd(data_num)%grid_id = grid_id
  mdc(comp_id)%rd(data_num)%num_of_data = num_of_data
  mdc(comp_id)%rd(data_num)%is_recv = .true.
  mdc(comp_id)%rd(data_num)%is_average = (recv_mode == "AVR")
  mdc(comp_id)%rd(data_num)%interval = interval
  mdc(comp_id)%rd(data_num)%time_lag = time_lag
  mdc(comp_id)%rd(data_num)%mapping_tag = mapping_tag
  mdc(comp_id)%rd(data_num)%exchange_tag = exchange_tag
  mdc(comp_id)%rd(data_num)%send_model = send_model
  mdc(comp_id)%rd(data_num)%send_data  = send_data_name
  mdc(comp_id)%rd(data_num)%send_model_id = get_comp_id_from_name(send_model)

end subroutine set_recv_config

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine exchange_send_config_info(comp_id)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_GetLeaderRank, jml_BcastGlobal
  use jcup_comp, only : is_my_component
  implicit none
  integer, intent(IN) :: comp_id
  integer :: num_of_data  
  integer :: data_buffer(1)
  character(len=NAME_LEN), pointer :: data_name(:)
  integer :: leader_rank
  integer :: i

  leader_rank = jml_GetLeaderRank(comp_id)

  if (jml_isLocalLeader(comp_id)) then
    data_buffer(1) = mdc(comp_id)%num_of_send_data

    call jml_BcastGlobal(data_buffer, 1, 1, leader_rank)
 
    allocate(data_name(data_buffer(1)))
    data_name(:) = " "

    do i = 1, data_buffer(1)
      data_name(i) = trim(mdc(comp_id)%sd(i)%name)
    end do 
    call jml_BcastGlobal(data_name, NAME_LEN, leader_rank)
  else
    call jml_BcastGlobal(data_buffer, 1, 1, leader_rank)
    if (.not.is_my_component(comp_id)) then
      call init_send_config(comp_id, data_buffer(1))
    end if

    allocate(data_name(data_buffer(1)))
    call jml_BcastGlobal(data_name, NAME_LEN, leader_rank)  
    if (.not.is_my_component(comp_id)) then
      do i = 1, data_buffer(1)
        mdc(comp_id)%sd(i)%name = trim(data_name(i))
        mdc(comp_id)%sd(i)%is_send = .true.
      end do
    end if
  end if

  if (associated(data_name)) deallocate(data_name)

end subroutine exchange_send_config_info

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine exchange_recv_config_info(comp_id)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_GetLeaderRank, jml_BcastGlobal
  use jcup_comp, only : is_my_component, get_comp_id_from_name
  implicit none
  integer, intent(IN) :: comp_id
  integer :: num_of_data  
  integer :: data_buffer(1)
  integer, pointer :: int_buffer(:)
  character(len=NAME_LEN), pointer :: name_buffer(:)
  integer :: leader_rank
  integer :: i, is

  leader_rank = jml_GetLeaderRank(comp_id)

  if (jml_isLocalLeader(comp_id)) then
    data_buffer(1) = mdc(comp_id)%num_of_recv_data

    call jml_BcastGlobal(data_buffer, 1, 1, leader_rank)
 
    allocate(name_buffer(data_buffer(1)*3))
    allocate(int_buffer(5*data_buffer(1)))

    do i = 1, data_buffer(1)
      name_buffer(data_buffer(1)*0+i) = trim(mdc(comp_id)%rd(i)%name)
      name_buffer(data_buffer(1)*1+i) = trim(mdc(comp_id)%rd(i)%send_model)
      name_buffer(data_buffer(1)*2+i) = trim(mdc(comp_id)%rd(i)%send_data)

      is = 5*(i-1)

      if (mdc(comp_id)%rd(i)%is_average) then 
        int_buffer(is+1) = 1 
      else 
        int_buffer(is+1) = 0 
      end if
      int_buffer(is+2) = mdc(comp_id)%rd(i)%interval
      int_buffer(is+3) = mdc(comp_id)%rd(i)%time_lag
      int_buffer(is+4) = mdc(comp_id)%rd(i)%mapping_tag
      int_buffer(is+5) = mdc(comp_id)%rd(i)%exchange_tag

    end do

    call jml_BcastGlobal(name_buffer, NAME_LEN, leader_rank)
    call jml_BcastGlobal(int_buffer, 1, size(int_buffer), leader_rank)

  else
    call jml_BcastGlobal(data_buffer, 1, 1, leader_rank)
    if (.not.is_my_component(comp_id)) then
      call init_recv_config(comp_id, data_buffer(1))
    end if

    allocate(name_buffer(data_buffer(1)*3))
    allocate(int_buffer(5*data_buffer(1)))

    call jml_BcastGlobal(name_buffer, NAME_LEN, leader_rank)
    call jml_BcastGlobal(int_buffer, 1, size(int_buffer), leader_rank)

    if (.not.is_my_component(comp_id)) then
      do i = 1, data_buffer(1)
        mdc(comp_id)%rd(i)%name       = name_buffer(data_buffer(1)*0+i)
        mdc(comp_id)%rd(i)%send_model = name_buffer(data_buffer(1)*1+i)
        mdc(comp_id)%rd(i)%send_data  = name_buffer(data_buffer(1)*2+i)
        mdc(comp_id)%rd(i)%send_model_id = get_comp_id_from_name(mdc(comp_id)%rd(i)%send_model)

        is = 5*(i-1)
        mdc(comp_id)%rd(i)%is_average   = (int_buffer(is+1) == 1)
        mdc(comp_id)%rd(i)%interval     = int_buffer(is+2)
        mdc(comp_id)%rd(i)%time_lag     = int_buffer(is+3)
        mdc(comp_id)%rd(i)%mapping_tag  = int_buffer(is+4)
        mdc(comp_id)%rd(i)%exchange_tag = int_buffer(is+5)
        mdc(comp_id)%rd(i)%is_recv = .true.
      end do
    end if
  end if

  if (associated(name_buffer)) deallocate(name_buffer)
  if (associated(int_buffer)) deallocate(int_buffer)

end subroutine exchange_recv_config_info

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_configuration()
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  use jcup_utils, only : open_log_file, close_log_file
  use jcup_comp, only : get_num_of_total_component, is_my_component
  implicit none
  integer :: i

  call set_send_average_flag()
  call set_data_id()
  call set_recv_data_info()
  call cal_exchange_interval()

  do i = 1, get_num_of_total_component()
    if (is_my_component(i)) then
      call set_current_conf(i)
      call check_configuration(current_conf)
    end if
  end do

  call modify_my_conf_data()
  call set_recv_tag()
  call set_model_exchange_type()

  if (jml_GetMyrankGlobal()==0) then
    do i = 1, get_num_of_total_component()
     !if (is_my_component(i)) then
        call set_current_conf(i)
        !if (jml_isLocalLeader(i)) then
          call open_log_file("./"//trim(get_comp_name_from_comp_id(i))//".conf.log", WRITE_CONF_UNIT+i)
          call write_configure(current_conf, WRITE_CONF_UNIT+i)
          call close_log_file(WRITE_CONF_UNIT+i)
        !end if
      !end if
    end do

    call open_log_file("./"//trim(conf_file_name)//".log", WRITE_CONF_UNIT)
    do i = 1, get_num_of_total_component()
      call set_current_conf(i)
      call write_configure_2(current_conf, WRITE_CONF_UNIT)
    end do
    call close_log_file(WRITE_CONF_UNIT)

  end if

  return


end subroutine set_configuration

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_remapping_table_conf(fl)
  implicit none
  type(remapping_table_conf_type), intent(INOUT) :: fl

  fl%send_model_id = -1
  fl%grid_put = ""
  fl%grid_get = ""
  fl%table_file_name = NO_FILE

end subroutine init_remapping_table_conf

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_send_data_conf(sd)
  implicit none
  type(send_data_conf_type), intent(INOUT) :: sd

  sd%name = trim(NO_NAME)
  sd%data_dimension = NO_DATA

end subroutine init_send_data_conf

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_send_data_conf(sd)
  use jcup_utils, only : error
  implicit none
  type(send_data_conf_type), intent(IN) :: sd

  if (trim(sd%name)==trim(NO_NAME)) then
    call error("check_send_data_conf", "data name is not assigned, check configure file")
  end if

end subroutine check_send_data_conf

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_recv_data_conf(rd)
  implicit none
  type(recv_data_conf_type), intent(INOUT) :: rd

  rd%name = trim(NO_NAME)
  rd%interval = NO_DATA
  rd%mapping_tag = NO_DATA
  rd%exchange_tag = NO_DATA
  rd%send_model = trim(NO_NAME)
  rd%send_data = trim(NO_NAME)
  rd%data_dimension = NO_DATA

end subroutine init_recv_data_conf

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_recv_data_conf(rd)
  use jcup_utils, only : error
  implicit none
  type(recv_data_conf_type), intent(IN) :: rd

  if (trim(rd%name)==trim(NO_NAME)) then
    call error("check_recv_data_conf", "data name is not assigned, check configure file")
  end if

  if (rd%interval==NO_DATA) then
    call error("check_recv_data_conf", "data:"//trim(rd%name)//", interval is not assigned, check configure file")
  end if

  if (rd%mapping_tag==NO_DATA) then
    call error("check_recv_data_conf", "data:"//trim(rd%name)//", mapping_tag is not assigned, check configure file")
  end if

  if (rd%exchange_tag==NO_DATA) then
    call error("check_recv_data_conf", "data:"//trim(rd%name)//", exchange_tag is not assigned, check configure file")
  end if

  if (trim(rd%send_model)==trim(NO_NAME)) then
    call error("check_recv_data_conf", "data:"//trim(rd%name)//", send_model is not assigned, check configure file")
  end if

  if (trim(rd%send_data)==trim(NO_NAME)) then
    call error("check_recv_data_conf", "data:"//trim(rd%name)//", send_data is not assigned, check configure file")
  end if

end subroutine check_recv_data_conf


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_send_average_flag
  use jcup_utils, only : put_log
  use jcup_comp, only : is_my_component
  implicit none
  
  integer :: mdl, m, i, j 

  do mdl = 1, size(mdc)
    do i = 1, mdc(mdl)%num_of_send_data
      do m = 1, size(mdc)
        if (m==mdl) cycle
        do j =1, mdc(m)%num_of_recv_data
          if ((mdc(m)%rd(j)%is_recv).and.(mdc(m)%rd(j)%is_average)) then
            if (trim(mdc(m)%rd(j)%send_model)==trim(mdc(mdl)%model_name)) then
              if (trim(mdc(m)%rd(j)%send_data)==trim(mdc(mdl)%sd(i)%name)) then
                if (is_my_component(mdl)) call put_log("set send data average flag, data name : "//trim(mdc(mdl)%sd(i)%name)) 
                mdc(mdl)%sd(i)%is_average = .true.
                goto 1000
              end if
            end if
          end if
        end do 
      end do
      1000 continue
    end do 

  end do

end subroutine set_send_average_flag


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_recv_data_info()
  implicit none
  integer :: mdl, m, i, j
  integer :: data_counter

  do mdl = 1, size(mdc)
    do i = 1, mdc(mdl)%num_of_send_data

      data_counter = 0
      do m = 1, size(mdc)
        do j = 1, mdc(m)%num_of_recv_data
          if (trim(mdc(mdl)%model_name)==trim(mdc(m)%rd(j)%send_model)) then
            if (mdc(m)%rd(j)%is_recv) then
              if (trim(mdc(mdl)%sd(i)%name)==trim(mdc(m)%rd(j)%send_data)) then
                data_counter = data_counter+1
              end if
            end if
          end if
        end do
      end do

      mdc(mdl)%sd(i)%num_of_my_recv_data = data_counter
      allocate(mdc(mdl)%sd(i)%my_recv_conf(data_counter))

      data_counter = 0
      do m = 1, size(mdc)
        do j = 1, mdc(m)%num_of_recv_data
          if (trim(mdc(mdl)%model_name)==trim(mdc(m)%rd(j)%send_model)) then
            if (mdc(m)%rd(j)%is_recv) then
              if (trim(mdc(mdl)%sd(i)%name)==trim(mdc(m)%rd(j)%send_data)) then
                data_counter = data_counter+1
                mdc(mdl)%sd(i)%my_recv_conf(data_counter) = mdc(m)%rd(j)
              end if
            end if
          end if
        end do
      end do

    end do
  end do

end subroutine set_recv_data_info

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_data_id()
  implicit none
  integer :: m, i

  do m = 1, size(mdc)
    do i = 1, mdc(m)%num_of_send_data
      mdc(m)%sd(i)%data_id = m*1000+1*100+i
    end do
    do i = 1, mdc(m)%num_of_recv_data
      mdc(m)%rd(i)%data_id = m*1000+2*100+i
    end do
  end do

end subroutine set_data_id

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_exchange_interval
  implicit none
  integer :: mdl, m, i, j
  integer :: min_int

  do mdl = 1, size(mdc)
    do m = 1, size(mdc)
      min_int = NO_EXCHANGE
      if (m==mdl) cycle
      do i = 1, mdc(m)%num_of_recv_data
        if (mdc(m)%rd(i)%is_recv) then
          if (trim(mdc(mdl)%model_name) == trim(mdc(m)%rd(i)%send_model)) then
            min_int = min(min_int, mdc(m)%rd(i)%interval)
          end if
        end if
      end do
      mdc(mdl)%exchange_interval(m) = min_int
    end do

    do i = 1, mdc(mdl)%num_of_recv_data
      if (mdc(mdl)%rd(i)%is_recv) then
        do j = 1, size(mdc)
          if (trim(mdc(j)%model_name)==trim(mdc(mdl)%rd(i)%send_model)) then
            mdc(mdl)%exchange_interval(j) = min(mdc(mdl)%exchange_interval(j),mdc(mdl)%rd(i)%interval)
          end if
        end do
      end if
    end do
  end do

end subroutine cal_exchange_interval

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_model_exchange_type()
  use jcup_constant, only : CONCURRENT_SEND_RECV, ADVANCE_SEND_RECV, BEHIND_SEND_RECV, IMMEDIATE_SEND_RECV, NO_SEND_RECV
  use jcup_utils, only : error
  implicit none
  integer :: send_model_id
  integer :: ii, i, j

  allocate(comp_exchange_type(size(mdc), size(mdc))) ! (my_component_id, target_component_id)
  comp_exchange_type(:,:) = NO_SEND_RECV

  do ii = 1, size(mdc)
    do i = 1, mdc(ii)%num_of_recv_data
      if (.not.mdc(ii)%rd(i)%is_recv) cycle
      select case(mdc(ii)%rd(i)%time_lag)
      case(1)
        comp_exchange_type(ii, mdc(ii)%rd(i)%send_model_id) = BEHIND_SEND_RECV ! set 1
      case(-1)
        comp_exchange_type(ii, mdc(ii)%rd(i)%send_model_id) = CONCURRENT_SEND_RECV
        send_model_id = mdc(ii)%rd(i)%send_model_id
        do j = 1, mdc(send_model_id)%num_of_recv_data
          if (.not.mdc(send_model_id)%rd(j)%is_recv) cycle
          if (mdc(send_model_id)%rd(j)%send_model_id == ii) then
            if (mdc(send_model_id)%rd(j)%time_lag == 1) then
              comp_exchange_type(ii, send_model_id) = ADVANCE_SEND_RECV ! set -1
            end if
            cycle
          end if
        end do
      case(0)
        comp_exchange_type(ii,mdc(ii)%rd(i)%send_model_id) = IMMEDIATE_SEND_RECV
      case default
        call error("set_model_exchange_type", "time_lag setting error")
      end select
    end do
  end do

  do i = 1, size(mdc)
    do j = 1, size(mdc)
      if (comp_exchange_type(i,j) ==NO_SEND_RECV) then
        if (comp_exchange_type(j,i) == CONCURRENT_SEND_RECV) then
          comp_exchange_type(i,j) = CONCURRENT_SEND_RECV
        end if
        if (comp_exchange_type(j,i) == IMMEDIATE_SEND_RECV) then
          comp_exchange_type(i,j) = IMMEDIATE_SEND_RECV
        end if
      end if
    end do
  end do

end subroutine set_model_exchange_type

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_current_conf_name(comp_name)
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: comp_name

  current_conf_id = get_comp_id_from_name(comp_name)

  current_conf => mdc(current_conf_id)

end subroutine set_current_conf_name

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_current_conf_id(comp_id)
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  integer, intent(IN) :: comp_id

  current_conf_id = comp_id
  current_conf => mdc(comp_id)

end subroutine set_current_conf_id

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_current_comp_id()
  implicit none

  get_current_comp_id = current_conf_id

end function get_current_comp_id

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_my_send_data(data_name)
  implicit none
  character(len=*), intent(IN) :: data_name

  integer :: i

  do i = 1, current_conf%num_of_send_data
    if (trim(current_conf%sd(i)%name) == trim(data_name)) then
      is_my_send_data = .true.
      return
    end if
  end do

  is_my_send_data = .false.

end function is_my_send_data

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_my_recv_data(data_name)
  implicit none
  character(len=*), intent(IN) :: data_name

  integer :: i

  do i = 1, current_conf%num_of_recv_data
    if (trim(current_conf%rd(i)%name) == trim(data_name)) then
      is_my_recv_data = .true.
      return
    end if
  end do

  is_my_recv_data = .false.

end function is_my_recv_data

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_remapping_table_id(comp_id)
  implicit none
  integer, intent(IN) :: comp_id

  get_num_of_remapping_table_id = mdc(comp_id)%num_of_remapping_table

end function get_num_of_remapping_table_id

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_remapping_table_name(comp_name)
  implicit none
  character(len=*), intent(IN) :: comp_name
  integer :: mdl

  do mdl = 1, size(mdc)
    if (trim(mdc(mdl)%model_name)==trim(comp_name)) then
      get_num_of_remapping_table_name = mdc(mdl)%num_of_remapping_table
      return
    end if
  end do

  get_num_of_remapping_table_name = 0

end function get_num_of_remapping_table_name

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_remapping_table_current()
  implicit none

  get_num_of_remapping_table_current = current_conf%num_of_remapping_table

end function get_num_of_remapping_table_current

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_send_data_id(comp_id)
  implicit none
  integer, intent(IN) :: comp_id

  get_num_of_send_data_id = mdc(comp_id)%num_of_send_data

end function get_num_of_send_data_id

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_recv_data_id(comp_id)
  implicit none
  integer, intent(IN) :: comp_id

  get_num_of_recv_data_id = mdc(comp_id)%num_of_recv_data

end function get_num_of_recv_data_id

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function GetNumofModelSendData(model_name)
  implicit none
  character(len=*), intent(IN) :: model_name
  integer :: mdl

  do mdl = 1, size(mdc)
    if (trim(mdc(mdl)%model_name)==trim(model_name)) then
      GetNumofModelSendData = mdc(mdl)%num_of_send_data
      return
    end if
  end do

  GetNumOfModelSendData = 0

end function GetNumofModelSendData


!=======+=========+=========+=========+=========+=========+=========+=========+

integer function GetNumofModelRecvData(model_name)
  implicit none
  character(len=*), intent(IN) :: model_name
  integer :: mdl

  do mdl = 1, size(mdc)
    if (trim(mdc(mdl)%model_name)==trim(model_name)) then
      GetNumofModelRecvData = mdc(mdl)%num_of_recv_data
      return
    end if
  end do

  GetNumOfModelRecvData = 0

end function GetNumofModelRecvData

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function GetNumofMySendData()
  implicit none

  GetNumofMySendData = current_conf%num_of_send_data

end function GetNumofMySendData

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function GetNumofMyRecvData()
  implicit none

  GetNumofMyRecvData = current_conf%num_of_recv_data

end function GetNumofMyRecvData

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function GetModelIndex(model_name)
  use jcup_utils, only : error
  implicit none
  character(len=*),intent(IN) :: model_name
  integer :: mdl

  do mdl = 1, size(mdc,1)
    if (trim(mdc(mdl)%model_name) == trim(model_name)) then
      GetModelIndex = mdl
      return
    end if
  end do

  call error("GetModelIndex","no such model : "//trim(model_name)//", check your program and data.conf file")
  
end function GetModelIndex

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function GetSendDataIndex(model_name, data_name)
  use jcup_utils, only : error
  implicit none
  character(len=*),intent(IN) :: model_name, data_name
  integer :: mdl, dt

  do mdl = 1, size(mdc,1)
    if (trim(mdc(mdl)%model_name) == trim(model_name)) then
      do dt = 1, mdc(mdl)%num_of_send_data
        if (trim(mdc(mdl)%sd(dt)%name) == trim(data_name)) then
          GetSendDataIndex = dt
          return
        end if
      end do
    end if
  end do

  call error("GetSendDataIndex","no such data : "//trim(data_name)//", check your program and data.conf file")
  
end function GetSendDataIndex

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function GetRecvDataIndex(model_name, data_name)
  use jcup_utils, only : error
  implicit none
  character(len=*),intent(IN) :: model_name, data_name
  integer :: mdl, dt

  do mdl = 1, size(mdc,1)
    if (trim(mdc(mdl)%model_name) == trim(model_name)) then
      do dt = 1, mdc(mdl)%num_of_recv_data
        if (trim(mdc(mdl)%rd(dt)%name) == trim(data_name)) then
          GetRecvDataIndex = dt
          return
        end if
      end do
    end if
  end do

  call error("GetRecvDataIndex","no such data : "//trim(data_name)//", check your program and data.conf file")
  
end function GetRecvDataIndex

!=======+=========+=========+=========+=========+=========+=========+=========+

function GetSendDataPtr1(model_name, data_index) result(SendDataPtr)
  use jcup_utils, only : error
  implicit none
  character(len=*), optional, intent(IN) :: model_name
  integer, intent(IN) :: data_index

  type(send_data_conf_type), pointer :: SendDataPtr
  integer :: mdl


  if (present(model_name)) then
    mdl = GetModelIndex(model_name)
  else
    mdl = current_conf_id
  end if

  if (data_index > size(mdc(mdl)%sd)) then
    call error("GetSendDataPtr1", "data_index size error")
  end if

  SendDataPtr => mdc(mdl)%sd(data_index)

end function GetSendDataPtr1

!=======+=========+=========+=========+=========+=========+=========+=========+

function GetSendDataPtr2(model_name, data_name) result(SendDataPtr)
  use jcup_utils, only : error
  implicit none
  character(len=*), optional, intent(IN) :: model_name
  character(len=*), intent(IN) :: data_name

  type(send_data_conf_type), pointer :: SendDataPtr
  integer :: mdl, i

  if (present(model_name)) then
    mdl = GetModelIndex(model_name)
  else
    mdl = current_conf_id
  end if

  do i = 1, mdc(mdl)%num_of_send_data
    if (trim(mdc(mdl)%sd(i)%name) == trim(data_name)) then 
      SendDataPtr => mdc(mdl)%sd(i)
      return
    end if
  end do

  call error("GetSendDataPtr","no such data : "//trim(data_name)//", check your program and data.conf file")

end function GetSendDataPtr2

!=======+=========+=========+=========+=========+=========+=========+=========+

function GetRecvDataPtr1(model_name, data_index) result(RecvDataPtr)
  use jcup_utils, only : error
  implicit none
  character(len=*), optional, intent(IN) :: model_name
  integer, intent(IN) :: data_index

  type(recv_data_conf_type), pointer :: RecvDataPtr
  integer :: mdl

  if (present(model_name)) then
    mdl = GetModelIndex(model_name)
  else
    mdl = current_conf_id
  end if

  if (data_index > size(mdc(mdl)%rd)) then
    call error("GetRecvDataPtr1", "data_index size error")
  end if

  RecvDataPtr => mdc(mdl)%rd(data_index)

end function GetRecvDataPtr1

!=======+=========+=========+=========+=========+=========+=========+=========+

function GetRecvDataPtr2(model_name, data_name) result(RecvDataPtr)
  use jcup_utils, only : error
  implicit none
  character(len=*), optional, intent(IN) :: model_name
  character(len=*),           intent(IN) :: data_name

  type(recv_data_conf_type), pointer :: RecvDataPtr
  integer :: mdl, i

  if (present(model_name)) then
    mdl = GetModelIndex(model_name)
  else
    mdl = current_conf_id
  end if

  do i = 1, mdc(mdl)%num_of_recv_data
    if (trim(mdc(mdl)%rd(i)%name) == trim(data_name)) then 
      RecvDataPtr => mdc(mdl)%rd(i)
      return
    end if
  end do

  call error("GetRecvDataPtr","no such data : "//trim(data_name)//", check your program data.conf file")

end function GetRecvDataPtr2

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_source_model(send_model_id, recv_model_id)
  implicit none
  integer, intent(IN) :: send_model_id, recv_model_id
  integer :: i

  is_source_model = .true.

  do i = 1, mdc(recv_model_id)%num_of_recv_data
    if (mdc(recv_model_id)%rd(i)%is_recv) then
      if (mdc(recv_model_id)%rd(i)%send_model_id== send_model_id) return
    end if
  end do

  is_source_model = .false.

end function is_source_model

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function isSendData(model_name, data_name)
  implicit none
  character(len=*), optional, intent(IN) :: model_name
  character(len=*), intent(IN) :: data_name
  type(send_data_conf_type), pointer :: sd
  integer :: mdl, i

  if (present(model_name)) then
    mdl = GetModelIndex(model_name)
  else
    mdl = current_conf_id
  end if

  do i = 1, mdc(mdl)%num_of_send_data
    if (trim(mdc(mdl)%sd(i)%name) == trim(data_name)) then 
      isSendData = mdc(mdl)%sd(i)%is_send 
      return
    end if
  end do

  isSendData = .false.

end function isSendData

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function isRecvData(model_name, data_name)
  implicit none
  character(len=*), optional, intent(IN) :: model_name
  character(len=*), intent(IN) :: data_name
  integer :: mdl, i

  if (present(model_name)) then
    mdl = GetModelIndex(model_name)
  else
    mdl = current_conf_id
  end if

  do i = 1, mdc(mdl)%num_of_recv_data
    if (trim(mdc(mdl)%rd(i)%name) == trim(data_name)) then 
      isRecvData = mdc(mdl)%rd(i)%is_recv 
      return
    end if
  end do

  isRecvData = .false.

end function isRecvData

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function isRecvData2(source_model_name, my_data_name)
  implicit none
  character(len=*), optional, intent(IN) :: source_model_name
  character(len=*), intent(IN) :: my_data_name
  integer :: mdl, i

  if (present(source_model_name)) then
    mdl = GetModelIndex(source_model_name)
  else
    mdl = current_conf_id
  end if

  do i = 1, mdc(mdl)%num_of_send_data
    if (trim(mdc(mdl)%sd(i)%name) == trim(my_data_name)) then 
      isRecvData2 = mdc(mdl)%sd(i)%is_send 
      return
    end if
  end do

  isRecvData2 = .false.

end function isRecvData2

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_mean_data_name(model_name, data_name)
  use jcup_utils, only : error
  use jcup_constant, only : NAME_LEN
  implicit none
  character(len=*), optional, intent(IN) :: model_name
  character(len=*), intent(IN) :: data_name
  type(send_data_conf_type), pointer :: sd
  integer :: mdl, i
  character(len=NAME_LEN) :: d_name
  d_name = trim(data_name)

  if (present(model_name)) then
    mdl = GetModelIndex(model_name)
  else
    mdl = current_conf_id
  end if

  do i = 1, mdc(mdl)%num_of_send_data
    if (trim(mdc(mdl)%sd(i)%name) == d_name) then 
      is_mean_data_name = mdc(mdl)%sd(i)%is_average 
      return
    end if
  end do

  call error("is_mean_data_name","data name :"//trim(data_name)//" mismatch")

end function is_mean_data_name


!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_mean_data(send_comp_id, data_name)
  use jcup_utils, only : error
  use jcup_constant, only : NAME_LEN
  implicit none
  integer, intent(IN) :: send_comp_id
  character(len=*), intent(IN) :: data_name
  type(send_data_conf_type), pointer :: sd
  integer ::  i

  do i = 1, mdc(send_comp_id)%num_of_send_data
    if (trim(mdc(send_comp_id)%sd(i)%name) == trim(data_name)) then
      is_mean_data = mdc(send_comp_id)%sd(i)%is_average 
      return
    end if
  end do

  call error("is_mean_data","data name :"//trim(data_name)//" mismatch")

end function is_mean_data

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_average_data(recv_comp_id, data_name)
  use jcup_utils, only : error
  use jcup_constant, only : NAME_LEN
  implicit none
  integer, intent(IN) :: recv_comp_id
  character(len=*), intent(IN) :: data_name
  integer ::  i

  do i = 1, mdc(recv_comp_id)%num_of_recv_data
    if (trim(mdc(recv_comp_id)%rd(i)%name) == trim(data_name)) then
      is_average_data = mdc(recv_comp_id)%rd(i)%is_average 
      return
    end if
  end do

  call error("is_average_data","data name :"//trim(data_name)//" mismatch")

end function is_average_data


!=======+=========+=========+=========+=========+=========+=========+=========+

integer function GetMappingTag(recv_model_id, data_name)
  use jcup_utils, only : error
  integer, intent(IN), optional :: recv_model_id
  character(len=*), intent(IN) :: data_name

  type(recv_data_conf_type), pointer :: rd
  integer :: mdl, i

  if (present(recv_model_id)) then
    mdl = recv_model_id
  else
    mdl = current_conf_id
  end if

  !write(0,*) "GetMappingTag ", mdl,trim(data_name)

  do i = 1, mdc(mdl)%num_of_recv_data
    if (trim(mdc(mdl)%rd(i)%send_data) == trim(data_name)) then
      GetMappingTag = mdc(mdl)%rd(i)%mapping_tag
      return
    end if
  end do

  call error("GetMappigTag","data name :"//trim(data_name)//" mismatch")

end function GetMappingTag

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function GetRecvMappingTag(recv_model_id, recv_data_name)
  use jcup_utils, only : error
  integer, intent(IN), optional :: recv_model_id
  character(len=*), intent(IN) :: recv_data_name

  type(recv_data_conf_type), pointer :: rd
  integer :: mdl, i

  if (present(recv_model_id)) then
    mdl = recv_model_id
  else
    mdl = current_conf_id
  end if

  !write(0,*) "GetRecvMappingTag ", mdl,trim(recv_data_name)

  do i = 1, mdc(mdl)%num_of_recv_data
    if (trim(mdc(mdl)%rd(i)%name) == trim(recv_data_name)) then
      GetRecvMappingTag = mdc(mdl)%rd(i)%mapping_tag
      return
    end if
  end do

  call error("GetRecvMappigTag","data name :"//trim(recv_data_name)//" mismatch")

end function GetRecvMappingTag

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function GetExchangeTag(data_name)
  use jcup_utils, only : error
  character(len=*), intent(IN) :: data_name

  type(recv_data_conf_type), pointer :: rd
  integer :: mdl, i

  mdl = current_conf_id
 
  do i = 1, mdc(mdl)%num_of_recv_data
    if (trim(mdc(mdl)%rd(i)%name) == trim(data_name)) then
      GetExchangeTag = mdc(mdl)%rd(i)%exchange_tag
      return
    end if
  end do

  call error("GetExchangeTag","data name :"//trim(data_name)//" mismatch")

end function GetExchangeTag

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function GetTimeStepInterval(model_name, data_name)
  use jcup_constant, only : NAME_LEN
  use jcup_utils, only : error
  implicit none
  character(len=*), optional, intent(IN) :: model_name
  character(len=*), intent(IN) :: data_name
  character(len=NAME_LEN) :: d_name
  integer :: mdl, i

  mdl = current_conf_id

  if (present(model_name)) mdl = GetModelIndex(trim(model_name))
 
  d_name = trim(data_name)

  do i = 1, mdc(mdl)%num_of_send_data
    if (d_name == trim(mdc(mdl)%sd(i)%name)) then
      !GetTimeStepInterval = mdc(mdl)%sd(i)%interval
      return
    end if
  end do

  do i = 1, mdc(mdl)%num_of_recv_data
    if (d_name == trim(mdc(mdl)%rd(i)%name)) then
      GetTimeStepInterval = mdc(mdl)%rd(i)%interval
      return
    end if
  end do

  call error("GetTimeStepInterval","no such data : "//trim(data_name)//", check your program and data conf file")

end function GetTimeStepInterval

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function isOnlyFirstStep(model_name, data_name, domain)
  use jcup_utils, only : error
  implicit none
  character(len=*), intent(IN) :: model_name
  character(len=*), intent(IN) :: data_name
  integer         , intent(IN) :: domain
  integer :: mdl, i

  isOnlyFirstStep = (GetTimeStepInterval(MODEL_NAME = trim(model_name), DATA_NAME = trim(data_name)) == -1)

end function isOnlyFirstStep

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=NAME_LEN) function GetSendModelName1(data_name)
  use jcup_utils, only : error
  implicit none
  character(len=*), intent(IN) :: data_name
  type(recv_data_conf_type), pointer :: rd
  integer :: mdl, i
  character(len=NAME_LEN) :: d_name

  d_name = trim(data_name)

  mdl = current_conf_id

  do i = 1, mdc(mdl)%num_of_recv_data
    if (d_name == trim(mdc(mdl)%rd(i)%name)) then 
      GetSendModelName1 = mdc(mdl)%rd(i)%send_model 
      return
    end if
  end do

  !rd => GetRecvDataPtr(DATA_NAME = data_name)
  !GetSendModelName1 = rd%send_model

end function GetSendModelName1

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=NAME_LEN) function GetSendModelName2(data_index)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: data_index

  if (data_index > current_conf%num_of_recv_data) call error("GetSendDomain","data index error")

  GetSendModelName2 = current_conf%rd(data_index)%send_model

end function GetSendModelName2

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=NAME_LEN) function GetSendDataName1(data_name)
  use jcup_utils, only : error
  implicit none
  character(len=*), intent(IN) :: data_name
  type(recv_data_conf_type), pointer :: rd
  integer :: mdl, i
  character(len=NAME_LEN) :: d_name

  d_name = trim(data_name)

  mdl = current_conf_id

  do i = 1, mdc(mdl)%num_of_recv_data
    if (d_name == trim(mdc(mdl)%rd(i)%name)) then 
      GetSendDataName1 = mdc(mdl)%rd(i)%send_data
      return
    end if
  end do

  !rd => GetRecvDataPtr(DATA_NAME = data_name)
  !GetSendDataName1 = rd%send_data

end function GetSendDataName1

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=NAME_LEN) function GetSendDataName2(data_index)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: data_index

  if (data_index > current_conf%num_of_recv_data) call error("GetSendDomain","data index error")

  GetSendDataName2 = current_conf%rd(data_index)%send_data

end function GetSendDataName2

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=NAME_LEN) function GetSendDataName3(mdl,data_index)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: mdl,data_index

  if (data_index > mdc(mdl)%num_of_recv_data) call error("GetSendDomain","data index error")

  GetSendDataName3 = mdc(mdl)%rd(data_index)%send_data

end function GetSendDataName3

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=NAME_LEN) function get_my_send_data_name(model_name, data_index)
  use jcup_utils, only : error
  implicit none
  character(len=*), intent(IN) :: model_name
  integer, intent(IN) :: data_index
  integer :: mdl


  do mdl = 1, size(mdc,1)
    if (trim(mdc(mdl)%model_name) == trim(model_name)) then
      if (data_index > mdc(mdl)%num_of_send_data) call error("get_my_send_data_name","data index error")
      get_my_send_data_name = mdc(mdl)%sd(data_index)%name
      return
    end if
  end do

  call error("get_my_send_data_name", "no such model name: "//trim(model_name))

end function get_my_send_data_name
  
!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=NAME_LEN) function get_my_recv_data_name(model_name, data_index)
  use jcup_utils, only : error
  implicit none
  character(len=*), intent(IN) :: model_name
  integer, intent(IN) :: data_index
  integer :: mdl


  do mdl = 1, size(mdc,1)
    if (trim(mdc(mdl)%model_name) == trim(model_name)) then
      if (data_index > mdc(mdl)%num_of_recv_data) call error("get_my_recv_data_name","data index error")
      get_my_recv_data_name = mdc(mdl)%rd(data_index)%name
      return
    end if
  end do

  call error("get_my_send_data_name", "no such model name: "//trim(model_name))

end function get_my_recv_data_name
  
!=======+=========+=========+=========+=========+=========+=========+=========+

function get_send_data_conf_ptr_from_id(comp_id, data_num) result(send_data_conf_ptr)
  implicit none
  integer, intent(IN) :: comp_id, data_num
  type(send_data_conf_type), pointer :: send_data_conf_ptr

  send_data_conf_ptr => mdc(comp_id)%sd(data_num)

end function get_send_data_conf_ptr_from_id

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_recv_data_conf_ptr_from_id(comp_id, data_num) result(recv_data_conf_ptr)
  implicit none
  integer, intent(IN) :: comp_id, data_num
  type(recv_data_conf_type), pointer :: recv_data_conf_ptr

  recv_data_conf_ptr => mdc(comp_id)%rd(data_num)

end function get_recv_data_conf_ptr_from_id

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_other_send_data_conf_ptr(model_name, data_num) result(send_data_conf_ptr)
  implicit none
  character(len=*), intent(IN) :: model_name
  integer, intent(IN) :: data_num
  type(send_data_conf_type), pointer :: send_data_conf_ptr
  integer :: mdl

  do mdl = 1, size(mdc)
    if (trim(mdc(mdl)%model_name)==trim(model_name)) then
      send_data_conf_ptr => mdc(mdl)%sd(data_num)
      return
    end if
  end do

end function get_other_send_data_conf_ptr

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_send_data_conf_ptr_name(data_name) result(send_data_conf_ptr)
  implicit none
  character(len=*), intent(IN) :: data_name
  type(send_data_conf_type), pointer :: send_data_conf_ptr
  integer :: i

  do i = 1, current_conf%num_of_send_data
    if (trim(current_conf%sd(i)%name)==trim(data_name)) then
      send_data_conf_ptr => current_conf%sd(i)
    end if
  end do

end function get_send_data_conf_ptr_name

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_send_data_conf_ptr_id(data_num) result(send_data_conf_ptr)
  implicit none
  integer, intent(IN) :: data_num
  type(send_data_conf_type), pointer :: send_data_conf_ptr

  send_data_conf_ptr => current_conf%sd(data_num)

end function get_send_data_conf_ptr_id

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_other_recv_data_conf_ptr(model_name, data_num) result(recv_data_conf_ptr)
  implicit none
  character(len=*), intent(IN) :: model_name
  integer, intent(in) :: data_num
  type(recv_data_conf_type), pointer :: recv_data_conf_ptr
  integer :: mdl

  do mdl = 1, size(mdc)
    if (trim(mdc(mdl)%model_name)==trim(model_name)) then
      recv_data_conf_ptr => mdc(mdl)%rd(data_num)
      return
    end if
  end do

end function get_other_recv_data_conf_ptr

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_recv_data_conf_ptr_name(data_name) result(recv_data_conf_ptr)
  implicit none
  character(len=*), intent(IN) :: data_name
  type(recv_data_conf_type), pointer :: recv_data_conf_ptr
  integer :: i

  do i = 1, current_conf%num_of_recv_data
    if (trim(current_conf%rd(i)%name)==trim(data_name)) then
      recv_data_conf_ptr => current_conf%rd(i)
    end if
  end do

end function get_recv_data_conf_ptr_name

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_recv_data_conf_ptr_id(data_num) result(recv_data_conf_ptr)
  implicit none
  integer, intent(IN) :: data_num
  type(recv_data_conf_type), pointer :: recv_data_conf_ptr

  recv_data_conf_ptr => current_conf%rd(data_num)

end function get_recv_data_conf_ptr_id

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_exchange_step_all_from_ct(c_time)
  use jcup_time, only : time_type, is_exchange_step_from_c_time
  implicit none
  type(time_type), intent(IN) :: c_time
  integer :: i

  do i = 1, size(current_conf%exchange_interval)
    if (current_conf%exchange_interval(i)/=NO_EXCHANGE) then
      if (is_exchange_step_from_c_time(current_conf_id, 1, current_conf%exchange_interval(i), c_time)) then
        is_exchange_step_all_from_ct = .true.
        return
      end if
    end if
  end do

  is_exchange_step_all_from_ct = .false.

end function is_exchange_step_all_from_ct

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_exchange_step_all_models()
  use jcup_time, only : is_exchange_step
  implicit none
  integer :: i
  
  do i = 1, size(current_conf%exchange_interval)
    if (current_conf%exchange_interval(i)/=NO_EXCHANGE) then
      if (is_exchange_step(current_conf_id, 1, current_conf%exchange_interval(i))) then
        is_exchange_step_all_models = .true.
        return
      end if
    end if
  end do

  is_exchange_step_all_models = .false.

end function is_exchange_step_all_models
 
!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_exchange_step_some_model(target_model_id)
  use jcup_time, only : is_exchange_step
  implicit none
  integer, intent(IN) :: target_model_id

  if (current_conf%exchange_interval(target_model_id) /= NO_EXCHANGE) then
    if (is_exchange_step(current_conf_id, 1, current_conf%exchange_interval(target_model_id))) then
      is_exchange_step_some_model = .true.
      return
    end if
  end if

  is_exchange_step_some_model = .false.

end function is_exchange_step_some_model

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_exchange_step_send_recv_model(my_comp_id, target_comp_id, c_time)
  use jcup_time, only : time_type, is_exchange_step_from_c_time
  implicit none
  integer, intent(IN) :: my_comp_id, target_comp_id
  type(time_type), intent(IN) :: c_time

  if (mdc(my_comp_id)%exchange_interval(target_comp_id) /= NO_EXCHANGE) then
    if (is_exchange_step_from_c_time(my_comp_id, 1, mdc(my_comp_id)%exchange_interval(target_comp_id), c_time)) then
      is_exchange_step_send_recv_model = .true.
      return
    end if
  end if

  is_exchange_step_send_recv_model = .false.

end function is_exchange_step_send_recv_model

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_put_step_data(data_name)
  use jcup_time, only : is_next_exchange_step
  implicit none
  character(len=*), intent(IN) :: data_name
  integer :: i, j
  
  do i = 1, current_conf%num_of_send_data
    if (trim(data_name)==trim(current_conf%sd(i)%name)) then
      do j = 1, current_conf%sd(i)%num_of_my_recv_data
        if (is_next_exchange_step(current_conf_id, 1, current_conf%sd(i)%my_recv_conf(j)%interval)) then
          is_put_step_data = .true.
          return
        end if
      end do
    end if
  end do

  is_put_step_data = .false.

end function is_put_step_data

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_send_step_data(dest_model_id, data_name)
  use jcup_time, only : is_exchange_step
  implicit none
  integer, intent(IN) :: dest_model_id
  character(len=*), intent(IN) :: data_name
  integer :: i, j

  do i = 1, current_conf%num_of_send_data
    if (trim(data_name)==trim(current_conf%sd(i)%name)) then
      do j = 1, current_conf%sd(i)%num_of_my_recv_data
        if (dest_model_id==current_conf%sd(i)%my_recv_conf(j)%model_id) then
          if (is_exchange_step(current_conf_id, 1, current_conf%sd(i)%my_recv_conf(j)%interval)) then
            is_send_step_data = .true.
            return
          end if
        end if
      end do
    end if
  end do

  is_send_step_data = .false.

end function is_send_step_data

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_recv_step_data(data_name)
  use jcup_time, only : is_exchange_step
  implicit none
  character(len=*), intent(IN) :: data_name
  integer :: i, j

  do i = 1, current_conf%num_of_recv_data
    if (trim(data_name)==trim(current_conf%rd(i)%name)) then
        if (is_exchange_step(current_conf_id, 1, current_conf%rd(i)%interval)) then
          is_recv_step_data = .true.
          return
        end if
    end if
  end do

  is_recv_step_data = .false.

end function is_recv_step_data

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_comp_id_from_comp_name(comp_name)
  use jcup_utils, only : error, IntToStr
  implicit none
  character(len=*), intent(IN) :: comp_name
  integer :: i

  do i = 1, size(mdc)
    if (trim(mdc(i)%model_name) == trim(comp_name)) then
      get_comp_id_from_comp_name = mdc(i)%model_id
      return
    end if
  end do

  call error("get_comp_id_from_comp_name", "no such component name : "//trim(comp_name))

end function get_comp_id_from_comp_name
  
!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=NAME_LEN) function get_comp_name_from_comp_id(comp_id)
  use jcup_utils, only : error, IntToStr
  implicit none
  integer, intent(IN) :: comp_id
  integer :: i

  do i = 1, size(mdc)
    if (mdc(i)%model_id == comp_id) then
      get_comp_name_from_comp_id = mdc(i)%model_name
      return
    end if
  end do

  call error("get_comp_name_from_comp_id", "no such component id : "//trim(IntToStr(comp_id)))

end function get_comp_name_from_comp_id
  
!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_comp_exchange_type(my_comp_id, target_comp_id)
  implicit none
  integer, intent(IN) :: my_comp_id, target_comp_id

  get_comp_exchange_type = comp_exchange_type(my_comp_id, target_comp_id)

end function get_comp_exchange_type

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_send_comp_id_from_data_name(data_name)
  use jcup_utils, only : error
  implicit none
  character(len=*), intent(IN) :: data_name
  integer :: i, j

  do i = 1, size(mdc)
    do j = 1, mdc(i)%num_of_send_data
      if (trim(data_name)==trim(mdc(i)%sd(j)%name)) then
        get_send_comp_id_from_data_name = mdc(i)%model_id
        return
      end if
    end do
  end do

  call error("get_send_comp_id_from_data_name", "no such data name : "//trim(data_name))

end function get_send_comp_id_from_data_name

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_recv_comp_id_from_data_name(data_name)
  use jcup_utils, only : error
  implicit none
  character(len=*), intent(IN) :: data_name
  integer :: i, j

  do i = 1, size(mdc)
    do j = 1, mdc(i)%num_of_recv_data
      if (trim(data_name)==trim(mdc(i)%rd(j)%name)) then
        get_recv_comp_id_from_data_name = mdc(i)%model_id
        return
      end if
    end do
  end do

  call error("get_recv_comp_id_from_data_name", "no such data name : "//trim(data_name))

end function get_recv_comp_id_from_data_name

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_send_data_id_from_data_name(data_name)
  use jcup_utils, only : error
  implicit none
  character(len=*), intent(IN) :: data_name
  integer :: i, j

  do i = 1, size(mdc)
    do j = 1, mdc(i)%num_of_send_data
      if (trim(data_name)==trim(mdc(i)%sd(j)%name)) then
        get_send_data_id_from_data_name = mdc(i)%sd(j)%data_id
        return
      end if
    end do
  end do

  call error("get_data_comp_id_from_data_name", "no such data name : "//trim(data_name))

end function get_send_data_id_from_data_name

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_recv_data_id_from_data_name(data_name)
  use jcup_utils, only : error
  implicit none
  character(len=*), intent(IN) :: data_name
  integer :: i, j

  do i = 1, size(mdc)
    do j = 1, mdc(i)%num_of_recv_data
      if (trim(data_name)==trim(mdc(i)%rd(j)%name)) then
        get_recv_data_id_from_data_name = mdc(i)%rd(j)%data_id
        return
      end if
    end do
  end do

  call error("get_recv_data_id_from_data_name", "no such data name : "//trim(data_name))

end function get_recv_data_id_from_data_name


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_configuration(my_conf)
  !!!!use jcup_mpi_lib, only : jml_isLocalLeader
  use jcup_utils, only : error, IntToStr
  use jcup_comp, only : get_num_of_total_component
  implicit none
  type(model_data_conf_type), intent(IN) :: my_conf
  integer :: i, j, k
  integer, allocatable :: min_recv_interval(:)
  integer, allocatable :: min_lag(:), max_lag(:)

  !if (.not.jml_isLocalLeader()) return

  ! check model name, data name
  do i = 1, my_conf%num_of_recv_data
    if (.not.my_conf%rd(i)%is_recv) cycle
    do j = 1, size(mdc)
      if (trim(my_conf%rd(i)%send_model)==trim(mdc(j)%model_name)) then
        do k = 1, size(mdc(j)%sd)
          if (trim(my_conf%rd(i)%send_data)==trim(mdc(j)%sd(k)%name)) then
             goto 1000
          end if
        end do
        call error("check_configuration", "No such data, data_name:"//trim(my_conf%rd(i)%send_data)// &
                   ", Check jcup_def_varg in the model "//trim(my_conf%model_name)// &
                   " or jcup_def_varp in the model "//trim(my_conf%rd(i)%send_model))
      end if
    end do
    call error("check_configuration", "No such model, model_name:"//trim(my_conf%rd(i)%send_model)// &
               ", Check jcup_def_varg in the model "//trim(my_conf%model_name))
  1000 continue
  end do

  ! check exchange interval
  allocate(min_recv_interval(get_num_of_total_component()))
  min_recv_interval(:) = NO_EXCHANGE

  do i = 1, my_conf%num_of_recv_data
    if (my_conf%rd(i)%is_recv) then
      k = my_conf%rd(i)%send_model_id
      min_recv_interval(k) = min(min_recv_interval(k), my_conf%rd(i)%interval)
    end if
  end do

  do i = 1, my_conf%num_of_recv_data
    if (my_conf%rd(i)%is_recv) then
      k = my_conf%rd(i)%send_model_id
      if (mod(my_conf%rd(i)%interval, min_recv_interval(k))/=0) then
        call error("check_configuration","recv data exchange interval error, data name: "//trim(my_conf%rd(i)%name))
      end if
    end if
  end do

  deallocate(min_recv_interval)


  ! check time lag
  allocate(min_lag(get_num_of_total_component()))
  allocate(max_lag(get_num_of_total_component()))
  min_lag(:) = 888
  max_lag(:) = -888

  do i = 1, my_conf%num_of_recv_data
    if (my_conf%rd(i)%is_recv) then
      k = my_conf%rd(i)%send_model_id
      if (my_conf%rd(i)%time_lag /= 0) min_lag(k) = min(min_lag(k), my_conf%rd(i)%time_lag)
      if (my_conf%rd(i)%time_lag /= 0) max_lag(k) = max(max_lag(k), my_conf%rd(i)%time_lag)
    end if
  end do

  do i = 1, my_conf%num_of_recv_data
    if (my_conf%rd(i)%is_recv) then
      k = my_conf%rd(i)%send_model_id
      if (min_lag(k) /= max_lag(k)) then
        if ((min_lag(k) /= 888).and.(max_lag(k) /= -888)) then
          call error("check_configuration","recv data time lag error, data name: "//trim(my_conf%rd(i)%name) &
                     //"min lag = "//trim(IntToStr(min_lag(k)))//", max lag = "//trim(IntToStr(max_lag(k))))
        end if
      end if
    end if
  end do

  deallocate(min_lag)
  deallocate(max_lag)

  do i = 1, my_conf%num_of_recv_data
    if (my_conf%rd(i)%is_recv) then
      if (my_conf%rd(i)%time_lag /= 0) then
        call check_time_lag(mdc(my_conf%rd(i)%send_model_id), my_conf%model_id, my_conf%rd(i)%time_lag)
      end if
    end if
  end do

  ! check send recv
  do i = 1, my_conf%num_of_recv_data
    if (my_conf%rd(i)%is_recv) then
      do j = 1, mdc(my_conf%rd(i)%send_model_id)%num_of_send_data      
        if(trim(my_conf%rd(i)%send_data) == trim(mdc(my_conf%rd(i)%send_model_id)%sd(j)%name)) then
          if(.not.mdc(my_conf%rd(i)%send_model_id)%sd(j)%is_send) then
            call error("check_configuration","recv data "//trim(my_conf%rd(i)%name)//" required, but send_flag = 0")
          end if
        end if
      end do
    end if
  end do

end subroutine check_configuration

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_time_lag(target_conf, my_model_id, my_time_lag)
  use jcup_utils, only : error, IntToStr
  use jcup_comp, only : get_component_name
  implicit none
  type(model_data_conf_type), intent(IN) :: target_conf
  integer, intent(IN) :: my_model_id, my_time_lag
  integer :: i

  do i = 1, target_conf%num_of_recv_data
    if (target_conf%rd(i)%is_recv) then
      if (target_conf%rd(i)%send_model_id == my_model_id) then
        select case(target_conf%rd(i)%time_lag)
        case (1)
          if (my_time_lag == -1) cycle
        case(-1)
          if ((my_time_lag == -1).or.(my_time_lag == 1)) cycle
        case(0)
          cycle ! skip if time lag == 0
          if (my_time_lag == 0) cycle
        end select
        call error("check_time_lag", "time_lag setting error between " &
                   //trim(get_component_name(my_model_id))//trim(IntTostr(my_time_lag))//" and "//trim(get_component_name(target_conf%model_id))//trim(IntToStr(target_conf%rd(i)%time_lag)))
      end if
    end if
  end do

end subroutine check_time_lag
  
!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine modify_my_conf_data()
  use jcup_utils, only : put_log
  use jcup_comp, only : is_model_running, is_my_component
  implicit none
  integer :: ii, i, j, k

  do ii = 1, size(mdc)
    do i = 1, size(mdc(ii)%sd)
      do j = 1, size(mdc)
        if (is_model_running(mdc(j)%model_name)) then
          do k = 1, size(mdc(j)%rd)
            if (trim(mdc(j)%rd(k)%send_model)==trim(mdc(ii)%model_name)) then
              if (trim(mdc(ii)%sd(i)%name)==trim(mdc(j)%rd(k)%send_data)) then
                if (mdc(j)%rd(k)%is_recv) then
                  goto 1000 ! next data
                end if
              end if
            end if
          end do
        end if
      end do
   
      if (is_my_component(ii)) call put_log("No model requires send data : "//trim(mdc(ii)%sd(i)%name)//", set send_flag = 0" )
      mdc(ii)%sd(i)%is_send = .false. ! no model or no recv flag

    1000 continue
    end do
  end do

end subroutine modify_my_conf_data

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_recv_tag()
  implicit none
  integer :: mdl, i, j
  do mdl = 1, size(mdc)
    do i = 1, mdc(mdl)%num_of_recv_data
      call cal_recv_tag(mdc(mdl)%rd(i))
    end do
  end do

  do mdl = 1, size(mdc)
    do i = 1, mdc(mdl)%num_of_send_data
      do j = 1, mdc(mdl)%sd(i)%num_of_my_recv_data
        call cal_recv_tag(mdc(mdl)%sd(i)%my_recv_conf(j))
      end do
    end do
  end do

end subroutine set_recv_tag

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_recv_tag(rd)
  implicit none
  type(recv_data_conf_type), intent(INOUT) :: rd
  integer :: mode

  mode = 0
  if (rd%is_average) mode = 1

  rd%recv_tag = rd%interval*100000+mode*10000 &
               +rd%mapping_tag*100+rd%exchange_tag

  rd%recv_tag = rd%recv_tag*100000*1000000 +rd%send_model_id

end subroutine cal_recv_tag

!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine write_configure(my_conf, out_unit)
  implicit none
  type(model_data_conf_type), intent(IN) :: my_conf
  integer, intent(IN) :: out_unit

  integer :: i

    write(out_unit,*) "======================================================="
    write(out_unit,*) "my name : ", trim(my_conf%model_name)
    write(out_unit,*) "  exchange interval "
    write(out_unit,'(8A12)') "   ", (trim(mdc(i)%model_name), i = 1, size(mdc))
    write(out_unit,*) "           ", (my_conf%exchange_interval(i), i = 1, size(mdc))
    write(out_unit,*) "           ", (comp_exchange_type(current_conf_id, i), i = 1, size(mdc))

    write(out_unit,*) "-------------------------------------------------------"
    do i = 1, my_conf%num_of_send_data
      call write_send_conf(my_conf%sd(i), out_unit)
    end do

    write(out_unit,*) "-------------------------------------------------------"
    do i = 1, my_conf%num_of_recv_data
      call write_recv_conf(my_conf%rd(i), out_unit)
    end do

    do i = 1, size(mdc)
      call write_model_conf(mdc(i),out_unit)
    end do

end subroutine write_configure

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine write_model_conf(mc, out_unit)
  implicit none
  type(model_data_conf_type), intent(IN) :: mc
  integer, intent(IN) :: out_unit
  integer :: i

  write(out_unit,*) "======================================================="
  write(out_unit, '(A12,I3,I3)') mc%model_name, mc%num_of_send_data, mc%num_of_recv_data

  write(out_unit,*) "-------------------------------------------------------"
  do i = 1, mc%num_of_send_data
    call write_send_conf(mc%sd(i), out_unit)
  end do

  write(out_unit,*) "-------------------------------------------------------"
  do i = 1, mc%num_of_recv_data
    call write_recv_conf(mc%rd(i), out_unit)
  end do

end subroutine write_model_conf

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine write_send_conf(sd, out_unit)
  use jcup_comp, only : get_component_name
  implicit none
  type(send_data_conf_type), intent(IN) :: sd
  integer, intent(IN) :: out_unit
  integer :: i

  write(out_unit, '(A12,L3,L3)') sd%name, sd%is_send, sd%is_average!, sd%interval
  write(out_unit,'(A12,A12,A8,A12)') "recv model", "recv data","intrvl","is average"
  do i = 1, sd%num_of_my_recv_data
    write(out_unit, '(A12,A12,I8,L12)') trim(get_component_name(sd%my_recv_conf(i)%model_id)), &
                                    trim(sd%my_recv_conf(i)%name), &
                                    sd%my_recv_conf(i)%interval, &
                                    sd%my_recv_conf(i)%is_average
  end do

end subroutine write_send_conf

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine write_recv_conf(rd, out_unit)
  implicit none
  type(recv_data_conf_type), intent(IN) :: rd
  integer, intent(IN) :: out_unit

  write(out_unit, '(A12,L3,L3,I6,I3,I5," ",A12,A12)') rd%name, rd%is_recv, rd%is_average, rd%interval, &
                   rd%mapping_tag, rd%exchange_tag, rd%send_model, rd%send_data

end subroutine write_recv_conf

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine write_configure_2(my_conf, out_unit)
  implicit none
  type(model_data_conf_type), intent(IN) :: my_conf
  integer, intent(IN) :: out_unit

  character(len=NAME_LEN) :: name
  character(len=NAME_LEN) :: data_name
  integer :: send_flag 
  integer :: recv_flag
  character(len=3) :: recv_mode
  integer :: interval, time_lag, mapping_tag, exchange_tag
  character(len=NAME_LEN) :: send_model, send_data
  namelist /model/ name
  namelist /senddata/ data_name, send_flag
  namelist /recvdata/ data_name, recv_flag, recv_mode, interval, time_lag, mapping_tag, exchange_tag, &
                      send_model, send_data
  integer :: i

  write(out_unit, *)
  write(out_unit, *) "--------------------------------------------------------------------------"

  name = my_conf%model_name
  write(out_unit, nml = model)

  do i = 1, my_conf%num_of_send_data
    write(out_unit, *)
    data_name = my_conf%sd(i)%name
    send_flag = 1
    write(out_unit, nml = senddata)
  end do

  do i = 1, my_conf%num_of_recv_data
    write(out_unit, *)
    data_name = my_conf%rd(i)%name
    recv_flag = 1
    recv_mode = "SNP"
    if (my_conf%rd(i)%is_average) recv_mode = "AVR"
    interval = my_conf%rd(i)%interval
    time_lag = my_conf%rd(i)%time_lag
    mapping_tag = my_conf%rd(i)%mapping_tag
    exchange_tag = my_conf%rd(i)%exchange_tag
    send_model = my_conf%rd(i)%send_model
    send_data = my_conf%rd(i)%send_data
    write(out_unit, nml = recvdata)
  end do

end subroutine write_configure_2


!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine read_model_name(file_name, model_name)
  use jcup_utils, only : error, startsWith, up2lw, is_comment_line, open_log_file, close_log_file
  implicit none
  character(len=*), intent(IN) :: file_name
  character(len=NAME_LEN), pointer :: model_name(:)

  integer,parameter :: FILE_ID = 111
  character(len=NAME_LEN) :: data_str, lw_str
  integer :: model_counter
  integer :: m
  character(len=NAME_LEN) :: name
  namelist /model/ name


  ! set model name
  ! count num. of data
  open(FILE_ID, file = trim(file_name), status = "OLD", action = "READ", err = 500)

  model_counter = 0

  do 
    read(FILE_ID,*,END = 100) data_str
    lw_str = trim(up2lw(data_str))
    !write(0,*) data_str, trim(up2lw(data_str))
    if (startsWith(lw_str, "&model")) model_counter = model_counter+1
  end do    

100 continue

  if (model_counter==0) then
    call error("read_conf_file","a number of model == 0: check "//trim(file_name))
  end if
 
  allocate(model_name(model_counter))

  rewind(FILE_ID)

  do m = 1, model_counter
    read(FILE_ID, nml = model)
    model_name(m) = name
  end do

  close(FILE_ID)

  return

500 call error("read_model_name","data conf file : "//trim(file_name)//" open error")

end subroutine read_model_name

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine read_conf_file_1()
  use jcup_utils, only : error
  implicit none
  integer,parameter :: FILE_ID = 110
  integer :: model_counter
  integer :: send_counter, recv_counter
  character(len=NAME_LEN) :: data_str, lw_str
  integer :: m,i,j


  ! set model name
  ! count num. of data
  open(FILE_ID, file = trim(conf_file_name), status = "OLD", action = "READ", err = 500)

  call read_coupler_setting(FILE_ID)

  close (FILE_ID)

  return

500 call error("read_conf_file","data conf file : "//trim(conf_file_name)//" open error")

end subroutine read_conf_file_1

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine read_conf_file_2()
  use jcup_utils, only : error, startsWith, up2lw, is_comment_line, open_log_file, close_log_file
  use jcup_mpi_lib, only : jml_GetMyrankGlobal, jml_isLocalLeader
  use jcup_comp, only : get_num_of_total_component, is_my_component
  implicit none
  integer,parameter :: FILE_ID = 110
  integer :: model_counter
  integer :: table_counter
  integer :: send_counter, recv_counter
  character(len=NAME_LEN) :: data_str, lw_str
  integer :: m,i,j

  open(FILE_ID, file = trim(conf_file_name), status = "OLD", action = "READ", err = 500)

  ! set model name
  ! count num. of data
  model_counter = 0

  do 
    read(FILE_ID,*,END = 100) data_str
    lw_str = trim(up2lw(data_str))
    !write(0,*) data_str, trim(up2lw(data_str))
    if (startsWith(lw_str, "&model")) model_counter = model_counter+1
  end do    

100 continue

  if (model_counter==0) then
    call error("read_conf_file","a number of model == 0: check "//trim(conf_file_name))
  end if

  if (model_counter < get_num_of_total_component()) then
    call error("read_conf_file","a number of component in coupling.conf < defined component in the program") 
  end if

  allocate(mdc_tmp(model_counter))
  mdc_tmp(:)%num_of_remapping_table = 0
 
  rewind(FILE_ID)

  model_counter = 0

  do 

    read(FILE_ID, *, END = 200) data_str

    if (is_comment_line(data_str)) cycle

    lw_str = trim(up2lw(data_str))

    if (startsWith(lw_str,"&model")) then
      if (model_counter > 0) then

        if (table_counter>0) then
          allocate(mdc_tmp(model_counter)%fl(table_counter))
          do i = 1, table_counter
            call init_remapping_table_conf(mdc_tmp(model_counter)%fl(i))
          end do
        end if
        allocate(mdc_tmp(model_counter)%sd(send_counter))
        do i = 1, send_counter
          call init_send_data_conf(mdc_tmp(model_counter)%sd(i))
        end do
        allocate(mdc_tmp(model_counter)%rd(recv_counter))
        do i = 1, recv_counter
          call init_recv_data_conf(mdc_tmp(model_counter)%rd(i))
        end do
        mdc_tmp(model_counter)%num_of_remapping_table = table_counter
        mdc_tmp(model_counter)%num_of_send_data = send_counter
        mdc_tmp(model_counter)%num_of_recv_data = recv_counter
      end if
      model_counter = model_counter+1
      table_counter = 0
      send_counter = 0 ; recv_counter = 0
    end if
    if (startsWith(lw_str,"&remap")) table_counter = table_counter + 1
    if (startsWith(lw_str,"&senddata")) send_counter = send_counter+1
    if (startsWith(lw_str,"&recvdata")) recv_counter = recv_counter+1
  end do    

200 continue

        if (table_counter>0) then
          allocate(mdc_tmp(model_counter)%fl(table_counter))
          do i = 1, table_counter
            call init_remapping_table_conf(mdc_tmp(model_counter)%fl(i))
          end do
        end if
        allocate(mdc_tmp(model_counter)%sd(send_counter))
        do i = 1, send_counter
          call init_send_data_conf(mdc_tmp(model_counter)%sd(i))
        end do
        allocate(mdc_tmp(model_counter)%rd(recv_counter))
        do i = 1, recv_counter
          call init_recv_data_conf(mdc_tmp(model_counter)%rd(i))
        end do
        mdc_tmp(model_counter)%num_of_remapping_table = table_counter
        mdc_tmp(model_counter)%num_of_send_data = send_counter
        mdc_tmp(model_counter)%num_of_recv_data = recv_counter

  rewind(FILE_ID)

  do m = 1, model_counter

    allocate(mdc_tmp(m)%exchange_interval(model_counter))
    mdc_tmp(m)%exchange_interval(:) = NO_EXCHANGE

    call read_model_namelist(FILE_ID, mdc_tmp(m))

    do i = 1, mdc_tmp(m)%num_of_remapping_table
      call read_remapping_namelist(FILE_ID, mdc_tmp(m)%fl(i))
    end do

    do i = 1, mdc_tmp(m)%num_of_send_data
      call read_send_namelist(FILE_ID, mdc_tmp(m)%sd(i))
    end do

    do i = 1, mdc_tmp(m)%num_of_recv_data
      call read_recv_namelist(FILE_ID, mdc_tmp(m)%rd(i))
    end do

  end do

  close(FILE_ID)

  call reorder_conf_data() ! change ordering of configure data to fit component information
  call set_send_average_flag()
  call set_data_id()
  call set_recv_data_info()
  call cal_exchange_interval()


  do i = 1, get_num_of_total_component()
    if (is_my_component(i)) then
      call set_current_conf(i)
      call check_configuration(current_conf)
    end if
  end do


  call modify_my_conf_data()


  call set_recv_tag()
  call set_model_exchange_type()


  if (jml_GetMyrankGlobal()==0) then
    do i = 1, get_num_of_total_component()
     !if (is_my_component(i)) then
        call set_current_conf(i)
        !if (jml_isLocalLeader(i)) then
          call open_log_file("./"//trim(get_comp_name_from_comp_id(i))//".conf.log", WRITE_CONF_UNIT+i)
          call write_configure(current_conf, WRITE_CONF_UNIT+i)
          call close_log_file(WRITE_CONF_UNIT+i)
        !end if
      !end if
    end do
  end if

  return

500 call error("read_conf_file","data conf file : "//trim(conf_file_name)//" open error")

end subroutine read_conf_file_2


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine read_coupler_setting(FILE_ID)
  use jcup_utils, only : set_log_level, error
  implicit none
  integer, intent(IN) :: FILE_ID
  integer :: log_level = 0
  logical :: log_stderr = .false.
  integer :: status

  namelist /coupler_config/ log_level, log_stderr

  read(FILE_ID, nml = coupler_config, IOSTAT = status)

  if (status < 0) call error("read_coupler_setting", "coupler.conf format error")

  call set_log_level(log_level, log_stderr)

end subroutine read_coupler_setting

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine read_model_namelist(FILE_ID, md)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: FILE_ID
  type(model_data_conf_type), intent(INOUT) :: md

  integer :: status
  character(len=NAME_LEN) :: name
  namelist /model/ name

  read(FILE_ID, nml = model, IOSTAT = status)

  if (status < 0) call error("read_model_namelist", "coupler.conf format error")

  md%model_name = trim(name)

end subroutine read_model_namelist

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine read_remapping_namelist(FILE_ID, fl)
  use jcup_utils, only : error
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  integer, intent(IN) :: FILE_ID
  type(remapping_table_conf_type), intent(INOUT) :: fl
  integer :: status
  character(len=NAME_LEN) :: model_put, model_get, grid_put, grid_get
  character(len=STRING_LEN) :: fl_remap
  namelist /remap/ model_put, model_get, grid_put, grid_get, fl_remap

  model_put="NO_MODEL" ; model_get="" ; grid_put="" ; grid_get=""
  fl_remap = NO_FILE

  read(FILE_ID, nml = remap, IOSTAT = status)

  if (status < 0) call error("read_remapping_namelist", "coupler.conf format error")

  
  if (trim(model_put) /= "NO_MODEL") fl%send_model_id = get_comp_id_from_name(trim(model_put))
  fl%grid_put = trim(grid_put)
  fl%grid_get = trim(grid_get)
  fl%table_file_name = trim(fl_remap)
  
end subroutine read_remapping_namelist

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine read_send_namelist(FILE_ID, sd)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: FILE_ID
  type(send_data_conf_type), intent(INOUT) :: sd
  character(len=NAME_LEN) :: data_name
  integer :: status
  integer :: send_flag, interval
  namelist /senddata/ data_name, send_flag

  read(FILE_ID, nml = senddata, IOSTAT = status)

  if (status < 0) call error("read_send_namelist", "coupler.conf format error")

  sd%name = trim(data_name)
  sd%is_send = (send_flag==1)

end subroutine read_send_namelist

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine read_recv_namelist(FILE_ID, rd)
  use jcup_utils, only : error
  implicit none
  integer, intent(IN) :: FILE_ID
  type(recv_data_conf_type), intent(INOUT) :: rd
  integer :: status
  character(len=NAME_LEN) :: data_name
  integer :: recv_flag
  character(len=NAME_LEN) :: recv_mode
  integer :: interval, time_lag
  integer :: mapping_tag, exchange_tag
  character(len=NAME_LEN) :: send_model, send_data
  namelist /recvdata/ data_name, recv_flag, recv_mode, interval, time_lag, &
                      mapping_tag, exchange_tag, send_model, send_data

  read(FILE_ID, nml = recvdata, IOSTAT = status)

  if (status < 0) call error("read_recv_namelist", "coupler.conf format error")

  rd%name = trim(data_name)
  rd%is_recv = (recv_flag==1)
  rd%is_average = (trim(recv_mode)=='AVR')
  rd%interval = interval*60 ! (min to sec)
  rd%time_lag = time_lag
  rd%mapping_tag = mapping_tag
  rd%exchange_tag = exchange_tag
  rd%send_model = trim(send_model)
  rd%send_data = trim(send_data)

end subroutine read_recv_namelist

!=======+=========+=========+=========+=========+=========+=========+=========+


subroutine reorder_conf_data
  use jcup_utils, only : Error
  use jcup_comp, only : get_num_of_total_component, get_component_name, get_comp_id_from_name, &
                        is_model_running
  implicit none
  integer :: mdl, m

  do mdl = 1, get_num_of_total_component()
    do m = 1, size(mdc_tmp)
      if (trim(get_component_name(mdl))==trim(mdc_tmp(m)%model_name)) then
        mdc(mdl) = mdc_tmp(m)
        mdc(mdl)%model_id = mdl
        goto 555 !exit
      end if
    end do
    call Error("reorder_conf_data", "no such model "//trim(get_component_name(mdl))//" listed in configure file")
555 continue
  end do


  do mdl = 1, get_num_of_total_component()
    do m = 1, mdc(mdl)%num_of_send_data
      mdc(mdl)%sd(m)%model_id = mdl
    end do
    do m = 1, mdc(mdl)%num_of_recv_data
      mdc(mdl)%rd(m)%model_id = mdl
      if (mdc(mdl)%rd(m)%is_recv) then
        if (is_model_running(trim(mdc(mdl)%rd(m)%send_model))) then
          mdc(mdl)%rd(m)%send_model_id = get_comp_id_from_name(trim(mdc(mdl)%rd(m)%send_model))
        else
          mdc(mdl)%rd(m)%is_recv = .false.
        end if
      end if
    end do
  end do

end subroutine reorder_conf_data

!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcup_config




