!=======+=========+=========+=========+=========+=========+=========+=========+

!
!Copyright (c) 2011, arakawa@rist.jp
!All rights reserved.
!

module jcup_comp
  use jcup_constant, only : NAME_LEN, STRING_LEN, MAX_MODEL, MAX_DOMAIN, CONF_FILE
  implicit none
  private 

!--------------------------------   public  ----------------------------------!
 
  public :: set_my_component ! subroutine (component_name)
  public :: init_model_process
  public :: get_num_of_total_component ! integer function (NONE)
  public :: get_component_name         ! character function (component_id)
  public :: get_num_of_my_component    ! integer function (NONE)
  public :: get_comp_id_from_name  ! integer function (component_name)
  public :: is_my_component ! logical function (component_id) or (component_name)
  public :: is_model_running ! logical function (component_name)
  public :: get_component_relation ! integer function (my_component_id, target_component_id)

!--------------------------------   private  ---------------------------------!

  interface is_my_component
    module procedure is_my_component_id, is_my_component_name
  end interface

  integer, private :: num_of_total_pe
  integer, private :: my_rank_global
  integer, private :: my_rank_local  
  integer, private :: my_model_id
  integer, private :: num_of_model
  integer, private :: num_of_task

  integer, private :: num_of_my_component = 0! the number of my component
  character(len=NAME_LEN) :: my_component_name(MAX_MODEL)
  integer, private :: num_of_total_component = 0 ! the number of total component

  type component_def_type
    character(len=NAME_LEN) :: component_name
    integer :: component_id
    integer :: leader_rank ! global rank of my leader proessor
    integer :: num_of_pe
  end type

  type(component_def_type), private, pointer :: my_comp(:)
  type(component_def_type), private, pointer :: all_comp(:)  

  integer, allocatable :: config_model_id_to_comp_id(:) ! table from config.file model id to internal component id
  integer, allocatable :: comp_id_to_config_model_id(:) ! table from internal component id to config.file model id
 
contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_my_component(component_name)
  implicit none
  character(len=*), intent(IN) :: component_name

  num_of_my_component = num_of_my_component + 1
  
  my_component_name(num_of_my_component) = trim(component_name)

end subroutine set_my_component

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_global_component(total_comp_name)
  use jcup_mpi_lib, only : jml_BcastGlobal
  implicit none
  character(len=NAME_LEN), pointer :: total_comp_name(:)
  character(len=NAME_LEN) :: name_buffer(MAX_MODEL)
  integer :: int_buffer(1)
  integer :: num_of_comp
  integer :: current_process
  integer :: next_process
  integer :: i
  
  num_of_comp = 0
  current_process = 0
  do 
    call search_next_process(current_process, name_buffer, num_of_comp, next_process)
    if (next_process > 999999) exit
    current_process = next_process
  end do

  allocate(total_comp_name(num_of_comp))
  do i = 1, num_of_comp
    total_comp_name(i) = name_buffer(i)
  end do

end subroutine set_global_component

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine search_next_process(current_process, comp_name, num_of_comp, next_process)
  use jcup_mpi_lib, only : jml_BcastGlobal, jml_AllReduceMin
  implicit none
  integer, intent(IN) :: current_process
  character(len=NAME_LEN), intent(INOUT) :: comp_name(MAX_MODEL)
  integer, intent(INOUT) :: num_of_comp
  integer, intent(INOUT) :: next_process
  integer :: int_buffer(1)
  character(len=NAME_LEN), pointer :: name_buffer(:)
  integer :: i, j
  integer :: same_index
  logical :: same_flag

  same_index = 999999999

  if (my_rank_global == current_process) then 
    call add_comp_name(current_process, comp_name, num_of_comp)

    int_buffer(1) = num_of_comp
    call jml_BcastGlobal(int_buffer, 1, 1, current_process)
    allocate(name_buffer(int_buffer(1)))
    do i = 1, int_buffer(1)
      name_buffer(i) = trim(comp_name(i))
    end do
    call jml_BcastGlobal(name_buffer, NAME_LEN, current_process)
  else
    call jml_BcastGlobal(int_buffer, 1, 1, current_process)
    num_of_comp = int_buffer(1)
    allocate(name_buffer(num_of_comp))
    call jml_BcastGlobal(name_buffer, NAME_LEN, current_process)
    do i = 1, num_of_comp
      comp_name(i) = trim(name_buffer(i))
    end do

    if (my_rank_global > current_process) then
      do i = 1, num_of_my_component
        same_flag = .false.
        do j = 1, int_buffer(1)
          if (trim(name_buffer(j)) == trim(my_component_name(i))) same_flag = .true.
        end do
        if (.not.same_flag) then
          same_index = my_rank_global
          exit
        end if
      end do
    end if
  end if

  deallocate(name_buffer)

  int_buffer(1) = same_index

  call jml_AllReduceMin(int_buffer(1), next_process)

end subroutine search_next_process

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine add_comp_name(current_process, comp_name, num_of_comp)
  use jcup_mpi_lib, only : jml_SendGlobal, jml_RecvGlobal
  implicit none
  integer, intent(IN) :: current_process
  character(len=NAME_LEN), intent(INOUT) :: comp_name(MAX_MODEL)
  integer, intent(INOUT) :: num_of_comp
  integer :: num_of_new_name
  logical :: same_flag
  integer :: i, j

  if (my_rank_global == current_process) then
    num_of_new_name = 0
    do i = 1, num_of_my_component
      same_flag = .false. 
      do j = 1, num_of_comp
        if (trim(my_component_name(i)) == trim(comp_name(j))) then
          same_flag = .true.
          exit
        end if
      end do
      if (.not.same_flag) then
        num_of_new_name = num_of_new_name + 1
        comp_name(num_of_comp+num_of_new_name) = trim(my_component_name(i))
      end if
    end do
    num_of_comp = num_of_comp+num_of_new_name
  end if

end subroutine add_comp_name

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_my_component_info(total_comp_name)
  use jcup_mpi_lib, only : jml_AllreduceMax
  use jcup_utils, only : error
  implicit none
  character(len=NAME_LEN), intent(IN) :: total_comp_name(:)
  integer :: num_of_config_comp
  integer :: counter, local_counter
  integer :: i, j
  integer, allocatable :: local_comp_flag(:)
  integer, allocatable :: global_comp_flag(:)
  logical :: error_flag

  num_of_config_comp = size(total_comp_name)

  do i = 1, num_of_my_component
    error_flag = .true.
    do j = 1, num_of_config_comp
      if (trim(total_comp_name(j))==trim(my_component_name(i))) error_flag = .false.
    end do
    if (error_flag)&
      & call error("init_my_component_info", "no such component: "//trim(my_component_name(i))//" listed in coupling.conf file")
  end do

  allocate(local_comp_flag(num_of_config_comp))
  local_comp_flag(:) = 0

  allocate(global_comp_flag(num_of_config_comp))
  global_comp_flag(:) = 0

  do i = 1, num_of_config_comp
    do j = 1, num_of_my_component
      if (trim(total_comp_name(i))==trim(my_component_name(j))) then
        local_comp_flag(i) = 1
        exit
      end if
    end do

    call jml_AllreduceMax(local_comp_flag(1), global_comp_flag(i))

  end do

  counter = 0
  do i = 1, num_of_config_comp
    if (global_comp_flag(i)/=0) counter = counter+1
  end do

  num_of_total_component = counter
  
  allocate(config_model_id_to_comp_id(num_of_config_comp))
  allocate(comp_id_to_config_model_id(num_of_total_component))

  config_model_id_to_comp_id(:) = 0
  comp_id_to_config_model_id(:) = 0

  counter = 0
  do i = 1, num_of_config_comp
    if (global_comp_flag(i)/=0) then
      counter = counter + 1
      config_model_id_to_comp_id(i) = counter
      comp_id_to_config_model_id(counter) = i
    end if
  end do

  allocate(all_comp(num_of_total_component))

  counter = 0
  do i = 1, num_of_config_comp
    if (global_comp_flag(i)/=0) then
      counter = counter + 1
      all_comp(counter)%component_name = total_comp_name(i)
      all_comp(counter)%component_id   = counter
      all_comp(counter)%leader_rank  = 0
      all_comp(counter)%num_of_pe = 0
    end if
  end do
  
  allocate(my_comp(num_of_my_component))
  counter = 0
  local_counter = 0
  do i = 1, num_of_config_comp
    if (global_comp_flag(i)/=0) then
      counter = counter+1
      if (local_comp_flag(i)/=0) then
        local_counter = local_counter + 1
        my_comp(local_counter) = all_comp(counter)
      end if
    end if
  end do

  deallocate(local_comp_flag)
  deallocate(global_comp_flag)


end subroutine init_my_component_info

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_component_info()
  use jcup_mpi_lib, only : jml_GetLeaderRank, jml_GetCommSizeLocal  
  implicit none
  character(len=NAME_LEN) :: name_buffer
  integer :: comp_id
  integer :: i

  do i = 1, num_of_total_component
    comp_id = all_comp(i)%component_id
    all_comp(i)%leader_rank = jml_GetLeaderRank(comp_id)
    all_comp(i)%num_of_pe = jml_GetCommSizeLocal(comp_id)
  end do

end subroutine set_component_info

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_model_process(isCallInit)
  use jcup_mpi_lib, only : jml_init, jml_GetCommSizeGlobal, jml_GetMyrankGlobal, jml_create_communicator
  use jcup_utils, only : error
  use jcup_time, only : init_all_time
  implicit none
  logical,intent(IN) :: isCallInit
  integer, allocatable :: my_comp_id(:)
  character(len=NAME_LEN), pointer :: total_comp_name(:)
  integer :: p

  call jml_init(isCallInit)
  my_rank_global = jml_GetMyRankGlobal() 
  num_of_total_pe = jml_GetCommSizeGlobal()

  if (num_of_my_component == 0) then
    call error("init_model_process ", "No component name assigned.")
  end if

  call set_global_component(total_comp_name)
  
  call init_my_component_info(total_comp_name)

  deallocate(total_comp_name)

  allocate(my_comp_id(num_of_my_component))
  do p =1, num_of_my_component
    my_comp_id(p) = my_comp(p)%component_id
  end do

  call jml_create_communicator(my_comp_id)

  call set_component_info()

  return

end subroutine init_model_process

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_total_component()
  implicit none

  get_num_of_total_component = num_of_total_component

end function get_num_of_total_component

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=NAME_LEN) function get_component_name(component_id)
  implicit none
  integer, intent(IN) :: component_id
 
  get_component_name = all_comp(component_id)%component_name

end function get_component_name

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_my_component()
  implicit none
  
  get_num_of_my_component = num_of_my_component

end function get_num_of_my_component

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_comp_id_from_name(component_name)
  use jcup_utils, only : error
  implicit none
  character(len=*), intent(IN) :: component_name
  integer :: i

  get_comp_id_from_name = 0

  do i = 1, num_of_total_component
    if (trim(all_comp(i)%component_name)==trim(component_name)) then
      get_comp_id_from_name = all_comp(i)%component_id
      return
    end if
  end do

  call error("get_comp_id_from_name", "no such component name : "//trim(component_name))

end function get_comp_id_from_name
    
!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_my_component_id(component_id)
  implicit none
  integer, intent(IN) :: component_id
  integer :: i

  is_my_component_id = .true.
  do i = 1, num_of_my_component
    if (component_id==my_comp(i)%component_id) then
      return
    end if
  end do

  is_my_component_id = .false.
  
end function is_my_component_id

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_my_component_name(component_name)
  implicit none
  character(len=*), intent(IN) :: component_name
  integer :: comp_id

  comp_id = get_comp_id_from_name(component_name)

  is_my_component_name = is_my_component_id(comp_id)

end function is_my_component_name

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_model_running(model_name)
  implicit none
  character(len=*), intent(IN) :: model_name
  integer :: i

  do i = 1, num_of_total_component
    if (trim(all_comp(i)%component_name)==trim(model_name)) then
      is_model_running=.true.
      return
    end if
  end do

  is_model_running = .false.

end function is_model_running

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_component_relation(my_component_id, target_component_id)
  use jcup_constant, only : COMP_PARALLEL, COMP_SERIAL, COMP_SUPERSET, COMP_SUBSET, COMP_OVERLAP
  implicit none
  integer ,intent(IN) :: my_component_id, target_component_id
  integer :: my_min_pe, my_max_pe
  integer :: target_min_pe, target_max_pe

  my_min_pe = all_comp(my_component_id)%leader_rank
  my_max_pe = my_min_pe + all_comp(my_component_id)%num_of_pe - 1
  target_min_pe = all_comp(target_component_id)%leader_rank
  target_max_pe = target_min_pe + all_comp(target_component_id)%num_of_pe - 1

  if ((my_max_pe < target_min_pe).or.(my_min_pe > target_max_pe)) then
    get_component_relation = COMP_PARALLEL
    return
  end if

  if ((my_min_pe == target_min_pe).and.(my_max_pe == target_max_pe)) then
    get_component_relation = COMP_SERIAL
    return
  end if

  if ((my_min_pe <= target_min_pe).and.(my_max_pe >= target_max_pe)) then
    get_component_relation = COMP_SUPERSET
    return
  end if

  if ((my_min_pe >= target_min_pe).and.(my_max_pe <= target_max_pe)) then
    get_component_relation = COMP_SUBSET
    return
  end if

  get_component_relation = COMP_OVERLAP
  return

end function get_component_relation

!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcup_comp









