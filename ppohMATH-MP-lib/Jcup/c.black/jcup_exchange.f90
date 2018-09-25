!
!Copyright (c) 2011, arakawa@rist.jp
!All rights reserved.
!
module jcup_exchange
  use jcup_constant, only : NUM_OF_EXCHANGE_DATA, NUM_OF_EXCHANGE_GRID, REAL_DATA, DOUBLE_DATA
  use jcup_constant, only : DATA_2D, DATA_25D, DATA_3D
  use jcup_constant, only : NAME_LEN
  use jcup_config, only : recv_data_conf_type
  use jcup_task, only : task_type
  use jcup_time, only : time_type
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: init_exchange_buffer ! subroutine (NONE)
  public :: set_exchange_table   ! subroutine (NONE)
  public :: get_num_of_send_data ! integer function (task_id)
  public :: get_send_data_id     ! integer function (task_id, data_num)
  public :: get_data_ptr_send    ! type(recv_data_conf_type) function(task_id, data_num)
  public :: get_num_of_recv_data ! integer function (task_id)
  public :: get_recv_data_id     ! integer function (task_id, data_num)
  public :: get_data_ptr_recv    ! type(recv_data_conf_type) function(task_id, data_num)

!--------------------------------   private  ---------------------------------!

  type source_task_type
    integer :: task_id
    integer :: num_of_data
    integer, pointer :: recv_data_id(:)
    type(recv_data_conf_type), pointer :: recv_data_ptr(:)
  end type

  type dest_task_type
    integer :: task_id
    integer :: num_of_data
    integer, pointer :: send_data_id(:)
    type(recv_data_conf_type), pointer :: recv_data_ptr(:)
  end type

  type exchange_table_type
    type(source_task_type), pointer :: st(:) ! source task data
    type(dest_task_type)  , pointer :: dt(:) ! destination task data  
  end type

  type(exchange_table_type) :: et ! exchange control table


  real(kind=4), pointer, private :: buffer_real2d(:,:,:)
  real(kind=8), pointer, private :: buffer_double2d(:,:,:) ! nx, ny, num_of_exchange_data
  real(kind=8), pointer, private :: buffer_double25d(:,:,:) ! nx, ny, num_of_2d_array
  real(kind=8), pointer, private :: buffer_double3d(:,:,:,:) ! nx, ny, nz, num_of_exchange_data


contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_exchanger()

end subroutine init_exchanger

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_exchange_table()
  use jcup_task, only : get_num_of_task, get_my_task_id, get_num_of_component, &
                        get_component_id, is_my_component, get_task_num_from_id
  use jcup_config, only : recv_data_conf_type, get_recv_data_conf_ptr_from_id, &
                          send_data_conf_type, get_send_data_conf_ptr_from_id, &
                          get_num_of_send_data, get_num_of_recv_data
  use jcup_mpi, only : jml_GetMyrankGlobal
  implicit none
  integer :: i, j, k, r
  integer :: counter
  integer, allocatable :: recv_counter(:)
  integer :: task_id, comp_id
  
  type(recv_data_conf_type), pointer :: rd
  type(send_data_conf_type), pointer :: sd

  allocate(et%st(get_num_of_task()), et%dt(get_num_of_task()))

  do i = 1, get_num_of_task()
    et%st(i)%task_id = i
    et%dt(i)%task_id = i
  end do

  do i = 1, get_num_of_task()
    if (i == get_my_task_id()) cycle

    ! count the number of my send data
    counter = 0
    do j = 1, get_num_of_component(i)
      comp_id = get_component_id(i,j)
      do k = 1, get_num_of_recv_data(comp_id)
        rd => get_recv_data_conf_ptr_from_id(comp_id, k)
        if (is_my_component(rd%send_model_id)) then
          counter = counter+1
        end if 
      end do
    end do
   
    et%dt(i)%num_of_data = counter
    allocate(et%dt(i)%send_data_id(counter))

    ! set my send data id
    counter = 0
    do j = 1, get_num_of_component(i)
      comp_id = get_component_id(i,j)
      do k = 1, get_num_of_recv_data(comp_id)
        rd => get_recv_data_conf_ptr_from_id(comp_id, k)
        if (is_my_component(rd%send_model_id)) then
          counter = counter+1
          et%dt(i)%send_data_id(counter) = rd%data_id
          et%dt(i)%recv_data_ptr(counter) = rd
        end if 
      end do
    end do

  end do
  
  ! count the number of my recv data
  allocate(recv_counter(get_num_of_task()))

  i =  get_my_task_id()

  recv_counter(:) = 0
  do j = 1, get_num_of_component(i)
    comp_id = get_component_id(i,j)    
    do k = 1, get_num_of_recv_data(comp_id)
      rd => get_recv_data_conf_ptr_from_id(comp_id, k)
      task_id = get_task_num_from_id(rd%send_model_id)
      recv_counter(task_id) = recv_counter(task_id)+1
    end do
  end do
  
  do i = 1, get_num_of_task()
    et%st(i)%num_of_data = recv_counter(i)
    allocate(et%st(i)%recv_data_id(recv_counter(i)))
    allocate(et%st(i)%recv_data_ptr(recv_counter(i)))
  end do

  i =  get_my_task_id()

  recv_counter(:) = 0
  do j = 1, get_num_of_component(i)
    comp_id = get_component_id(i,j)    
    do k = 1, get_num_of_recv_data(comp_id)
      rd => get_recv_data_conf_ptr_from_id(comp_id, k)
      task_id = get_task_num_from_id(rd%send_model_id)
      recv_counter(task_id) = recv_counter(task_id)+1
      et%st(task_id)%recv_data_id(recv_counter(task_id)) = rd%data_id
      et%st(task_id)%recv_data_ptr(recv_counter(task_id)) = rd
    end do
  end do
  
  !call write_exchange_table(250+jml_GetMyrankGlobal())

end subroutine set_exchange_table

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine write_exchange_table(file_id)
  implicit none
  integer, intent(IN) ::  file_id
  integer :: i, j

  write(file_id,*) "send data id"
  do i = 1, size(et%dt)
    write(file_id,*) "task id = ",i
    do j = 1, size(et%dt(i)%send_data_id)
      write(file_id, *) "data id = ", et%dt(i)%send_data_id(j)
    end do
  end do

  write(file_id, *) "recv data id"
  do i = 1, size(et%st)
    write(file_id, *) "task id = ", i
    do j = 1, size(et%st(i)%recv_data_id)
      write(file_id, *) "data id = ", et%st(i)%recv_data_id(j)
    end do
  end do


  
end subroutine write_exchange_table

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_send_data(task_id)
  implicit none
  integer, intent(IN) :: task_id

  get_num_of_send_data = et%dt(task_id)%num_of_data

end function get_num_of_send_data

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_send_data_id(task_id, data_num)
  implicit none
  integer, intent(IN) :: task_id, data_num

  get_send_data_id = et%dt(task_id)%send_data_id(data_num)

end function get_send_data_id

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_data_ptr_send(task_id, data_num) result(data_ptr)
  implicit none
  integer, intent(IN) :: task_id, data_num
  type(recv_data_conf_type), pointer :: data_ptr

  data_ptr = et%dt(task_id)%recv_data_ptr(data_num)

end function get_data_ptr_send

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_num_of_recv_data(task_id)
  implicit none
  integer, intent(IN) :: task_id

  get_num_of_recv_data = et%st(task_id)%num_of_data

end function get_num_of_recv_data

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_recv_data_id(task_id, data_num)
  implicit none
  integer, intent(IN) :: task_id, data_num

  get_recv_data_id = et%st(task_id)%recv_data_id(data_num)

end function get_recv_data_id

!=======+=========+=========+=========+=========+=========+=========+=========+

function get_data_ptr_recv(task_id, data_num) result(data_ptr)
  implicit none
  integer, intent(IN) :: task_id, data_num
  type(recv_data_conf_type), pointer :: data_ptr

  data_ptr = et%st(task_id)%recv_data_ptr(data_num)

end function get_data_ptr_recv

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_exchange_buffer(max_i_2d, max_j_2d, max_i_3d, max_j_3d, max_k_3d)
  implicit none
  integer, intent(IN) :: max_i_2d, max_j_2d, max_i_3d, max_j_3d, max_k_3d
  allocate(buffer_real2d(max_i_2d, max_j_2d, NUM_OF_EXCHANGE_DATA))
  allocate(buffer_double2d(max_i_2d, max_j_2d, NUM_OF_EXCHANGE_DATA))
  allocate(buffer_double3d(max_i_3d, max_j_3d, max_k_3d, NUM_OF_EXCHANGE_DATA))

end subroutine init_exchange_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcup_exchange
