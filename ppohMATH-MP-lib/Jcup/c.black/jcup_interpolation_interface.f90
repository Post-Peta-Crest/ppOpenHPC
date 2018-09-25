!
!Copyright (c) 2011, arakawa@rist.jp
!All rights reserved.
!
module jcup_interpolation_interface
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: init_interpolation_interface
  public :: get_local_operation_index
  public :: get_send_grid_index ! subroutine (recv_model_num, send_model_num, grid_tag, num_of_grid, grid_index)
  public :: get_recv_grid_index ! subroutine (recv_model_num, send_model_num, grid_tag, num_of_grid, grid_index)
  public :: get_num_of_send_grid
  public :: get_num_of_recv_grid
  public :: send_array_to_recv_model   ! subroutine (my_comp_name, recv_comp_name, array)
  public :: recv_array_from_send_model ! subroutine (my_comp_name, send_comp_name, array)
  public :: send_coef_to_recv_model
  public :: recv_coef_from_send_model
  public :: set_local_coef ! subroutine (my_comp_name, global_coef, local_coef, coef_type)

  integer, parameter, public :: OPERATION_COEF = 0
  integer, parameter, public :: SEND_COEF = 1
  integer, parameter, public :: RECV_COEF = 2

!--------------------------------   private  ---------------------------------!

  interface recv_coef_from_send_model
    module procedure recv_coef_from_send_model_n, recv_coef_from_send_model_base
  end interface

  interface set_local_coef
    module procedure set_local_coef_n, set_local_coef_base
  end interface

  interface send_array_to_recv_model
    module procedure send_array_to_recv_model_int
    module procedure send_array_to_recv_model_real
    module procedure send_array_to_recv_model_dbl
  end interface

  interface recv_array_from_send_model
    module procedure recv_array_from_send_model_int
    module procedure recv_array_from_send_model_real
    module procedure recv_array_from_send_model_dbl
  end interface

  integer, private :: my_model
  !integer, private :: num_of_send_grid
  !integer, private :: num_of_recv_grid
  !integer, pointer, private :: global_index_of_local_operation(:)
  !integer, pointer, private :: global_index_of_local_send_data(:)
  !integer, pointer, private :: global_index_of_local_recv_data(:)
  !integer, pointer, private :: global_index_of_local_send_coef(:) 
  !integer, pointer, private :: global_index_of_local_recv_coef(:)

contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_interpolation_interface(my_model_id)
  implicit none
  integer, intent(IN) :: my_model_id

  my_model = my_model_id

end subroutine init_interpolation_interface

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_local_operation_index(recv_model_name, send_model_name, grid_tag, &
                                     num_of_operation, operation_index, &
                                     send_data_index, recv_data_index, &
                                     send_coef_index, recv_coef_index)
  use jcup_grid, only : get_interpolation_index, &
                        get_send_grid_index, get_recv_grid_index
  implicit none
  character(len=*), intent(IN) :: recv_model_name, send_model_name
  integer, optional, intent(IN) :: grid_tag
  integer, intent(INOUT) :: num_of_operation
  integer, pointer :: operation_index(:)
  integer, pointer :: send_data_index(:)
  integer, pointer :: recv_data_index(:)
  integer, pointer :: send_coef_index(:)
  integer, pointer :: recv_coef_index(:)

  integer :: num_of_grid
  integer :: grid_num

  if (present(grid_tag)) then
    grid_num = grid_tag
  else
    grid_num = 1
  end if

  call get_interpolation_index(recv_model_name, send_model_name, grid_num, num_of_operation, operation_index, &
                               send_data_index, recv_data_index, send_coef_index, recv_coef_index)

  !global_index_of_local_operation => operation_index

  !call get_send_grid_index(recv_model_name, send_model_name, grid_num, num_of_grid, global_index_of_local_send_coef)
  !call get_recv_grid_index(recv_model_name, send_model_name, grid_num, num_of_grid, global_index_of_local_recv_coef)

end subroutine get_local_operation_index

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_send_grid_index(recv_model_name, send_model_name, grid_tag, num_of_grid, grid_index)
  use jcup_grid, only : get_send_grid => get_send_grid_index
  implicit none
  character(len=*), intent(IN) :: recv_model_name, send_model_name
  integer, optional, intent(IN) :: grid_tag
  integer, intent(INOUT) :: num_of_grid
  integer, pointer :: grid_index(:)
  integer :: grid_num

  if (present(grid_tag)) then
    grid_num = grid_tag
  else
    grid_num = 1
  end if

  call get_send_grid(recv_model_name, send_model_name, grid_num, num_of_grid, grid_index)

end subroutine get_send_grid_index

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_recv_grid_index(recv_model_name, send_model_name, grid_tag, num_of_grid, grid_index)
  use jcup_grid, only : get_recv_grid => get_recv_grid_index
  implicit none
  character(len=*), intent(IN) :: recv_model_name, send_model_name
  integer, optional, intent(IN) :: grid_tag
  integer, intent(INOUT) :: num_of_grid
  integer, pointer :: grid_index(:)
  integer :: grid_num

  if (present(grid_tag)) then
    grid_num = grid_tag
  else
    grid_num = 1
  end if

  call get_recv_grid(recv_model_name, send_model_name, grid_num, num_of_grid, grid_index)

end subroutine get_recv_grid_index

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_num_of_send_grid(recv_model_name, send_model_name, grid_tag, num_of_grid)
  implicit none
  character(len=*), intent(IN) :: recv_model_name, send_model_name
  integer, optional, intent(IN) :: grid_tag
  integer, intent(INOUT) :: num_of_grid

  integer, pointer :: dummy_array(:)
  integer :: grid_num

  if (present(grid_tag)) then
    grid_num = grid_tag
  else
    grid_num = 1
  end if

  call get_send_grid_index(recv_model_name, send_model_name, grid_num, num_of_grid, dummy_array)

end subroutine get_num_of_send_grid

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine get_num_of_recv_grid(recv_model_name, send_model_name, grid_tag, num_of_grid)
  implicit none
  character(len=*), intent(IN) :: recv_model_name, send_model_name
  integer, optional, intent(IN) :: grid_tag
  integer, intent(INOUT) :: num_of_grid

  integer, pointer :: dummy_array(:)
  integer :: grid_num

  if (present(grid_tag)) then
    grid_num = grid_tag
  else
    grid_num = 1
  end if

  call get_recv_grid_index(recv_model_name, send_model_name, grid_num, num_of_grid, dummy_array)

end subroutine get_num_of_recv_grid

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine send_array_to_recv_model_int(my_comp_name, recv_comp_name, array)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_SendLeader
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: my_comp_name, recv_comp_name
  integer, intent(IN) :: array(:)
  integer :: recv_model
  integer :: array_size(1)

  my_model = get_comp_id_from_name(my_comp_name)
  recv_model = get_comp_id_from_name(recv_comp_name)

  if (jml_isLocalLeader(my_model)) then
    call jml_SendLeader(array, 1, size(array), recv_model-1) 
  end if

end subroutine send_array_to_recv_model_int

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine recv_array_from_send_model_int(my_comp_name, send_comp_name, array)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_RecvLeader, jml_BcastLocal
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: my_comp_name, send_comp_name
  integer, intent(INOUT) :: array(:)
  integer  :: send_model

  my_model = get_comp_id_from_name(my_comp_name)
  send_model = get_comp_id_from_name(send_comp_name)

  if (jml_isLocalLeader(my_model)) then
    call jml_RecvLeader(array, 1, size(array), send_model-1)
  end if

  call jml_BcastLocal(my_model, array, 1, size(array))

end subroutine recv_array_from_send_model_int

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine send_array_to_recv_model_real(my_comp_name, recv_comp_name, array)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_SendLeader
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: my_comp_name, recv_comp_name
  real(kind=4), intent(IN) :: array(:)
  integer :: recv_model
  integer :: array_size(1)

  my_model = get_comp_id_from_name(my_comp_name)
  recv_model = get_comp_id_from_name(recv_comp_name)

  if (jml_isLocalLeader(my_model)) then
    call jml_SendLeader(array, 1, size(array), recv_model-1) 
  end if

end subroutine send_array_to_recv_model_real

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine recv_array_from_send_model_real(my_comp_name, send_comp_name, array)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_RecvLeader, jml_BcastLocal
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: my_comp_name, send_comp_name
  real(kind=4), intent(INOUT) :: array(:)
  integer :: send_model

  my_model = get_comp_id_from_name(my_comp_name)
  send_model = get_comp_id_from_name(send_comp_name)

  if (jml_isLocalLeader(my_model)) then
    call jml_RecvLeader(array, 1, size(array), send_model-1)
  end if

  call jml_BcastLocal(my_model, array, 1, size(array))

end subroutine recv_array_from_send_model_real

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine send_array_to_recv_model_dbl(my_comp_name, recv_comp_name, array)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_SendLeader
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: my_comp_name, recv_comp_name
  real(kind=8), intent(IN) :: array(:)
  integer :: recv_model
  integer :: array_size(1)

  my_model = get_comp_id_from_name(my_comp_name)
  recv_model = get_comp_id_from_name(recv_comp_name)

  if (jml_isLocalLeader(my_model)) then
    call jml_SendLeader(array, 1, size(array), recv_model-1) 
  end if

end subroutine send_array_to_recv_model_dbl

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine recv_array_from_send_model_dbl(my_comp_name, send_comp_name, array)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_RecvLeader, jml_BcastLocal
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: my_comp_name, send_comp_name
  real(kind=8), intent(INOUT) :: array(:)
  integer  :: send_model

  my_model = get_comp_id_from_name(my_comp_name)
  send_model = get_comp_id_from_name(send_comp_name)

  if (jml_isLocalLeader(my_model)) then
    call jml_RecvLeader(array, 1, size(array), send_model-1)
  end if

  call jml_BcastLocal(my_model, array, 1, size(array))

end subroutine recv_array_from_send_model_dbl

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine send_coef_to_recv_model(my_comp_name, recv_comp_name, coef)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_SendLeader
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: my_comp_name, recv_comp_name
  real(kind=8), intent(IN) :: coef(:)
  integer :: recv_model
  integer :: array_size(1)

  my_model = get_comp_id_from_name(my_comp_name)
  recv_model = get_comp_id_from_name(recv_comp_name)

  if (jml_isLocalLeader(my_model)) then
    array_size(1) = size(coef)
    call jml_SendLeader(array_size, 1, 1, recv_model-1)
    call jml_SendLeader(coef, 1, array_size(1), recv_model-1) 
  end if

end subroutine send_coef_to_recv_model

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine recv_coef_from_send_model_n(my_comp_name, send_comp_name,  mapping_tag, coef, coef_type)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_RecvLeader
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: my_comp_name, send_comp_name
  integer, intent(IN) :: mapping_tag
  real(kind=8), intent(INOUT) :: coef(:)
  integer, intent(IN) :: coef_type
  integer :: send_model
  integer :: size_of_global_coef
  real(kind=8), allocatable :: coef_buffer(:)

  integer :: array_size(1) 

  my_model = get_comp_id_from_name(my_comp_name)
  send_model = get_comp_id_from_name(send_comp_name)

  if (jml_isLocalLeader(my_model)) then
    call jml_RecvLeader(array_size, 1, 1, send_model-1)
    size_of_global_coef = array_size(1)
    allocate(coef_buffer(size_of_global_coef))
    call jml_RecvLeader(coef_buffer, 1, size_of_global_coef, send_model-1)
  else
    allocate(coef_buffer(1))
  end if

  call set_local_coef_n(my_comp_name, send_comp_name, mapping_tag, coef_buffer, coef, coef_type)
  deallocate(coef_buffer)

end subroutine recv_coef_from_send_model_n

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine recv_coef_from_send_model_base(my_comp_name, send_comp_name, local_coef_index, coef)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_RecvLeader
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: my_comp_name, send_comp_name
  integer, intent(IN)  :: local_coef_index(:)
  real(kind=8), intent(INOUT) :: coef(:)

  integer :: size_of_global_coef
  real(kind=8), allocatable :: coef_buffer(:)
  integer :: send_model
  integer :: array_size(1) 

  my_model = get_comp_id_from_name(my_comp_name)
  send_model = get_comp_id_from_name(send_comp_name)

  if (jml_isLocalLeader(my_model)) then
    call jml_RecvLeader(array_size, 1, 1, send_model-1)
    size_of_global_coef = array_size(1)
    allocate(coef_buffer(size_of_global_coef))
    call jml_RecvLeader(coef_buffer, 1, size_of_global_coef, send_model-1)
  else
    allocate(coef_buffer(1))
  end if

  call set_local_coef_base(my_comp_name, coef_buffer, local_coef_index, coef)
  deallocate(coef_buffer)

end subroutine recv_coef_from_send_model_base

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_local_coefficients(my_comp_name, global_coef, local_coef_index, local_coef)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_GetCommSizeLocal, &
                           jml_ReduceMaxLocal, jml_SendLocal, jml_RecvLocal
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: my_comp_name
  real(kind=8), intent(IN) :: global_coef(:)
  integer     , intent(IN) :: local_coef_index(:)
  real(kind=8), intent(INOUT) :: local_coef(:)
  integer :: max_local_array_size
  real(kind=8), allocatable :: local_array_buffer(:)
  integer, allocatable :: local_array_index_buffer(:)
  integer :: local_array_size
  integer :: int_buffer(1)
  integer :: pe
 
  my_model = get_comp_id_from_name(my_comp_name)

  if (jml_isLocalLeader(my_model)) then
    
    local_array_size = size(local_coef)

    call global_to_local(global_coef, size(local_coef), local_coef_index, local_coef)
    
    int_buffer(1) = local_array_size
    call jml_ReduceMaxLocal(my_model, int_buffer(1), max_local_array_size)

    allocate(local_array_buffer(max_local_array_size))
    allocate(local_array_index_buffer(max_local_array_size))

    do pe = 2, jml_GetCommSizeLocal(my_model)
      call jml_RecvLocal(my_model, int_buffer,1,1, pe-1)
      local_array_size = int_buffer(1)
      call jml_RecvLocal(my_model, local_array_index_buffer, 1, local_array_size, pe-1)
      call global_to_local(global_coef, local_array_size, local_array_index_buffer, local_array_buffer)
      call jml_SendLocal(my_model, local_array_buffer, 1, local_array_size, pe-1)
    end do

    deallocate(local_array_index_buffer)
    deallocate(local_array_buffer)

  else
    int_buffer(1) = size(local_coef)
    call jml_ReduceMaxLocal(my_model, int_buffer(1), max_local_array_size)

    call jml_SendLocal(my_model, int_buffer, 1, 1, 0)
    call jml_SendLocal(my_model, local_coef_index, 1, size(local_coef), 0)    
    call jml_RecvLocal(my_model, local_coef, 1, size(local_coef), 0)
  end if

end subroutine set_local_coefficients

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_local_coef_n(my_comp_name, send_comp_name, mapping_tag, global_coef, local_coef, coef_type)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_GetCommSizeLocal, jml_GatherLocal, jml_GatherVLocal, &
                           jml_ScatterVLocal
  use jcup_grid, only : get_operation_index, get_send_grid_index, get_recv_grid_index
  implicit none
  character(len=*), intent(IN) :: my_comp_name
  character(len=*), intent(IN) :: send_comp_name
  integer, intent(IN) :: mapping_tag
  real(kind=8), intent(IN)    :: global_coef(:)
  real(kind=8), intent(INOUT) :: local_coef(:)
  integer, intent(IN) :: coef_type

  integer, pointer :: local_coef_index(:)
  integer :: num_of_index
 
  select case(coef_type)
    case(OPERATION_COEF)
      call get_operation_index(my_comp_name, send_comp_name, mapping_tag, num_of_index, local_coef_index)
    case(SEND_COEF)
      call get_send_grid_index(my_comp_name, send_comp_name, mapping_tag, num_of_index, local_coef_index)
    case(RECV_COEF)
      call get_recv_grid_index(my_comp_name, send_comp_name, mapping_tag, num_of_index, local_coef_index)
  end select

  call set_local_coef_base(my_comp_name, global_coef, local_coef_index, local_coef)

end subroutine set_local_coef_n

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_local_coef_base(my_comp_name, global_coef, local_coef_index, local_coef)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_GetCommSizeLocal, jml_GatherLocal, jml_GatherVLocal, &
                           jml_ScatterVLocal
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: my_comp_name
  real(kind=8), intent(IN)    :: global_coef(:)
  integer     , intent(IN)    :: local_coef_index(:)
  real(kind=8), intent(INOUT) :: local_coef(:)
  integer :: global_array_size
  real(kind=8), allocatable :: array_buffer(:)
  integer, allocatable :: array_index_buffer(:)
  integer, allocatable :: array_size(:)
  integer, allocatable :: offset(:)
  integer :: int_buffer(1)
  integer :: pe, i
  
  my_model = get_comp_id_from_name(my_comp_name)

  if (jml_isLocalLeader(my_model)) then

    allocate(array_size(jml_GetCommSizeLocal(my_model)))
    allocate(offset(jml_GetCommSizeLocal(my_model)))
    int_buffer(1) = size(local_coef)
    call jml_GatherLocal(my_model, int_buffer,1,1,array_size)

    offset(:) = 0
    do i = 2, size(offset)
      offset(i) = offset(i-1)+array_size(i-1)
    end do 

    global_array_size = offset(size(offset))+array_size(size(offset))

    allocate(array_buffer(global_array_size))
    allocate(array_index_buffer(global_array_size))

    call jml_GatherVlocal(my_model, local_coef_index, int_buffer(1), array_index_buffer, array_size, offset) 

    do i = 1, global_array_size
      array_buffer(i) = global_coef(array_index_buffer(i)) 
    end do

    call jml_ScatterVLocal(my_model, array_buffer, array_size, offset, local_coef, int_buffer(1))

    deallocate(array_index_buffer)
    deallocate(array_buffer)
    deallocate(array_size)
    deallocate(offset)

  else

    int_buffer(1) = size(local_coef)
    allocate(array_size(1), offset(1))
    allocate(array_buffer(1), array_index_buffer(1))    
    call jml_GatherLocal(my_model, int_buffer,1,1,array_size)
    call jml_GatherVLocal(my_model, local_coef_index, int_buffer(1), array_index_buffer, array_size, offset)
    call jml_ScatterVLocal(my_model, array_buffer, array_size, offset, local_coef, int_buffer(1))
    deallocate(array_buffer, array_index_buffer, array_size, offset)

  end if

end subroutine set_local_coef_base

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine global_to_local(global_array, local_array_size, local_array_index, local_array)
  implicit none
  real(kind=8), intent(IN) :: global_array(:)
  integer,      intent(IN) :: local_array_size
  integer,      intent(IN) :: local_array_index(:)
  real(kind=8), intent(INOUT) :: local_array(:)

  integer :: i

  do i = 1, local_array_size
    local_array(i) = global_array(local_array_index(i))
  end do

end subroutine global_to_local

!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcup_interpolation_interface
