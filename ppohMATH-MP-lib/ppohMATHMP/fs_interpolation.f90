!=======+=========+=========+=========+=========+=========+=========+=========+
!>
!! interpolation module
!! 
module fs_interpolation
  implicit none
  private
!--------------------------------   public  ----------------------------------!

  public :: init_interpolation ! subroutine (recv_comp_name, send_comp_name, num_of_mapping_tag)
  public :: set_fs_coef ! subroutine (global_coef, mapping_tag)
  public :: interpolate_seism_to_fistr ! subroutine (mapping_tag, send_data, recv_data)

!--------------------------------  private  ----------------------------------!

  type operation_index_type
    integer :: num_of_operation  ! num of my interpolation operation
    integer, pointer :: my_operation_index(:)   ! index of my operation 
    integer, pointer :: send_data_index(:) ! local send data index of each operation
    integer, pointer :: recv_data_index(:) ! local recv data index of each operation
    integer, pointer :: recv_coef_index(:) ! local recv coef index of each operation
    integer, pointer :: send_coef_index(:) ! local send coef index of each operation

    integer :: num_of_recv_coef ! array size of my local recv coef
    integer :: num_of_send_coef ! array size of my local send coef
    integer, pointer :: global_recv_coef_index(:)      ! global index of local recv coef array
    integer, pointer :: global_send_coef_index(:)      ! global index of local send coef array
    integer :: send_model_id
    integer :: index_tag
    real(kind=8), pointer :: coef(:)
  end type

  type(operation_index_type), pointer :: operation_index(:)
  type(operation_index_type), pointer :: coi ! current operationn index
  
  integer, parameter :: NAME_LEN = 32
  character(len=NAME_LEN) :: RECV_COMP
  character(len=NAME_LEN) :: SEND_COMP

contains

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! define mapping table
subroutine init_interpolation(recv_comp_name, send_comp_name, num_of_mapping_tag)
  use jcup_interface, only : jcup_get_local_operation_index, &
                             jcup_get_num_of_send_grid, jcup_get_num_of_recv_grid
  implicit none
  character(len=*), intent(IN) :: recv_comp_name, send_comp_name
  integer, intent(IN) :: num_of_mapping_tag
  integer :: mapping_tag
  integer :: i

  RECV_COMP = recv_comp_name
  SEND_COMP = send_comp_name

  allocate(operation_index(num_of_mapping_tag))

  do i = 1, num_of_mapping_tag
    mapping_tag = i
    coi => operation_index(i)
    call jcup_get_local_operation_index(recv_comp_name, send_comp_name, mapping_tag, &
                                        coi%num_of_operation, &
                                        coi%my_operation_index, &
                                        coi%send_data_index, &
                                        coi%recv_data_index, &
                                        coi%send_coef_index, &
                                        coi%recv_coef_index)

    call jcup_get_num_of_send_grid(recv_comp_name, send_comp_name, mapping_tag, coi%num_of_send_coef)
    call jcup_get_num_of_recv_grid(recv_comp_name, send_comp_name, mapping_tag, coi%num_of_recv_coef)
    allocate(coi%coef(coi%num_of_operation))
  end do

end subroutine init_interpolation


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! define mapping table
subroutine set_fs_coef(coef_g, mapping_tag)
  use jcup_interface, only : jcup_get_mpi_parameter, &
                             OPERATION_COEF, &
                             jcup_set_local_coef
  implicit none
  real(kind=8), optional, intent(IN) :: coef_g(:) ! global coef
  integer, intent(IN) :: mapping_tag
  real(kind=8) :: coef_buffer(1)
  integer :: my_comm, my_group, my_size, my_rank

  coi => operation_index(mapping_tag)
  
  call jcup_get_mpi_parameter(RECV_COMP, my_comm, my_group, my_size, my_rank)

  if (my_rank==0) then
    call jcup_set_local_coef(RECV_COMP, SEND_COMP, mapping_tag, coef_g, coi%coef, OPERATION_COEF)
  else
    call jcup_set_local_coef(RECV_COMP, SEND_COMP, mapping_tag, coef_buffer, coi%coef, OPERATION_COEF)
  end if

end subroutine set_fs_coef

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! define mapping table
subroutine interpolate_seism_to_fistr(mapping_tag, send_data, recv_data)
  implicit none
  integer, intent(IN) :: mapping_tag
  real(kind=8), intent(IN) :: send_data(:,:)
  real(kind=8), intent(INOUT) :: recv_data(:,:)
  integer :: send_point, recv_point
  integer :: i, j

  recv_data(:,:) = 0.d0

  coi => operation_index(mapping_tag)

  do j =1, size(send_data,2)
    do i = 1, size(coi%send_data_index)
      send_point = coi%send_data_index(i)
      recv_point = coi%recv_data_index(i)

      recv_data(recv_point,j) = recv_data(recv_point,j) + send_data(send_point,j)*coi%coef(i)
    end do
  end do

end subroutine interpolate_seism_to_fistr


end module


