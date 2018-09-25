!=======+=========+=========+=========+=========+=========+=========+=========+

module jcup_interpolation_sample
  implicit none
 
!--------------------------------   public  ----------------------------------!
 
  public :: init_interpolation
  public :: set_operation_index
  public :: finalize_interpolation

!--------------------------------   private  ---------------------------------!

!  integer, parameter, private :: NO_GRID = -999999999
  integer, parameter, private :: NO_GRID = 0
  integer, parameter, private :: LARGE_VALUE = 999999999

  integer, parameter, private :: AGCM = 1
  integer, parameter, private :: OGCM = 2

  integer, private :: my_model

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
  end type

  type(operation_index_type), pointer :: operation_index(:,:,:)
  type(operation_index_type), pointer :: coi ! current operation index

contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_interpolation(num_of_model, num_of_mapping_tag, my_model_id)
  implicit none
  integer, intent(IN) :: num_of_model
  integer, intent(IN) :: num_of_mapping_tag
  integer, intent(IN) :: my_model_id

  allocate(operation_index(num_of_model, num_of_model, num_of_mapping_tag))

  my_model = my_model_id

end subroutine init_interpolation

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine finalize_interpolation()
  implicit none

end subroutine finalize_interpolation


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_operation_index(recv_model_name, send_model_name, mapping_tag)
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  use jcup_interface, only : jcup_get_model_id, jcup_get_local_operation_index, &
                             jcup_get_num_of_send_grid, jcup_get_num_of_recv_grid
  implicit none
  character(len=*), intent(IN) :: recv_model_name, send_model_name
  integer, optional, intent(IN) :: mapping_tag
  integer :: recv_model_id, send_model_id
  integer :: grid_tag

  if (present(mapping_tag)) then
    grid_tag = mapping_tag
  else
    grid_tag = 1
  end if
  
  call jcup_get_model_id(recv_model_name, recv_model_id)
  call jcup_get_model_id(send_model_name, send_model_id)

  coi => operation_index(recv_model_id, send_model_id, grid_tag)

  coi%send_model_id = send_model_id
  coi%index_tag = grid_tag

  call jcup_get_local_operation_index(recv_model_name, send_model_name, grid_tag, &
                                      coi%num_of_operation, &
                                      coi%my_operation_index, &
                                      coi%send_data_index, &
                                      coi%recv_data_index, &
                                      coi%send_coef_index, &
                                      coi%recv_coef_index)

  if ((recv_model_id==2).and.(send_model_id==1)) write(700,*) "set operation index ", recv_model_id, send_model_id, size(coi%send_data_index)

  call jcup_get_num_of_send_grid(recv_model_name, send_model_name, grid_tag, coi%num_of_send_coef)
  call jcup_get_num_of_recv_grid(recv_model_name, send_model_name, grid_tag, coi%num_of_recv_coef)

  
end subroutine set_operation_index

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine interpolate_data_sample(recv_model, send_model, send_data, & 
                                   recv_data, num_of_data, grid_num, exchange_tag)
  use jcup_interface, only : jcup_get_comp_num_from_name
  implicit none
  character(len=*), intent(IN) :: recv_model, send_model
  real(kind=8), intent(IN) :: send_data(:,:)
  real(kind=8), intent(INOUT) :: recv_data(:,:)
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: grid_num
  integer, intent(IN) :: exchange_tag(:)

  integer :: i, d, j, ri
  integer :: send_point, recv_point, operation_coef, send_coef, recv_coef
  real(kind=8) :: fatm, aatm 
  real(kind=8) :: aocn
  real(kind=8) :: ufact, vfact
  real(kind=8) :: temp
  integer :: counter


    coi => operation_index(jcup_get_comp_num_from_name(recv_model), jcup_get_comp_num_from_name(send_model),grid_num)
    !write(0,*) "interpolation data test ", send_model, recv_model, grid_num, num_of_data, &
    !            size(operation_index(recv_model, send_model, grid_num)%send_data_index), & 
    !            send_data(1,1), send_data(2,1)

    do d = 1, num_of_data
      do i = 1, size(coi%send_data_index)
        send_point = coi%send_data_index(i)
        recv_point = coi%recv_data_index(i) 
        recv_data(recv_point,d) = send_data(send_point,d)
      end do
    end do

    return

end subroutine interpolate_data_sample

!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcup_interpolation_sample


!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine interpolate_data(recv_model, send_model, mapping_tag, sn1, sn2, send_data, & 
                            rn1, rn2, recv_data, num_of_data, tn, exchange_tag)
  use jcup_interpolation_sample, only : interpolate_data_sample
  implicit none
  character(len=*), intent(IN) :: recv_model, send_model
  integer, intent(IN) :: mapping_tag
  integer, intent(IN) :: sn1, sn2
  real(kind=8), intent(IN) :: send_data(sn1,sn2)
  integer, intent(IN) :: rn1, rn2
  real(kind=8), intent(INOUT) :: recv_data(rn1,rn2)
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: tn
  integer, intent(IN) :: exchange_tag(tn)

  call interpolate_data_sample(recv_model, send_model, send_data, recv_data, num_of_data, mapping_tag, exchange_tag)

end subroutine interpolate_data

!=======+=========+=========+=========+=========+=========+=========+=========+
