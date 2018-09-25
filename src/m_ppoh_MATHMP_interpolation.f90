module ppoh_MATHMP_interpolation
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: init_interpolation ! subroutine ()
  public :: set_interpolation  ! subroutine 
  public :: interpolation

!--------------------------------  private  ----------------------------------!
  
  integer, parameter :: NAME_LEN = 32

  type scup_intpl_type
    character(len=NAME_LEN) :: send_comp
    character(len=NAME_LEN) :: send_grid
    character(len=NAME_LEN) :: recv_comp ! my_component
    character(len=NAME_LEN) :: recv_grid ! my_grid 
    integer :: mapping_tag
    integer :: num_of_operation
    integer, pointer :: operation_index(:)
    integer, pointer :: send_data_index(:)
    integer, pointer :: recv_data_index(:)
    integer, pointer :: send_coef_index(:)
    integer, pointer :: recv_coef_index(:)
    integer, pointer :: global_index_of_local_send_coef(:)
    integer, pointer :: global_index_of_local_recv_coef(:)
    real(kind=8), pointer :: coef(:,:) ! size(num_of_operation, num_of_coef)
    real(kind=8), pointer :: coef_vec(:,:) ! size(num_of_operation, 2)
    type(scup_intpl_type), pointer :: next_ptr
  end type


  integer :: num_of_interpolation
  type(scup_intpl_type), pointer :: start_ptr
  type(scup_intpl_type), pointer :: intpl_ptr

contains


!=======+=========+=========+=========+=========+=========+=========+=========+
!>
!! initialize interpolation module
!! \protected
subroutine init_interpolation()
  use jcup_interface, only : jcup_get_num_of_component, jcup_is_my_component
  implicit none
  integer :: i

  num_of_interpolation = 0
  start_ptr => null()
  intpl_ptr => null()

end subroutine init_interpolation

!=======+=========+=========+=========+=========+=========+=========+=========+
!>
!! set interpolation information
!! \protected
subroutine set_interpolation(send_comp, send_grid, recv_comp, recv_grid, mapping_tag, &
                             send_grid_index, recv_grid_index, coef)
  implicit none
  character(len=*), intent(IN) :: send_comp   !< name of send component
  character(len=*), intent(IN) :: send_grid   !< name of send grid
  character(len=*), intent(IN) :: recv_comp   !< name of recv component
  character(len=*), intent(IN) :: recv_grid   !< name of recv grid
  integer, intent(IN)          :: mapping_tag !< tag of remapping table
  integer, optional, intent(IN) :: send_grid_index(:) !< grid index of send grid
  integer, optional, intent(IN) :: recv_grid_index(:) !< grid index of recv grid
  real(kind=8), optional, intent(IN)  :: coef(:,:) !< interpolation coefficient coef(:, num_of_global_operation)
  type(scup_intpl_type), pointer :: before_ptr

  if (.not.associated(start_ptr)) then
    allocate(start_ptr)
    intpl_ptr => start_ptr
  else

    intpl_ptr => start_ptr
    before_ptr => null()

    do while(associated(intpl_ptr))
       before_ptr => intpl_ptr
       intpl_ptr => intpl_ptr%next_ptr
    end do
  
    allocate(intpl_ptr)
    before_ptr%next_ptr => intpl_ptr
  end if
  
  if (present(coef)) then
    call set_intpl(intpl_ptr, send_comp, send_grid, recv_comp, recv_grid, mapping_tag, coef)
  else
    call set_intpl(intpl_ptr, send_comp, send_grid, recv_comp, recv_grid, mapping_tag)
  end if

  intpl_ptr%next_ptr => null()

  num_of_interpolation = num_of_interpolation + 1
  
end subroutine set_interpolation

!=======+=========+=========+=========+=========+=========+=========+=========+
!> set intarpolation
!! \private
subroutine set_intpl(current_ptr, send_comp, send_grid, recv_comp, recv_grid, mapping_tag, coef)
  use jcup_interface, only : jcup_get_local_operation_index, jcup_set_local_coef, OPERATION_COEF
  implicit none
  type(scup_intpl_type), pointer :: current_ptr
  character(len=*), intent(IN) :: send_comp
  character(len=*), intent(IN) :: send_grid
  character(len=*), intent(IN) :: recv_comp
  character(len=*), intent(IN) :: recv_grid
  real(kind=8), optional, intent(IN) :: coef(:,:) ! (num_of_coef, num_of_global_iteration)
  real(kind=8), allocatable :: global_coef(:), local_coef(:)
  integer, intent(IN) :: mapping_tag
  character(len=256) :: log_str
  integer :: i

  current_ptr%send_comp = send_comp
  current_ptr%send_grid = send_grid
  current_ptr%recv_comp = recv_comp
  current_ptr%recv_grid = recv_grid
  current_ptr%mapping_tag = mapping_tag
  current_ptr%next_ptr => null()

  call jcup_get_local_operation_index(recv_comp, send_comp, mapping_tag, &
                                      current_ptr%num_of_operation, current_ptr%operation_index, &
                                      current_ptr%send_data_index, current_ptr%recv_data_index, &
                                      current_ptr%send_coef_index, current_ptr%recv_coef_index) 


  current_ptr%send_comp = send_comp
  current_ptr%send_grid = send_grid
  current_ptr%recv_comp = recv_comp
  current_ptr%recv_grid = recv_grid
  current_ptr%mapping_tag = mapping_tag

  if (present(coef)) then

    allocate(global_coef(size(coef, 2)))
    allocate(current_ptr%coef(current_ptr%num_of_operation, 1))
    if (size(coef, 1) > 1) then ! vector coef
      allocate(current_ptr%coef_vec(current_ptr%num_of_operation, 2))
    else
      current_ptr%coef_vec => null()
    end if

    allocate(local_coef(current_ptr%num_of_operation))

    do i = 1, 1 !num_of_s_coef ! set scalar local coef
      global_coef(:) = coef(i,:)
      call jcup_set_local_coef(recv_comp, send_comp, mapping_tag, global_coef, local_coef, OPERATION_COEF)
      current_ptr%coef(:,i) = local_coef(:)
    end do

    if (size(coef, 1) > 1) then ! set vector coef
      global_coef(:) = coef(1+1,:)
      call jcup_set_local_coef(recv_comp, send_comp, mapping_tag, global_coef, local_coef, OPERATION_COEF)
      current_ptr%coef_vec(:,1) = local_coef(:)
      global_coef(:) = coef(1+2,:)
      call jcup_set_local_coef(recv_comp, send_comp, mapping_tag, global_coef, local_coef, OPERATION_COEF)
      current_ptr%coef_vec(:,2) = local_coef(:)
    end if

    deallocate(global_coef, local_coef)
  else
     current_ptr%coef => null()
     current_ptr%coef_vec => null()
  end if

end subroutine set_intpl

!=======+=========+=========+=========+=========+=========+=========+=========+
!> interpolate 2D scalar value
!! \private
subroutine interpolate_2d_scalar(send_data, recv_data, num_of_operation, send_data_index, recv_data_index, coef)
  implicit none
  real(kind=8), intent(IN) :: send_data(:)
  real(kind=8), intent(INOUT) :: recv_data(:)
  integer, intent(IN)      :: num_of_operation
  integer, intent(IN)      :: send_data_index(:)
  integer, intent(IN)      :: recv_data_index(:)
  real(Kind=8), optional, intent(IN) :: coef(:)
  integer :: i,j, send_grid, recv_grid
 
  if (present(coef)) then
    do i = 1, num_of_operation
       send_grid = send_data_index(i)
       recv_grid = recv_data_index(i)
       recv_data(recv_grid) = recv_data(recv_grid) + send_data(send_grid)*coef(i)
       !write(0,*) "interpolation ", send_grid, send_data(send_grid), recv_grid, recv_data(recv_grid)
    end do
  else
    do i = 1, num_of_operation
       send_grid = send_data_index(i)
       recv_grid = recv_data_index(i)
       recv_data(recv_grid) = recv_data(recv_grid) + send_data(send_grid)
       !write(0,*) "interpolation ", send_grid, send_data(send_grid), recv_grid, recv_data(recv_grid)
    end do
  end if

end subroutine interpolate_2d_scalar

!=======+=========+=========+=========+=========+=========+=========+=========+
!> interpolate 2D vector value
!! \private
subroutine interpolate_2d_vector(send_data1, send_data2, recv_data1, recv_data2, &
                                 num_of_operation, send_data_index, recv_data_index, coef1, coef2)
  implicit none
  real(kind=8), intent(IN) :: send_data1(:)
  real(kind=8), intent(IN) :: send_data2(:)
  real(kind=8), intent(INOUT) :: recv_data1(:)
  real(kind=8), intent(INOUT) :: recv_data2(:)
  integer, intent(IN)      :: num_of_operation
  integer, intent(IN)      :: send_data_index(:)
  integer, intent(IN)      :: recv_data_index(:)
  real(Kind=8), optional, intent(IN) :: coef1(:)
  real(Kind=8), optional, intent(IN) :: coef2(:)
  integer :: i,j, send_grid, recv_grid
 
  if (present(coef1)) then
    do i = 1, num_of_operation
       send_grid = send_data_index(i)
       recv_grid = recv_data_index(i)
       recv_data1(recv_grid) = recv_data1(recv_grid) + send_data1(send_grid)*coef1(i) &
                                                     - send_data2(send_grid)*coef2(i)
       recv_data2(recv_grid) = recv_data2(recv_grid) + send_data1(send_grid)*coef2(i) &
                                                     + send_data2(send_grid)*coef1(i)
    end do
  else
    do i = 1, num_of_operation
       send_grid = send_data_index(i)
       recv_grid = recv_data_index(i)
       recv_data1(recv_grid) = recv_data1(recv_grid) + send_data1(send_grid)
       recv_data2(recv_grid) = recv_data2(recv_grid) + send_data2(send_grid)
    end do
  end if

end subroutine interpolate_2d_vector

!=======+=========+=========+=========+=========+=========+=========+=========+
!> interpolate 3D vector value
!! \private
subroutine interpolate_3d_vector(send_data, recv_data, num_of_data, &
                                 num_of_operation, send_data_index, recv_data_index, coef1, coef2)
  use jcup_interface, only : jcup_error
  implicit none
  real(kind=8), intent(IN)    :: send_data(:,:)
  real(kind=8), intent(INOUT) :: recv_data(:,:)
  integer, intent(IN)      :: num_of_data
  integer, intent(IN)      :: num_of_operation
  integer, intent(IN)      :: send_data_index(:)
  integer, intent(IN)      :: recv_data_index(:)
  real(Kind=8), optional, intent(IN) :: coef1(:)
  real(Kind=8), optional, intent(IN) :: coef2(:)
  integer :: i,j, send_grid, recv_grid
  integer :: k, num_of_vgrid

  !write(6, *) "tcup_interpolate_3d_vector IN ", size(send_data, 1), size(send_data, 2), num_of_data, num_of_operation, send_data(:,1), send_data(:,2)

  num_of_vgrid = num_of_data

  if (mod(num_of_vgrid,2) == 1) then
    call jcup_error("tcup_interpolation, interpolate_vector", "num_of_vgrid error")
  end if
 
  num_of_vgrid = num_of_vgrid/2
  
  if (present(coef1)) then
    do k = 1, num_of_vgrid
      do i = 1, num_of_operation
        send_grid = send_data_index(i)
        recv_grid = recv_data_index(i)

        !write(6, *) "vector interpolation ", recv_grid, send_grid, coef1(i), coef2(i), send_data(send_grid, 1), send_data(send_grid, 2)

        recv_data(recv_grid, k               ) = recv_data(recv_grid, k               ) + send_data(send_grid, k)*coef1(i) &
                                                                                        - send_data(send_grid, k + num_of_vgrid)*coef2(i)
        recv_data(recv_grid, k + num_of_vgrid) = recv_data(recv_grid, k + num_of_vgrid) + send_data(send_grid, k)*coef2(i) &
                                                                                        + send_data(send_grid, k + num_of_vgrid)*coef1(i)
      end do
    end do
  else
    do k = 1, num_of_vgrid
      do i = 1, num_of_operation
         send_grid = send_data_index(i)
         recv_grid = recv_data_index(i)

        !write(6, *) "vector interpolation ", recv_grid, send_grid, send_data(send_grid, 1), send_data(send_grid, 2)

         recv_data(recv_grid, k               ) = recv_data(recv_grid, k               ) + send_data(send_grid, k)
         recv_data(recv_grid, k + num_of_vgrid) = recv_data(recv_grid, k + num_of_vgrid) + send_data(send_grid, k + num_of_vgrid)
      end do
    end do
  end if

  !write(6, *) "tcup_interpolate_3d_vector OUT ", size(recv_data, 1), size(recv_data, 2), num_of_data, recv_data(:,1), recv_data(:,2)

end subroutine interpolate_3d_vector

!=======+=========+=========+=========+=========+=========+=========+=========+
!> interpolate data
!! \protected
subroutine interpolation(recv_model_name, send_model_name, mapping_tag, send_data, & 
                         recv_data, num_of_data, exchange_tag)
  use jcup_interface, only : jcup_error
  implicit none
  character(len=*), intent(IN) :: recv_model_name, send_model_name
  integer, intent(IN) :: mapping_tag
  real(kind=8), intent(IN) :: send_data(:,:)
  real(kind=8), intent(INOUT) :: recv_data(:,:)
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: exchange_tag
  logical :: error_flag
  integer :: send_grid, recv_grid, send_coef, recv_coef
  integer :: i, n


  !write(6, *) "tcup_interpolation start ", trim(send_model_name)//", "//trim(recv_model_name)
  !write(6, *) mapping_tag, exchange_tag, size(send_data,1), size(send_data,2), num_of_data
  !write(6, *) send_data(:,1)

  recv_data(:,:) = 0.d0

  error_flag = .true.

  intpl_ptr => start_ptr

  do while(associated(intpl_ptr))
    if ((trim(intpl_ptr%send_comp) == trim(send_model_name)).and.(trim(intpl_ptr%recv_comp) == trim(recv_model_name))) then
      if (intpl_ptr%mapping_tag == mapping_tag) then
        error_flag = .false.
        exit
      end if
    end if
    intpl_ptr=> intpl_ptr%next_ptr
  end do

  if (error_flag) then
    write(0,*)             "interpolation error!!! send_model="//trim(send_model_name)//", recv_model="//trim(recv_model_name)
    write(0,'(A,I5,A,I5)') "                        mapping_tag=",mapping_tag,", num_of_interpolation=",num_of_interpolation

    write(0,*) associated(start_ptr)

    intpl_ptr => start_ptr
    do while(associated(intpl_ptr))
       write(0,*) "               send_comp="//trim(intpl_ptr%send_comp)//", recv_comp="//trim(intpl_ptr%recv_comp)// &
                ", mapping_tag=", intpl_ptr%mapping_tag
       intpl_ptr => intpl_ptr%next_ptr
    end do

    stop
  end if


  !!!!!!!!!!!!!!!!!!!!!!!!!!! exchange_tagが100より大ならvectorデータ
  if (exchange_tag > 100) then ! vector data
      if (associated(intpl_ptr%coef)) then
        call interpolate_3d_vector(send_data, recv_data, num_of_data, &
                                   intpl_ptr%num_of_operation, intpl_ptr%send_data_index, intpl_ptr%recv_data_index, &
                                   intpl_ptr%coef_vec(:,1), intpl_ptr%coef_vec(:,2))
    
      else
        call interpolate_3d_vector(send_data, recv_data, num_of_data, &
                                 intpl_ptr%num_of_operation, intpl_ptr%send_data_index, intpl_ptr%recv_data_index)
      end if

  else ! scalar data

    select case(num_of_data)
    case (1) ! scalar data
   
      if (associated(intpl_ptr%coef)) then
        call interpolate_2d_scalar(send_data(:,1), recv_data(:,1), &
                                   intpl_ptr%num_of_operation, intpl_ptr%send_data_index, intpl_ptr%recv_data_index, &
                                   intpl_ptr%coef(:,1))
      else
        call interpolate_2d_scalar(send_data(:,1), recv_data(:,1), &
                                 intpl_ptr%num_of_operation, intpl_ptr%send_data_index, intpl_ptr%recv_data_index)
      end if

    case default ! multi data
      if (associated(intpl_ptr%coef)) then
        do n = 1, num_of_data
          do i = 1, intpl_ptr%num_of_operation
             send_grid = intpl_ptr%send_data_index(i)
             recv_grid = intpl_ptr%recv_data_index(i)
             send_coef = intpl_ptr%send_coef_index(i)
             recv_coef = intpl_ptr%recv_coef_index(i)
             recv_data(recv_grid, n) = recv_data(recv_grid, n) + send_data(send_grid, n)*intpl_ptr%coef(i, 1) 
             !write(6, *) n, i, recv_data(recv_grid, n), send_data(send_grid, n)
          end do
        end do
      else
        do n = 1, num_of_data
          do i = 1, intpl_ptr%num_of_operation
             send_grid = intpl_ptr%send_data_index(i)
             recv_grid = intpl_ptr%recv_data_index(i)
             send_coef = intpl_ptr%send_coef_index(i)
             recv_coef = intpl_ptr%recv_coef_index(i)
             recv_data(recv_grid, n) = recv_data(recv_grid, n) + send_data(send_grid, n)
             !write(6, *) n, i, recv_data(recv_grid, n), send_data(send_grid, n)
          end do
        end do
      end if
    end select
  end if

end subroutine interpolation

!=======+=========+=========+=========+=========+=========+=========+=========+

end module ppoh_MATHMP_interpolation

