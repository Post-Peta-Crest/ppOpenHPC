module mod_ice
use jcup_interface
use field_common
use component_field, only : component_field_type
private

public :: ice_init_grid
public :: ice_init_data
public :: ice_set_grid_mapping
public :: ice_put_initial_data
public :: ice_run
public :: ice_fin

integer :: DIV_X
integer :: DIV_Y
integer :: my_comm, my_group, my_rank, my_size

type(component_field_type) :: field


integer :: itime(6)
integer :: delta_t
integer :: step_counter
integer :: file_id = 45

contains

!======================================================================================================

subroutine ice_init_grid(is_restart)
  use jcup_interpolation_sample
  use field_def, only : init_field_def, set_field_def, cal_mn, get_local_field, cal_grid_index
  use component_field, only : init_field, init_field_data
  implicit none
  logical, intent(IN) :: is_restart
  integer, allocatable :: grid_index(:)
  integer :: lis, lie, ljs, lje
  integer :: i
  integer :: comp_id(2)


  call jcup_get_mpi_parameter(ICE, my_comm, my_group, my_size, my_rank)

  call cal_mn(my_size, DIV_X, DIV_Y)

  call set_field_def(component_name = ICE, grid_name = ICE_GRID, &
                     g_nx = GNXI, g_ny = GNYI, g_nz = 1, hallo = 2, div_x = DIV_X, div_y = DIV_Y)
  call get_local_field(component_name = ICE, grid_name = ICE_GRID, &
                       local_is = lis, local_ie = lie, local_js = ljs, local_je = lje)

  call init_field(field, ICE_GRID, lis, lie, ljs, lje, 1, 1)

  call cal_grid_index(ICE,ICE_GRID, field%grid_index)
  call jcup_def_grid(field%grid_index, ICE, ICE_GRID)

  step_counter = 0

  if (is_restart) then
    open(unit = file_id, file = trim(ICE)//".mst", form = "formatted", status = "old", err= 200)
    read(file_id, *) step_counter
    close(file_id)
  end if

  return

200 continue

  write(0,*) "file "//trim(ice)//".mst open error"
  stop

end subroutine ice_init_grid

!======================================================================================================

!======================================================================================================

subroutine ice_init_data()
  use jcup_mpi_lib
  use jcup_interpolation_sample 
  use field_def, only : init_field_def, set_field_def, cal_mn, get_local_field, cal_grid_index
  use component_field, only : init_field, init_field_data
  implicit none
  integer, allocatable :: grid_index(:)
  integer :: lis, lie, ljs, lje
  integer :: i
  integer :: comp_id(2)


  call init_field_data(field, 1, 2, 3)

  call jcup_def_varp(field%varp(1)%varp_ptr, ICE,"ice_1", ICE_GRID)
  call jcup_def_varp(field%varp(2)%varp_ptr, ICE,"io_1", ICE_GRID)
  call jcup_def_varg(field%varg(1)%varg_ptr, ICE, "oi_1", ICE_GRID, &
                     SEND_MODEL_NAME = OCN, SEND_DATA_NAME = "oi_1", &
                     RECV_MODE = "SNP", INTERVAL = 180, TIME_LAG = 0, MAPPING_TAG = 1, EXCHANGE_TAG = 1)
  call jcup_def_varg(field%varg(2)%varg_ptr, ICE, "ai_1", ICE_GRID, &
                     SEND_MODEL_NAME = ATM, SEND_DATA_NAME = "a_2d_1", &
                     RECV_MODE = "SNP", INTERVAL = 60, TIME_LAG = -1, MAPPING_TAG = 1, EXCHANGE_TAG = 1)
  call jcup_def_varg(field%varg(3)%varg_ptr, ICE, "li_1", ICE_GRID, &
                     SEND_MODEL_NAME = LAND, SEND_DATA_NAME = "land_1", &
                     RECV_MODE = "SNP", INTERVAL = 60, TIME_LAG = -1, MAPPING_TAG = 1, EXCHANGE_TAG = 1)

end subroutine ice_init_data

!======================================================================================================

subroutine ice_set_grid_mapping()
  use jcup_interface
  use jcup_interpolation_sample
  implicit none

  call set_operation_index(ICE,"ocn",1)

  call jcup_set_mapping_table(ICE, LAND, LAND_GRID, ICE, ICE_GRID, 1)
  call jcup_set_mapping_table(ICE, ICE, ICE_GRID, LAND, LAND_GRID, 1)
  call set_operation_index(ICE,LAND,1)

end subroutine ice_set_grid_mapping

!======================================================================================================

subroutine ice_put_initial_data(is_restart)
  use jcup_interpolation_sample
  use mod_config, only : get_start_time
  implicit none
  logical, intent(IN) :: is_restart

  itime = get_start_time()

  delta_t = 30

  !call jcup_init_time(itime)

  if (.not.is_restart) call set_and_put_data(0)

end subroutine ice_put_initial_data

!======================================================================================================

subroutine set_and_put_data(step)
  use field_def, only : set_send_data_2d
  implicit none
  integer, intent(IN) :: step
  integer :: k

  call set_send_data_2d(ICE,ICE_GRID, field%send_2d(:,:), step, 4)
  call jcup_put_data(field%varp(1)%varp_ptr, pack(field%send_2d, MASK = field%mask2d))
  call jcup_put_data(field%varp(2)%varp_ptr, pack(field%send_2d, MASK = field%mask2d))

end subroutine set_and_put_data

!======================================================================================================

subroutine get_and_write_data()
  use field_def, only : write_data_2d
  implicit none
  integer :: k

  goto 8000

  field%buffer1d(:) = 0.d0
  field%recv_2d(:,:) = 0.d0

  call jcup_get_data(field%varg(1)%varg_ptr, field%buffer1d)
  field%recv_2d(:,:) = unpack(field%buffer1d, field%mask2d, field%recv_2d)
  call write_data_2d(ICE,ICE_GRID, "oi_1", field%recv_2d)

  field%buffer1d(:) = 0.d0
  field%recv_2d(:,:) = 0.d0

  call jcup_get_data(field%varg(2)%varg_ptr, field%buffer1d)
  field%recv_2d(:,:) = unpack(field%buffer1d, field%mask2d, field%recv_2d)
  call write_data_2d(ICE,ICE_GRID, "li_1", field%recv_2d)

  return


8000 continue


  field%buffer1d(:) = 0.d0
  field%recv_2d(:,:) = 0.d0

  call jcup_get_data(field%varg(1)%varg_ptr, field%buffer1d)
  field%recv_2d(:,:) = unpack(field%buffer1d, field%mask2d, field%recv_2d)
  call write_data_2d(ICE,ICE_GRID, "oi_1", field%recv_2d)

  field%buffer1d(:) = 0.d0
  field%recv_2d(:,:) = 0.d0

  call jcup_get_data(field%varg(2)%varg_ptr, field%buffer1d)
  field%recv_2d(:,:) = unpack(field%buffer1d, field%mask2d, field%recv_2d)
  call write_data_2d(ICE,ICE_GRID, "ai_1", field%recv_2d)

  field%buffer1d(:) = 0.d0
  field%recv_2d(:,:) = 0.d0

  call jcup_get_data(field%varg(3)%varg_ptr, field%buffer1d)
  field%recv_2d(:,:) = unpack(field%buffer1d, field%mask2d, field%recv_2d)
  call write_data_2d(ICE,ICE_GRID, "li_1", field%recv_2d)


end subroutine get_and_write_data

!======================================================================================================

subroutine ice_run(loop_flag)
  use field_def
  implicit none
  logical, intent(INOUT) :: loop_flag
  integer :: i, k

  do i = 1, 6
    step_counter = step_counter+1
    call jcup_set_time(ICE, itime, delta_t)

    call get_and_write_data()

    call set_and_put_data(step_counter)

    call jcup_inc_time(ICE, itime)

  end do

  loop_flag = .false.

end subroutine ice_run

!======================================================================================================

subroutine ice_fin()
  use jcup_interface
  implicit none
  integer :: i

  open(unit = file_id, file = trim(ICE)//".mst", form = "formatted", status = "replace", err = 200)
  write(file_id, *) step_counter
  close(file_id)

  return

200 continue
  write(0,*) "ice data output error"

end subroutine ice_fin

!======================================================================================================


end module mod_ice

