module mod_ocn
use jcup_interface
use field_common
use component_field, only : component_field_type
private

public :: ocn_init
public :: ocn_run
public :: ocn_fin

integer :: DIV_X
integer :: DIV_Y
integer :: my_comm, my_group, my_rank, my_size

type(component_field_type) :: field

integer :: send_grid_ol(GNXL*GNYL)
integer :: recv_grid_ol(GNXL*GNYL)
integer :: send_grid_lo(GNXO*GNYO)
integer :: recv_grid_lo(GNXO*GNYO)

integer :: send_grid_oi(GNXI*GNYI)
integer :: recv_grid_oi(GNXI*GNYI)
integer :: send_grid_io(GNXO*GNYO)
integer :: recv_grid_io(GNXO*GNYO)


integer :: start_time(6)
integer :: end_time(6)
integer :: itime(6)
integer :: delta_t

contains

!======================================================================================================

subroutine ocn_init()
  use jcup_interpolation_sample
  use field_def
  use component_field, only : init_field, init_field_data
  use mod_config, only : read_conf, get_start_time, get_end_time, get_step
  use mod_ice, only : ice_init_grid, ice_init_data, ice_put_initial_data
  implicit none
  integer, allocatable :: grid_index(:)
  integer :: lis, lie, ljs, lje
  integer :: i
  integer :: comp_id(2)


  call jcup_set_new_comp(OCN)
  call jcup_set_new_comp(ICE)
  call jcup_initialize(OCN)

  call read_conf("sample.2.cnf")

  call init_interpolation(6,1,2)

  call jcup_get_mpi_parameter(OCN, my_comm, my_group, my_size, my_rank)
  call cal_mn(my_size, DIV_X, DIV_Y)

  call init_field_def(2)

  call set_field_def(component_name = OCN, grid_name = OCN_GRID, &
                     g_nx = GNXO, g_ny = GNYO, g_nz = 1, hallo = 2, div_x = DIV_X, div_y = DIV_Y)
  call get_local_field(component_name = OCN, grid_name = OCN_GRID, &
                       local_is = lis, local_ie = lie, local_js = ljs, local_je = lje)

  call init_field(field, OCN_GRID, lis, lie, ljs, lje, 1, 1)
  call cal_grid_index(OCN,OCN_GRID, field%grid_index)


  call jcup_def_grid(field%grid_index, OCN, OCN_GRID)

  call ice_init_grid()


  call jcup_end_grid_def()


  call init_field_data(field, GN25, 2, 7)

  call jcup_def_varp(field%varp(1)%varp_ptr, OCN, "ocn_1", OCN_GRID)
  call jcup_def_varp(field%varp(2)%varp_ptr, OCN, "oi_1", OCN_GRID)
  call jcup_def_varg(field%varg(1)%varg_ptr, OCN, "atm_1", OCN_GRID, 1, &
                     SEND_MODEL_NAME = ATM, SEND_DATA_NAME = "a_2d_1", &
                     RECV_MODE = "AVR", INTERVAL = 180, TIME_LAG = -1, MAPPING_TAG = 1, EXCHANGE_TAG = 1)
  call jcup_def_varg(field%varg(2)%varg_ptr, OCN, "atm_2", OCN_GRID, 1, &
                     SEND_MODEL_NAME = ATM, SEND_DATA_NAME = "a_2d_2", &
                     RECV_MODE = "AVR", INTERVAL = 180, TIME_LAG = -1, MAPPING_TAG = 1, EXCHANGE_TAG = 1)
  call jcup_def_varg(field%varg(3)%varg_ptr, OCN, "atm_3", OCN_GRID, 1, &
                     SEND_MODEL_NAME = ATM, SEND_DATA_NAME = "a_2d_3", &
                     RECV_MODE = "SNP", INTERVAL = 180, TIME_LAG = -1, MAPPING_TAG = 1, EXCHANGE_TAG = 1)
  call jcup_def_varg(field%varg(4)%varg_ptr, OCN, "atm_4", OCN_GRID, GN25, &
                     SEND_MODEL_NAME = ATM, SEND_DATA_NAME = "a_25d_1", &
                     RECV_MODE = "AVR", INTERVAL = 180, TIME_LAG = -1, MAPPING_TAG = 1, EXCHANGE_TAG = 4)
  call jcup_def_varg(field%varg(5)%varg_ptr, OCN, "atm_5", OCN_GRID, GN25, &
                     SEND_MODEL_NAME = ATM, SEND_DATA_NAME = "a_25d_2", &
                     RECV_MODE = "SNP", INTERVAL = 180, TIME_LAG = -1, MAPPING_TAG = 1, EXCHANGE_TAG = 5)
  call jcup_def_varg(field%varg(6)%varg_ptr, OCN, "land_1", OCN_GRID, 1, &
                     SEND_MODEL_NAME = LAND, SEND_DATA_NAME = "land_1", &
                     RECV_MODE = "SNP", INTERVAL = 180, TIME_LAG = -1, MAPPING_TAG = 1, EXCHANGE_TAG = 8)
  call jcup_def_varg(field%varg(7)%varg_ptr, OCN, "ice_1", OCN_GRID, 1, &
                     SEND_MODEL_NAME = ICE, SEND_DATA_NAME = "io_1", &
                     RECV_MODE = "SNP", INTERVAL = 180, TIME_LAG = 0, MAPPING_TAG = 1, EXCHANGE_TAG = 5)


  call ice_init_data()


  call jcup_end_var_def()




  call jcup_set_mapping_table(ATM, ATM, ATM_GRID_2D, OCN, OCN_GRID, 1)
  call jcup_set_mapping_table(ATM, OCN, OCN_GRID, ATM, ATM_GRID_2D, 1)
  call set_operation_index(OCN,ATM,1)


  call set_grid_mapping(OCN, OCN_GRID, GNXO, GNYO, ICE, ICE_GRID, GNXI, GNYI, send_grid_oi,recv_grid_oi)
  call set_grid_mapping(ICE, ICE_GRID, GNXI, GNYI, OCN, OCN_GRID, GNXO, GNYO, send_grid_io,recv_grid_io)

  call jcup_set_mapping_table(OCN, OCN, OCN_GRID, ICE, ICE_GRID, 1, send_grid_oi, recv_grid_oi)
  call jcup_set_mapping_table(OCN, ICE, ICE_GRID, OCN, OCN_GRID, 1, send_grid_io, recv_grid_io)
  call set_operation_index(OCN,ICE,1)
  call set_operation_index(ICE,OCN,1)

  call jcup_set_mapping_table(ATM, ATM, ATM_GRID_2D, ICE, ICE_GRID, 1)
  call jcup_set_mapping_table(ATM, ICE, ICE_GRID, ATM, ATM_GRID_2D, 1)
  call set_operation_index(ICE,ATM,1)

  call set_grid_mapping(OCN, OCN_GRID, GNXO, GNYO, LAND, LAND_GRID, GNXL, GNYL, send_grid_ol,recv_grid_ol)
  call set_grid_mapping(LAND, LAND_GRID, GNXL, GNYL, OCN, OCN_GRID, GNXO, GNYO, send_grid_lo,recv_grid_lo)
  call jcup_set_mapping_table(OCN, OCN, OCN_GRID, LAND, LAND_GRID, 1, send_grid_ol, recv_grid_ol)
  call jcup_set_mapping_table(OCN, LAND, LAND_GRID, OCN, OCN_GRID, 1, send_grid_lo, recv_grid_lo)
  call set_operation_index(OCN,LAND,1)

  call jcup_set_mapping_table(LAND, LAND, LAND_GRID, ICE, ICE_GRID, 1)
  call jcup_set_mapping_table(LAND, ICE, ICE_GRID, LAND, LAND_GRID, 1)
  call set_operation_index(ICE,LAND,1)


  start_time = get_start_time()
  end_time = get_end_time()

  itime = start_time
  delta_t = get_step(OCN)

  call jcup_init_time(start_time)


  !!call mpi_finalize(i)
  !!stop

  call set_and_put_data(0)

  call ice_put_initial_data()

end subroutine ocn_init

!======================================================================================================

subroutine set_and_put_data(step)
  use field_def, only : set_send_data_2d
  implicit none
  integer, intent(IN) :: step
  integer :: k

  call set_send_data_2d(OCN,OCN_GRID, field%send_2d(:,:), step, 3)
  call jcup_put_data(field%varp(1)%varp_ptr, pack(field%send_2d, MASK = field%mask2d))
  call jcup_put_data(field%varp(2)%varp_ptr, pack(field%send_2d, MASK = field%mask2d))

end subroutine set_and_put_data

!======================================================================================================

subroutine get_and_write_data()
  use field_def, only : write_data_2d
  implicit none
  integer :: k

  field%buffer25d = 0.d0
  field%recv_25d = 0.d0

  field%buffer1d(:) = 0.d0

  call jcup_get_data(field%varg(1)%varg_ptr, field%buffer1d)
  field%recv_2d(:,:) = unpack(field%buffer1d, field%mask2d, field%recv_2d)
  call write_data_2d(OCN,OCN_GRID, "atm_1", field%recv_2d)

  field%buffer1d(:) = 0.d0

  call jcup_get_data(field%varg(2)%varg_ptr, field%buffer1d)
  field%recv_2d(:,:) = unpack(field%buffer1d, field%mask2d, field%recv_2d)
  call write_data_2d(OCN,OCN_GRID, "atm_2", field%recv_2d)

  field%buffer1d(:) = 0.d0

  call jcup_get_data(field%varg(3)%varg_ptr, field%buffer1d)
  field%recv_2d(:,:) = unpack(field%buffer1d, field%mask2d, field%recv_2d)
  call write_data_2d(OCN,OCN_GRID, "atm_3", field%recv_2d)

  call jcup_get_data(field%varg(4)%varg_ptr, field%buffer25d, GN25)
  do k = 1, GN25
    field%recv_25d(:,:,k) = unpack(field%buffer25d(:,k), field%mask2d, field%recv_25d(:,:,k))
  end do
  do k = 1, GN25
    call write_data_2d(OCN,OCN_GRID, "atm_4", field%recv_25d(:,:,k))
  end do

  call jcup_get_data(field%varg(5)%varg_ptr, field%buffer25d, GN25)
  do k = 1, GN25
    field%recv_25d(:,:,k) = unpack(field%buffer25d(:,k), field%mask2d, field%recv_25d(:,:,k))
  end do
  do k = 1, GN25
    call write_data_2d(OCN,OCN_GRID, "atm_5", field%recv_25d(:,:,k))
  end do

  field%buffer1d(:) = 0.d0

  call jcup_get_data(field%varg(6)%varg_ptr, field%buffer1d)
  field%recv_2d(:,:) = unpack(field%buffer1d, field%mask2d, field%recv_2d)
  call write_data_2d(OCN,OCN_GRID, "land_1", field%recv_2d)

  field%buffer1d(:) = 0.d0
  call jcup_get_data(field%varg(7)%varg_ptr, field%buffer1d)
  field%recv_2d(:,:) = unpack(field%buffer1d, field%mask2d, field%recv_2d)
  call write_data_2d(OCN,OCN_GRID, "ice_1", field%recv_2d)

end subroutine get_and_write_data

!======================================================================================================

subroutine ocn_run(loop_flag)
  use field_def
  use mod_ice, only : ice_run
  use mod_config, only : is_end_step
  implicit none
  logical, intent(INOUT) :: loop_flag
  integer :: i, k

  i = 0
  do !i = 1, 2
   i = i + 1
   call jcup_set_time(OCN, itime, delta_t)

    call get_and_write_data()

    call set_and_put_data(i)

    call jcup_inc_time(OCN, itime)

    call ice_run(loop_flag)

    if (is_end_step(itime, end_time)) exit
    
  end do

  loop_flag = .false.

end subroutine ocn_run

!======================================================================================================

subroutine ocn_fin()
  use jcup_interface
  implicit none
  integer :: i
  
  call jcup_coupling_end(itime, .true.)

end subroutine ocn_fin

!======================================================================================================


end module mod_ocn

