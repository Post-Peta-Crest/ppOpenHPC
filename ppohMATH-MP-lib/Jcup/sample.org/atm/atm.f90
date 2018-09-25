module mod_atm
use jcup_interface
use field_common
use component_field, only : component_field_type
private

public :: atm_init
public :: atm_run
public :: atm_fin

integer :: DIV_X
integer :: DIV_Y
integer :: my_comm, my_group, my_rank, my_size

type(component_field_type) :: field, field3d, field_land

integer :: send_grid_ao(GNXO*GNYO)
integer :: recv_grid_ao(GNXO*GNYO)
integer :: send_grid_oa(GNXA*GNYA)
integer :: recv_grid_oa(GNXA*GNYA)

integer :: send_grid_ac(GNXA*GNYA*GNZA)
integer :: recv_grid_ac(GNXA*GNYA*GNZA)
integer :: send_grid_ca(GNXA*GNYA*GNZA)
integer :: recv_grid_ca(GNXA*GNYA*GNZA)

integer :: send_grid_al(GNXL*GNYL)
integer :: recv_grid_al(GNXL*GNYL)
integer :: send_grid_la(GNXA*GNYA)
integer :: recv_grid_la(GNXA*GNYA)

integer :: send_grid_ai(GNXI*GNYI)
integer :: recv_grid_ai(GNXI*GNYI)
integer :: send_grid_ia(GNXA*GNYA)
integer :: recv_grid_ia(GNXA*GNYA)

integer :: send_grid_li(GNXI*GNYI)
integer :: recv_grid_li(GNXI*GNYI)
integer :: send_grid_il(GNXL*GNYL)
integer :: recv_grid_il(GNXL*GNYL)

integer :: itime(6)
integer :: itimel(6)
integer :: delta_t

contains

!======================================================================================================

subroutine atm_init()
  use jcup_interpolation_sample
  use field_def, only : init_field_def, set_field_def, cal_mn, get_local_field, cal_grid_index, &
                        set_grid_mapping, set_grid_mapping_3d
  use component_field, only : init_field, init_field_data
  use dynamics, only : dynamics_init
  use mod_land, only : land_init_grid, land_init_data, land_put_initial_data
  implicit none
  integer :: lis, lie, ljs, lje
  integer :: ij, i, j, k
  integer, allocatable :: grid_index(:)
  integer :: comp_id(2)

  call jcup_set_new_comp(ATM)
  call jcup_set_new_comp(LAND)
  call jcup_initialize(ATM, LOG_LEVEL = 2, LOG_STDERR = 1)

  call init_interpolation(6,1,1)

  call jcup_get_mpi_parameter(ATM, my_comm, my_group, my_size, my_rank)
  call cal_mn(my_size, DIV_X, DIV_Y)

  call init_field_def(2)

  call set_field_def(ATM, ATM_GRID_2D, GNXA, GNYA, 1, 2, DIV_X, DIV_Y)
  call set_field_def(ATM, ATM_GRID_3D, GNXA, GNYA, GNZA, 2, DIV_X, DIV_Y)
  call get_local_field(component_name = ATM, grid_name = ATM_GRID_2D, &
                       local_is = lis, local_ie = lie, local_js = ljs, local_je = lje)
  call init_field(field, ATM_GRID_2D, lis, lie, ljs, lje, 1, 1)
  call init_field(field3d, ATM_GRID_3D, lis, lie, ljs, lje, 1, GNZA)
  call init_field(field_land, ATM_GRID_2D, lis, lie, ljs, lje, 1, 1)

  call cal_grid_index(ATM,ATM_GRID_2D, field%grid_index)
  call cal_grid_index(ATM,ATM_GRID_3D, field3d%grid_index)

  call jcup_def_grid(field%grid_index, ATM, ATM_GRID_2D,GN25)
  call jcup_def_grid(field3d%grid_index, ATM, ATM_GRID_3D)


  call land_init_grid()

  call jcup_end_grid_def()


  call init_field_data(field, GN25, 5, 1)
  call init_field_data(field_land, 1, 1, 2)
  call init_field_data(field3d, 1, 1, 1)


  call jcup_def_varp(field%varp(1)%varp_ptr, ATM,"a_2d_1", ATM_GRID_2D)
  call jcup_def_varp(field%varp(2)%varp_ptr, ATM,"a_2d_2", ATM_GRID_2D)
  call jcup_def_varp(field%varp(3)%varp_ptr, ATM,"a_2d_3", ATM_GRID_2D)
  call jcup_def_varp(field%varp(4)%varp_ptr, ATM,"a_25d_1", ATM_GRID_2D, GN25)
  call jcup_def_varp(field%varp(5)%varp_ptr, ATM,"a_25d_2", ATM_GRID_2D, GN25)
  call jcup_def_varp(field3d%varp(1)%varp_ptr, ATM,"a_3d_1", ATM_GRID_3D)


  call jcup_def_varg(field%varg(1)%varg_ptr, ATM, "oa_1", ATM_GRID_2D, &
                     SEND_MODEL_NAME = OCN, &
                     SEND_DATA_NAME = "ocn_1", &
                     RECV_MODE = "SNP", &
                     INTERVAL = 180, &
                     TIME_LAG = -1, &
                     MAPPING_TAG = 1, &
                     EXCHANGE_TAG = 1)
  call jcup_def_varg(field3d%varg(1)%varg_ptr, ATM, "ca_1", ATM_GRID_3D, &
                     SEND_MODEL_NAME = CHM, SEND_DATA_NAME = "chm_1", &
                     RECV_MODE = "SNP", INTERVAL = 120, TIME_LAG = -1, MAPPING_TAG = 1, EXCHANGE_TAG = 1)
  call jcup_def_varg(field_land%varg(1)%varg_ptr, ATM, "la_1", ATM_GRID_2D, &
                     SEND_MODEL_NAME = LAND, SEND_DATA_NAME = "land_1", &
                     RECV_MODE = "SNP", INTERVAL = 60, TIME_LAG = -1, MAPPING_TAG = 1, EXCHANGE_TAG = 1)
  call jcup_def_varg(field_land%varg(2)%varg_ptr, ATM, "ia_1", ATM_GRID_2D, &
                     SEND_MODEL_NAME = ICE, SEND_DATA_NAME = "ice_1", &
                     RECV_MODE = "SNP", INTERVAL = 60, TIME_LAG = -1, MAPPING_TAG = 1, EXCHANGE_TAG = 1)


  call land_init_data()


  call jcup_end_var_def()


  call set_grid_mapping(ATM, ATM_GRID_2D, GNXA, GNYA, OCN, OCN_GRID, GNXO, GNYO, send_grid_ao,recv_grid_ao)
  call set_grid_mapping(OCN, OCN_GRID, GNXO, GNYO, ATM, ATM_GRID_2D, GNXA, GNYA, send_grid_oa,recv_grid_oa)

  call jcup_set_mapping_table(ATM, ATM, ATM_GRID_2D, OCN, OCN_GRID, 1, send_grid_ao, recv_grid_ao)
  call jcup_set_mapping_table(ATM, OCN, OCN_GRID, ATM, ATM_GRID_2D, 1, send_grid_oa, recv_grid_oa)
  call set_operation_index(ATM,OCN,1)

  call set_grid_mapping_3d(ATM,ATM_GRID_3D, GNXA, GNYA, GNZA, CHM, CHM_GRID, GNXC, GNYC, GNZC, send_grid_ac, recv_grid_ac)
  call set_grid_mapping_3d(CHM,CHM_GRID, GNXC, GNYC, GNZC, ATM, ATM_GRID_3D, GNXA, GNYA, GNZA, send_grid_ca, recv_grid_ca)

  call jcup_set_mapping_table(ATM, ATM, ATM_GRID_3D, CHM, CHM_GRID, 1, send_grid_ac, recv_grid_ac)
  call jcup_set_mapping_table(ATM, CHM, CHM_GRID, ATM, ATM_GRID_3D, 1, send_grid_ca, recv_grid_ca)
  call set_operation_index(ATM,CHM,1)

  call set_grid_mapping(ATM, ATM_GRID_2D, GNXA, GNYA, LAND, LAND_GRID, GNXL, GNYL, send_grid_al,recv_grid_al)
  call set_grid_mapping(LAND, LAND_GRID, GNXL, GNYL, ATM, ATM_GRID_2D, GNXA, GNYA, send_grid_la,recv_grid_la)

  call jcup_set_mapping_table(ATM, ATM, ATM_GRID_2D, LAND, LAND_GRID, 1, send_grid_al, recv_grid_al)
  call jcup_set_mapping_table(ATM, LAND, LAND_GRID, ATM, ATM_GRID_2D, 1, send_grid_la, recv_grid_la)
  call set_operation_index(ATM,LAND,1)
  call set_operation_index(LAND,ATM,1)

  call set_grid_mapping(ATM, ATM_GRID_2D, GNXA, GNYA, ICE, ICE_GRID, GNXI, GNYI, send_grid_ai,recv_grid_ai)
  call set_grid_mapping(ICE, ICE_GRID, GNXI, GNYI, ATM, ATM_GRID_2D, GNXA, GNYA, send_grid_ia,recv_grid_ia)
  call jcup_set_mapping_table(ATM, ATM, ATM_GRID_2D, ICE, ICE_GRID, 1, send_grid_ai, recv_grid_ai)
  call jcup_set_mapping_table(ATM, ICE, ICE_GRID, ATM, ATM_GRID_2D, 1, send_grid_ia, recv_grid_ia)
  call set_operation_index(ATM,ICE,1)


  call jcup_set_mapping_table(LAND, OCN, OCN_GRID, LAND, LAND_GRID, 1)
  call jcup_set_mapping_table(LAND, LAND, LAND_GRID, OCN, OCN_GRID, 1)
  call set_operation_index(LAND,OCN,1)

  call set_grid_mapping(LAND, LAND_GRID, GNXL, GNYL, ICE, ICE_GRID, GNXI, GNYI, send_grid_li,recv_grid_li)
  call set_grid_mapping(ICE, ICE_GRID, GNXI, GNYI, LAND, LAND_GRID, GNXL, GNYL, send_grid_il,recv_grid_il)
  call jcup_set_mapping_table(LAND, LAND, LAND_GRID, ICE, ICE_GRID, 1, send_grid_li, recv_grid_li)
  call jcup_set_mapping_table(LAND, ICE, ICE_GRID, LAND, LAND_GRID, 1, send_grid_il, recv_grid_il)
  call set_operation_index(LAND,ICE,1)


  itime(1) = 2004
  itime(2) = 12
  itime(3) = 31
  itime(4) = 0
  itime(5) = 0
  itime(6) = 0
  itimel = itime

  delta_t = 60

  call jcup_init_time(itime)


  call set_and_put_data(0)

  call dynamics_init()

  call land_put_initial_data()

end subroutine atm_init

!======================================================================================================

subroutine set_and_put_data(step)
  use field_def, only : set_send_data_2d, set_send_data_3d
  implicit none
  integer, intent(IN) :: step
  integer :: k

  call set_send_data_2d(ATM,ATM_GRID_2D, field%send_2d(:,:), step, 1)
  call jcup_put_data(field%varp(1)%varp_ptr, pack(field%send_2d, MASK = field%mask2d))
  call set_send_data_2d(ATM,ATM_GRID_2D, field%send_2d(:,:), step, 1)
  call jcup_put_data(field%varp(2)%varp_ptr, pack(field%send_2d, MASK = field%mask2d))
  call set_send_data_2d(ATM,ATM_GRID_2D, field%send_2d(:,:), step, 1)
  call jcup_put_data(field%varp(3)%varp_ptr, pack(field%send_2d, MASK = field%mask2d))

  do k = 1, GN25
    call set_send_data_2d(ATM,ATM_GRID_2D, field%send_25d(:,:,k), step, k+3)
    field%buffer25d(:,k) = pack(field%send_25d(:,:,k), MASK = field%mask2d)  
  end do

  call jcup_put_data(field%varp(4)%varp_ptr, field%buffer25d, GN25) 

  do k = 1, GN25
    call set_send_data_2d(ATM,ATM_GRID_2D, field%send_25d(:,:,k), step, k+4)
    field%buffer25d(:,k) = pack(field%send_25d(:,:,k), MASK = field%mask2d)  
  end do

  call jcup_put_data(field%varp(5)%varp_ptr, field%buffer25d, GN25) 


  call set_send_data_3d(ATM,ATM_GRID_3D, field3d%send_3d(:,:,:), step, 1)
  call jcup_put_data(field3d%varp(1)%varp_ptr, pack(field3d%send_3d, MASK = field3d%mask3d))


end subroutine set_and_put_data

!======================================================================================================

subroutine get_and_write_data()
  use field_def, only : write_data_2d, write_data_3d
  implicit none

  field%buffer1d(:) = 0.d0

  call jcup_get_data(field%varg(1)%varg_ptr, field%buffer1d)
  field%recv_2d = unpack(field%buffer1d, field%mask2d, field%recv_2d)
  call write_data_2d(ATM,ATM_GRID_2D, "oa_1", field%recv_2d)

  field3d%buffer1d(:) = 0.d0

  call jcup_get_data(field3d%varg(1)%varg_ptr, field3d%buffer1d)
  field3d%recv_3d = unpack(field3d%buffer1d, field3d%mask3d, field3d%recv_3d)
  call write_data_3d(ATM,ATM_GRID_3D,"ca_1", field3d%recv_3d)

  field_land%buffer1d(:) = 0.d0

  call jcup_get_data(field_land%varg(1)%varg_ptr, field_land%buffer1d)
  field_land%recv_2d = unpack(field_land%buffer1d, field_land%mask2d, field_land%recv_2d)
  call write_data_2d(ATM,ATM_GRID_2D, "la_1", field_land%recv_2d)

  field_land%buffer1d(:) = 0.d0

  call jcup_get_data(field_land%varg(2)%varg_ptr, field_land%buffer1d)
  field_land%recv_2d = unpack(field_land%buffer1d, field_land%mask2d, field_land%recv_2d)
  call write_data_2d(ATM,ATM_GRID_2D, "ia_1", field_land%recv_2d)


end subroutine get_and_write_data

!======================================================================================================

subroutine atm_run(loop_flag)
  use field_def
  use dynamics, only : dynamics_run
  use mod_land, only : land_run
  implicit none
  logical, intent(INOUT) :: loop_flag
  integer :: i, k

  do i = 1, 6

    call jcup_set_time(ATM, itime, delta_t)

    call get_and_write_data()

    call set_and_put_data(i)

    call jcup_inc_time(ATM, itime)

    call land_run(loop_flag)

  end do

  loop_flag = .false.
  call dynamics_run()

end subroutine atm_run

!======================================================================================================

subroutine atm_fin()
  use jcup_interface
  use dynamics, only : dynamics_fin
  implicit none
  integer :: i

  call jcup_write_restart(32)

  call jcup_coupling_end(itime,.true.)
  call dynamics_fin()

end subroutine atm_fin

!======================================================================================================


end module mod_atm

