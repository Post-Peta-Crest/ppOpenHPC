!=======+=========+=========+=========+=========+=========+=========+=========+
!>
!! ppohMATHMP FEM model api
!! 
module m_ppohMATHMP_FEM
  use jcup_interface, only : jcup_varg_type
  use fs_interpolation
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: ppohMATHMP_FEM_init ! subroutine (conf_file_name)
  public :: ppohMATHMP_FEM_abort ! subroutine ()
  public :: ppohMATHMP_FEM_get_my_comm ! integer function()
  public :: ppohMATHMP_FEM_get_my_size ! integer function()
  public :: ppohMATHMP_FEM_get_my_rank ! integer function()
  public :: ppohMATHMP_FEM_def_grid ! subroutine (file_name, my_node)
  public :: ppohMATHMP_FEM_def_varg ! subroutine ()
  public :: ppohMATHMP_FEM_set_mapping_table ! subroutine ()
  public :: ppohMATHMP_FEM_get_num_of_coupling_nodes ! subroutine (num_of_node)
  public :: ppohMATHMP_FEM_get_global_node_index ! subroutine (num_of_node, node_index)
  public :: ppohMATHMP_FEM_get_local_node_index ! subroutine (num_of_node, node_index)
  public :: ppohMATHMP_FEM_get_value ! subroutine (num_of_node, value, idof)
  public :: ppohMATHMP_FEM_write_value ! subroutine (num_of_node, value, idof)
  public :: ppohMATHMP_FEM_start_integration ! subroutine ()
  public :: ppohMATHMP_FEM_init_time ! subroutine ()
  public :: ppohMATHMP_FEM_get_num_of_integration_loop ! function ()
  public :: ppohMATHMP_FEM_set_time ! subroutine (is_finish)
  public :: ppohMATHMP_FEM_finalize ! subroutine ()

!--------------------------------  private  ----------------------------------!

  integer, private, parameter :: LOG_FID = 434

  integer, private, parameter :: STR_LEN = 128
  integer, private, parameter :: NAME_LEN = 32

  character(len=NAME_LEN), private :: FEM_MODEL_NAME
  character(len=NAME_LEN), private :: FEM_GRID_NAME
  character(len=NAME_LEN), private :: FDM_MODEL_NAME
  character(len=NAME_LEN), private :: FDM_GRID_NAME 

  integer, private :: my_comm, my_group, my_size, my_rank

  private :: exchange_node_type

  type exchange_node_type 
    integer :: num_of_my_exchange_node
    integer, pointer :: node_index(:) ! global index of my exchange node
    integer, pointer :: local_index(:) ! convarsion table from exchange index to my local index
  end type

  type(exchange_node_type), private :: ex_node

  private :: varg_type

  type varg_type
    logical :: is_recv_ok
    integer :: id
    character(len=NAME_LEN) :: name
    integer :: int ! interval
    integer :: lag
    character(len=NAME_LEN) :: flag ! "AVR" or "SNP"
    character(len=NAME_LEN) :: grid
    integer :: remap_tag ! remapping_tag
    integer :: tag ! data tag
    character(len=NAME_LEN) :: send_comp
    character(len=NAME_LEN) :: send_data
    type(jcup_varg_type), pointer :: varg
    real(kind=8), pointer :: data(:)
  end type

  type(varg_type), allocatable, private :: varg(:)

  real(kind=4), private, parameter :: FISTR_DELTA_T = 0.001 ! 1 msec

  integer, private :: time(6)
  integer, private :: step_num

  real(kind=8) :: velocity_factor
  integer, private :: coupling_interval
  integer, private :: delta_t
  integer, private :: fem_loop_num ! number of time step loop of my (FEM) model
  character(len=6) :: coupling_mode = "NORMAL"
 
contains

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! initialize ppohMATHMP_FEM
subroutine ppohMATHMP_FEM_init(conf_file_name)
  use jcup_interface, only : jcup_set_new_comp, jcup_initialize, jcup_get_mpi_parameter
  use fs_namelist, only : init_configuration, write_configuration
  use fem_grid, only : init_fem_grid, read_coupling_info
  implicit none
  character(len=*), intent(IN) :: conf_file_name

  character(len=NAME_LEN) :: model_name, grid_name
  integer :: total_rank
  integer :: xorg, yorg
  real(kind=8) :: angle
  character(len=STR_LEN) :: global_node_file
  character(len=STR_LEN) :: coupling_mesh_file
  character(len=STR_LEN) :: coupling_info_file 
  namelist / fem_model_config / model_name, grid_name, total_rank, xorg, yorg, angle, global_node_file, coupling_mesh_file, coupling_info_file

  integer :: delta_x, delta_y, delta_z
  integer :: kfs
  character(len=STR_LEN) :: cod_file, index_file
  namelist / fdm_model_config / model_name, grid_name, xorg, yorg, delta_x, delta_y, delta_z, kfs, cod_file, index_file

  integer :: log_level, log_stderr
  namelist / coupler_config / log_level, log_stderr

  character(len=STR_LEN) :: data_dir, data_code
  namelist /fdm_data_config / data_dir, data_code, velocity_factor

  integer :: initial_time(6)
  integer :: start_step, end_step
  integer :: coupling_intvl, fdm_delta_t, fem_delta_t
  namelist /coupling_config / initial_time, start_step, end_step, coupling_intvl, fdm_delta_t, fem_delta_t, coupling_mode

  integer, parameter :: FID = 128
  integer :: ierror

  character(len=STR_LEN) :: log_file_name

  open(unit = FID, file = trim(conf_file_name), status="OLD", iostat=ierror)
  if (ierror /= 0) then
     write(0,*) "module m_ppohMATHMP_FEM, subroutine ppohMATHMP_FEM_init: Cannot open parameter file! file name = "//trim(conf_file_name)
     stop 999
  end if
  rewind(FID)
  read(FID, nml = fdm_model_config)
  FDM_MODEL_NAME = trim(model_name)
  FDM_GRID_NAME  = trim(grid_name)

  rewind(FID)
  read(FID, nml=fem_model_config)
  FEM_MODEL_NAME = trim(model_name)
  FEM_GRID_NAME = trim(grid_Name)
  rewind(FID)
  read(FID, nml=coupler_config)
  rewind(FID)
  read(FID, nml=coupling_config)
  rewind(FID)
  read(FID, nml=fdm_data_config)
  close(FID)

  call jcup_set_new_comp(FEM_MODEL_NAME)
  call jcup_initialize(FEM_MODEL_NAME, .true., log_level, (log_stderr >= 1))

  call jcup_get_mpi_parameter(FEM_MODEL_NAME, my_comm, my_group, my_size, my_rank)


  write(log_file_name, '(A,".log.pe",I5.5)') trim(FEM_MODEL_NAME), my_rank
  open(unit=LOG_FID, file = trim(log_file_name), status="replace", iostat = ierror)

  if (ierror /= 0) then
     write(0,*) "module m_ppohMATHMP_FEM, subroutine ppohMATHMP_FEM_init: Cannot open log file! file name = "//trim(log_file_name)
     stop 999
  end if


  call init_configuration(conf_file_name)
  call write_configuration(LOG_FID)

  call init_fem_grid(LOG_FID, total_rank, xorg, yorg, angle, global_node_file, coupling_mesh_file, coupling_info_file)
  call read_coupling_info(my_rank)

  time(:) = initial_time(:)

  step_num = 0

  coupling_interval = coupling_intvl
  delta_t = fem_delta_t

end subroutine ppohMATHMP_FEM_init

!=======+========+=========+=========+=========+=========+=========+=========+
!> 
!! abourt my process
subroutine ppohMATHMP_FEM_abort()
  implicit none
  integer :: ierror

  call mpi_abort(my_comm, 0, ierror)
  stop 999

end subroutine ppohMATHMP_FEM_abort

!=======+========+=========+=========+=========+=========+=========+=========+
!> 
!! get mpi communicator
integer function ppohMATHMP_FEM_get_my_comm()
  implicit none
   
  ppohMATHMP_FEM_get_my_comm = my_comm

end function ppohMATHMP_FEM_get_my_comm

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get mpi size
integer function ppohMATHMP_FEM_get_my_size()
  implicit none
   
  ppohMATHMP_FEM_get_my_size = my_size

end function ppohMATHMP_FEM_get_my_size

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get mpi rank
integer function ppohMATHMP_FEM_get_my_rank()
  implicit none
   
  ppohMATHMP_FEM_get_my_rank = my_rank

end function ppohMATHMP_FEM_get_my_rank

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! define frontISTR grid
subroutine ppohMATHMP_FEM_def_grid()
  use jcup_interface, only : jcup_def_grid, jcup_end_grid_def
  use grid_base, only : node_type
  use fem_grid, only : get_num_of_exchange_node, get_exchange_node_ptr
  implicit none
  type(node_type), pointer :: node_ptr
  integer :: i

  ex_node%num_of_my_exchange_node = get_num_of_exchange_node()
  allocate(ex_node%node_index(ex_node%num_of_my_exchange_node))
  allocate(ex_node%local_index(ex_node%num_of_my_exchange_node))

  do i = 1, ex_node%num_of_my_exchange_node
    node_ptr => get_exchange_node_ptr(i)
    ex_node%node_index(i) = node_ptr%index
    ex_node%local_index(i) = node_ptr%local_index
  end do

  call jcup_def_grid(ex_node%node_index, trim(FEM_MODEL_NAME), FEM_GRID_NAME)
  call jcup_end_grid_def()


end subroutine ppohMATHMP_FEM_def_grid


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! define frontISTR grid
subroutine ppohMATHMP_FEM_def_varg()
  use jcup_interface, only : jcup_def_varg, jcup_end_var_def
  use fs_namelist, only : get_num_of_varg, get_varg_conf
  implicit none
  integer :: num_of_varg
  integer :: i
  
  num_of_varg = get_num_of_varg(FEM_MODEL_NAME)
  allocate(varg(num_of_varg))

  do i = 1, num_of_varg
    varg(i)%id = i
    varg(i)%tag = i
    call get_varg_conf(FEM_MODEL_NAME, i, varg(i)%name, varg(i)%grid, varg(i)%send_comp, varg(i)%send_data, &
                       varg(i)%remap_tag, varg(i)%int, varg(i)%lag, varg(i)%flag)
    allocate(varg(i)%data(ex_node%num_of_my_exchange_node))
  end do

  do i = 1, num_of_varg
    call jcup_def_varg(varg(i)%varg, FEM_MODEL_NAME, varg(i)%name, varg(i)%grid, 1, varg(i)%send_comp, varg(i)%send_data, &
                       varg(i)%flag, varg(i)%int, varg(i)%lag, varg(i)%remap_tag, varg(i)%tag)
  end do

  call jcup_end_var_def()

end subroutine ppohMATHMP_FEM_def_varg

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! define mapping table
subroutine ppohMATHMP_FEM_set_mapping_table()
  use jcup_interface, only : jcup_set_mapping_table
  use fs_namelist, only : get_mapping_table_file
  use fs_grid, only : read_mapping_table
  use fs_interpolation, only : init_interpolation, set_fs_coef
  implicit none
  integer, pointer :: send_grid(:), recv_grid(:)
  real(kind=8), pointer :: coefu(:), coefv(:), coefw(:)
  character(len=STR_LEN) :: mapping_table_file
  integer :: num_of_mapping_tag
  integer :: i

  if (my_rank == 0) then
    call get_mapping_table_file(FDM_MODEL_NAME, FDM_GRID_NAME, FEM_MODEL_NAME, FEM_GRID_NAME, 1, mapping_table_file)
    call read_mapping_table(trim(mapping_table_file), send_grid, recv_grid, coefu)
    call jcup_set_mapping_table(FEM_MODEL_NAME, FDM_MODEL_NAME, FDM_GRID_NAME, FEM_MODEL_NAME, FEM_GRID_NAME, 1, send_grid, recv_grid)
    deallocate(send_grid, recv_grid)

    call get_mapping_table_file(FDM_MODEL_NAME, FDM_GRID_NAME, FEM_MODEL_NAME, FEM_GRID_NAME, 2, mapping_table_file)
    call read_mapping_table(trim(mapping_table_file), send_grid, recv_grid, coefv)
    call jcup_set_mapping_table(FEM_MODEL_NAME, FDM_MODEL_NAME, FDM_GRID_NAME, FEM_MODEL_NAME, FEM_GRID_NAME, 2, send_grid, recv_grid)
    deallocate(send_grid, recv_grid)

    call get_mapping_table_file(FDM_MODEL_NAME, FDM_GRID_NAME, FEM_MODEL_NAME, FEM_GRID_NAME, 3, mapping_table_file)
    call read_mapping_table(trim(mapping_table_file), send_grid, recv_grid, coefw)
    call jcup_set_mapping_table(FEM_MODEL_NAME, FDM_MODEL_NAME, FDM_GRID_NAME, FEM_MODEL_NAME, FEM_GRID_NAME, 3, send_grid, recv_grid)
    deallocate(send_grid, recv_grid)

  else
    allocate(send_grid(1), recv_grid(1), coefu(1), coefv(1), coefw(1))
    call jcup_set_mapping_table(FEM_MODEL_NAME, FDM_MODEL_NAME, FDM_GRID_NAME, FEM_MODEL_NAME, FEM_GRID_NAME, 1, send_grid, recv_grid)
    call jcup_set_mapping_table(FEM_MODEL_NAME, FDM_MODEL_NAME, FDM_GRID_NAME, FEM_MODEL_NAME, FEM_GRID_NAME, 2, send_grid, recv_grid)
    call jcup_set_mapping_table(FEM_MODEL_NAME, FDM_MODEL_NAME, FDM_GRID_NAME, FEM_MODEL_NAME, FEM_GRID_NAME, 3, send_grid, recv_grid)
    deallocate(send_grid, recv_grid)

  end if

  num_of_mapping_tag = 3  
  call init_interpolation(FEM_MODEL_NAME, FDM_MODEL_NAME, num_of_mapping_tag)
  call set_fs_coef(coefu, 1)
  call set_fs_coef(coefv, 2)
  call set_fs_coef(coefw, 3)
  deallocate(coefu, coefv, coefw)

end subroutine ppohMATHMP_FEM_set_mapping_table


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get number of my coupling node
subroutine ppohMATHMP_FEM_get_num_of_coupling_nodes(n)
  implicit none
  integer, intent(OUT) :: n ! number of coupling nodes

  n = ex_node%num_of_my_exchange_node
  
end subroutine ppohMATHMP_FEM_get_num_of_coupling_nodes

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get local index of my coupling node
subroutine ppohMATHMP_FEM_get_global_node_index(n, nodes_global_id)
  implicit none
  integer, intent(IN) :: n ! number of my coupling node
  integer, intent(OUT) :: nodes_global_id(n)
  integer :: i

  do i = 1, ex_node%num_of_my_exchange_node
    nodes_global_id(i) = ex_node%node_index(i)
  end do
  
end subroutine ppohMATHMP_FEM_get_global_node_index

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get local index of my coupling node
subroutine ppohMATHMP_FEM_get_local_node_index(n, nodes_local_id)
  implicit none
  integer, intent(IN) :: n ! number of my coupling node
  integer, intent(OUT) :: nodes_local_id(n)
  integer :: i

  do i = 1, ex_node%num_of_my_exchange_node
    nodes_local_id(i) = ex_node%local_index(i)
  end do
  
end subroutine ppohMATHMP_FEM_get_local_node_index

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get local index of my coupling node
subroutine ppohMATHMP_FEM_get_value(n, value, idof)
  use jcup_interface, only : jcup_get_data
  implicit none
  integer, intent(IN) :: n ! number of coupling node
  real(kind=4), intent(INOUT) :: value(n)
  integer, intent(IN) :: idof
  integer :: i

  do i = 1, size(varg)
    if (idof == varg(i)%id) then
      if (varg(i)%is_recv_ok) then
        value(:) = varg(i)%data(:) 
      end if
      return
    end if
  end do

  write(0,'(A,I5)') "ppohMATHMP_FEM_get_value, data index idof is not correct!!! idof = ",idof
  call ppohMATHMP_FEM_abort()

end subroutine ppohMATHMP_FEM_get_value

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! write data 
subroutine ppohMATHMP_FEM_write_value(step_num, n, value, idof)
  implicit none
  integer, intent(IN) :: step_num
  integer, intent(IN) :: n ! number of coupling node
  real(kind=4), intent(IN) :: value(n)
  integer, intent(IN) :: idof
  integer, parameter :: FID = 222
  character(len=STR_LEN) :: file_name
  integer :: i

  write(file_name, '(A,I5.5,A,I5.5)') "test_out."//trim(varg(idof)%name)//".",step_num,".pe",my_rank

  open(unit = FID, file=trim(file_name))
  write(FID,*) "step = ", step_num
  do i = 1, n
    write(FID, *) ex_node%local_index(i), value(i)  
  end do

  close(FID)
  
end subroutine ppohMATHMP_FEM_write_value


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! waite integration start
subroutine ppohMATHMP_FEM_start_integration()
  use jcup_interface, only : jcup_recv_array
  implicit none
  integer :: int_buffer(3)
  integer :: fdm_integ_time

  call jcup_recv_array(FEM_MODEL_NAME, FDM_MODEL_NAME, int_buffer)

  fdm_integ_time = (int_buffer(2)-int_buffer(1))*int_buffer(3) ! (end_step - first_step)*delta_t

  fem_loop_num = fdm_integ_time/delta_t + 1
  
end subroutine ppohMATHMP_FEM_start_integration

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get the number of time integration loop
integer function ppohMATHMP_FEM_get_num_of_integration_loop()
  implicit none

  ppohMATHMP_FEM_get_num_of_integration_loop = fem_loop_num

end function ppohMATHMP_FEM_get_num_of_integration_loop


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! initialize time
subroutine ppohMATHMP_FEM_init_time()
  use jcup_interface, only : jcup_init_time
  implicit none
  
  call jcup_init_time(time)

end subroutine ppohMATHMP_FEM_init_time

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! set current time and delta t
subroutine ppohMATHMP_FEM_set_time()
  use jcup_interface, only : jcup_set_time, jcup_inc_time, jcup_get_data
  implicit none
  integer :: i

  call jcup_set_time(FEM_MODEL_NAME, time, delta_t)
  call jcup_inc_time(FEM_MODEL_NAME, time)

  do i = 1, size(varg)
    call jcup_get_data(varg(i)%varg, varg(i)%data, varg(i)%is_recv_ok)
  end do

  if (trim(coupling_mode) /= "DEBUG") then
    call converte_axis_direction()
    call converte_velocity_to_displacement()
  end if

end subroutine ppohMATHMP_FEM_set_time

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! finalize coupling
subroutine ppohMATHMP_FEM_finalize(finalize_flag)
  use jcup_interface, only : jcup_coupling_end
  implicit none
  logical, intent(IN) :: finalize_flag

  close(LOG_FID)

  call jcup_coupling_end(time, finalize_flag)

end subroutine ppohMATHMP_FEM_finalize

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! axis direction convert
subroutine converte_axis_direction()
  implicit none
  integer :: i

  do i = 1, ex_node%num_of_my_exchange_node
    varg(2)%data(i) = -1.d0 * varg(2)%data(i)  ! southward to northward
    varg(3)%data(i) = -1.d0 * varg(3)%data(i)  ! douwnward to upward
  end do

end subroutine converte_axis_direction

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! axis direction convert
! 2014/11/21 [MOD] coupling_inerval -> delta_t
subroutine converte_velocity_to_displacement()
  implicit none
  integer :: i, j

  do i = 1, ex_node%num_of_my_exchange_node
     do j = 1, 3
       !varg(j)%data(i) = varg(j)%data(i) * coupling_interval*0.001d0 * velocity_factor! (mean velocity * coupling_interval * milli second)
       varg(j)%data(i) = varg(j)%data(i) * delta_t*0.001d0 * velocity_factor! (mean velocity * coupling_interval * milli second)
     end do
  end do

end subroutine converte_velocity_to_displacement

end module


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! define mapping table
subroutine interpolate_data(recv_comp_name, send_comp_name, mapping_tag, &
                            sn1, sn2, send_data, rn1, rn2, recv_data, num_of_data, num_of_tag, exchange_tag)
  use fs_interpolation, only : interpolate_seism_to_fistr
  implicit none
  character(len=*), intent(IN) :: recv_comp_name, send_comp_name
  integer, intent(IN) :: mapping_tag
  integer, intent(IN) :: sn1, sn2
  real(kind=8), intent(IN) :: send_data(sn1, sn2)
  integer, intent(IN) :: rn1, rn2
  real(kind=8), intent(INOUT) :: recv_data(rn1, rn2)
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: num_of_tag
  integer, intent(IN) :: exchange_tag

  !write(0,*) "interpolation parameters ", sn1, sn2, rn1, rn2, num_of_data, num_of_tag, exchange_tag

  call interpolate_seism_to_fistr(mapping_tag, send_data, recv_data)

end subroutine interpolate_data
