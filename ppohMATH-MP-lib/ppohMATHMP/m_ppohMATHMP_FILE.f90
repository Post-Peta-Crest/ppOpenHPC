module m_ppohMATHMP_FILE
  use jcup_interface, only : jcup_varp_type
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: ppohMATHMP_FILE_init  ! subroutine (conf_file_name)
  public :: ppohMATHMP_FILE_abort ! subroutine ()
  public :: ppohMATHMP_FILE_set_exchange_rank  ! subroutine ()
  public :: ppohMATHMP_FILE_cal_exchange_rank  ! subroutine ()
  public :: ppohMATHMP_FILE_def_grid           ! subroutine ()
  public :: ppohMATHMP_FILE_def_varp           ! subroutine ()
  public :: ppohMATHMP_FILE_set_mapping_tbl    ! subroutine ()
  public :: ppohMATHMP_FILE_init_time         ! subroutine ()
  public :: ppohMATHMP_FILE_read_data         ! subroutine ()
  public :: ppohMATHMP_FILE_get_start_step    ! integer function ()
  public :: ppohMATHMP_FILE_get_end_step      ! integer function ()
  public :: ppohMATHMP_FILE_start_integration ! subroutine ()
  public :: ppohMATHMP_FILE_set_time          ! subroutine ()
  public :: ppohMATHMP_FILE_write_grads_data  ! subroutine (file_name)
  public :: ppohMATHMP_FILE_finalize          ! subroutine ()

!--------------------------------  private  ----------------------------------!
  
  integer, parameter :: LOG_FID = 434

  integer, parameter :: NAME_LEN = 32
  integer, parameter :: STR_LEN  = 128
  character(len=NAME_LEN), private :: FEM_MODEL_NAME
  character(len=NAME_LEN), private :: FEM_GRID_NAME 
  character(len=NAME_LEN), private :: FDM_MODEL_NAME
  character(len=NAME_LEN), private :: FDM_GRID_NAME 

  integer :: NX = 2496
  integer :: NY = 2496
  integer :: NZ = 496

  integer :: num_of_total_rank
  logical, allocatable :: exchange_rank_flag(:)
  integer, pointer :: is(:), ie(:), js(:), je(:), ks(:), ke(:)

  integer :: x_grid_min
  integer :: x_grid_max
  integer :: y_grid_min
  integer :: y_grid_max
  integer :: z_grid_min
  integer :: z_grid_max

  integer :: my_comm, my_group, my_size, my_rank

  integer :: time(6) 
  integer :: step_num 
  integer :: delta_t = 1

  type exchange_rank_type
    integer :: my_rank
    integer :: is, ie, js, je, ks, ke ! global index of my rank
  end type

  integer :: num_of_exchange_rank
  
  type(exchange_rank_type), pointer :: exchange_rank(:)
 
  integer :: num_of_exchange_grid

  type varp_type
    integer :: id
    character(len=NAME_LEN) :: name
    character(len=NAME_LEN) :: grid
    type(jcup_varp_type), pointer :: varp
    real(kind=8), pointer :: data(:)
  end type 

  type(varp_type), allocatable :: varp(:)

  character(len=STR_LEN) :: data_dir, data_code
  integer :: start_step, end_step

contains

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! initialize seism_file
subroutine ppohMATHMP_FILE_init(conf_file_name)
  use jcup_interface, only : jcup_set_new_comp, jcup_initialize, jcup_get_mpi_parameter
  use fs_namelist, only : init_configuration, write_configuration
  use fdm_grid, only : get_grid_size_from_cod_file, get_grid_info_from_cod_file
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


  real(kind=8) :: velocity_factor
  namelist / fdm_data_config / data_dir, data_code, velocity_factor

  integer :: initial_time(6)
  integer :: coupling_intvl, fdm_delta_t, fem_delta_t
  character(len=6) :: coupling_mode = "NORMAL"
  namelist /coupling_config / initial_time, start_step, end_step, coupling_intvl, fdm_delta_t, fem_delta_t, coupling_mode

  character(len=STR_LEN) :: log_file_name

  integer, parameter :: FID = 128
  integer :: ierror

  open(unit = FID, file = trim(conf_file_name), status="OLD", iostat=ierror)
  if (ierror /= 0) then
     write(0,*) "module jcup_seism, subroutine js_init: Cannot open parameter file! file name = "//trim(conf_file_name)
     stop 999
  end if
  rewind(FID)
  read(FID, nml=fdm_model_config)
  FDM_MODEL_NAME = trim(model_name)
  FDM_GRID_NAME  = trim(grid_name)

  rewind(FID)
  read(FID, nml=fem_model_config)
  FEM_MODEL_NAME = trim(model_name)
  FEM_GRID_NAME  = trim(grid_name)

  rewind(FID)
  read(FID, nml=coupler_config)
  rewind(FID)
  read(FID, nml=coupling_config)
  rewind(FID)
  read(FID, nml=fdm_data_config)
  close(FID)

  call get_grid_size_from_cod_file(cod_file, nx, ny, nz)
  call get_grid_info_from_cod_file(cod_file, num_of_total_rank, is, ie, js, je, ks, ke)
  
  allocate(exchange_rank_flag(num_of_total_rank))
  exchange_rank_flag(:) = .false.

  call jcup_set_new_comp(FDM_MODEL_NAME)
  call jcup_initialize(FDM_MODEL_NAME, .true., log_level, (log_stderr >= 1))

  call jcup_get_mpi_parameter(FDM_MODEL_NAME, my_comm, my_group, my_size, my_rank)

  write(log_file_name, '(A,".log.pe",I5.5)') trim(FDM_MODEL_NAME), my_rank
  open(unit=LOG_FID, file = trim(log_file_name), status="replace", iostat = ierror)

  if (ierror /= 0) then
     write(0,*) "module m_ppohMATHMP_FILE, subroutine ppohMATHMP_FILE_init: Cannot open log file! file name = "//trim(log_file_name)
     stop 999
  end if


  x_grid_min = 99999
  x_grid_max = 0
  y_grid_min = 99999
  y_grid_max = 0
  z_grid_min = 99999
  z_grid_max = 0

  !time(1) = 2014
  !time(2) = 7
  !time(3) = 1
  !time(4) = 0
  !time(5) = 0
  !time(6) = 0

  time(:) = initial_time(:)

  step_num = 0

  delta_t = fdm_delta_t

  call init_configuration(conf_file_name)
  call write_configuration(LOG_FID)

end subroutine ppohMATHMP_FILE_init

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! abort this component
subroutine ppohMATHMP_FILE_abort()
  implicit none
  integer :: ierror

  call mpi_abort(my_comm, 0, ierror)
  stop 999

end subroutine ppohMATHMP_FILE_abort


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! set exchange rank from mapping table
subroutine ppohMATHMP_FILE_set_exchange_rank()
  use fs_namelist, only : get_mapping_table_file
  implicit none
  character(len=STR_LEN) :: mapping_file_name

  call get_mapping_table_file(FDM_MODEL_NAME, FDM_GRID_NAME, FEM_MODEL_NAME, FEM_GRID_NAME, 1, mapping_file_name)
  call read_mapping_table(mapping_file_name)
  call get_mapping_table_file(FDM_MODEL_NAME, FDM_GRID_NAME, FEM_MODEL_NAME, FEM_GRID_NAME, 2, mapping_file_name)
  call read_mapping_table(mapping_file_name)
  call get_mapping_table_file(FDM_MODEL_NAME, FDM_GRID_NAME, FEM_MODEL_NAME, FEM_GRID_NAME, 3, mapping_file_name)
  call read_mapping_table(mapping_file_name)
 
end subroutine ppohMATHMP_FILE_set_exchange_rank

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! read mapping table and set exchange rank
subroutine read_mapping_table(file_name)
  implicit none
  character(len=*), intent(IN) :: file_name ! mapping table file name
  integer, parameter :: FID = 228
  character(len=128) :: file_str
  integer :: r_grid, r_rank, s_grid, si, sj, sk
  real(kind=8) :: fcoef
  integer :: counter
  integer :: istat
  integer :: i, j

  open(unit=FID, file=trim(file_name))
  read(FID,*) file_str
  counter = 0
  do 
    read(FID, *, iostat = istat) r_grid, r_rank, si, sj, sk, s_grid, fcoef
    if (istat /= 0) exit

    if (si > x_grid_max) x_grid_max = si
    if (si < x_grid_min) x_grid_min = si
    if (sj > y_grid_max) y_grid_max = sj
    if (sj < y_grid_min) y_grid_min = sj
    if (sk > z_grid_max) z_grid_max = sk
    if (sk < z_grid_min) z_grid_min = sk

    exchange_rank_flag(get_exchange_rank(si,sj,sk)) = .true.
    counter = counter + 1
  end do

  close(FID)

  !write(0,*) x_grid_min, x_grid_max, y_grid_min, y_grid_max, z_grid_min, z_grid_max


end subroutine read_mapping_table

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! calculate exchange rank
integer function get_exchange_rank(si, sj, sk)
  implicit none
  integer, intent(IN) :: si, sj, sk
  integer :: i

  do i = 1, num_of_total_rank
    if (si < is(i)) cycle
    if (si > ie(i)) cycle
    if (sj < js(i)) cycle
    if (sj > je(i)) cycle
    if (sk < ks(i)) cycle
    if (sk > ke(i)) cycle
    get_exchange_rank = i
    return
  end do

  write(0,*) "cal_exchange_rank, parameter error ", si, sj, sk
  stop

end function get_exchange_rank

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! read mapping table and set exchange rank
subroutine ppohMATHMP_FILE_cal_exchange_rank()
  implicit none
  integer :: counter
  integer :: i, j

  num_of_exchange_rank  = 0
  do i = 1, num_of_total_rank
    if (exchange_rank_flag(i)) then
      num_of_exchange_rank = num_of_exchange_rank + 1
    end if
  end do

  allocate(exchange_rank(num_of_exchange_rank))

  write(LOG_FID, *)
  write(LOG_FID, *) "ppohMATHMP_FILE_cal_exchange_rank"

  counter = 0
  do i = 1, num_of_total_rank
      if (exchange_rank_flag(i)) then
        counter = counter + 1
        exchange_rank(counter)%my_rank = i-1
        exchange_rank(counter)%is = is(i)
        exchange_rank(counter)%ie = ie(i)
        exchange_rank(counter)%js = js(i)
        exchange_rank(counter)%je = je(i)
        exchange_rank(counter)%ks = max(ks(i), z_grid_min)
        exchange_rank(counter)%ke = min(ke(i), z_grid_max)

        write(LOG_FID,*) "cal_exchange_rank " ,exchange_rank(counter)%my_rank, &
                   exchange_rank(counter)%is, exchange_rank(counter)%ie, &
                   exchange_rank(counter)%js, exchange_rank(counter)%je, &
                   exchange_rank(counter)%ks, exchange_rank(counter)%ke

      end if
  end do

  deallocate(is, ie, js, je, ks, ke)
  deallocate(exchange_rank_flag)

end subroutine ppohMATHMP_FILE_cal_exchange_rank

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! read mapping table and set exchange rank
subroutine ppohMATHMP_FILE_def_grid()
  use jcup_interface, only : jcup_def_grid, jcup_end_grid_def
  implicit none
  integer :: i, j, k, r
  integer, allocatable :: grid_index(:)
  integer :: counter

  num_of_exchange_grid = 0
  do i = 1, num_of_exchange_rank
     num_of_exchange_grid = (exchange_rank(i)%ie - exchange_rank(i)%is + 1)*(exchange_rank(i)%je - exchange_rank(i)%js + 1) &
                           *(exchange_rank(i)%ke - exchange_rank(i)%ks + 1) + num_of_exchange_grid
     !write(0,*) exchange_rank(i)%ie, exchange_rank(i)%is, exchange_rank(i)%je, exchange_rank(i)%js,  &
     !                      exchange_rank(i)%ke,  exchange_rank(i)%ks 
  end do

  !write(0,*) "num_of_exchange_grid = ", num_of_exchange_grid

  allocate(grid_index(num_of_exchange_grid))

  counter = 0
  do r = 1, num_of_exchange_rank
    do k = exchange_rank(r)%ks, exchange_rank(r)%ke
      do j = exchange_rank(r)%js, exchange_rank(r)%je
        do i = exchange_rank(r)%is, exchange_rank(r)%ie
           counter = counter + 1
           grid_index(counter) = i + (j-1)*nx + (k-1)*nx*ny
        end do
      end do
    end do
  end do

  call jcup_def_grid(grid_index, FDM_MODEL_NAME, FDM_GRID_NAME)
  call jcup_end_grid_def()

  deallocate(grid_index)

end subroutine ppohMATHMP_FILE_def_grid

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! define seism put data
subroutine ppohMATHMP_FILE_def_varp
  use jcup_interface, only : jcup_def_varp, jcup_end_var_def
  use fs_namelist, only : get_num_of_varp, get_varp_conf
  implicit none
  integer :: num_of_varp
  integer :: i

  num_of_varp = get_num_of_varp(FDM_MODEL_NAME)
  allocate(varp(num_of_varp))

  do i = 1, num_of_varp
    varp(i)%id = i
    call get_varp_conf(FDM_MODEL_NAME, i, varp(i)%name, varp(i)%grid)
    allocate(varp(i)%data(num_of_exchange_grid))
  end do

  do i = 1, num_of_varp
    call jcup_def_varp(varp(i)%varp, FDM_MODEL_NAME, varp(i)%name, varp(i)%grid, 1)
  end do

  call jcup_end_var_def()

end subroutine ppohMATHMP_FILE_def_varp



!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! define mapping table
subroutine ppohMATHMP_FILE_set_mapping_tbl()
  use jcup_interface, only : jcup_get_num_of_component, jcup_is_my_component, jcup_get_component_name, jcup_set_mapping_table
  implicit none
  character(len=32) :: comp_name
  integer :: i
 
  do i = 1, jcup_get_num_of_component()

    if (jcup_is_my_component(i)) cycle ! skip my component

    comp_name = jcup_get_component_name(i)
    call jcup_set_mapping_table(FDM_MODEL_NAME, FDM_MODEL_NAME, FDM_GRID_NAME, trim(comp_name), FEM_GRID_NAME, 1)
    call jcup_set_mapping_table(FDM_MODEL_NAME, FDM_MODEL_NAME, FDM_GRID_NAME, trim(comp_name), FEM_GRID_NAME, 2)
    call jcup_set_mapping_table(FDM_MODEL_NAME, FDM_MODEL_NAME, FDM_GRID_NAME, trim(comp_name), FEM_GRID_NAME, 3)

  end do

end subroutine ppohMATHMP_FILE_set_mapping_tbl

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! initialize time
subroutine ppohMATHMP_FILE_init_time()
  use jcup_interface, only : jcup_init_time
  implicit none
  
  call jcup_init_time(time)

end subroutine ppohMATHMP_FILE_init_time

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! read u, v, w data from file
subroutine ppohMATHMP_FILE_read_and_put_data()
  use jcup_interface, only : jcup_put_data
  implicit none
  integer :: i, j, k, r
  integer :: counter
  real(kind=8), parameter :: PI = 3.141592

  step_num = step_num + 1  

  counter = 0
  !do r = 1, num_of_exchange_rank
  !  do k = ecod(r)%ks_out, ecod(r)%ke_out
  !    do j = ecod(r)%js_out, ecod(r)%je_out
  !      do i = ecod(r)%is_out, ecod(r)%ie_out
  !        counter = counter + 1
  !        varp(1)%data(counter) = AMPLITUDE*sin(PI/STEP_CYCLE*(step_num-1))
  !        varp(2)%data(counter) = AMPLITUDE*sin(PI/STEP_CYCLE*(step_num-1))
  !        varp(3)%data(counter) = AMPLITUDE*sin(PI/STEP_CYCLE*(step_num-1))
          !varp(1)%data(counter) = i + 100*j + 10000*k
          !varp(2)%data(counter) = i + 100*j + 10000*k
          !varp(3)%data(counter) = i + 100*j + 10000*k
  !      end do
  !    end do
  !  end do
  !end do


end subroutine ppohMATHMP_FILE_read_and_put_data


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! read data from file
subroutine ppohMATHMP_FILE_read_data(step)
  use jcup_interface, only : jcup_put_data
  implicit none
  integer, intent(IN) :: step
  integer, parameter :: FID = 133
  character(len=STR_LEN) :: file_name
  real(kind=4), allocatable :: data_buffer(:,:,:)
  integer :: rec_len
  integer :: istat
  integer :: i, j, k
  integer :: r
  integer :: counter

  ! read code is troll/seism_data/src/seism_io.f90


  counter = 0

  do r = 1, num_of_exchange_rank


    allocate(data_buffer(3, exchange_rank(r)%is:exchange_rank(r)%ie, exchange_rank(r)%js:exchange_rank(r)%je))
  
    write(file_name, '(A,"/",A,"-",I4.4,"-",I4.4,".dat")') trim(data_dir), trim(data_code), &
                                                           exchange_rank(r)%my_rank, step

    rec_len = (exchange_rank(r)%ie-exchange_rank(r)%is+1)*(exchange_rank(r)%je-exchange_rank(r)%js+1)

    open(unit=FID, file=trim(file_name), status="OLD", access="DIRECT", recl = rec_len*4*3, iostat = istat)

    if (istat /= 0) then
       write(0,*) "ppohMATHMP_FILE_read_data file open error!!!"//trim(file_name)
       stop

    end if

    do k = exchange_rank(r)%ks, exchange_rank(r)%ke

      read(FID, rec=k) data_buffer

      do j = exchange_rank(r)%js, exchange_rank(r)%je
        do i = exchange_rank(r)%is, exchange_rank(r)%ie
          counter = counter + 1
          varp(1)%data(counter) = data_buffer(1, i, j)
          varp(2)%data(counter) = data_buffer(2, i, j)
          varp(3)%data(counter) = data_buffer(3, i, j)

        end do
      end do

    end do

    close(FID)

    deallocate(data_buffer)

    !write(0,*) "ppohMATHMP_FILE_read_data, read "//trim(file_name)//" OK"

  end do
  
  call jcup_put_data(varp(1)%varp, varp(1)%data)
  call jcup_put_data(varp(2)%varp, varp(2)%data)
  call jcup_put_data(varp(3)%varp, varp(3)%data)

end subroutine ppohMATHMP_FILE_read_data

subroutine ppohMATHMP_FILE_write_grads_data(file_name)
  implicit none
  character(len=*), intent(IN) :: file_name
  integer, parameter :: OFID = 118
  real(kind=4), allocatable :: out_buffer(:,:,:,:)
  integer :: i, j, k, r, counter

  open(unit=OFID, file=trim(file_name), status = "replace", access="direct", recl = 156*2*156*2*16*4*3)

  allocate(out_buffer(156*2,156*2,16,3))

  counter = 0

  do r = 1, num_of_exchange_rank
    do k = 1, exchange_rank(r)%ke-exchange_rank(r)%ks+1
      do j = 1, exchange_rank(r)%je-exchange_rank(r)%js+1
        do i = 1, exchange_rank(r)%ie-exchange_rank(r)%is+1
         counter = counter + 1
         select case(exchange_rank(r)%my_rank)
         case(119)
           out_buffer(i,j,k,1) = varp(1)%data(counter)
           out_buffer(i,j,k,2) = varp(2)%data(counter)
           out_buffer(i,j,k,3) = varp(3)%data(counter)
         case(120)
           out_buffer(156+i,j,k,1) = varp(1)%data(counter)
           out_buffer(156+i,j,k,2) = varp(2)%data(counter)
           out_buffer(156+i,j,k,3) = varp(3)%data(counter)
         case(135)
           out_buffer(i,156+j,k,1) = varp(1)%data(counter)
           out_buffer(i,156+j,k,2) = varp(2)%data(counter)
           out_buffer(i,156+j,k,3) = varp(3)%data(counter)
         case(136)
           out_buffer(156+i,156+j,k,1) = varp(1)%data(counter)
           out_buffer(156+i,156+j,k,2) = varp(2)%data(counter)
           out_buffer(156+i,156+j,k,3) = varp(3)%data(counter)
         case default
           write(0,*) "ppohMATHMP_FILE_write_grads_data, rank error ", exchange_rank(r)%my_rank
           stop
         end select
        end do
      end do
    end do
  end do

  write(OFID, rec=1) out_buffer

  write(0,*) "write grads data OK, data size = ",size(out_buffer), size(out_buffer)*4
  close(OFID)

  deallocate(out_buffer)

end subroutine ppohMATHMP_FILE_write_grads_data

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get start step
integer function ppohMATHMP_FILE_get_start_step()
  implicit none
  
  ppohMATHMP_FILE_get_start_step = start_step

end function ppohMATHMP_FILE_get_start_step

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get end step
integer function ppohMATHMP_FILE_get_end_step()
  implicit none
  
  ppohMATHMP_FILE_get_end_step = end_step

end function ppohMATHMP_FILE_get_end_step


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! set current time and delta t
subroutine ppohMATHMP_FILE_start_integration()
  use jcup_interface, only : jcup_get_num_of_component, jcup_is_my_component, jcup_get_component_name, &
                             jcup_send_array
  implicit none
  integer :: start_flag(3)
  character(len=32) :: comp_name
  integer :: i

  start_flag(1) = start_step
  start_flag(2) = end_step
  start_flag(3) = delta_t

  do i = 1, jcup_get_num_of_component()

    if (jcup_is_my_component(i)) cycle ! skip my component

    comp_name = jcup_get_component_name(i)

    call jcup_send_array(FDM_MODEL_NAME, trim(comp_name), start_flag)

  end do

end subroutine ppohMATHMP_FILE_start_integration

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! set current time and delta t
subroutine ppohMATHMP_FILE_set_time()
  use jcup_interface, only : jcup_set_time, jcup_inc_time
  implicit none

  call jcup_set_time(FDM_MODEL_NAME, time, delta_t)
  call jcup_inc_time(FDM_MODEL_NAME, time)

end subroutine ppohMATHMP_FILE_set_time


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! finalize coupling
subroutine ppohMATHMP_FILE_finalize(finalize_flag)
  use jcup_interface, only : jcup_coupling_end
  implicit none
  logical, intent(IN) :: finalize_flag

  close(LOG_FID)
  call jcup_coupling_end(time, finalize_flag)

end subroutine ppohMATHMP_FILE_finalize

end module m_ppohMATHMP_FILE


