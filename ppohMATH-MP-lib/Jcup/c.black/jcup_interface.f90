!====================================================================================================
!> @brief
!> jcup interface module 
!
!Copyright (c) 2011, arakawa@rist.jp
!All rights reserved.
!
module jcup_interface 
  use jcup_constant, only : NUM_OF_EXCHANGE_DATA, NUM_OF_EXCHANGE_GRID, REAL_DATA, DOUBLE_DATA
  use jcup_constant, only : DATA_1D, DATA_2D, DATA_25D, DATA_3D
  use jcup_constant, only : NAME_LEN
  use jcup_mpi_lib, only : jcup_get_myrank_global => jml_GetMyrankGlobal
  use jcup_mpi_lib, only : jcup_get_leader_rank => jml_GetLeaderRank
  use jcup_mpi_lib, only : jcup_get_comm_size => jml_GetCommSizeLocal
  use jcup_comp, only : jcup_get_num_of_component => get_num_of_total_component
  use jcup_comp, only : jcup_get_component_name => get_component_name
  use jcup_comp, only : jcup_is_my_component => is_my_component
  use jcup_comp, only : jcup_get_comp_num_from_name => get_comp_id_from_name
  use jcup_comp, only : jcup_is_model_running => is_model_running
  use jcup_time, only : time_type
  use jcup_config, only : jcup_get_num_of_send_data => get_num_of_send_data 
  use jcup_config, only : jcup_get_send_data_name => get_my_send_data_name
  use jcup_config, only : jcup_get_num_of_recv_data => get_num_of_recv_data 
  use jcup_config, only : jcup_get_recv_data_name => get_my_recv_data_name
  use jcup_data, only : jcup_varp_type => varp_type
  use jcup_data, only : jcup_varg_type => varg_type
  use jcup_grid_base, only : jcup_get_grid_info => get_grid_info
  use jcup_interpolation, only : interpolate_data
  use jcup_interpolation_interface, only : jcup_get_local_operation_index  => get_local_operation_index
  use jcup_interpolation_interface, only : jcup_get_send_grid_index => get_send_grid_index 
  use jcup_interpolation_interface, only : jcup_get_recv_grid_index => get_recv_grid_index
  use jcup_interpolation_interface, only : jcup_get_num_of_send_grid => get_num_of_send_grid
  use jcup_interpolation_interface, only : jcup_get_num_of_recv_grid => get_num_of_recv_grid
  use jcup_interpolation_interface, only : jcup_send_array => send_array_to_recv_model
  use jcup_interpolation_interface, only : jcup_recv_array => recv_array_from_send_model
  use jcup_interpolation_interface, only : jcup_send_coef => send_coef_to_recv_model
  use jcup_interpolation_interface, only : jcup_recv_coef => recv_coef_from_send_model
  use jcup_interpolation_interface, only : jcup_set_local_coef => set_local_coef
  use jcup_interpolation_interface, only : OPERATION_COEF, SEND_COEF, RECV_COEF
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: jcup_set_new_comp ! subroutine (component_name)
  public :: jcup_initialize   ! subroutine (component_name, is_call_mpi_init, log_level, log_stderr)
  public :: jcup_coupling_end ! subroutine (time_array, is_call_mpi_finalize)

  public :: jcup_log          ! subroutine (sub_name, log_string, log_level)
  public :: jcup_error        ! subroutine (sub_name, error_string)
  public :: jcup_suspend_log  ! subroutine ()
  public :: jcup_get_mpi_parameter ! subroutine (com_name, my_comm, my_group, my_size, my_rank)
  public :: jcup_get_model_id      ! subroutine (model_name, model_id)

  public :: jcup_get_myrank_global ! integer function (NONE)
  public :: jcup_get_leader_rank   ! integer function (component_id)
  public :: jcup_get_comm_size     ! integer function (component_id)
  public :: jcup_get_num_of_component ! integer function (NONE)
  public :: jcup_is_model_running     ! logical function (COMP_NAME)

  public :: jcup_get_component_name   ! character(len=name_len) function (component_id)
  public :: jcup_is_my_component ! logical function (component_id)
  public :: jcup_def_grid          ! subroutine (grid_index, model_name, grid_name, num_of_vgrid)
  public :: jcup_end_grid_def      ! subroutine ()
  public :: jcup_get_grid_info     ! subroutine (comp_name, grid_name, num_of_index, min_index, max_index) 2013.09.20 [ADD]
  public :: jcup_set_default_configuration ! subroutine (my_comp_name, send_comp_name, recv_mode, interval, time_lag, mapping_tag, exchange_tag)
  public :: jcup_def_varp          ! subroutine (data_type_ptr, comp_name, data_name, grid_name, num_of_data)
  public :: jcup_def_varg ! subroutine (data_type_ptr, comp_name, data_name, grid_name, num_of_data, 
                          ! send_model_name, send_data_name, recv_mode, interval, time_lag, mapping_tag, exchange_tag)
  public :: jcup_end_var_def       ! subroutine ()

  public :: jcup_init_time         ! subroutine (time_array) :: integer time_array(6)
  public :: jcup_set_time          ! subroutine (component_name, time_array, delta_t, is_exchange)
                                   ! subroutine (component_name, time_real, delta_t)  ! 2013.0910 [ADD]
  public :: jcup_set_mapping_table ! subroutine (my_comp_name, send_comp_name, send_grid_name, recv_comp_name, recv_grid_name, 
                                   !             mapping_tag, send_grid_index, recv_grid_index)
  public :: jcup_inc_time          ! subroutine (component_name, time_array)
  public :: jcup_put_data          ! subroutien (data_type, data, num_of_data)
  public :: jcup_get_data          ! subroutine (data_type, data, num_of_data, is_recv_ok)

  public :: jcup_write_restart     ! subroutine (file_id, time_array) 2013.05.29 [ADD]
  public :: jcup_read_restart      ! subroutine (file_id, time_array) 2013.05.29 [ADD]

  public :: jcup_send_data_immediately
  public :: jcup_recv_data_immediately

  public :: jcup_get_comp_num_from_name ! integer function (component_name)
  public :: jcup_get_num_of_send_data ! integer function (compnent_id or component_name or current_component)
  public :: jcup_get_send_data_name   ! character(len=NAME_LEN) function (component_name, data_index)
  public :: jcup_get_num_of_recv_data ! integer function (compnent_id or component_name or current_component)
  public :: jcup_get_recv_data_name   ! character(len=NAME_LEN) function (component_name, data_index)
  
  public :: jcup_varp_type
  public :: jcup_varg_type
  public :: jcup_get_local_operation_index ! subroutine (recv_comp_name, send_comp_name, tag, num_of_operation, operation_index,  
                                           !             send_data_index, recv_data_index, send_coef_index, recv_coef_index)
  public :: jcup_get_send_grid_index ! subroutine (recv_comp_id, send_cpmp_id, grid_tag, num_of_grid, grid_index) 2013.06.21 [ADD]
  public :: jcup_get_recv_grid_index ! subroutine (recv_comp_id, send_cpmp_id, grid_tag, num_of_grid, grid_index) 2013.06.21 [ADD]
  public :: jcup_get_num_of_send_grid
  public :: jcup_get_num_of_recv_grid
  public :: jcup_send_array ! subroutine (my_comp_name, recv_comp_name, array)
  public :: jcup_recv_array ! subroutine (my_comp_name, send_comp_name, array)
  public :: jcup_send_coef  ! subroutine (my_comp_name, recv_comp_name, coef)
  public :: jcup_recv_coef  ! subroutine (my_comp_name, send_comp_name, coef)
  public :: jcup_set_local_coef ! subroutine (my_comp_name, global_coef, local_coef, operation_type)
  public :: OPERATION_COEF, SEND_COEF, RECV_COEF

!--------------------------------   private  ---------------------------------!

  integer, private :: my_coupler
  integer, private :: current_domain

  logical, private :: is_Initialized
  logical, private :: is_SetGrid
  logical, private :: is_InitTime
  logical, private :: is_SetMappingTable
  logical, private :: is_Initialize_completed
  logical, private :: is_EndDef
  logical, private :: is_first_step
  logical, private :: is_EndVarDef
  logical, private :: is_init_conf 

  interface jcup_init_time
    module procedure jcup_init_time_int
  end interface

  interface jcup_set_time
    module procedure jcup_set_date_time_int, jcup_set_date_time_real
  end interface

  interface jcup_put_data
    module procedure  jcup_put_data_1d_double, jcup_put_data_25d_double
  end interface

  interface jcup_get_data
    module procedure jcup_get_data_1d_double, jcup_get_data_25d_double
  end interface


  integer, private, pointer :: send_table_checker(:,:), recv_table_checker(:,:) ! (my_comp_id, target_comp_id)
  integer, private, pointer :: send_mapping_tag(:,:), recv_mapping_tag(:,:) ! (my_comp_id, target_comp_id)
  integer, private, pointer :: my_send_grid_tag(:,:,:), my_recv_grid_tag(:,:,:) ! (my_comp_id, target_comp_id, grid_num)
  integer, private :: current_grid_tag
  integer, private :: current_comp_id ! current component id (this id is set in the subroutine jcup_set_time) 
  integer, private :: max_i_1d
  real(kind=8), pointer, private :: buffer_double1d(:,:) ! nx, num_of_exchange_data
  real(kind=8), pointer, private :: buffer_double25d(:,:) ! nx, num_of_2d_array

  logical, pointer, private :: recv_flag(:) ! recv flag for data recv
  logical, pointer, private :: is_initial_step(:) ! first step flag. valid for serial exchange only 

  integer, private :: rec_counter 

  type(time_type), private :: current_time

  integer :: max_num_of_exchange_data

  character(len=NAME_LEN) :: my_model_name

contains

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> set component name
!> @param[in] component_name name of component
subroutine jcup_set_new_comp(component_name)
  use jcup_comp, only : set_my_component
  implicit none
  character(len=*), intent(IN) :: component_name

  call set_my_component(component_name)

end subroutine jcup_set_new_comp

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> initialize Jcup
!> @param[in] model_name model name
!> @param[in] inCallInit flag call MPI_Init or not
subroutine jcup_initialize(model_name, isCallInit, log_level, log_stderr)
  use jcup_config, only : init_conf
  use jcup_comp, only : init_model_process, get_num_of_total_component, is_my_component, get_component_name
  use jcup_utils, only : set_log_level, init_log, put_log
  use jcup_buffer, only : init_buffer, buffer_check_write
  use jcup_time, only : time_type, init_all_time, init_each_time, set_time_data
  use jcup_data, only : init_data_def
  use jcup_grid, only : init_grid
  implicit none
  character(len=*),intent(IN) :: model_name ! main component name of my task 
  logical,optional,intent(IN) :: isCallInit
  integer, optional, intent(IN) :: log_level ! 0, 1, 2
  logical, optional, intent(IN) :: log_stderr 
  integer :: num_of_comp 
  type(time_type) :: time
  integer :: mdl
  integer :: opt_log_level
  logical :: opt_log_stderr

  is_InitTime = .false.
  is_SetGrid = .false.
  is_SetMappingTable = .false.
  is_Initialize_completed = .false.
  is_EndDef = .false.
  is_first_step = .true.
  is_EndVarDef = .false.
  is_init_conf = .false.
 

  if (present(isCallInit)) then
    call init_model_process(isCallInit)
  else
    call init_model_process(.true.)
  end if

  num_of_comp = get_num_of_total_component()

  max_num_of_exchange_data = NUM_OF_EXCHANGE_DATA ! set initial value 2013/04/02 

  call init_all_time(num_of_comp)


  do mdl = 1, num_of_comp
    call init_each_time(mdl, 1) ! the number of domain is set to 1
  end do

  call init_conf(num_of_comp)

  if (present(log_level)) then
    opt_log_level = log_level
  else
    opt_log_level = 0 ! default no output log
  end if

  if (present(log_stderr)) then
    opt_log_stderr = log_stderr
  else
    opt_log_stderr = .false. ! default no output stderr
  end if

  call set_log_level(opt_log_level, opt_log_stderr)

  call init_log(trim(model_name))


  call init_buffer()

  allocate(send_table_checker(num_of_comp, num_of_comp))
  send_table_checker(:,:) = 0
  allocate(recv_table_checker(num_of_comp, num_of_comp))
  recv_table_checker(:,:) = 0

  allocate(send_mapping_tag(num_of_comp, num_of_comp))
  send_mapping_tag(:,:) = 1
  allocate(recv_mapping_tag(num_of_comp, num_of_comp))
  recv_mapping_tag(:,:) = 1

  allocate(my_send_grid_tag(num_of_comp, num_of_comp, NUM_OF_EXCHANGE_GRID))
  my_send_grid_tag(:,:,:) = -9999
  allocate(my_recv_grid_tag(num_of_comp, num_of_comp, NUM_OF_EXCHANGE_GRID))
  my_recv_grid_tag(:,:,:) = -9999

  allocate(is_initial_step(num_of_comp)) 
  is_initial_step(:) = .true.

  call init_grid()

  is_Initialized = .true.

  rec_counter = 0

  max_i_1d = 1

  call set_time_data(current_time, 0, 0, 0, 0, 0, 0)
  current_time%delta_t = 0

  call put_log("coupler initialization compoleted ", 1)

  do mdl = 1, get_num_of_total_component()
    if (is_my_component(mdl)) then
      call put_log("assigned component name : "//trim(get_component_name(mdl)))
    end if
  end do

  my_model_name = model_name

end subroutine jcup_initialize

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> finalize Jcup
!> @param[in] time_array end time
!> @param[in] inCallFinalize flag call MPI_Finalize or not
subroutine jcup_coupling_end(time_array, isCallFinalize)
  use jcup_constant, only : ADVANCE_SEND_RECV
  use jcup_config, only : get_comp_exchange_type, set_current_conf
  use jcup_mpi_lib, only : jml_finalize!, jml_Send1D_m2c, jml_destruct_window
  use jcup_time, only : destruct_all_time
  use jcup_utils, only : finalize_log, put_log
  use jcup_buffer, only : buffer_check_write, destruct_buffer
  use jcup_comp, only : get_num_of_total_component, get_component_name, is_my_component
  implicit none
  integer, intent(IN) :: time_array(6)
  logical,optional,intent(IN) :: isCallFinalize
  character(len=NAME_LEN) :: component_name
  integer :: i, j

  call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
  call put_log("!!!!!!!!!!!!!!!!   COUPLER FINALIZE  !!!!!!!!!!!!!!! ", 1)
  call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)


  call put_log("check  extra data send", 1)
  do i = 1, get_num_of_total_component()
    if (is_my_component(i)) then
      call set_current_conf(i)
      component_name = get_component_name(i)
      call jcup_set_time(component_name, time_array, 0, IS_EXCHANGE=.false.)
      do j = 1, get_num_of_total_component()
        if (get_comp_exchange_type(i,j) == ADVANCE_SEND_RECV) then
          call put_log("!!!!!!!!!!!!!! extra data send start !!!!!!!!!!!!!", 1)
          call jcup_exchange_data_send(i, j, .true.)  ! send final step data, if recv model time lag == 1
        end if
      end do
    end if
  end do

  !!!!call buffer_check_write()

  call destruct_buffer()
  call destruct_all_time()

  call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
  call put_log("!!!!!!!!!!!!!!!!  COUPLING COMPLETED !!!!!!!!!!!!!!! ", 1)
  call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)

  call finalize_log()

  if (associated(buffer_double1d)) deallocate(buffer_double1d)
  if (associated(buffer_double25d)) deallocate(buffer_double25d)
  if (associated(send_table_checker)) deallocate(send_table_checker)
  if (associated(recv_table_checker)) deallocate(recv_table_checker)
  if (associated(send_mapping_tag)) deallocate(send_mapping_tag)
  if (associated(recv_mapping_tag)) deallocate(recv_mapping_tag)

  if (.not.present(isCallFinalize)) then
    call jml_finalize()
  else 
    if (isCallFinalize) call jml_finalize()
  end if

end subroutine jcup_coupling_end

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> abort jcup
!> @param[in] sub_name subroutine name
!> @param[in] error_str error message string
!> @param[in] log_level log level (1:standard, 2:detail)
subroutine jcup_log(sub_name, error_str, log_level)
  use jcup_utils, only : put_log
  character(len=*), intent(IN) :: sub_name
  character(len=*), intent(IN) :: error_str
  integer, optional, intent(IN) :: log_level
  integer :: ll

  ll = 2
  if (present(log_level)) ll = log_level

  call put_log("Sub["//trim(sub_name)//"] : "//trim(error_str), ll)

end subroutine jcup_log

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> abort jcup
!> @param[in] sub_name subroutine name
!> @param[in] error_str error message string

subroutine jcup_error(sub_name, error_str)
  use jcup_utils, only : Error
  character(len=*), intent(IN) :: sub_name
  character(len=*), intent(IN) :: error_str

  call Error(sub_name, error_str)

end subroutine jcup_error

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> suspend log output

subroutine jcup_suspend_log()
  use jcup_utils, only : set_log_level, NO_OUTPUT_LOG, NO_OUTPUT_STDERR

  call set_log_level(NO_OUTPUT_LOG, NO_OUTPUT_STDERR)

end subroutine jcup_suspend_log

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> get mpi parameters
!> @param[in] comp_name name of component
!> @param[out] my_comm communicator of the component
!> @param[out] my_group mpi group of the component
!> @param[out] my_size mpi size of the component
!> @param[out] my_rank mpi rank of the component
subroutine jcup_get_mpi_parameter(comp_name, my_comm, my_group, my_size, my_rank)
  use jcup_mpi_lib, only : jml_GetComm, jml_GetMyGroup, jml_GetCommSizeLocal, jml_GetMyRank, jml_GetCommNULL
  use jcup_comp, only : get_comp_id_from_name, is_my_component
  implicit none
  character(len=*), intent(IN) :: comp_name
  integer, intent(OUT) :: my_comm, my_group, my_size, my_rank
  integer :: comp_id

  if (trim(comp_name)=="GLOBAL") then
    call jcup_get_mpi_parameter_global(my_comm, my_size, my_rank)
    my_group = 0
  else

    comp_id = get_comp_id_from_name(comp_name)

    if (is_my_component(comp_id)) then
      my_comm = jml_GetComm(comp_id) 
    else
      my_comm = jml_GetCommNULL()
    end if

    my_group = jml_GetMyGroup(comp_id) 
    my_size  = jml_GetCommSizeLocal(comp_id) 
    my_rank  = jml_GetMyRank(comp_id)

  end if

end subroutine jcup_get_mpi_parameter

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_get_mpi_parameter_global(my_comm, my_size, my_rank)
  use jcup_mpi_lib, only : jml_GetCommGlobal, jml_GetCommSizeGlobal, jml_GetMyrankGlobal
  integer, intent(OUT) :: my_comm, my_size, my_rank

  my_comm = jml_GetCommGlobal()
  my_size = jml_GetCommSizeGlobal()
  my_rank = jml_GetMyrankGlobal()

end subroutine jcup_get_mpi_parameter_global

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> return my component id number 
!> @param[in] comp_name name of component
!> @param[out] model_id component id number 
subroutine jcup_get_model_id(model_name, model_id)
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: model_name
  integer, intent(OUT) :: model_id

  model_id = get_comp_id_from_name(model_name)

end subroutine jcup_get_model_id

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> set grid definition
!> @param[in] grid_index array of grid indexes
!> @param[in] model_name name of component
!> @param[in] grid_name name of grid
!> @param[in] num_of_vgrid number of data or number of vertical grid
subroutine jcup_def_grid(grid_index, model_name, grid_name, num_of_vgrid)
  use jcup_comp, only : get_num_of_my_component, is_my_component
  use jcup_grid, only : def_grid
  use jcup_utils, only : error, put_log, IntToStr
  implicit none
  integer, intent(IN) :: grid_index(:)
  character(len=*), intent(IN) :: model_name ! model (component) name
  character(len=*), intent(IN) :: grid_name
  integer, optional, intent(IN) :: num_of_vgrid
  integer :: i

  if (.not.is_my_component(model_name)) then
    call error("jcup_def_grid", "Component name : "//trim(model_name)//" is not defined")
  end if
  
  if (minval(grid_index) <= 0) then
    call error("jcup_def_grid", "grid index must be >= 1")
  end if

  if (present(num_of_vgrid)) then
    if (num_of_vgrid > max_num_of_exchange_data) then
      max_num_of_exchange_data = num_of_vgrid
    end if
  end if

  call fapp_start("jcup_def_grid", 1, 1)

  call def_grid(grid_index, model_name, grid_name)

  call fapp_stop("jcup_def_grid", 1, 1)

  max_i_1d = max(max_i_1d, size(grid_index))

  call put_log("jcup_def_grid : component name : "//trim(model_name)//", grid name : "//trim(grid_name)//", grid size : " &
                                //trim(IntToStr(size(grid_index))) &
               //", min : "//trim(IntToStr(minval(grid_index)))//", max : "//trim(IntToStr(maxval(grid_index))))

end subroutine jcup_def_grid

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> finalize grid definition
subroutine jcup_end_grid_def()
  use jcup_mpi_lib, only : jml_AllreduceMax
  use jcup_utils, only : put_log, IntToStr
  use jcup_grid, only : GetNumOfMyGrid, end_def
  implicit none
  integer :: num_of_grid
  integer :: sis, sie, sjs, sje
  integer :: grd
  integer :: max_i, max_j, max_k
  integer :: int_buffer(1)

  if (.not.is_Initialized) then
    call jcup_abnormal_end("jcup_SetGrid","jcup_Initialize not called")
  end if
  num_of_grid = GetNumOfMyGrid()

  int_buffer(1) = max_num_of_exchange_data

  call jml_AllreduceMax(int_buffer(1), NUM_OF_EXCHANGE_DATA)

  call put_log("jcup_end_grid_def : NUM_OF_EXCHANGE_DATA = "//trim(IntToStr(NUM_OF_EXCHANGE_DATA)))

  call fapp_start("jcup_end_grid_def", 1, 1)

  call end_def()

  call fapp_stop("jcup_end_grid_def", 1, 1)

  allocate(buffer_double1d(max_i_1d, NUM_OF_EXCHANGE_DATA))

  is_EndDef = .true.
  is_SetGrid = .true.

  call put_log("Grid definition completed")

end subroutine jcup_end_grid_def

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> define send data
!> @param[inout] data_type_ptr pointer of data_type variable
!> @param[in] data_name name of send/recv data
!> @param[in] grid_name name of grid 
!> @param[in] num_of_data number of data
subroutine jcup_def_varp(data_type_ptr, comp_name, data_name, grid_name, num_of_data)
  use jcup_constant, only : NO_GRID, STRING_LEN
  use jcup_utils, only : error, IntToStr
  use jcup_data, only : varp_type, def_varp
  use jcup_grid, only : get_my_grid_num, is_my_grid
  use jcup_data, only : init_data_def 
  implicit none
  type(varp_type), pointer :: data_type_ptr
  character(len=*), intent(IN) :: comp_name
  character(len=*), intent(IN) :: data_name
  character(len=*), intent(IN) :: grid_name
  integer, optional, intent(IN) :: num_of_data
  character(len=STRING_LEN) :: logstr
  integer :: grid_id

  if (index(data_name, "__") > 0) then
    call error("jcup_def_varp", "string __ is not allowed for data_name")
  end if

  if (.not.is_init_conf) then
    call init_data_def()
    is_init_conf = .true.
  end if

  if (.not.is_my_grid(grid_name)) then
    call error("jcup_def_varp", "Grid name : "//trim(grid_name)//" is not defined")
  end if

  grid_id = get_my_grid_num(grid_name)

  if (grid_id==NO_GRID) then
    call error("jcup_def_varp", "Grid name : "//trim(grid_name)//" is not defined")
  end if

  call fapp_start("jcup_def_varp", 1, 1)

  if (present(num_of_data)) then

    if (num_of_data > NUM_OF_EXCHANGE_DATA) then
      logstr = "parameter num_of_data must be <= NUM_OF_EXCHANGE_DATA. num_of_data = " &
               //trim(IntToStr(num_of_data))
      call error("jcup_def_varp", trim(logstr))
    end if

    if (num_of_data > 1) then
      if (associated(buffer_double25d)) then
        if (num_of_data > size(buffer_double25d,2)) then
          deallocate(buffer_double25d)
          allocate(buffer_double25d(max_i_1d, num_of_data))
        end if
      else
        allocate(buffer_double25d(max_i_1d, num_of_data))
      end if
    end if
    call def_varp(data_type_ptr, comp_name, data_name, grid_id, num_of_data)
  else 
    call def_varp(data_type_ptr, comp_name, data_name, grid_id)
  end if

  call fapp_stop("jcup_def_varp", 1, 1)

end subroutine jcup_def_varp

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> define recv data
!> @param[in] my_comp my component name
!> @param[in] send_com send component name
!> @param[in] recv_mode recieve mode
!> @param[in] interval recieve interval
!> @param[in] time_lag time lag
!> @param[in] mapping_tag mapping tag
!> @param[in] exchange_tag exchange_tag
subroutine jcup_set_default_configuration(my_comp, send_comp, recv_mode, interval, time_lag, mapping_tag, exchange_tag)
  use jcup_data, only : init_data_def, set_default_config
  implicit none
  character(len=*), intent(IN) :: my_comp, send_comp
  character(len=3), optional, intent(IN) :: recv_mode
  integer, optional, intent(IN) :: interval
  integer, optional, intent(IN) :: time_lag
  integer, optional, intent(IN) :: mapping_tag
  integer, optional, intent(IN) :: exchange_tag

  if (.not.is_init_conf) then
    call init_data_def()
    is_init_conf = .true.
  end if

  call set_default_config(my_comp, send_comp, recv_mode, interval, time_lag, mapping_tag, exchange_tag)

end subroutine jcup_set_default_configuration

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> define recv data
!> @param[inout] data_type_ptr pointer of data_type variable
!> @param[in] data_name name of send/recv data
!> @param[in] grid_name name of grid 
!> @param[in] num_of_data number of data
subroutine jcup_def_varg(data_type_ptr, comp_name, data_name, grid_name, num_of_data, &
                         send_model_name, send_data_name, recv_mode, interval, time_lag, mapping_tag, exchange_tag)
  use jcup_constant, only : NO_GRID, STRING_LEN
  use jcup_utils, only : error, IntToStr
  use jcup_data, only : varg_type, def_varg
  use jcup_grid, only : get_my_grid_num, is_my_grid
  use jcup_data, only : init_data_def 
  implicit none
  type(varg_type), pointer :: data_type_ptr
  character(len=*), intent(IN) :: comp_name
  character(len=*), intent(IN) :: data_name
  character(len=*), intent(IN) :: grid_name
  integer, optional, intent(IN) :: num_of_data
  character(len=*), intent(IN) :: send_model_name
  character(len=*), intent(IN) :: send_data_name
  character(len=3), optional, intent(IN) :: recv_mode
  integer, optional, intent(IN) :: interval
  integer, optional, intent(IN) :: time_lag
  integer, optional, intent(IN) :: mapping_tag
  integer, optional, intent(IN) :: exchange_tag

  character(len=STRING_LEN) :: logstr
  integer :: num_of_25d_data
  integer :: grid_id

  if (index(data_name, "__") > 0) then
    call error("jcup_def_varg", "string __ is not allowed for data_name")
  end if

  if (.not.is_init_conf) then
    call init_data_def()
    is_init_conf = .true.
  end if

  if (.not.is_my_grid(grid_name)) then
    call error("jcup_def_varg", "Grid name : "//trim(grid_name)//" is not defined")
  end if

  grid_id = get_my_grid_num(grid_name)

  if (grid_id==NO_GRID) then
    call error("jcup_def_varg", "Grid name : "//trim(grid_name)//" is not defined")
  end if

  call fapp_start("jcup_def_varg", 1, 1)

  if (present(num_of_data)) then

    if (num_of_data > NUM_OF_EXCHANGE_DATA) then
      logstr = "parameter num_of_data must be <= NUM_OF_EXCHANGE_DATA. num_of_data = " &
               //trim(IntToStr(num_of_data))
      call error("jcup_def_varg", trim(logstr))
    end if

    if (num_of_data > 1) then
      if (associated(buffer_double25d)) then
        if (num_of_data > size(buffer_double25d,2)) then
          deallocate(buffer_double25d)
          allocate(buffer_double25d(max_i_1d, num_of_data))
        end if
      else
        allocate(buffer_double25d(max_i_1d, num_of_data))
      end if
    end if
    num_of_25d_data = num_of_data
  else 
    num_of_25d_data = 1
    !write(0,*) "def var_g ", trim(data_name)
  end if

  call def_varg(data_type_ptr, comp_name, data_name, grid_id, num_of_25d_data, &
                send_model_name, send_data_name, recv_mode, &
                interval, time_lag, mapping_tag, exchange_tag)


  call fapp_stop("jcup_def_varg", 1, 1)

end subroutine jcup_def_varg

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> finalize data definition
subroutine jcup_end_var_def()
  use jcup_comp, only : get_num_of_total_component, is_my_component
  use jcup_config, only : exchange_send_config_info, exchange_recv_config_info, set_configuration
  use jcup_data, only : end_def_varp, end_def_varg, check_data_definition
  implicit none
  integer :: i

  call fapp_start("jcup_end_var_def", 1, 1)

  do i = 1, get_num_of_total_component()
    if (is_my_component(i)) then
      call end_def_varp(i)
      call end_def_varg(i)
    end if
  end do

  do i = 1, get_num_of_total_component()
    call exchange_send_config_info(i)
    call exchange_recv_config_info(i)
  end do

  call set_configuration()

  call check_data_definition()

  is_EndVarDef = .true.

  call fapp_stop("jcup_end_var_def", 1, 1)

end subroutine jcup_end_var_def

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> initialize coupling time
!> @param[in] time_array array of initial time
subroutine jcup_init_time_int(time_array)
  use jcup_utils, only : put_log, IntToStr
  use jcup_time, only : set_start_time, set_current_time
  use jcup_comp, only : get_num_of_total_component, is_my_component
  use jcup_grid, only : write_grid_mapping_info

  implicit none
  integer, intent(IN) :: time_array(6)
  integer :: yyyy, mo, dd, hh, mm, ss
  integer :: i

  if (.not.is_Initialized) then
    call jcup_abnormal_end("jcup_InitTime","jcup_Initialize not called")
  end if

  if (is_InitTime) then
    call jcup_abnormal_end("jcup_InitTime","init time double call")
  end if

  !!!!!!!call check_mapping_table_setting()

  call write_grid_mapping_info()

  yyyy = time_array(1) ; mo = time_array(2) ; dd = time_array(3)
  hh = time_array(4) ; mm = time_array(5) ; ss = time_array(6)

  do i = 1, get_num_of_total_component()
    !if (is_my_component(i)) then
      call set_start_time(i, 1, yyyy, mo, dd, hh, mm, ss)
      call set_current_time(i, 1, yyyy, mo, dd, hh, mm, ss)
    !end if
  end do

  is_InitTime = .true.

  call put_log("Time Initialize OK. Start Time : "//trim(IntToStr(yyyy))//"/"//trim(IntToStr(mo))//"/"//trim(IntToStr(dd)) &
             //"/"//trim(IntToStr(hh))//"/"//trim(IntToStr(mm))//"/"//trim(IntToStr(ss)))

end subroutine jcup_init_time_int

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> mapping table definition
!> @param[in] my_model_name name of my component
!> @param[in] send_model_name name of send component
!> @param[in] send_grid_name name of send grid
!> @param[in] recv_model_name name of recv component
!> @param[in] recv_grid_name name of recv grid
!> @param[in] mapping_tag tag number of this mapping table
!> @param[in] send_grid array of send grid indexes
!> @param[in] recv_grid array of recv grid indexes
subroutine jcup_set_mapping_table(my_model_name, &
                                  send_model_name, send_grid_name, recv_model_name,  recv_grid_name, mapping_tag, &
                                  send_grid, recv_grid)
  use jcup_constant, only : NUM_OF_EXCHANGE_GRID, MAX_GRID, NO_GRID
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_BcastLocal, jml_SendLeader, jml_RecvLeader, jml_GetMyrank, &
                           jml_GetLeaderRank
  use jcup_utils, only : put_log, IntToStr, error
  use jcup_grid, only : set_grid_mapping_1d, exchange_grid_mapping, &
                        send_grid_mapping, recv_grid_mapping, finish_grid_mapping
  use jcup_grid_base, only : get_grid_num, send_index2pe, recv_index2pe, get_grid_min_index, get_grid_max_index
  use jcup_comp, only : get_comp_id_from_name,is_my_component
  implicit none
  character(len=*), intent(IN)  :: my_model_name
  character(len=*), intent(IN)  :: send_model_name, send_grid_name
  character(len=*), intent(IN)  :: recv_model_name, recv_grid_name
  integer, intent(IN)           :: mapping_tag
  integer, intent(IN), optional :: send_grid(:), recv_grid(:)

  integer :: my_model_id, send_model_id, recv_model_id
  integer :: nrx, nry, nsg
  integer :: int_buffer(4)
  integer, allocatable :: send_table(:), recv_table(:)
  logical :: is_my_table
  integer :: map_num, send_grid_num, recv_grid_num
  integer :: i

  call put_log("set mapping table start : "//trim(send_model_name)//":"//trim(recv_model_name) &
              //", grid = "//trim(send_grid_name)//":"//trim(recv_grid_name),1)

  if (.not.is_SetGrid) then
    call jcup_abnormal_end("jcup_set_mapping_table","jcup_SetGrid not called")
  end if

  if (.not.is_EndVarDef) then
    call jcup_abnormal_end("jcup_set_mapping_table","jcup_set_varp, jcup_set_varg, jcup_end_var_def not called")
  end if

  map_num = mapping_tag

  send_grid_num = get_grid_num(send_model_name, send_grid_name)

  if (send_grid_num==NO_GRID) then
    call jcup_abnormal_end("jcup_set_mapping_table", "send model name "//trim(send_model_name)// &
                          " or grid name "//trim(send_grid_name)//" is not defined")
  end if
  
  recv_grid_num = get_grid_num(recv_model_name, recv_grid_name)

  if (recv_grid_num==NO_GRID) then
    call jcup_abnormal_end("jcup_set_mapping_table", "recv model name "//trim(recv_model_name)// &
                          " or grid name "//trim(recv_grid_name)//" is not defined")
  end if

  if (map_num>NUM_OF_EXCHANGE_GRID) then
     call jcup_abnormal_end("set_mapping_table", "mapping_tag must be <= " &
                            //trim(IntToStr(NUM_OF_EXCHANGE_GRID)))
  end if
 
  if (send_grid_num>MAX_GRID) then
     call jcup_abnormal_end("set_mapping_table", "send_grid_tag must be <= " &
                            //trim(IntToStr(MAX_GRID)))
  end if
 
  if (recv_grid_num>MAX_GRID) then
     call jcup_abnormal_end("set_mapping_table", "recv_grid_tag must be <= " &
                            //trim(IntToStr(MAX_GRID)))
  end if

 
  my_model_id   = get_comp_id_from_name(trim(my_model_name))
  send_model_id = get_comp_id_from_name(trim(send_model_name))
  recv_model_id = get_comp_id_from_name(trim(recv_model_name))

  is_my_table = present(send_grid)


  call fapp_start("jcup_set_mapping_table", 1, 1)


  if (is_my_table) then

    if (jml_isLocalLeader(my_model_id)) then ! 2012/04/12 T.Arakawa [ADD]
      if (minval(send_grid) < get_grid_min_index(send_model_id, send_grid_num)) then
        call error("jcup_set_mapping_table", "send_grid_index < defined grid index, check index")
      end if
      if (maxval(send_grid) > get_grid_max_index(send_model_id, send_grid_num)) then
        call error("jcup_set_mapping_table", "send_grid_index > defined grid index, check index")
      end if

      if (minval(recv_grid) < get_grid_min_index(recv_model_id, recv_grid_num)) then
        call error("jcup_set_mapping_table", "recv_grid_index < defined grid index, check index")
      end if
      if (maxval(recv_grid) > get_grid_max_index(recv_model_id, recv_grid_num)) then
        call error("jcup_set_mapping_table", "recv_grid_index > defined grid index, check index")
      end if
    end if

    nrx = size(send_grid)

  end if


  if (jml_GetLeaderRank(send_model_id) /= jml_GetLeaderRank(recv_model_id)) then
    call send_recv_index2pe()
  end if

 if ((is_my_component(send_model_id)).and.(.not.is_my_component(recv_model_id))) then

    if ((is_my_table).and.(jml_isLocalLeader(send_model_id))) then
      call send_grid_info()
    end if

    call set_mapping_table(send_table_checker(send_model_id,:), recv_model_id, map_num)
    call recv_grid_mapping(send_model_id, recv_model_id, map_num)
    my_send_grid_tag(send_model_id, recv_model_id, map_num) = send_grid_num

  end if    


  if ((is_my_component(recv_model_id)).and.(.not.is_my_component(send_model_id))) then

     if (is_my_table) then
       call set_grid_mapping_1d(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num, send_grid, recv_grid)
      else
        if (jml_isLocalLeader(recv_model_id)) then
          call recv_grid_info()
        else
          nrx = 1 
          allocate(send_table(1), recv_table(1))
        end if
        call set_grid_mapping_1d(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num, send_table, recv_table)
        deallocate(send_table, recv_table)

      end if

    call set_mapping_table(recv_table_checker(recv_model_id,:), send_model_id, map_num)
    call send_grid_mapping(send_model_id, recv_model_id, map_num)
    my_recv_grid_tag(recv_model_id, send_model_id, map_num) = recv_grid_num

  end if

  if (is_my_component(send_model_id).and.(is_my_component(recv_model_id))) then

    if (.not.is_my_table) return

    if (jml_GetLeaderRank(send_model_id)==jml_GetLeaderRank(recv_model_id)) then
      call set_grid_mapping_1d(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num, send_grid, recv_grid)
    else
      if (my_model_id==recv_model_id) then
        call set_grid_mapping_1d(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num, send_grid, recv_grid)
      else
        if (jml_isLocalLeader(send_model_id)) then
          call send_grid_info()
        end if

        if (jml_isLocalLeader(recv_model_id)) then
          call recv_grid_info()
        else
          allocate(send_table(1), recv_table(1))
        end if  

        call set_grid_mapping_1d(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num, send_table, recv_table)
        deallocate(send_table, recv_table)

      end if

    end if

    call exchange_grid_mapping(send_model_id, recv_model_id, map_num)
    my_send_grid_tag(send_model_id, recv_model_id, map_num) = send_grid_num
    my_recv_grid_tag(recv_model_id, send_model_id, map_num) = recv_grid_num

    call set_mapping_table(send_table_checker(send_model_id,:), recv_model_id, map_num)
    call set_mapping_table(recv_table_checker(recv_model_id,:), send_model_id, map_num)

  end if

  call finish_grid_mapping(send_model_id, recv_model_id, map_num, send_grid_num, recv_grid_num)

  call check_table_data_mismatch(send_model_id, recv_model_id, send_grid_num, recv_grid_num, map_num)


  call fapp_stop("jcup_set_mapping_table", 1, 1)


  call put_log("set mapping table end : "//trim(send_model_name)//":"//trim(recv_model_name) &
              //", table number = "//trim(IntToStr(map_num)) &
              //", grid number = "//trim(IntToStr(send_grid_num))//":"//trim(IntToStr(recv_grid_num)),1)

  is_Initialize_completed = .true.


  return

  contains

!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine send_recv_index2pe()
    implicit none

   call fapp_start("jcup_send_recv_index2pe", 1, 1)

  ! exchange index2pe 
   if (is_my_component(recv_model_id)) then
     if (jml_isLocalLeader(recv_model_id)) then
       call recv_index2pe(recv_model_id, send_model_id, send_grid_num)
     end if
   end if


   if (is_my_component(send_model_id)) then
     if (jml_isLocalLeader(send_model_id)) then
       call send_index2pe(send_model_id, send_grid_num, recv_model_id, recv_grid_num)
    end if
   end if

   call fapp_stop("jcup_send_recv_index2pe", 1, 1)

  end subroutine send_recv_index2pe

!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine send_grid_info()
    implicit none
      int_buffer(1) = nrx 
      call jml_SendLeader(int_buffer,1,1,recv_model_id-1)
      call jml_SendLeader(send_grid,1,nrx,recv_model_id-1)
      call jml_SendLeader(recv_grid,1,nrx,recv_model_id-1)
  end subroutine send_grid_info

!=======+=========+=========+=========+=========+=========+=========+=========+

  subroutine recv_grid_info()
    implicit none
          call jml_RecvLeader(int_buffer,1,1,send_model_id-1)
          nrx = int_buffer(1) 
          allocate(send_table(nrx), recv_table(nrx))
          call jml_RecvLeader(send_table,1,nrx,send_model_id-1)
          call jml_RecvLeader(recv_table,1,nrx,send_model_id-1)
 
  end subroutine recv_grid_info

end subroutine jcup_set_mapping_table


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_mapping_table(mapping_table_checker, model_num, grid_num)
  use jcup_constant, only : NUM_OF_EXCHANGE_GRID
  use jcup_utils, only : IntToStr
  implicit none
  integer, intent(INOUT) :: mapping_table_checker(:)
  integer, intent(IN) :: model_num, grid_num

  if (grid_num>NUM_OF_EXCHANGE_GRID) then
     call jcup_abnormal_end("set_mapping_table", "grid_tag must be <= " &
                            //trim(IntToStr(NUM_OF_EXCHANGE_GRID)))
  end if
 
  if (mapping_table_checker(model_num)/=grid_num-1) then
     write(0,*) mapping_table_checker(:)
     call jcup_abnormal_end("set_mapping_table", "mapping table check err, model:" &
                            //trim(IntToStr(model_num))//", index:"//trim(IntToStr(grid_num)))
  end if

  mapping_table_checker(model_num) = max(mapping_table_checker(model_num),grid_num)


end subroutine set_mapping_table

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_mapping_table(mapping_table_checker, model_num, grid_num)
  use jcup_constant, only : NUM_OF_EXCHANGE_GRID
  use jcup_utils, only : IntToStr
  implicit none
  integer, intent(INOUT) :: mapping_table_checker(:)
  integer, intent(IN) :: model_num, grid_num
  return

  if (grid_num>NUM_OF_EXCHANGE_GRID) then
     call jcup_abnormal_end("check_mapping_table", "grid_tag must be <= " &
                            //trim(IntToStr(NUM_OF_EXCHANGE_GRID)))
  end if

  if (mapping_table_checker(model_num)<grid_num) then
     call jcup_abnormal_end("check_mapping_table", "mapping table check err, model:" &
                            //trim(IntToStr(model_num))//", index:"//trim(IntToStr(grid_num)))
  end if

end subroutine check_mapping_table

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_table_data_mismatch(send_comp_id, recv_comp_id, &
                                     send_grid_id, recv_grid_id, &
                                     mapping_tag)
  use jcup_utils, only : error
  use jcup_config, only : send_data_conf_type, get_num_of_send_data, get_send_data_conf_ptr_from_id, &
                          recv_data_conf_type, get_num_of_recv_data, get_recv_data_conf_ptr_from_id
  use jcup_comp, only : is_my_component
  implicit none
  integer, intent(IN) :: send_comp_id, recv_comp_id
  integer, intent(IN) :: send_grid_id, recv_grid_id
  integer, intent(IN) :: mapping_tag
  type(send_data_conf_type), pointer :: sd
  type(recv_data_conf_type), pointer :: rd
  integer :: i, j
 
  if (is_my_component(send_comp_id)) then
    do i = 1, get_num_of_send_data(send_comp_id)
      sd => get_send_data_conf_ptr_from_id(send_comp_id, i)
      do j = 1, sd%num_of_my_recv_data
        rd => sd%my_recv_conf(j)
        if ((rd%model_id == recv_comp_id).and.(rd%mapping_tag == mapping_tag)) then
          if (sd%grid_id /= send_grid_id) then
            call error("check_table_data_mismatch", "data: "//trim(sd%name)// &
                       ", grid id mismatch!!! Check jcup_set_varp and jcup_set_mapping_table.") 
          end if
        end if
      end do
    end do
  end if

  if (is_my_component(recv_comp_id)) then
    do i = 1, get_num_of_recv_data(recv_comp_id)
      rd => get_recv_data_conf_ptr_from_id(recv_comp_id, i)
      if ((rd%send_model_id == send_comp_id).and.(rd%mapping_tag == mapping_tag)) then
        if (rd%grid_id /= recv_grid_id) then
          call error("check_table_data_mismatch", "data: "//trim(rd%name)// &
                     ", grid id mismatch!!! Check jcup_set_varg and jcup_set_mapping_table.") 
        end if
      end if
    end do
  end if

end subroutine check_table_data_mismatch

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_mapping_table_setting()
  use jcup_constant, only : NO_SEND_RECV
  use jcup_utils, only : error
  use jcup_comp, only : get_num_of_total_component, is_my_component, get_component_name
  use jcup_config, only : get_comp_exchange_type
  implicit none
  integer :: i, j

  do i = 1, get_num_of_total_component()
    if (is_my_component(i)) then
      do j = 1, get_num_of_total_component()
        if (get_comp_exchange_type(i,j) /= NO_SEND_RECV) then
          if (send_table_checker(i,j)==0.and.recv_table_checker(i,j)==0) then
            call error("check_mapping_table_setting", "subroutine jcup_set_mapping_table must be called on component : " &
                      //trim(get_component_name(i))//" and "//trim(get_component_name(j)))
          end if
        end if
      end do
    end if
  end do

end subroutine check_mapping_table_setting


!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> set current time
!> @param[in] component_name name of component
!> @param[in] time_real real time from start date
!> @param[in] delta_t delta t
!> @param[in] 
subroutine jcup_set_date_time_real(component_name, time_real, delta_t)
  use jcup_comp, only : get_comp_id_from_name
  use jcup_time, only : time_type, get_start_time, TimeToSecond, SecondToTime
  implicit none
  character(len=*), intent(IN) :: component_name
  real(kind=8), intent(IN) :: time_real
  real(kind=8), intent(IN) :: delta_t
  type(time_type) :: start_time
  integer(kind=8) :: time_sec
  integer :: comp_id
  integer :: time_array(6)

  comp_id = get_comp_id_from_name(component_name)
  call get_start_time(comp_id, 1, start_time)

  time_sec = TimeToSecond(start_time)
  time_sec = time_sec + int(time_real)
  start_time = SecondToTime(time_sec)

  time_array(1) = start_time%yyyy
  time_array(2) = start_time%mo
  time_array(3) = start_time%dd
  time_array(4) = start_time%hh
  time_array(5) = start_time%mm
  time_array(6) = start_time%ss

  call jcup_set_date_time_int(component_name, time_array, int(delta_t))

end subroutine jcup_set_date_time_real

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> set current time
!> @param[in] component_name name of component
!> @param[in] time_array array of current time
!> @param[in] delta_t delta t
!> @param[in] 
subroutine jcup_set_date_time_int(component_name, time_array, delta_t, is_exchange)
  use jcup_constant, only : ADVANCE_SEND_RECV, BEHIND_SEND_RECV
  use jcup_utils, only : put_log, LongIntToStr, IntToStr
  use jcup_time, only : set_current_time, get_current_time, get_before_time, time_type, set_time_data, operator(==)
  use jcup_buffer, only : remove_past_send_data, remove_past_recv_data
  use jcup_comp, only : get_comp_id_from_name, get_num_of_total_component, is_my_component, get_component_name
  use jcup_config, only : set_current_conf, get_comp_exchange_type
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  implicit none
  character(len=*), intent(IN) :: component_name
  integer, intent(IN) :: time_array(6)
  integer, intent(IN) :: delta_t
  logical, optional :: is_exchange
  integer :: yyyy, mo, dd, hh, mm, ss
  type(time_type) :: time
  integer :: comp_id
  integer :: comp

  call fapp_start("jcup_set_time", 1, 1)

  yyyy = time_array(1) ; mo = time_array(2) ; dd = time_array(3)
  hh = time_array(4) ; mm = time_array(5) ; ss = time_array(6)

  call put_log("Set Current Time : "//trim(IntToStr(yyyy))//"/"//trim(IntToStr(mo))//"/"//trim(IntToStr(dd)) &
             //"/"//trim(IntToStr(hh))//"/"//trim(IntToStr(mm))//"/"//trim(IntToStr(ss)) &
             // ", Delta T : "//trim(IntToStr(delta_t))//", component : "//trim(component_name))

  comp_id = get_comp_id_from_name(component_name)

  do comp = 1, get_num_of_total_component() ! set current time to all my component
    if (comp_id == comp) cycle ! skip to set my component time to avoid double setting
    if (is_my_component(comp)) then
      if (jcup_is_set_time(comp, time_array)) then ! 
        call put_log("set current time : "//trim(IntToStr(yyyy))//"/"//trim(IntToStr(mo))//"/"//trim(IntToStr(dd)) &
                     //"/"//trim(IntToStr(hh))//"/"//trim(IntToStr(mm))//"/"//trim(IntToStr(ss)) &
                     //", component : "//trim(get_component_name(comp)))
        call set_current_time(comp, 1, yyyy, mo, dd, hh, mm, ss)
      end if
    end if
  end do

  call set_current_conf(comp_id) 
  current_comp_id = comp_id

  call set_current_time(comp_id, 1, yyyy,mo,dd,hh,mm,ss, delta_t) ! set time and delta t of current component
  call get_current_time(comp_id, 1, time)


  if (current_time == time) then
    call put_log("Same Time : "//trim(IntToStr(yyyy))//"/"//trim(IntToStr(mo))//"/"//trim(IntToStr(dd)) &
             //"/"//trim(IntToStr(hh))//"/"//trim(IntToStr(mm))//"/"//trim(IntToStr(ss)) &
             // " has been set. PARALLEL SEND RECV skipped")
  else
    if (present(is_exchange)) then
      if (is_exchange) then
        call jcup_exchange_data_parallel()
      end if
    else
      call jcup_exchange_data_parallel()
    end if
  end if

  call set_current_conf(comp_id) 
  current_comp_id = comp_id

  if (present(is_exchange)) then
    if (is_exchange) then
      do comp = 1, get_num_of_total_component()
        if (comp_id == comp) cycle
        if ((get_comp_exchange_type(comp_id, comp) == ADVANCE_SEND_RECV).or. &
            (get_comp_exchange_type(comp_id, comp) == BEHIND_SEND_RECV)) then
          call jcup_exchange_data_serial(comp_id, comp)
        end if
      end do
    end if
  else
    do comp = 1, get_num_of_total_component()
      if (comp_id == comp) cycle
      if ((get_comp_exchange_type(comp_id, comp) == ADVANCE_SEND_RECV).or. &
          (get_comp_exchange_type(comp_id, comp) == BEHIND_SEND_RECV)) then
        call jcup_exchange_data_serial(comp_id, comp)
      end if
    end do
  end if
  call remove_past_recv_data(time, comp_id)
  call get_before_time(comp_id, 1, time)
  call remove_past_send_data(time, comp_id)

  call set_time_data(current_time, yyyy, mo, dd, hh, mm, ss)

  call fapp_stop("jcup_set_time", 1, 1)

end subroutine jcup_set_date_time_int

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function jcup_is_set_time(comp_id, itime)
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  use jcup_time, only : time_type, get_current_time, get_delta_t, TimeToSecond 
  implicit none
  integer, intent(IN) :: comp_id
  integer, intent(IN) :: itime(6)
  type(time_type) :: c_time
  integer         :: delta_t
  type(time_type) :: n_time
  integer(kind=8) :: c_time_sec, n_time_sec

  call get_current_time(comp_id, 1, c_time)
  call get_delta_t(comp_id, 1, delta_t)

  c_time_sec = TimeToSecond(c_time)+delta_t

  n_time%yyyy = itime(1)
  n_time%mo   = itime(2)
  n_time%dd   = itime(3)
  n_time%hh   = itime(4)
  n_time%mm   = itime(5)
  n_time%ss   = itime(6)

  n_time_sec = TimeToSecond(n_time)

  jcup_is_set_time = (c_time_sec == n_time_sec)

end function jcup_is_set_time

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> increment current time
!> @param[in] component_name name of component
!> @param[in] itime increment time
subroutine jcup_inc_time(component_name, itime)
  use jcup_time, only : get_current_time, time_type, TimeToSecond, get_delta_t, &
                        SecondToTime
  use jcup_comp, only : get_comp_id_from_name
  implicit none
  character(len=*), intent(IN) :: component_name
  integer, intent(INOUT) :: itime(6)
  type(time_type) :: time
  integer(kind=8) :: time_sec
  integer :: del_t
  integer :: comp_id

  comp_id = get_comp_id_from_name(component_name)

  call get_current_time(comp_id, 1, time)
  call get_delta_t(comp_id, 1, del_t)

  time_sec = TimeToSecond(time)
  time_sec = time_sec + del_t
  time = SecondToTime(time_sec)

  itime(1) = time%yyyy
  itime(2) = time%mo
  itime(3) = time%dd
  itime(4) = time%hh
  itime(5) = time%mm
  itime(6) = time%ss
  
end subroutine jcup_inc_time

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_abnormal_end(routine_name, message)
  use jcup_utils, only : error, put_log
  implicit none
  character(len=*),intent(IN) :: routine_name, message

  call put_log("!!! abnorman termination, jc_AbnormalEnd called", 1)

  call error(trim(routine_name),trim(message))

end subroutine jcup_abnormal_end

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_terminate_send_recv(routine_name, message)
  implicit none
  character(len=*), intent(IN) :: routine_name, message

  call jcup_abnormal_end(trim(routine_name),trim(message))

end subroutine jcup_terminate_send_recv

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_exchange_data_send(send_comp_id, recv_comp_id, is_final_step)
  use jcup_constant, only : STRING_LEN
  use jcup_utils, only : put_log, IntToStr, error, NO_OUTPUT_LOG, get_log_level
  use jcup_config, only : get_num_of_recv_data, get_recv_data_conf_ptr, &
                          recv_data_conf_type, is_source_model, &
                          is_my_exchange_step, is_send_step_data
  use jcup_comp, only : get_num_of_total_component, get_component_name
  use jcup_data, only : get_send_data_dimension, get_num_of_exchange_send_data
  implicit none
  integer, intent(IN) :: send_comp_id
  integer, intent(IN) :: recv_comp_id
  logical, intent(IN) :: is_final_step

  integer :: i,j,mdl
  integer :: my_model
  integer :: d, dd
  type(recv_data_conf_type), pointer :: rd, rrdd
  character(len=NAME_LEN) :: data_name(NUM_OF_EXCHANGE_DATA)
  character(len=NAME_LEN) :: average_data_name(NUM_OF_EXCHANGE_DATA)
  logical :: is_average(NUM_OF_EXCHANGE_DATA)
  integer :: num_of_data
  integer :: num_of_2d_array
  character(len=STRING_LEN) :: log_str
  integer :: exchange_data_id

  my_model = send_comp_id

  i = recv_comp_id

    if (i /= my_model) then

      recv_flag(:) = .false.
      num_of_data = 0

      do d = 1, get_num_of_recv_data(i)
        if (recv_flag(d)) cycle
        rd => get_recv_data_conf_ptr(get_component_name(i),d)
        if (.not.rd%is_recv) cycle

        if (trim(rd%send_model)==trim(get_component_name(my_model))) then

          if (rd%time_lag == 0) cycle ! skip when time lag == 0

          if ((is_final_step).and.(rd%time_lag /= 1)) cycle ! skip final step send if target time_lag /= 1

          if ((is_first_step).and.(rd%time_lag==1)) cycle ! skip first step send if target time_lag == 1

          if (is_send_step_data(rd%model_id, rd%send_data)) then

            ! set first data name
            num_of_data = 1
            data_name(num_of_data) = trim(rd%send_data)
            exchange_data_id = rd%data_id

            average_data_name(num_of_data) = data_name(num_of_data)
            is_average(num_of_data) = rd%is_average
            if (rd%is_average) then
              average_data_name(num_of_data) = trim(get_average_data_name(rd%send_data, rd%model_id, rd%name))
            end if
         
            ! count number of data
            if (get_send_data_dimension(send_comp_id, rd%send_data) == DATA_25D) then
              num_of_2d_array = get_num_of_exchange_send_data(send_comp_id, rd%send_data)
            else

              do dd = d+1, get_num_of_recv_data(i)
                rrdd => get_recv_data_conf_ptr(get_component_name(i),dd)
                if (.not.rrdd%is_recv) cycle
                if (trim(rrdd%send_model)==trim(get_component_name(current_comp_id))) then
                  if (rd%recv_tag == rrdd%recv_tag ) then
                    recv_flag(dd) = .true.
                    num_of_data = num_of_data+1
                    if (num_of_data > NUM_OF_EXCHANGE_DATA) then
                      call error("jcup_exchange_data_send", &
                      "The number of send data must be <= NUM_OF_EXCHANGE_DATA. Check and modify configure file exchange_tag")
                    end if
                    data_name(num_of_data) = trim(rrdd%send_data)
                    average_data_name(num_of_data) = data_name(num_of_data)
                    is_average(num_of_data) = rrdd%is_average
                    if (rrdd%is_average) then
                      average_data_name(num_of_data) = trim(get_average_data_name(rrdd%send_data, rrdd%model_id, rrdd%name))
                    end if
                  end if
                end if
              end do
            end if

            call put_log("SEND DATA START! dest model, "//trim(get_component_name(i)) &
                        //", number of send data:"//trim(IntToStr(num_of_data)), 1)

            if (get_log_level() /= NO_OUTPUT_LOG) then
              log_str = "SEND DATA NAME : "//trim(data_name(1))
              do dd = 2, num_of_data
                log_str = trim(log_str)//", "//trim(data_name(dd))
              end do
              call put_log(trim(log_str),1)
            end if

            select case(get_send_data_dimension(send_comp_id, rd%send_data))
            case (DATA_1D)
              call jcup_exchange_data_1d_double(get_component_name(i), data_name, average_data_name, &
                                                num_of_data, exchange_data_id, is_average)
            case (DATA_25D)
              call jcup_exchange_data_25d_double(get_component_name(i), data_name(1), average_data_name(1), &
                                                 num_of_2d_array, exchange_data_id, is_average(1))
            case default
              call error("jcup_exchange_data_send","data dimension error")
            end select
            call put_log("SEND DATA FINISH! dest model, "//trim(get_component_name(i)), 1) 
          end if
        end if
      end do
    end if

end subroutine jcup_exchange_data_send

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_exchange_data_parallel()
  use jcup_constant, only : DATA_2D, DATA_3D, CONCURRENT_SEND_RECV, NO_SEND_RECV, &
                            ADVANCE_SEND_RECV, BEHIND_SEND_RECV, IMMEDIATE_SEND_RECV
  use jcup_utils, only : put_log, IntToStr
  use jcup_config, only : get_num_of_recv_data, is_my_exchange_step, &
                          get_comp_exchange_type, get_current_comp_id, get_num_of_recv_data, &
                          set_current_conf, get_comp_name_from_comp_id, &
                          get_comp_exchange_type, is_exchange_step
  use jcup_comp, only : get_num_of_total_component, is_my_component
  use jcup_time, only : get_current_time

  implicit none
  integer :: i,j,mdl
  integer :: my_model
  integer :: num_of_data
  character(len=14) :: time_str
  type(time_type) :: c_time
  integer :: max_flag_size
  integer :: send_comp_id, recv_comp_id
  integer :: temp_current_comp, temp_target_comp
  integer :: exchange_type

  call fapp_start("jcup_exchange_data", 1, 1)

  call get_current_time(get_current_comp_id(), 1, time_str)
  call get_current_time(get_current_comp_id(), 1, c_time)
  !write(110+jml_GetMyrankGlobal(),*) "exchange data 2 3 ", get_current_comp_id()

  if (is_first_step) then
    max_flag_size = 0
    do i = 1, get_num_of_total_component()
      max_flag_size = max(max_flag_size, get_num_of_recv_data(i))
    end do
    allocate(recv_flag(max_flag_size))
  end if

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!! DATA EXCHANGE START !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!  PARALLEL  EXCHANGE !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!    "//time_str//"   !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("exchange component : "//trim(get_comp_name_from_comp_id(current_comp_id)))
 
    ! local data exchange
    do i = 1, get_num_of_total_component()
      do j = i+1, get_num_of_total_component()
        if (is_my_component(i).or.is_my_component(j)) then
          if (get_comp_exchange_type(i,j) == CONCURRENT_SEND_RECV) then 
            send_comp_id = i 
            recv_comp_id = j

            if (is_exchange_step(send_comp_id, recv_comp_id, c_time)) then
              call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  !!!!!!!!!!!!!! ", 1)
              call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
              call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
              call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH !!!!!!!!!!!!!! ", 1)
            end if
            send_comp_id = j
            recv_comp_id = i
            if (is_exchange_step(send_comp_id, recv_comp_id, c_time)) then
              !!call set_current_conf(temp_current_comp)
              call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  !!!!!!!!!!!!!! ", 1)
              call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
              call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
              call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH !!!!!!!!!!!!!! ", 1)
              !!call set_current_conf(current_comp_id)
            end if
          end if
        end if
      end do
    end do    

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!  PARALLEL  EXCHANGE  !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!! DATA EXCHANGE FINISH !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)


  if (is_first_step) is_first_step = .false.

  !write(0,*) "exchange data finish, ", my_model, jml_GetMyrank()

  call fapp_stop("jcup_exchange_data", 1, 1)

end subroutine jcup_exchange_data_parallel

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_exchange_data_serial(my_comp_id, target_comp_id)
  use jcup_constant, only : DATA_2D, DATA_3D, CONCURRENT_SEND_RECV, NO_SEND_RECV, &
                            ADVANCE_SEND_RECV, BEHIND_SEND_RECV, IMMEDIATE_SEND_RECV
  use jcup_utils, only : put_log, IntToStr
  use jcup_config, only : get_num_of_recv_data, is_my_exchange_step, &
                          get_comp_exchange_type, get_current_comp_id, get_num_of_recv_data, &
                          set_current_conf, get_comp_name_from_comp_id, &
                          get_comp_exchange_type
  use jcup_comp, only : get_num_of_total_component, is_my_component
  use jcup_time, only : get_current_time

  implicit none
  integer, intent(IN) :: my_comp_id, target_comp_id

  integer :: i,j,mdl
  integer :: my_model
  integer :: num_of_data
  character(len=14) :: time_str
  type(time_type) :: c_time
  integer :: max_flag_size
  integer :: send_comp_id, recv_comp_id
  integer :: temp_current_comp, temp_target_comp
  integer :: exchange_type

  !!!!write(0,*) "jcup_exchange_data_serial 1 ", my_comp_id, target_comp_id, current_comp_id

  call get_current_time(get_current_comp_id(), 1, time_str)
  call get_current_time(get_current_comp_id(), 1, c_time)
  !write(110+jml_GetMyrankGlobal(),*) "exchange data 2 3 ", get_current_comp_id()

  if (is_first_step) then
    max_flag_size = 0
    do i = 1, get_num_of_total_component()
      max_flag_size = max(max_flag_size, get_num_of_recv_data(i))
    end do
    allocate(recv_flag(max_flag_size))
  end if

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!! DATA EXCHANGE START !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!   SERIAL  EXCHANGE  !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!    "//time_str//"   !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("exchange component : "//trim(get_comp_name_from_comp_id(target_comp_id)))

    ! local data exchange
    if (is_my_component(target_comp_id)) then
      if (is_initial_step(my_comp_id)) then
        is_initial_step(my_comp_id) = .false.
        return
      end if
      send_comp_id = target_comp_id
      recv_comp_id = my_comp_id
      call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  !!!!!!!!!!!!!! ", 1)
      call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
      call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
      call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH !!!!!!!!!!!!!! ", 1)
    end if

  !!!!!write(0,*) "jcup_exchange_data_serial 2 "

    if ((get_comp_exchange_type(my_comp_id, target_comp_id) == ADVANCE_SEND_RECV)) then
      if (is_initial_step(my_comp_id)) then
        is_initial_step(my_comp_id) = .false.
        send_comp_id = target_comp_id
        recv_comp_id = my_comp_id

  !!!!write(0,*) "jcup_exchange_data_serial 3 ", my_comp_id, send_comp_id, recv_comp_id

        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  !!!!!!!!!!!!!! ", 1)
        call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
        call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH !!!!!!!!!!!!!! ", 1)
      else
        send_comp_id = my_comp_id
        recv_comp_id = target_comp_id

  !!!!!write(0,*) "jcup_exchange_data_serial 4 ", my_comp_id, send_comp_id, recv_comp_id

        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  !!!!!!!!!!!!!! ", 1)
        call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
        call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH !!!!!!!!!!!!!! ", 1)
        send_comp_id = target_comp_id
        recv_comp_id = my_comp_id

  !!!!!write(0,*) "jcup_exchange_data_serial 4.5 ", my_comp_id, send_comp_id, recv_comp_id

        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  !!!!!!!!!!!!!! ", 1)
        call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
        call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
        call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH !!!!!!!!!!!!!! ", 1)
       end if
    else ! BEHIND_SEND_RECV
      send_comp_id = my_comp_id
      recv_comp_id = target_comp_id

  !!!!write(0,*) "jcup_exchange_data_serial 5 ", my_comp_id, send_comp_id, recv_comp_id

      call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  !!!!!!!!!!!!!! ", 1)
      call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
      call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
      call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH !!!!!!!!!!!!!! ", 1)
      send_comp_id = target_comp_id
      recv_comp_id = my_comp_id
      call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE START  !!!!!!!!!!!!!! ", 1)
      call put_log("excahnge component id : "//trim(IntToStr(send_comp_id))//":"//trim(IntToStr(recv_comp_id)))
      call jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, .false.)
      call put_log("!!!!!!!!!!!!!!!! LOCAL EXCHANGE FINISH !!!!!!!!!!!!!! ", 1)
    end if

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!   SERIAL  EXCHANGE   !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!! DATA EXCHANGE FINISH !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)


  if (is_first_step) is_first_step = .false.

  !write(0,*) "exchange data finish, ", my_model, jml_GetMyrank()

end subroutine jcup_exchange_data_serial

!=======+=========+=========+=========+=========+=========+=========+=========+



subroutine jcup_exchange_data_local(send_comp_id, recv_comp_id, c_time, is_final_step)
  use jcup_constant, only : STRING_LEN
  use jcup_mpi_lib, only : jml_GetMyrankGlobal, jml_GetCommSizeLocal
  use jcup_utils, only : put_log, IntToStr, error, NO_OUTPUT_LOG, get_log_level
  use jcup_config, only : set_current_conf, get_num_of_recv_data, get_recv_data_conf_ptr, &
                          recv_data_conf_type, is_source_model, &
                          is_my_exchange_step, is_send_step_data, is_recv_step_data
  use jcup_comp, only : get_num_of_total_component, &
                        get_component_name, &
                        is_model_running, is_my_component
  use jcup_data, only : get_send_data_dimension, get_recv_data_dimension, &
                        get_num_of_exchange_send_data, get_num_of_exchange_recv_data
  implicit none
  integer, intent(IN) :: send_comp_id, recv_comp_id
  type(time_type), intent(IN) :: c_time
  logical, intent(IN) :: is_final_step

  integer :: d, dd
  type(recv_data_conf_type), pointer :: rd, rrdd
  character(len=NAME_LEN) :: send_data_name(NUM_OF_EXCHANGE_DATA)
  character(len=NAME_LEN) :: average_send_data_name(NUM_OF_EXCHANGE_DATA)
  character(len=NAME_LEN) :: recv_data_name(NUM_OF_EXCHANGE_DATA)
  logical :: is_average(NUM_OF_EXCHANGE_DATA)
  integer :: num_of_data
  integer :: num_of_2d_array
  character(len=STRING_LEN) :: log_str
  integer :: exchange_data_id
  integer :: target_comp
  integer :: data_dimension
  logical :: is_exchange_step

    recv_flag(:) = .false.
    num_of_data = 0

    !!!write(0,*) "exchange_data_local_new 1 ", jml_GetMyrankGlobal(), send_comp_id, recv_comp_id

    do d = 1, get_num_of_recv_data(recv_comp_id)
      if (recv_flag(d)) cycle

      rd => get_recv_data_conf_ptr(get_component_name(recv_comp_id),d)

      if (.not.rd%is_recv) cycle
  
        current_comp_id = send_comp_id

        call set_current_conf(current_comp_id)

        !!!write(0,*) "jcup_exchange_data_local_new 3 ", jml_GetMyrankGlobal(), current_comp_id

        if (.not.is_my_exchange_step(c_time)) then
          call put_log("Current time is not exchange step, data send skip. Component ID : " & 
                       //trim(IntToStr(send_comp_id)))
          cycle
        end if

        if (rd%send_model_id==send_comp_id) then

          if (rd%time_lag == 0) cycle ! skip when time lag == 0

          if (is_my_component(send_comp_id)) then
            if ((is_final_step).and.(rd%time_lag /= 1)) cycle ! skip final step send if target time_lag /= 1
            !if ((is_first_step).and.(rd%time_lag==1)) cycle ! skip first step send if target time_lag == 1
          end if


          if (is_my_component(send_comp_id)) then ! 2013/04/08 T.Arakawa [MOD]
            is_exchange_step = is_send_step_data(rd%model_id, rd%send_data)
          else
            call set_current_conf(recv_comp_id)
            is_exchange_step = is_recv_step_data(rd%name)
            call set_current_conf(current_comp_id)
          end if

          if (is_exchange_step) then

          !if (is_send_step_data(rd%model_id, rd%send_data)) then

            ! set first data name
            num_of_data = 1
            send_data_name(num_of_data) = trim(rd%send_data)
            recv_data_name(num_of_data) = trim(rd%name)
            exchange_data_id = rd%data_id

            data_dimension = 9999
            if (is_my_component(send_comp_id)) then
              data_dimension = get_send_data_dimension(send_comp_id, rd%send_data)
            end if
            if (is_my_component(recv_comp_id)) then
              data_dimension = get_recv_data_dimension(recv_comp_id, rd%name)
            end if

        !!!write(0,*) "jcup_exchange_data_local_new 3.5 ", current_comp_id, data_dimension, DATA_25D

            average_send_data_name(num_of_data) = send_data_name(num_of_data)
            is_average(num_of_data) = rd%is_average

            if (rd%is_average) then
              average_send_data_name(num_of_data) = trim(get_average_data_name(rd%send_data, rd%model_id, rd%name))
            end if
         
            ! count number of data
            if (data_dimension == DATA_25D) then
        !!!write(0,*) "jcup_exchange_data_local_new 3.6 ", current_comp_id, data_dimension, DATA_25D, send_comp_id, jml_GetMyrankGlobal()
              if (is_my_component(send_comp_id)) then
                num_of_2d_array = get_num_of_exchange_send_data(send_comp_id, rd%send_data)
              else
                num_of_2d_array = get_num_of_exchange_recv_data(recv_comp_id, rd%name)
              end if

        !!!write(0,*) "jcup_exchange_data_local_new 3.7 ", current_comp_id, data_dimension, DATA_25D
            else

              do dd = d+1, get_num_of_recv_data(recv_comp_id)
                rrdd => get_recv_data_conf_ptr(get_component_name(recv_comp_id),dd)
                if (.not.rrdd%is_recv) cycle
                if (rrdd%send_model_id==send_comp_id) then
                  if (rd%recv_tag == rrdd%recv_tag ) then
                    recv_flag(dd) = .true.
                    num_of_data = num_of_data+1
                    if (num_of_data > NUM_OF_EXCHANGE_DATA) then
                      call error("jcup_exchange_data_send", &
                      "The number of send data must be <= NUM_OF_EXCHANGE_DATA. Check and modify configure file exchange_tag")
                    end if
                    send_data_name(num_of_data) = trim(rrdd%send_data)
                    average_send_data_name(num_of_data) = send_data_name(num_of_data)
                    recv_data_name(num_of_data) = trim(rrdd%name)
                    is_average(num_of_data) = rrdd%is_average
                    if (rrdd%is_average) then
                      average_send_data_name(num_of_data) = trim(get_average_data_name(rrdd%send_data, rrdd%model_id, rrdd%name))
                    end if
                  end if
                end if
              end do
            end if

            call put_log("EXCHANGE DATA START! dest model, "//trim(get_component_name(recv_comp_id)) &
                        //", number of send data:"//trim(IntToStr(num_of_data)), 1)


            if (get_log_level() /= NO_OUTPUT_LOG) then
              log_str = "SEND DATA NAME : "//trim(send_data_name(1))
              do dd = 2, num_of_data
                log_str = trim(log_str)//", "//trim(send_data_name(dd))
              end do
              call put_log(trim(log_str),1)
            end if

        !!!write(0,*) "jcup_exchange_data_local_new 4 ", current_comp_id, jml_GetMyrankGlobal()
            select case(data_dimension)
            case (DATA_1D)
              call set_current_conf(current_comp_id)
              call jcup_exchange_data_1d_double(get_component_name(recv_comp_id), send_data_name, &
                                            average_send_data_name, &
                                            num_of_data, exchange_data_id, is_average)
              call set_current_conf(recv_comp_id)
              current_comp_id = recv_comp_id
              if (is_my_component(recv_comp_id)) then
                call jcup_interpolate_data_1d_double(trim(rd%send_model), recv_data_name, num_of_data, &
                                              exchange_data_id)
              end if
              current_comp_id = send_comp_id
              call set_current_conf(send_comp_id)
            case (DATA_25D)
              call set_current_conf(current_comp_id)
              call jcup_exchange_data_25d_double(get_component_name(recv_comp_id), send_data_name(1), &
                                             average_send_data_name(1), &
                                             num_of_2d_array, exchange_data_id, is_average(1))
              call set_current_conf(recv_comp_id)
              current_comp_id = recv_comp_id
              if (is_my_component(recv_comp_id)) then
                call jcup_interpolate_data_25d_double(trim(rd%send_model), recv_data_name(1), num_of_2d_array, &
                                               exchange_data_id)
              end if
              current_comp_id = send_comp_id
              call set_current_conf(send_comp_id)
            case default
              call error("jcup_exchange_data_local", "data dimension error")
            end select
            call put_log("EXCHANGE DATA FINISH! dest model, "//trim(get_component_name(recv_comp_id)), 1) 
          end if
        end if
      end do

end subroutine jcup_exchange_data_local

!=======+=========+=========+=========+=========+=========+=========+=========+

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_exchange_data_1d_double(dest_model_name, data_name, average_data_name, num_of_data, exchange_data_id, &
                                      is_average)
  use jcup_constant, only : DATA_2D
  use jcup_utils, only : error, put_log
  use jcup_grid_base, only : get_my_local_area
  use jcup_grid, only : set_data, exchange_data_comp
  use jcup_time, only : time_type, get_current_time, get_before_time
  use jcup_comp, only :  get_comp_id_from_name, is_my_component, get_component_name
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  use jcup_buffer, only : get_send_data_type, get_send_data
  use jcup_config, only : GetMappingTag
  implicit none
  character(len=*), intent(IN) :: dest_model_name
  character(len=NAME_LEN), intent(IN) :: data_name(:)
  character(len=NAME_LEN), intent(IN) :: average_data_name(:)
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: exchange_data_id
  logical, intent(IN) :: is_average(:)

  type(time_type) :: time
  integer :: recv_comp_id
  integer :: is, ie, js, je, ks, ke
  integer :: ni, nj
  integer :: d

  call fapp_start("jcup_exchange_data_1d", 1, 1)

  if (.not.jcup_isSendOK(trim(data_name(1)))) return
 
  call put_log("exchange_data_1d_double start. source model : "//trim(get_component_name(current_comp_id))//", dest model : " &
               //trim(dest_model_name))

  recv_comp_id = get_comp_id_from_name(trim(dest_model_name))

  !!!write(0,*) "jcup_exchange_data_1d_double_new ", current_comp_id, recv_comp_id

  call set_current_mapping_tag(current_comp_id, recv_comp_id, GetMappingTag(recv_comp_id, data_name(1)))

  if (is_my_component(current_comp_id)) then

    if ((is_first_step).or.(trim(data_name(1)) /= trim(average_data_name(1)))) then
      call get_current_time(current_comp_id, 1, time)
      call put_log("get_current_time ", 1)
    else 
      call get_before_time(current_comp_id, 1, time)
      call put_log("get_before_time ", 1)
    end if

    !!!write(0,*) "jcup_exchange_Data_1d_double_new 2 ", current_comp_id, current_grid_tag
    call get_my_local_area(current_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
    ni = ie-is+1

    !!!write(0,*) "jcup_exchange_Data_1d_double_new 3 "

    if (is_first_step) then
      do d = 1, num_of_data
        call get_send_data(buffer_double1d(1:ni,d), time, current_comp_id, &
                           get_send_data_id(recv_comp_id, data_name(d), is_average(d)), trim(data_name(d)))
      end do
    else
      do d = 1, num_of_data
        call get_send_data(buffer_double1d(1:ni,d), time, current_comp_id, &
                           get_send_data_id(recv_comp_id, data_name(d), is_average(d)), trim(average_data_name(d)))
      end do
    end if

    call set_data(buffer_double1d)

  end if

  call exchange_data_comp(current_comp_id, recv_comp_id, send_mapping_tag(current_comp_id, recv_comp_id), &
                           DOUBLE_DATA, num_of_data, exchange_data_id, DATA_1D)

  call put_log("exchange_data_1d_double completed. source model : "//trim(get_component_name(current_comp_id))//", dest model : " &
               //trim(dest_model_name))

  call fapp_stop("jcup_exchange_data_1d", 1, 1)

  !!write(0,*) "jcup_send_data_double_1d_new 2 ", jml_GetMyrankGlobal(), is_my_component(current_comp_id)
end subroutine jcup_exchange_data_1d_double

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_exchange_data_25d_double(dest_model_name, data_name, average_data_name, num_of_data, exchange_data_id, &
                                       is_average)
  use jcup_utils, only : error, put_log
  use jcup_grid_base, only : get_my_local_area
  use jcup_grid, only : set_data, send_data_1d, exchange_data_comp
  use jcup_time, only : time_type, get_current_time, get_before_time
  use jcup_comp, only : get_comp_id_from_name, is_my_component, get_component_name
  use jcup_buffer, only : get_send_data_type, get_send_data
  use jcup_config, only : GetMappingTag
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  implicit none
  character(len=*), intent(IN) :: dest_model_name
  character(len=NAME_LEN), intent(IN) :: data_name
  character(len=NAME_LEN), intent(IN) :: average_data_name
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: exchange_data_id
  logical, intent(IN) :: is_average

  type(time_type) :: time
  integer :: recv_comp_id
  integer :: is, ie, js, je, ks, ke
  integer :: ni, nj
  integer :: i

  if (.not.jcup_isSendOK(data_name)) return

  call put_log("exchange_data_25d_double start. source model : "//trim(get_component_name(current_comp_id))//", dest model : " &
               //trim(dest_model_name))

  recv_comp_id = get_comp_id_from_name(trim(dest_model_name))

  call set_current_mapping_tag(current_comp_id, recv_comp_id, GetMappingTag(recv_comp_id, data_name))

  if (is_my_component(current_comp_id)) then

    if ((is_first_step).or.(trim(data_name) /= trim(average_data_name))) then
      call get_current_time(current_comp_id, 1, time)
    else 
      call get_before_time(current_comp_id, 1, time)
    end if

    call get_my_local_area(current_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
    ni = ie-is+1

    if (is_first_step) then
      call get_send_data(buffer_double25d(1:ni,1:num_of_data), time, current_comp_id, &
                         get_send_data_id(recv_comp_id, data_name, is_average), data_name)
    else
      call get_send_data(buffer_double25d(1:ni,1:num_of_data), time, current_comp_id, &
                         get_send_data_id(recv_comp_id, data_name, is_average), average_data_name)
    end if

    call set_data(buffer_double25d)
  
  end if

  call exchange_data_comp(current_comp_id, recv_comp_id, send_mapping_tag(current_comp_id, recv_comp_id), &
                           DOUBLE_DATA, num_of_data, exchange_data_id, DATA_2D)


  call put_log("exchange_data_25d_double completed. source model : "//trim(get_component_name(current_comp_id))//", dest model : " &
               //trim(dest_model_name))

end subroutine jcup_exchange_data_25d_double

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_interpolate_data_1d_double(source_model_name, data_name, num_of_data, exchange_data_id)
  use jcup_utils, only : error, put_log
  use jcup_mpi_lib, only : jml_GetMyrankGlobal
  use jcup_grid_base, only : get_my_local_area
  use jcup_grid, only : interpolate_data_1d, get_data, recv_data
  use jcup_comp, only : get_comp_id_from_name, get_component_name
  use jcup_time, only : time_type, get_current_time
  use jcup_buffer, only : put_recv_data
  use jcup_config, only : GetRecvMappingTag, GetExchangeTag, get_recv_data_id_from_data_name
  implicit none
  character(len=*), intent(IN) :: source_model_name
  character(len=NAME_LEN), intent(IN) :: data_name(:)
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: exchange_data_id

  integer :: exchange_tag(NUM_OF_EXCHANGE_DATA)
  type(time_type) :: time
  integer :: i, j, counter
  integer :: is, ie, js, je, ks, ke
  integer :: ni, nj
  integer :: d
  integer :: source_comp_id

  if (.not.jcup_isRecvOK(trim(data_name(1)))) return

  call put_log("interpolate_data_1d_double start. source model : "//trim(source_model_name)//", dest model : " &
               //trim(get_component_name(current_comp_id)))


  do d = 1, num_of_data
    call jcup_check_recv_error(source_model_name, data_name(d))
  end do


  source_comp_id = get_comp_id_from_name(source_model_name)

  call set_current_mapping_tag(source_comp_id, current_comp_id, GetRecvMappingTag(RECV_DATA_NAME=trim(data_name(1))))

  do d = 1, num_of_data
    exchange_tag(d) = GetExchangeTag(trim(data_name(d)))
  end do

  call get_current_time(current_comp_id, 1, time)

  call interpolate_data_1d(current_comp_id, source_comp_id, recv_mapping_tag(current_comp_id, source_comp_id), &
                           DOUBLE_DATA, num_of_data, exchange_tag)

  buffer_double1d = 0.d0

  call get_data(current_comp_id, source_comp_id, recv_mapping_tag(current_comp_id, source_comp_id), &
                buffer_double1d(:,:), num_of_data)

  call get_my_local_area(current_comp_id, current_grid_tag, is, ie, js, je, ks, ke)

  ni = ie-is+1
  
  do d = 1, num_of_data
    call put_recv_data(buffer_double1d(1:ni,d), time, current_comp_id, &
                       get_recv_data_id_from_data_name(trim(data_name(d))), trim(data_name(d)))
  end do

  call put_log("interpolate_data_1d_double completed. source model : "//trim(source_model_name)//", dest model : " &
               //trim(get_component_name(current_comp_id)))

end subroutine jcup_interpolate_data_1d_double

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_interpolate_data_25d_double(source_model_name, data_name, num_of_data, exchange_data_id)
  use jcup_utils, only : error, put_log
  use jcup_grid_base, only : get_my_local_area
  use jcup_grid, only : interpolate_data_1d, get_data, recv_data
  use jcup_comp, only : get_comp_id_from_name, get_component_name
  use jcup_time, only : time_type, get_current_time
  use jcup_buffer, only : put_recv_data
  use jcup_config, only : GetRecvMappingTag, GetExchangeTag, get_recv_data_id_from_data_name
  implicit none
  character(len=*), intent(IN) :: source_model_name
  character(len=NAME_LEN), intent(IN) :: data_name
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: exchange_data_id

  integer :: exchange_tag(NUM_OF_EXCHANGE_DATA)
  type(time_type) :: time
  integer :: i, j, counter
  integer :: is, ie, js, je, ks, ke
  integer :: ni, nj
  integer :: source_comp_id

  if (.not.jcup_isRecvOK(data_name)) return

  call put_log("interpolate_data_25d_double start. source model : "//trim(source_model_name)//", dest model : " &
               //trim(get_component_name(current_comp_id)))

  source_comp_id = get_comp_id_from_name(trim(source_model_name))

  call jcup_check_recv_error(source_model_name, data_name)

  call set_current_mapping_tag(source_comp_id, current_comp_id, GetRecvMappingTag(RECV_DATA_NAME=trim(data_name)))

  exchange_tag(1) = GetExchangeTag(data_name)

  call get_current_time(current_comp_id, 1, time)

  call interpolate_data_1d(current_comp_id, source_comp_id, recv_mapping_tag(current_comp_id, source_comp_id), &
                           DOUBLE_DATA, num_of_data, exchange_tag)

  buffer_double25d = 0.d0

  call get_data(current_comp_id, source_comp_id, recv_mapping_tag(current_comp_id, source_comp_id), &
                buffer_double25d(:,:), num_of_data)

  call get_my_local_area(current_comp_id, current_grid_tag, is, ie, js, je, ks, ke)

  ni = ie-is+1

  call put_recv_data(buffer_double25d(1:ni,1:num_of_data), time, current_comp_id, &
                     get_recv_data_id_from_data_name(data_name), data_name)

  call put_log("interpolate_data_25d_double completed. source model : "//trim(source_model_name)//", dest model : " &
               //trim(get_component_name(current_comp_id)))

end subroutine jcup_interpolate_data_25d_double

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_check_recv_error(source_model_name, my_data_name)
  use jcup_config, only : recv_data_conf_type, get_recv_data_conf_ptr, isRecvData2
  use jcup_comp, only : is_model_running
  implicit none
  character(len=*), intent(IN) :: source_model_name, my_data_name
  integer :: i
  type(recv_data_conf_type), pointer :: recv_data_ptr

  if (.not.is_model_running(trim(source_model_name))) then
    call jcup_terminate_send_recv("jcup_check_recv_error", &
                              "Model "//trim(source_model_name)//" is not running")
  end if

  recv_data_ptr => get_recv_data_conf_ptr(DATA_NAME = my_data_name)

  if (.not.isRecvData2(source_model_name, recv_data_ptr%send_data)) then
    call jcup_terminate_send_recv("jcup_check_recv_error", &
                              "Data "//trim(my_data_name)//" is not a send data. Check coupling.conf file")
  end if

end subroutine jcup_check_recv_error

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> immediate send
!> @param[in] send_comp_name name of send component
!> @param[in] recv_comp_name name of recv component
!> @param[in] time_lag time_lag setting
subroutine jcup_send_data_immediately(send_comp_name, recv_comp_name, time_lag) !dest_task, c_time, is_final_step)
  use jcup_constant, only : STRING_LEN
  use jcup_utils, only : put_log, IntToStr, error, NO_OUTPUT_LOG, get_log_level
  use jcup_config, only : set_current_conf, get_num_of_recv_data, get_recv_data_conf_ptr, &
                          recv_data_conf_type, is_source_model, get_current_comp_id, &
                          is_my_exchange_step, is_send_step_data
  use jcup_comp, only : get_comp_id_from_name
  use jcup_data, only : get_send_data_dimension, get_num_of_exchange_send_data
  use jcup_time, only : get_current_time
  implicit none
  character(len=*), intent(IN) :: send_comp_name ! name of my component
  character(len=*), intent(IN) :: recv_comp_name ! name of destination component
  integer, intent(IN) :: time_lag

  integer :: send_comp_id, recv_comp_id
  integer :: d, dd
  type(recv_data_conf_type), pointer :: rd, rrdd
  character(len=NAME_LEN) :: data_name(NUM_OF_EXCHANGE_DATA)
  character(len=NAME_LEN) :: average_data_name(NUM_OF_EXCHANGE_DATA)
  logical :: is_average(NUM_OF_EXCHANGE_DATA)
  integer :: num_of_data
  integer :: num_of_2d_array
  character(len=STRING_LEN) :: log_str
  character(len=14) :: time_str
  integer :: exchange_data_id
  integer :: target_comp, target_comp_id
  integer :: my_comp, my_comp_id
  logical :: is_first_step_temp

  call get_current_time(get_current_comp_id(), 1, time_str)

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!   IMMEDIATE SEND    !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!   DATA SEND START   !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!    "//time_str//"   !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)

  send_comp_id = get_comp_id_from_name(send_comp_name)
  recv_comp_id = get_comp_id_from_name(recv_comp_name)

  my_comp_id = send_comp_id

  target_comp_id = recv_comp_id

    recv_flag(:) = .false.
    num_of_data = 0

    do d = 1, get_num_of_recv_data(target_comp_id)

      if (recv_flag(d)) cycle
      rd => get_recv_data_conf_ptr(recv_comp_name,d)
      if (.not.rd%is_recv) cycle

        current_comp_id = my_comp_id

        call set_current_conf(current_comp_id)

        if (rd%send_model_id==my_comp_id) then

          !!!if (is_send_step_data(rd%model_id, rd%send_data)) then

            ! set first data name
            num_of_data = 1
            data_name(num_of_data) = trim(rd%send_data)
            exchange_data_id = rd%data_id

            average_data_name(num_of_data) = data_name(num_of_data)
            is_average(num_of_data) = rd%is_average
            if (rd%is_average) then
              average_data_name(num_of_data) = trim(get_average_data_name(rd%send_data, rd%model_id, rd%name))
            end if
         
            ! count number of data
            if (get_send_data_dimension(send_comp_id, rd%send_data) == DATA_25D) then
              num_of_2d_array = get_num_of_exchange_send_data(send_comp_id, rd%send_data)
            else

              do dd = d+1, get_num_of_recv_data(target_comp_id)
                rrdd => get_recv_data_conf_ptr(recv_comp_name, dd)
                if (.not.rrdd%is_recv) cycle
                if (rrdd%send_model_id==my_comp_id) then
                  if (rd%recv_tag == rrdd%recv_tag ) then
                    recv_flag(dd) = .true.
                    num_of_data = num_of_data+1
                    if (num_of_data > NUM_OF_EXCHANGE_DATA) then
                      call error("jcup_exchange_data_send", &
                      "The number of send data must be <= NUM_OF_EXCHANGE_DATA. Check and modify configure file exchange_tag")
                    end if
                    data_name(num_of_data) = trim(rrdd%send_data)
                    average_data_name(num_of_data) = data_name(num_of_data)
                    is_average(num_of_data) = rrdd%is_average
                    if (rrdd%is_average) then
                      average_data_name(num_of_data) = trim(get_average_data_name(rrdd%send_data, rrdd%model_id, rrdd%name))
                    end if
                  end if
                end if
              end do
            end if

            call put_log("SEND DATA START! dest model, "//trim(recv_comp_name) &
                        //", number of send data:"//trim(IntToStr(num_of_data)), 1)

            if (get_log_level() /= NO_OUTPUT_LOG) then
              log_str = "SEND DATA NAME : "//trim(data_name(1))
              do dd = 2, num_of_data
                log_str = trim(log_str)//", "//trim(data_name(dd))
              end do
              call put_log(trim(log_str),1)
            end if

            if (time_lag==0) then
              is_first_step_temp = is_first_step
              is_first_step = .true. ! set is_first_step .true. for immediate data exchange
            end if

            select case(get_send_data_dimension(send_comp_id, rd%send_data))
            case (DATA_1D)
              call set_current_conf(current_comp_id)
              call jcup_exchange_data_1d_double(recv_comp_name, data_name, average_data_name, &
                                                num_of_data, exchange_data_id, is_average)
            case (DATA_25D)
              call set_current_conf(current_comp_id)
              call jcup_exchange_data_25d_double(recv_comp_name, data_name(1), average_data_name(1), &
                                                 num_of_2d_array, exchange_data_id, is_average(1))
            case default
              call error("jcup_send_data_immediately","data dimension error")
            end select

            if (time_lag==0) then
              is_first_step = is_first_step_temp
            end if

            call put_log("SEND DATA FINISH! dest model, "//trim(recv_comp_name), 1) 
          !!!end if
        end if
    end do

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!    IMMEDIATE SEND    !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!   DATA SEND FINISH   !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)

end subroutine jcup_send_data_immediately

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> immediate recv
!> @param[in] send_comp_name name of send component
!> @param[in] recv_comp_name name of recv component
subroutine jcup_recv_data_immediately(send_comp_name, recv_comp_name)
  use jcup_constant, only : STRING_LEN
  use jcup_utils, only : put_log, IntToStr, error, NO_OUTPUT_LOG, get_log_level
  use jcup_config, only : set_current_conf, get_num_of_recv_data, get_recv_data_conf_ptr, &
                          recv_data_conf_type, is_source_model, get_current_comp_id, &
                          is_my_exchange_step, is_send_step_data, is_recv_step_data, get_num_of_recv_data
  use jcup_comp, only : get_comp_id_from_name, is_model_running, is_my_component
  use jcup_data, only : get_recv_data_dimension, get_num_of_exchange_recv_data
  use jcup_time, only : get_current_time
  implicit none
  character(len=*), intent(IN) :: send_comp_name
  character(len=*), intent(IN) :: recv_comp_name ! my_comp_name
  integer :: source_task

  integer :: send_comp_id, recv_comp_id
  integer :: d, dd
  type(recv_data_conf_type), pointer :: rd, rrdd
  character(len=NAME_LEN) :: send_data_name(NUM_OF_EXCHANGE_DATA)
  character(len=NAME_LEN) :: average_send_data_name(NUM_OF_EXCHANGE_DATA)
  character(len=NAME_LEN) :: recv_data_name(NUM_OF_EXCHANGE_DATA)
  logical :: is_average(NUM_OF_EXCHANGE_DATA)
  integer :: num_of_data
  integer :: num_of_2d_array
  character(len=STRING_LEN) :: log_str
  character(len=14) :: time_str
  integer :: exchange_data_id
  integer :: target_comp, target_comp_id
  integer :: my_comp, my_comp_id

  call get_current_time(get_current_comp_id(), 1, time_str)

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!   IMMEDIATE RECV    !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!   DATA RECV START   !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!    "//time_str//"   !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)

  send_comp_id = get_comp_id_from_name(send_comp_name)
  recv_comp_id = get_comp_id_from_name(recv_comp_name)

    target_comp_id = send_comp_id
    my_comp_id = recv_comp_id
    current_comp_id = my_comp_id

    call set_current_conf(current_comp_id)


    recv_flag(:) = .false.

    do d = 1, get_num_of_recv_data(my_comp_id)

      if (recv_flag(d)) cycle

      rd => get_recv_data_conf_ptr(recv_comp_name,d)

      if (.not.rd%is_recv) cycle

        if (rd%send_model_id /= target_comp_id) cycle

        if (is_model_running(rd%send_model)) then

          !!!if (is_recv_step_data(rd%name)) then


            num_of_data = 1
            send_data_name(num_of_data) = trim(rd%send_data)
            recv_data_name(num_of_data) = trim(rd%name)

            exchange_data_id = rd%data_id

            average_send_data_name(num_of_data) = send_data_name(num_of_data)
            is_average(num_of_data) = rd%is_average

            if (get_recv_data_dimension(recv_comp_id, rd%name) == DATA_25D) then
              num_of_2d_array = get_num_of_exchange_recv_data(recv_comp_id, rd%name)
            else

              do dd = d+1, get_num_of_recv_data(my_comp_id)
                rrdd => get_recv_data_conf_ptr(recv_comp_name,dd)
                if (.not.rrdd%is_recv) cycle
                if (rd%recv_tag == rrdd%recv_tag ) then
                  recv_flag(dd) = .true.
                  num_of_data = num_of_data+1
                  if (num_of_data > NUM_OF_EXCHANGE_DATA) then
                    call error("jcup_exchange_data_send", &
                  "The number of send data must be <= NUM_OF_EXCHANGE_DATA. Check and modify configure file exchange_tag.")
                  end if
                  is_average(num_of_data) = rrdd%is_average
                  send_data_name(num_of_data) = trim(rrdd%send_data)
                  average_send_data_name(num_of_data) = send_data_name(num_of_data)
                  recv_data_name(num_of_data) = trim(rrdd%name)
                end if
              end do
            end if

            call put_log("RECV DATA START! source model:"//trim(rd%send_model) &
                        //", number of recv data:"//trim(IntToStr(num_of_data)), 1)

            if (get_log_level() /= NO_OUTPUT_LOG) then
              log_str = "RECV DATA NAME : "//trim(recv_data_name(1))
              do dd = 2, num_of_data
                log_str = trim(log_str)//", "//trim(recv_data_name(dd))
              end do
              call put_log(trim(log_str),1)
            end if

            select case(get_recv_data_dimension(recv_comp_id, rd%name))
            case (DATA_1D)
              current_comp_id = send_comp_id
              call set_current_conf(send_comp_id)
              call jcup_exchange_data_1d_double(recv_comp_name, send_data_name, &
                                                average_send_data_name, &
                                                num_of_data, exchange_data_id, is_average)
              call set_current_conf(recv_comp_id)
              current_comp_id = recv_comp_id
              if (is_my_component(recv_comp_id)) then
                call jcup_interpolate_data_1d_double(trim(rd%send_model), recv_data_name, num_of_data, exchange_data_id)
              end if
            case (DATA_25D)
              current_comp_id = send_comp_id
              call set_current_conf(send_comp_id)
              call jcup_exchange_data_25d_double(recv_comp_name, send_data_name(1), &
                                                 average_send_data_name(1), &
                                                 num_of_2d_array, exchange_data_id, is_average(1))
              call set_current_conf(recv_comp_id)
              current_comp_id = recv_comp_id
              if (is_my_component(recv_comp_id)) then
                call jcup_interpolate_data_25d_double(trim(rd%send_model), recv_data_name(1), num_of_2d_array, exchange_data_id)
              end if
            case default
              call error("jcup_recv_data_immediately", "data dimension error")
            end select
            call put_log("RECV DATA FINISH! source model, "//trim(rd%send_model), 1)

          !!!end if
        end if
    end do

    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!    IMMEDIATE RECV    !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!   DATA RECV FINISH   !!!!!!!!!!!!!!! ", 1)
    call put_log("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ", 1)


end subroutine jcup_recv_data_immediately

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function jcup_isSendOK(name)
  use jcup_config, only : isSendData
  use jcup_utils, only  : put_log
  implicit none
  character(len=*),intent(IN) :: name

  if (.not.is_Initialize_completed) then
     call jcup_abnormal_end("jcup_isSendOK", "jcup_SetMappigTable not called")
  end if

  jcup_isSendOK = .false.

  if (.not.isSendData(DATA_NAME = name)) then
    call put_log("Data : "//trim(name)//", send_flag /= 1, send data skip", 1)
    return
  end if

  jcup_isSendOK = .true.

end function jcup_isSendOK

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function jcup_isRecvOK(name)
  use jcup_config, only : is_my_recv_data, isRecvData, is_recv_step_data
  use jcup_utils, only  :  put_log
  implicit none
  character(len=*),intent(IN) :: name

  if (.not.is_Initialize_completed) then
     call jcup_abnormal_end("jcup_isRecvOK", "jcup_SetMappigTable not called")
  end if

  jcup_isRecvOK = .false.

  if (.not.is_my_recv_data(trim(name))) then
    call jcup_terminate_send_recv("jcup_isRecvOK", &
           "Data "//trim(name)//" is not listed in coupler.conf file. Check your code and file")
  end if
  
  if (.not.isRecvData(DATA_NAME = name)) then
    call put_log("Data : "//trim(name)//", recv_flag /= 1, recv skip", 1)
    return
  end if

  if (.not.is_recv_step_data(name)) then
    call put_log("Recv data skipped, data : "//trim(name), 1)
    return
  end if

  jcup_isRecvOK = .true.

end function jcup_isRecvOK

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_current_mapping_tag(send_comp_id, recv_comp_id, mapping_tag)
  use jcup_constant, only : NUM_OF_EXCHANGE_GRID
  implicit none
  integer, intent(IN) :: send_comp_id
  integer, intent(IN) :: recv_comp_id
  integer, intent(IN) :: mapping_tag

  if (current_comp_id==send_comp_id) then
     call check_mapping_table(send_table_checker(current_comp_id,:), recv_comp_id, mapping_tag)
     send_mapping_tag(current_comp_id, recv_comp_id) = mapping_tag
     current_grid_tag = my_send_grid_tag(current_comp_id, recv_comp_id, mapping_tag)
  else 
     call check_mapping_table(recv_table_checker(current_comp_id,:), send_comp_id, mapping_tag)
     recv_mapping_tag(current_comp_id, send_comp_id) = mapping_tag
     current_grid_tag = my_recv_grid_tag(current_comp_id, send_comp_id, mapping_tag)
  end if
  
end subroutine set_current_mapping_tag

!=======+=========+=========+=========+=========+=========+=========+=========+

character(len=NAME_LEN) function get_average_data_name(data_name, recv_model_id, recv_data_name)
  use jcup_utils, only : IntToStr
  character(len=*), intent(IN) :: data_name
  integer, intent(IN) :: recv_model_id
  character(len=*), intent(IN) :: recv_data_name

  get_average_data_name = trim(data_name)//"__"//trim(IntToStr(recv_model_id)) &
                          //"_"//trim(recv_data_name)
  
end function get_average_data_name

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function get_send_data_id(recv_comp_id, data_name, is_average)
  use jcup_config, only : get_send_data_id_from_data_name, get_send_comp_id_from_data_name , is_mean_data
  implicit none
  integer, intent(IN) :: recv_comp_id
  character(len=*), intent(IN) :: data_name
  logical, intent(IN) :: is_average
  integer :: send_comp_id

  send_comp_id = get_send_comp_id_from_data_name(data_name)

  !if (is_mean_data(send_comp_id, data_name)) then
  if (is_average) then
    get_send_data_id = recv_comp_id*1000000+get_send_data_id_from_data_name(data_name)
  else
    get_send_data_id = get_send_data_id_from_data_name(data_name)
  end if


end function get_send_data_id

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function jcup_isPutOK(data_type, time)
  use jcup_config, only : isSendData, get_send_comp_id_from_data_name, is_mean_data, is_put_step_data
  use jcup_utils, only  :  put_log
  use jcup_data, only : varp_type, get_time, get_data_name
  use jcup_time, only : operator(==)
  implicit none
  type(varp_type), pointer :: data_type
  type(time_type), intent(IN) :: time
  character(len=NAME_LEn) :: name
 
  name = get_data_name(data_type)

  jcup_isPutOK = .true.

  if (.not.isSendData(DATA_NAME = name)) then
    call put_log("Data : "//trim(name)//", send_flag /= 1, put data skip", 1)
    jcup_isPutOK = .false.
    return
  end if


  if (is_mean_data(get_send_comp_id_from_data_name(name), name)) return 

  if (is_first_step) return

  jcup_isPutOK = .false.

  !!!if (time == get_time(data_type)) then
  !!!!  call put_log("This data has been put, get data skipped, data : "//trim(name), 1)
  !!!!  return
  !!!!end if

  if (.not.is_put_step_data(name)) then
    call put_log("Current time is not send step, put data skipped, data : "//trim(name), 1)
    return
  end if

  jcup_isPutOK = .true.

end function jcup_isPutOK

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> put 1d data
!> @param[in] data_type send data type
!> @param[in] data array of send data
subroutine jcup_put_data_1d_double(data_type, data)
  use jcup_constant, only : IMMEDIATE_SEND_RECV
  use jcup_utils, only  : IntToStr, error
  use jcup_buffer, only : put_send_data
  use jcup_config, only : is_mean_data, send_data_conf_type, get_send_data_conf_ptr, is_put_step_data, &
                          get_send_comp_id_from_data_name, set_current_conf, get_comp_exchange_type
  use jcup_time, only : time_type, get_current_time, cal_next_exchange_time
  use jcup_data, only : varp_type, get_comp_id, &
                        get_data_name, &
                        is_data_defined
  use jcup_data, only : set_time
  implicit none
  type(varp_type), pointer :: data_type
  real(kind=8), intent(IN) :: data(:)
  type(time_type)   :: time, next_time
  character(NAME_LEN) :: average_data_name
  type(send_data_conf_type), pointer :: sd
  real(kind=8) :: averaging_weight
  integer :: my_comp_id
  integer :: i
  character(len=NAME_LEN) :: data_name
  logical :: is_average


  call fapp_start("jcup_put_data", 1, 1)

  if (.not.associated(data_type)) call error("jcup_put_data_1d_double", "data_type is not associated")

  !!!!!!call check_put_array_size(data_type, size(data,1), size(data,2), 1)

  my_comp_id = get_comp_id(data_type)

  call set_current_conf(my_comp_id)

  call get_current_time(my_comp_id, 1, time)

  data_name = get_data_name(data_type)

  if (.not.is_data_defined(my_comp_id, data_name)) then
    call jcup_abnormal_end("jcup_put_data_2d_double", "data : "//trim(data_name) &
                           //" is not defined (subroutine jcup_def_data is not called ")
  end if

  sd => get_send_data_conf_ptr(DATA_NAME = data_name)
  do i = 1, sd%num_of_my_recv_data
    if ((get_comp_exchange_type(my_comp_id, sd%my_recv_conf(i)%model_id) == IMMEDIATE_SEND_RECV).or. &
        (sd%my_recv_conf(i)%time_lag == 0)) then ! 2013.09.18 [ADD]
      call jcup_put_send_data_1d_double(sd, sd%my_recv_conf(i), data)
      if (sd%num_of_my_recv_data == 1) return
    end if
  end do

  if (.not.jcup_isPutOK(data_type, time)) return

  if (is_mean_data(my_comp_id, data_name)) then
    do i = 1, sd%num_of_my_recv_data
      if (get_comp_exchange_type(my_comp_id, sd%my_recv_conf(i)%model_id) == IMMEDIATE_SEND_RECV) cycle ! skip immediate_send_recv

      is_average = sd%my_recv_conf(i)%is_average
      if (sd%my_recv_conf(i)%is_average) then
        if (.not.is_first_step) then ! average data
          average_data_name = trim(get_average_data_name(data_name,sd%my_recv_conf(i)%model_id,sd%my_recv_conf(i)%name))
          averaging_weight = dble(time%delta_t)/dble(sd%my_recv_conf(i)%interval)
          call cal_next_exchange_time(my_comp_id, 1, sd%my_recv_conf(i)%interval, next_time)
          call put_send_data(data, next_time, my_comp_id, &
                             get_send_data_id(sd%my_recv_conf(i)%model_id, data_name, is_average), &
                             average_data_name, .true., averaging_weight)
        else ! first step of average data
          call put_send_data(data, time, my_comp_id, &
                             get_send_data_id(sd%my_recv_conf(i)%model_id, data_name, is_average), &
                             data_name, .false., 1.d0)
        end if
      else  ! non average data
        if ((is_first_step).or.(is_put_step_data(data_name))) then
           call put_send_data(data, time, my_comp_id, &
                              get_send_data_id(sd%my_recv_conf(i)%model_id, data_name, is_average), &
                              data_name, .false., 1.d0)
        end if
      end if
    end do
  else
    call put_send_data(data, time, my_comp_id, &
                       get_send_data_id(0, data_name, .false.), data_name, .false., 1.d0)
    !!!!!call put_send_data(data, time, current_comp_id, &
    !!!!!                   get_send_data_id(0, data_name, .false.), data_name, .false., 1.d0)
  end if

  call set_time(data_type, time)

  call fapp_stop("jcup_put_data", 1, 1)

end subroutine jcup_put_data_1d_double

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> put 2.5d data
!> @param[in] data_type send data type
!> @param[in] data array of send data
!> @param[in] num_of_data number of 2.5D data
subroutine jcup_put_data_25d_double(data_type, data, num_of_data)
  use jcup_constant, only : IMMEDIATE_SEND_RECV
  use jcup_utils, only  : IntToStr, error
  use jcup_buffer, only : put_send_data
  use jcup_config, only : is_mean_data, send_data_conf_type, get_send_data_conf_ptr, is_put_step_data, &
                          get_send_comp_id_from_data_name, set_current_conf, get_comp_exchange_type
  use jcup_time, only : time_type, get_current_time, cal_next_exchange_time
  use jcup_data, only : varp_type, is_data_defined, get_comp_id, get_data_name, set_time
  implicit none
  type(varp_type), pointer :: data_type
  real(kind=8), intent(IN) :: data(:,:)
  integer, intent(IN) :: num_of_data
  type(time_type)   :: time, next_time
  character(NAME_LEN) :: average_data_name
  type(send_data_conf_type), pointer :: sd
  real(kind=8) :: averaging_weight
  integer :: my_comp_id
  integer :: i
  character(len=NAME_LEN) :: data_name
  logical :: is_average

  if (.not.associated(data_type)) call error("jcup_put_data_25d_double", "data_type is not associated")

  !!!!call check_put_array_size(data_type, size(data,1), size(data,2), 1)

  my_comp_id = get_comp_id(data_type)
  call set_current_conf(my_comp_id)
  call get_current_time(my_comp_id, 1, time)

  data_name = get_data_name(data_type)

  if (.not.jcup_isPutOK(data_type, time)) return

  if (.not.is_data_defined(my_comp_id, data_name)) then
    call jcup_abnormal_end("jcup_put_data_2d_double", "data : "//trim(data_name) &
                          //" is not defined (subroutine jcup_def_data is not called) ")
  end if

  sd => get_send_data_conf_ptr(DATA_NAME = data_name)
  !do i = 1, sd%num_of_my_recv_data
  !  if (get_comp_exchange_type(my_comp_id, sd%my_recv_conf(i)%model_id) == IMMEDIATE_SEND_RECV) then
  !    call jcup_put_send_data_25d_double(sd, sd%my_recv_conf(i), data)
  !    if (sd%num_of_my_recv_data == 1) return
  !  end if
  !end do

  if (is_mean_data(my_comp_id, data_name)) then
    do i = 1, sd%num_of_my_recv_data

      is_average = sd%my_recv_conf(i)%is_average

      if (sd%my_recv_conf(i)%is_average) then
        if (.not.is_first_step) then
          average_data_name = trim(get_average_data_name(data_name,sd%my_recv_conf(i)%model_id,sd%my_recv_conf(i)%name))
          averaging_weight = dble(time%delta_t)/dble(sd%my_recv_conf(i)%interval)
          call cal_next_exchange_time(my_comp_id, 1, sd%my_recv_conf(i)%interval, next_time)
          call put_send_data(data, next_time, current_comp_id, &
                             get_send_data_id(sd%my_recv_conf(i)%model_id, data_name, is_average), &
                             average_data_name, .true., averaging_weight)
        else ! first step of average data
          call put_send_data(data, time, my_comp_id, &
                             get_send_data_id(sd%my_recv_conf(i)%model_id, data_name, is_average), &
                             data_name, .false., 1.d0)
        end if
      else ! non average data
        if (is_put_step_data(data_name)) then
           call put_send_data(data, time, my_comp_id, &
                              get_send_data_id(sd%my_recv_conf(i)%model_id, data_name, is_average), &
                              data_name, .false., 1.d0)
        end if
      end if
    end do
  else
    call put_send_data(data, time, my_comp_id, get_send_data_id(0, data_name, .false.), data_name, .false.,1.d0)
  end if

  call set_time(data_type, time)

end subroutine jcup_put_data_25d_double

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function jcup_isGetOK(data_type, time)
  use jcup_config, only : is_recv_step_data, isRecvData
  use jcup_utils, only  : put_log, error
  use jcup_time, only : operator(==)
  use jcup_data, only : varg_type, get_data_name, get_time
  implicit none
  type(varg_type), pointer :: data_type
  type(time_type), intent(IN) :: time
  character(len=NAME_LEN) :: name


  jcup_isGetOK = .false.
  name = get_data_name(data_type)

  !!!if (time == get_time(data_type)) then
  !!!  call put_log("This data has been gotten, get data skipped, data : "//trim(name), 1)
  !!!  return
  !!!end if

  if (.not.isRecvData(DATA_NAME = name)) then
    call error("jcup_isGetOK", "Data : "//trim(name)//", recv_flag /= 1, invalid subroutine call jcup_get_data")
    return
  end if

  if (.not.is_recv_step_data(name)) then
    call put_log("Current time is not recv step, get data skipped, data : "//trim(name), 1)
    return
  end if

  jcup_isGetOK = .true.

end function jcup_isGetOK

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> get 1d data
!> @param[in] data_type recv data type
!> @param[in] data array of recv data
subroutine jcup_get_data_1d_double(data_type, data, is_recv_ok)
  use jcup_utils, only : error
  use jcup_constant, only : IMMEDIATE_SEND_RECV
  use jcup_buffer, only : get_recv_data
  use jcup_time, only : time_type, get_current_time, operator(==)
  use jcup_config, only : recv_data_conf_type, get_send_comp_id_from_data_name, get_send_data_name, is_mean_data, &
                          get_recv_data_id_from_data_name, get_recv_comp_id_from_data_name, &
                          set_current_conf, get_recv_data_conf_ptr, get_comp_exchange_type, isRecvData
  use jcup_data, only : varg_type, get_comp_id, get_data_name, set_time
  implicit none
  type(varg_type), pointer :: data_type
  real(kind=8), intent(INOUT) :: data(:)
  logical, intent(OUT), optional :: is_recv_ok
  type(time_type) :: time
  logical :: is_data_reset
  integer :: my_comp_id
  character(len=NAME_LEN) :: send_data_name
  character(len=NAME_LEN) :: data_name
  type(recv_data_conf_type), pointer :: rd
  
  call fapp_start("jcup_get_data", 1, 1)

  if (.not.associated(data_type)) call error("jcup_get_data_1d_double", "data_type is not associated")

  my_comp_id = get_comp_id(data_type)

  call set_current_conf(my_comp_id)
  call get_current_time(my_comp_id, 1, time)
  
  if (present(is_recv_ok)) is_recv_ok = .false.

  data_name = get_data_name(data_type)

  if (.not.isRecvData(DATA_NAME = data_name)) then
    call error("jcup_isGetOK", "Data : "//trim(data_name)//", recv_flag /= 1, invalid subroutine call jcup_get_data")
    return
  end if

  rd => get_recv_data_conf_ptr(data_name)

  if ((get_comp_exchange_type(my_comp_id, rd%send_model_id) == IMMEDIATE_SEND_RECV).or. &
      (rd%time_lag == 0)) then ! 2013.09.18 [ADD]
    write(0,*) "jcup_get_data_1d_double, immediate recv start"

    if (present(is_recv_ok)) is_recv_ok = .true.
    call jcup_recv_get_data_1d_double(rd%send_model_id, rd, data)
    return
  end if

  if (.not.jcup_isGetOK(data_type, time)) return 

  if (present(is_recv_ok)) is_recv_ok = .true.

  send_data_name = get_send_data_name(data_name)
  is_data_reset = .not.is_mean_data(get_send_comp_id_from_data_name(send_data_name), send_data_name)

  !!!my_comp_id = current_comp_id ! get_recv_comp_id_from_data_name(data_name)
  call get_recv_data(data, time, my_comp_id, get_recv_data_id_from_data_name(data_name), data_name, is_data_reset)

  call set_time(data_type, time)

  call fapp_stop("jcup_get_data", 1, 1)

end subroutine jcup_get_data_1d_double

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> get 2.5d data
!> @param[in] data_type recv data type
!> @param[in] data array of recv data
!> @param[in] num_of_data number of 2.5d data
subroutine jcup_get_data_25d_double(data_type, data, num_of_data, is_recv_ok)
  use jcup_utils, only : error
  use jcup_buffer, only : get_recv_data 
  use jcup_time, only : time_type, get_current_time
  use jcup_config, only : get_send_comp_id_from_data_name, get_send_data_name, is_mean_data, &
                          get_recv_data_id_from_data_name, get_recv_comp_id_from_data_name, &
                          set_current_conf
  use jcup_data, only : varg_type, get_comp_id, get_data_name, set_time
  implicit none
  type(varg_type), pointer :: data_type
  real(kind=8), intent(INOUT) :: data(:,:)
  integer, intent(IN) :: num_of_data
  logical, intent(OUT), optional :: is_recv_ok
  type(time_type) :: time
  logical :: is_data_reset
  integer :: my_comp_id
  character(len=NAME_LEN) :: send_data_name
  character(len=NAME_LEN) :: data_name
  
  if (.not.associated(data_type)) call error("jcup_get_data_25d_double", "data_type is not associated")

  my_comp_id = get_comp_id(data_type)
  call set_current_conf(my_comp_id)
  call get_current_time(my_comp_id, 1, time)

  if (present(is_recv_ok)) is_recv_ok = .false.

  if (.not.jcup_isGetOK(data_type, time)) return 

  if (present(is_recv_ok)) is_recv_ok = .true.

  data_name = get_data_name(data_type)

  send_data_name = get_send_data_name(data_name)
  is_data_reset = .not.is_mean_data(get_send_comp_id_from_data_name(send_data_name), send_data_name)

  !my_comp_id = current_comp_id !get_recv_comp_id_from_data_name(data_name)
  call get_recv_data(data, time, current_comp_id, get_recv_data_id_from_data_name(data_name), data_name, is_data_reset)

  call set_time(data_type, time)

end subroutine jcup_get_data_25d_double

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_put_send_data_1d_double(sd, rd, data)
  use jcup_constant, only : COMP_PARALLEL, COMP_SERIAL, COMP_SUPERSET, COMP_SUBSET, COMP_OVERLAP
  use jcup_utils, only  : IntToStr, error, put_log
  use jcup_config, only : send_data_conf_type, recv_data_conf_type, GetMappingTag, get_recv_data_id_from_data_name
  use jcup_grid_base, only : get_my_local_area
  use jcup_grid, only : set_data, send_data_1d, exchange_data_comp, get_data, interpolate_data_1d
  use jcup_comp, only : get_component_relation, is_my_component
  use jcup_data, only : varp_type, get_comp_id, get_data_name, is_data_defined, set_time
  use jcup_buffer, only : put_send_data, put_recv_data
  implicit none
  type(send_data_conf_type), pointer :: sd
  type(recv_data_conf_type)          :: rd
  real(kind=8), intent(IN) :: data(:)
  integer :: send_comp_id
  integer :: recv_comp_id
  integer :: is, ie, js, je, ks, ke
  integer :: ni, nj
  integer :: d
  character(len=NAME_LEN) :: data_name
  character(len=NAME_LEN) :: recv_data_name
  type(time_type) :: time
  integer :: exchange_tag(1)

  data_name = sd%name

  if (.not.jcup_isSendOK(trim(data_name))) return

  recv_comp_id = rd%model_id
  current_comp_id = sd%model_id
  send_comp_id = current_comp_id

  !if (recv_model_id == current_comp_id) then
  !  call error("jcup_SendData2D_double","dest_model_name : "//trim(dest_model_name)//" error")
  !end if

  call set_current_mapping_tag(sd%model_id, recv_comp_id, GetMappingTag(recv_comp_id, data_name))

  call get_my_local_area(current_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
  ni = ie-is+1

  select case(get_component_relation(current_comp_id, recv_comp_id))
  case (COMP_PARALLEL)
    call put_log("immediate data put start, data name : "//trim(data_name)//", model : COMP_PARALLEL", 1)

    buffer_double1d(1:ni,1) = data(1:ni)

    call set_data(buffer_double1d)    
    call send_data_1d(sd%model_id, recv_comp_id, send_mapping_tag(sd%model_id, recv_comp_id), &
                      DOUBLE_DATA, 1, rd%data_id)

    call put_log("immediate data put completed, data name : "//trim(data_name)//", model : COMP_PARALLEL", 1)

  case (COMP_SERIAL, COMP_SUBSET)

    call put_log("immediate data put start, data name : "//trim(data_name)//", model : COMP_SERIAL or COMP_SUBSET", 1)

    time%yyyy = 9999 ; time%mo = 99 ; time%dd = 99 ; time%hh = 99 ; time%mm = 99 ; time%ss = 99

    call put_send_data(data, time, current_comp_id, &
                       get_send_data_id(0, data_name, rd%is_average), data_name, .false., 1.d0)

    call put_log("immediate data put completed, data name : "//trim(data_name)//", model : COMP_SERIAL or COMP_SUBSET", 1)

  case (COMP_SUPERSET)

    call put_log("immediate data put start, data name : "//trim(data_name)//", model : COMP_SUPERSET", 1)

    buffer_double1d(1:ni,1) = data(1:ni)

    call set_data(buffer_double1d)    

    call exchange_data_comp(send_comp_id, recv_comp_id, send_mapping_tag(send_comp_id, recv_comp_id), &
                            DOUBLE_DATA, 1, rd%data_id, DATA_2D)
    
    if (is_my_component(recv_comp_id)) then
      exchange_tag(1) = rd%exchange_tag
      call interpolate_data_1d(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                               DOUBLE_DATA, 1, exchange_tag)

      buffer_double1d = 0.d0

      call get_data(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                    buffer_double1d(:,:), 1)

      call get_my_local_area(recv_comp_id, current_grid_tag, is, ie, js, je, ks, ke)

      ni = ie-is+1

      time%yyyy = 9999 ; time%mo = 99 ; time%dd = 99 ; time%hh = 99 ; time%mm = 99 ; time%ss = 99

      recv_data_name = rd%name
  
      call put_recv_data(buffer_double1d(1:ni,1), time, recv_comp_id, &
                         get_recv_data_id_from_data_name(recv_data_name), recv_data_name)

    end if

    call put_log("immediate data put completed, data name : "//trim(data_name)//", model : COMP_SUPERSET", 1)

  case (COMP_OVERLAP)
    call put_log("immediate data put start, data name : "//trim(data_name)//", model : COMP_OVERLAP", 1)

    if (is_my_component(recv_comp_id)) then
      time%yyyy = 9999 ; time%mo = 99 ; time%dd = 99 ; time%hh = 99 ; time%mm = 99 ; time%ss = 99

      call put_send_data(data, time, current_comp_id, &
                         get_send_data_id(0, data_name, rd%is_average), data_name, .false., 1.d0)
    else
      buffer_double1d(1:ni,1) = data(1:ni)

      call set_data(buffer_double1d)    
      call send_data_1d(sd%model_id, recv_comp_id, send_mapping_tag(sd%model_id, recv_comp_id), &
                        DOUBLE_DATA, 1, rd%data_id)
    end if

    call put_log("immediate data put completed, data name : "//trim(data_name)//", model : COMP_OVERLAP", 1)

  case default
    call error("jcup_put_send_data_1d_double", "immediate exchange parameter error")
  end select

end subroutine jcup_put_send_data_1d_double

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_recv_get_data_1d_double(send_comp_id, rd, data)
  use jcup_constant, only : COMP_PARALLEL, COMP_SERIAL, COMP_SUPERSET, COMP_SUBSET, COMP_OVERLAP
  use jcup_utils, only  : IntToStr, error, put_log
  use jcup_config, only : send_data_conf_type, recv_data_conf_type, GetRecvMappingTag
  use jcup_grid_base, only : get_my_local_area
  use jcup_grid, only : set_data, get_data, recv_data, interpolate_data_1d, exchange_data_comp
  use jcup_comp, only : get_component_relation, is_my_component
  use jcup_data, only : varp_type, get_comp_id, get_data_name, is_data_defined, set_time
  use jcup_buffer, only : get_send_data
  use jcup_config, only : is_my_recv_data, isRecvData, is_recv_step_data
  implicit none
  integer, intent(IN) :: send_comp_id
  type(recv_data_conf_type)          :: rd
  real(kind=8), intent(INOUT) :: data(:)
  integer :: recv_comp_id
  integer :: is, ie, js, je, ks, ke
  integer :: ni, nj
  integer :: d
  integer :: exchange_tag(NUM_OF_EXCHANGE_DATA)
  character(len=NAME_LEN) :: data_name
  character(len=NAME_LEN) :: send_data_name
  type(time_type) :: time

  data_name = rd%name
  send_data_name = rd%send_data


  if (.not.is_Initialize_completed) then
     call jcup_abnormal_end("jcup_isRecvOK", "jcup_SetMappigTable not called")
  end if

  if (.not.is_my_recv_data(trim(data_name))) then
    call jcup_terminate_send_recv("jcup_isRecvOK", &
           "Data "//trim(data_name)//" is not listed in coupler.conf file. Check your code and file")
  end if
  
  if (.not.isRecvData(DATA_NAME = data_name)) then
    call put_log("Data : "//trim(data_name)//", recv_flag /= 1, recv skip", 1)
    return
  end if

  !if (.not.jcup_isRecvOK(trim(data_name))) return

  recv_comp_id = rd%model_id

  current_comp_id = recv_comp_id

  !if (recv_model_id == current_comp_id) then
  !  call error("jcup_SendData2D_double","dest_model_name : "//trim(dest_model_name)//" error")
  !end if

  call set_current_mapping_tag(send_comp_id, recv_comp_id, GetRecvMappingTag(recv_comp_id, data_name))

  select case(get_component_relation(send_comp_id, recv_comp_id))
  case (COMP_PARALLEL)

    call put_log("immediate data get start, data name : "//trim(data_name)//", model : COMP_PARALLEL", 1)

    call recv_data(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                   DOUBLE_DATA, 1, rd%data_id)

    exchange_tag(1) = rd%exchange_tag
    call interpolate_data_1d(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                             DOUBLE_DATA, 1, exchange_tag)

    call put_log("immediate data get completed, data name : "//trim(data_name)//", model : COMP_PARALLEL", 1)

  case(COMP_SERIAL, COMP_SUBSET)

    call put_log("immediate data get start, data name : "//trim(data_name)//", model : COMP_SERIAL or COMP_SUBSET", 1)

    if (is_my_component(send_comp_id)) then

      time%yyyy = 9999 ; time%mo = 99 ; time%dd = 99 ; time%hh = 99 ; time%mm = 99 ; time%ss = 99

      call get_my_local_area(send_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
      ni = ie-is+1

      call get_send_data(buffer_double1d(1:ni,1), time, current_comp_id, &
                         get_send_data_id(recv_comp_id, send_data_name, rd%is_average), &
                         trim(send_data_name))

      call set_data(buffer_double1d)    

    end if

    call exchange_data_comp(send_comp_id, recv_comp_id, send_mapping_tag(send_comp_id, recv_comp_id), &
                            DOUBLE_DATA, 1, rd%data_id, DATA_2D)

    exchange_tag(1) = rd%exchange_tag
    call interpolate_data_1d(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                             DOUBLE_DATA, 1, exchange_tag)


    call put_log("immediate data get completed, data name : "//trim(data_name)//", model : COMP_SERIAL or COMP_SUBSET", 1)

  case (COMP_SUPERSET)

    call put_log("immediate data get start, data name : "//trim(data_name)//", model : COMP_SUPERSET", 1)
    call put_log("immediate data get completed, data name : "//trim(data_name)//", model : COMP_SUPERSET", 1)

    ! do nothing

  case (COMP_OVERLAP)

    call put_log("immediate data get start, data name : "//trim(data_name)//", model : COMP_OVERLAP", 1)

    if (is_my_component(send_comp_id)) then
      time%yyyy = 9999 ; time%mo = 99 ; time%dd = 99 ; time%hh = 99 ; time%mm = 99 ; time%ss = 99

      call get_my_local_area(send_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
      ni = ie-is+1

      call get_send_data(buffer_double1d(1:ni,1), time, current_comp_id, &
                         get_send_data_id(recv_comp_id, send_data_name, rd%is_average), &
                         trim(send_data_name))

      call set_data(buffer_double1d)    
    end if

    call exchange_data_comp(send_comp_id, recv_comp_id, send_mapping_tag(send_comp_id, recv_comp_id), &
                            DOUBLE_DATA, 1, rd%data_id, DATA_2D)

    exchange_tag(1) = rd%exchange_tag
    call interpolate_data_1d(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                             DOUBLE_DATA, 1, exchange_tag)

    call put_log("immediate data get completed, data name : "//trim(data_name)//", model : COMP_OVERLAP", 1)

  case default
    call error("jcup_recv_get_data_1d_double", "immediate exchange parameter error")
  end select


  buffer_double1d = 0.d0

  call get_data(recv_comp_id, send_comp_id, recv_mapping_tag(recv_comp_id, send_comp_id), &
                buffer_double1d(:,:), 1)

  call get_my_local_area(current_comp_id, current_grid_tag, is, ie, js, je, ks, ke)
  ni = ie-is+1

  data(1:ni) = buffer_double1d(1:ni,1)


end subroutine jcup_recv_get_data_1d_double

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_put_array_size(data_type, s1, s2, s3)
  use jcup_utils, only : error
  use jcup_data, only : varp_type, data_array_size_ok
  implicit none
  type(varp_type), pointer :: data_type
  integer, intent(IN) :: s1, s2, s3

  if (.not.data_array_size_ok(data_type, s1, s2, s3)) then
    call error("check_put_array_size", "jcup_put_data, array size mismatch")
  end if

end subroutine check_put_array_size

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_write_restart(fid, time_array)
  use jcup_buffer, only :  buffer_check_write
  use jcup_comp, only : get_num_of_total_component, is_my_component, get_component_name 
  use jcup_io_base, only : jcup_init_io, jcup_io_create_type, jcup_write_restart_base
  use jcup_grid_base, only : local_area_type, get_num_of_grid, get_my_local_area_ptr
  implicit none
  integer, intent(IN) :: fid
  integer, intent(IN) :: time_array(6)
  type(local_area_type), pointer :: local_area
  integer :: i, j

  call jcup_init_io()

  do i = 1, get_num_of_total_component()

    if (is_my_component(i)) then

      do j = 1, get_num_of_grid(i)
        local_area => get_my_local_area_ptr(i, j)
        call jcup_io_create_type(i, local_area%grid_num, local_area%grid_index)
      end do
         
      call jcup_write_restart_base(fid, i, time_array)

    end if
  end do

  call buffer_check_write()

end subroutine jcup_write_restart

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jcup_read_restart(fid, time_array)
  use jcup_buffer, only :  buffer_check_write
  use jcup_comp, only : get_num_of_total_component, is_my_component, get_component_name 
  use jcup_io_base, only : jcup_init_io, jcup_io_create_type, jcup_read_restart_base
  use jcup_grid_base, only : local_area_type, get_num_of_grid, get_my_local_area_ptr
  use jcup_config, only : get_num_of_recv_data
  implicit none
  integer, intent(IN) :: fid
  integer, intent(IN) :: time_array(6)
  type(local_area_type), pointer :: local_area
  integer :: max_flag_size
  integer :: i, j

  max_flag_size = 0
  do i = 1, get_num_of_total_component()
    max_flag_size = max(max_flag_size, get_num_of_recv_data(i))
  end do
  allocate(recv_flag(max_flag_size))

  call jcup_init_io()

  do i = 1, get_num_of_total_component()

    if (is_my_component(i)) then

      do j = 1, get_num_of_grid(i)
        local_area => get_my_local_area_ptr(i, j)
        call jcup_io_create_type(i, local_area%grid_num, local_area%grid_index)
      end do
         
      call jcup_read_restart_base(fid, i, time_array)

    end if
  end do

  call buffer_check_write()

  is_first_step = .false.


end subroutine jcup_read_restart

!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcup_interface
