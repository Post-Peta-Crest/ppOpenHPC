module ppoh_MATHMP
  use ppoh_MATHMP_base, only : STR_LEN, &
                               ppoh_MATHMP_set_log_file_id => set_log_file_id, &
                               ppoh_MATHMP_put_log   => put_log, &
                               ppoh_MATHMP_error     => error
  use ppoh_MATHMP_mapping, only : ppoh_MATHMP_write_mapping_text   => write_mapping_text, &
                                  ppoh_MATHMP_write_mapping_binary => write_mapping_binary, &
                                  ppoh_MATHMP_read_mapping_text    => read_mapping_text, &
                                  ppoh_MATHMP_read_mapping_binary  => read_mapping_binary
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: ppoh_MATHMP_init            ! subroutine (comp_name, config_file_name, log_file_name)
  public :: ppoh_MATHMP_def_grid        ! subroutine (grid_name, grid_index(:)) 
                                        !         or (grid_naem, nx, ny, is, ie, js, je)
                                        !         or (grid_name, nx, ny, nz, is, je, js, je, ks, ke) 
  public :: ppoh_MATHMP_end_init        ! subroutine ()
  public :: ppoh_MATHMP_set_time        ! subroutine (delta_t)
  public :: ppoh_MATHMP_put_data        ! subroutine (data_name, data(*))
  public :: ppoh_MATHMP_get_data        ! subroutine (data_name, data(*))
  public :: ppoh_MATHMP_finalize        ! subroutine ()
  
  public :: ppoh_MATHMP_get_mpi_parameters ! subroutine (my_comm, my_size, my_rank)
  public :: ppoh_MATHMP_put_log            ! subroutine (log_str)
  public :: ppoh_MATHMP_error              ! subroutine (error_str)
  
  public :: ppoh_MATHMP_write_mapping_text    ! subroutine (file_name, send_grid, recv_grid, coef)
  public :: ppoh_MATHMP_write_mapping_binary  ! subroutine (file_name, send_grid, recv_grid, coef)

!--------------------------------  private  ----------------------------------!

  integer, parameter :: NAME_LEN = 32

  integer :: my_comm  ! local communicator
  integer :: my_group ! local mpi group
  integer :: my_size  ! local mpi size
  integer :: my_rank  ! local mpi rank
  integer :: my_rank_global ! global mpi rank

  character(len=NAME_LEN) :: my_name

  interface ppoh_MATHMP_def_grid 
     module procedure ppoh_MATHMP_def_grid_1d, ppoh_MATHMP_def_grid_2d, ppoh_MATHMP_def_grid_3d
  end interface


  integer :: current_time(6)
  integer :: initial_time(6)

contains

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> initialize ppoh_MATHMP
subroutine ppoh_MATHMP_init(comp_name, config_file_name, log_file_name)
  use jcup_interface, only : jcup_set_new_comp, jcup_initialize, jcup_get_mpi_parameter
  use ppoh_MATHMP_namelist, only : read_coupling_config, get_run_mode
  use ppoh_MATHMP_interpolation, only : init_interpolation
  use ppoh_MATHMP_rotation, only : init_rotation
  use ppoh_MATHMP_base, only : open_log_file
  use mpi
  implicit none
  character(len=*), intent(IN) :: comp_name
  character(len=*), intent(IN) :: config_file_name
  character(len=*), intent(IN) :: log_file_name
  integer :: log_level = 2
  logical :: is_initialized
  integer :: ierror
  integer :: t_value(8)


  call mpi_initialized(is_initialized, ierror)

  if (.not.is_initialized) call mpi_init(ierror)

  call mpi_comm_rank(MPI_COMM_WORLD, my_rank_global, ierror)

  call open_log_file(log_file_name, my_rank_global)

  call read_coupling_config(config_file_name)

  my_name = comp_name

  call jcup_set_new_comp(comp_name)

  log_level = get_run_mode()

  call jcup_initialize(comp_name, "SEC", log_level, (log_level >= 2))

  call jcup_get_mpi_parameter(trim(comp_name), my_comm, my_group, my_size, my_rank)


  call init_interpolation()

  call init_rotation(my_name)

  call MPI_COMM_rank(MPI_COMM_WORLD, my_rank_global, ierror)

  call date_and_time(values = t_value)

  initial_time(1:3) = t_value(1:3)
  initial_time(4:6) = t_value(5:7)

end subroutine ppoh_MATHMP_init


!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> define the 1D grid structure
subroutine ppoh_MATHMP_def_grid_1d(grid_name, grid_index)
  use jcup_interface, only : jcup_def_grid
  implicit none
  character(len=*), intent(IN) :: grid_name
  integer, intent(IN) :: grid_index(:)

  call jcup_def_grid(grid_index, my_name, grid_name)
  
end subroutine ppoh_MATHMP_def_grid_1d

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> define the 2D grid structure
subroutine ppoh_MATHMP_def_grid_2d(grid_name, nx, ny, is, ie, js, je)
  use jcup_interface, only : jcup_def_grid
  implicit none
  character(len=*), intent(IN) :: grid_name
  integer, intent(IN) :: nx, ny
  integer, intent(IN) :: is, ie, js, je
  integer, allocatable :: grid_index(:)
  integer :: i, j, counter

  allocate(grid_index((ie-is+1)*(je-js+1)))

  counter = 0
  do j = js, je
    do i = is, ie
      counter = counter + 1
      grid_index(counter) = i + nx*(j-1)
    end do
  end do

  call jcup_def_grid(grid_index, my_name, grid_name)

  deallocate(grid_index)
  
end subroutine ppoh_MATHMP_def_grid_2d

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> define the 3D grid structure
subroutine ppoh_MATHMP_def_grid_3d(grid_name, nx, ny, nz, is, ie, js, je, ks, ke)
  use jcup_interface, only : jcup_def_grid
  implicit none
  character(len=*), intent(IN) :: grid_name
  integer, intent(IN) :: nx, ny, nz
  integer, intent(IN) :: is, ie, js, je, ks, ke
  integer, allocatable :: grid_index(:)
  integer :: i, j, k, counter

  allocate(grid_index((ie-is+1)*(je-js+1)*(ke-ks+1)))

  counter = 0
  do k = ks, ke
    do j = js, je
      do i = is, ie
        counter = counter + 1
        grid_index(counter) = i + nx*(j-1) + nx*ny*(k-1)
      end do
    end do
  end do

  call jcup_def_grid(grid_index, my_name, grid_name)

  deallocate(grid_index)
  
end subroutine ppoh_MATHMP_def_grid_3d

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> define the grid structure
subroutine ppoh_MATHMP_end_init()
  use jcup_interface, only : jcup_end_grid_def, jcup_def_varp, jcup_def_varg, jcup_end_var_def, jcup_set_mapping_table
  use ppoh_MATHMP_namelist, only : exchange_config_type, get_num_of_comp_config, get_comp_config_ptr
  implicit none
  type (exchange_config_type), pointer :: conf_ptr
  integer :: i, j

  call jcup_end_grid_def()

  do i = 1, get_num_of_comp_config()
    conf_ptr => get_comp_config_ptr(i)
    if (trim(my_name) == trim(conf_ptr%send_comp)) then
      do j = 1, conf_ptr%num_of_data
        call jcup_def_varp(conf_ptr%ed(j)%varp_ptr, conf_ptr%send_comp, conf_ptr%ed(j)%send_data, conf_ptr%send_grid)
      end do 
    end if
  end do
  

  do i = 1, get_num_of_comp_config()
    conf_ptr => get_comp_config_ptr(i)
    if (trim(my_name) == trim(conf_ptr%recv_comp)) then
      do j = 1, conf_ptr%num_of_data
        call jcup_def_varg(conf_ptr%ed(j)%varg_ptr, conf_ptr%recv_comp, conf_ptr%ed(j)%recv_data, conf_ptr%recv_grid, 1, &
                           conf_ptr%send_comp, conf_ptr%ed(j)%send_data, conf_ptr%ed(j)%exchange_mode, conf_ptr%ed(j)%interval, &
                           -1, 1, j)
      end do 
    end if
  end do

  call jcup_end_var_def()

  call ppoh_MATHMP_set_mapping_table()

  call ppoh_MATHMP_init_time(initial_time(1:4))  

end subroutine ppoh_MATHMP_end_init

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> set mapping table
subroutine ppoh_MATHMP_set_mapping_table()
  use jcup_interface, only : jcup_set_mapping_table
  use ppoh_MATHMP_namelist, only : exchange_config_type, get_num_of_comp_config, get_comp_config_ptr
  use ppoh_MATHMP_mapping, only : read_mapping_text, read_mapping_binary
  use ppoh_MATHMP_interpolation, only : set_interpolation

  implicit none
  type (exchange_config_type), pointer :: conf_ptr
  integer :: i, j
  integer, pointer :: send_grid_index(:), recv_grid_index(:)
  real(kind=8), pointer :: coef(:,:)
 
  do i = 1, get_num_of_comp_config()
    conf_ptr => get_comp_config_ptr(i)
    if (trim(my_name) == trim(conf_ptr%send_comp)) then
       call jcup_set_mapping_table(my_name, conf_ptr%send_comp, conf_ptr%send_grid, conf_ptr%recv_comp, conf_ptr%recv_grid, 1)
       call set_interpolation(conf_ptr%send_comp, conf_ptr%send_grid, conf_ptr%recv_comp, conf_ptr%recv_grid, 1)
    end if
    if (trim(my_name) == trim(conf_ptr%recv_comp)) then
       if (conf_ptr%map_file_type == "T") then
         call read_mapping_text(conf_ptr%map_file_name, send_grid_index, recv_grid_index, coef)
       else
         call read_mapping_binary(conf_ptr%map_file_name, send_grid_index, recv_grid_index, coef)
       end if
       call jcup_set_mapping_table(my_name, conf_ptr%send_comp, conf_ptr%send_grid, conf_ptr%recv_comp, conf_ptr%recv_grid, 1, &
                                   send_grid_index, recv_grid_index)
       call set_interpolation(conf_ptr%send_comp, conf_ptr%send_grid, conf_ptr%recv_comp, conf_ptr%recv_grid, 1, &
                                   send_grid_index, recv_grid_index, coef)
       deallocate(send_grid_index, recv_grid_index, coef)
    end if
  end do


end subroutine ppoh_MATHMP_set_mapping_table

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> 
subroutine ppoh_MATHMP_init_time(time_array)
  use jcup_interface, only : jcup_init_time
  implicit none
  integer, intent(IN) :: time_array(4)

  current_time(:) = 0

  current_time(1:4) = time_array(1:4)

  call jcup_init_time(current_time)

end subroutine ppoh_MATHMP_init_time

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> set delta t of every time step
subroutine ppoh_MATHMP_set_time(delta_t)
  use jcup_interface, only : jcup_set_time, jcup_inc_time
  implicit none
  integer, intent(IN) :: delta_t

  call jcup_set_time(my_name, current_time, delta_t)

  call jcup_inc_time(my_name, current_time)

end subroutine ppoh_MATHMP_set_time


!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> put scalar data
subroutine ppoh_MATHMP_put_data(data_name, data)
  use ppoh_MATHMP_namelist, only : exchange_config_type, get_comp_config_ptr, get_num_of_comp_config
  use jcup_interface, only : jcup_put_value
  use ppoh_MATHMP_base, only : error
  implicit none
  character(len=*), intent(IN) :: data_name
  real(kind=8), intent(IN) :: data(*)
  type (exchange_config_type), pointer :: conf_ptr
  integer :: i, j

  do i = 1, get_num_of_comp_config()
    conf_ptr => get_comp_config_ptr(i)
    if (trim(my_name) == trim(conf_ptr%send_comp)) then
      do j = 1, conf_ptr%num_of_data
        if (trim(data_name) == conf_ptr%ed(j)%send_data) then
           call jcup_put_value(conf_ptr%ed(j)%varp_ptr, data)
           return
        end if
     end do
    end if
  end do

  call error("ppoh_MATHMP_put_data, no such data name is defined in configuration file : "//trim(data_name))

end subroutine ppoh_MATHMP_put_data

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> put 2D vector data
subroutine ppoh_MATHMP_put_data_vec_2d(data_name, data1, data2)
  use ppoh_MATHMP_namelist, only : vector_config_type, get_vector_config_ptr, get_num_of_vector_config
  use jcup_interface, only : jcup_put_value
  use ppoh_MATHMP_base, only : error
  implicit none
  character(len=*), intent(IN) :: data_name
  real(kind=8), intent(IN) :: data1(*), data2(*)
  type (vector_config_type), pointer :: conf_ptr
  integer :: i, j

  do i = 1, get_num_of_vector_config()
    conf_ptr => get_vector_config_ptr(i)
    if ((trim(my_name) == trim(conf_ptr%comp_name)).and.(trim(data_name)==trim(conf_ptr%data_name))) then
      call ppoh_MATHMP_put_data(conf_ptr%vector_data(1), data1)
      call ppoh_MATHMP_put_data(conf_ptr%vector_data(2), data2)
      return
    end if
  end do

  call error("ppoh_MATHMP_put_data_2d, no such data name is defined in configuration file : "//trim(data_name))

end subroutine ppoh_MATHMP_put_data_vec_2d

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> put 3D vector data
subroutine ppoh_MATHMP_put_data_vec_3d(data_name, data1, data2, data3)
  use ppoh_MATHMP_namelist, only : vector_config_type, get_vector_config_ptr, get_num_of_vector_config
  use jcup_interface, only : jcup_put_value
  use ppoh_MATHMP_base, only : error
  implicit none
  character(len=*), intent(IN) :: data_name
  real(kind=8), intent(IN) :: data1(*), data2(*), data3(*)
  type (vector_config_type), pointer :: conf_ptr
  integer :: i, j

  do i = 1, get_num_of_vector_config()
    conf_ptr => get_vector_config_ptr(i)
    if ((trim(my_name) == trim(conf_ptr%comp_name)).and.(trim(data_name)==trim(conf_ptr%data_name))) then
      call ppoh_MATHMP_put_data(conf_ptr%vector_data(1), data1)
      call ppoh_MATHMP_put_data(conf_ptr%vector_data(2), data2)
      call ppoh_MATHMP_put_data(conf_ptr%vector_data(3), data3)
      return
    end if
  end do

  call error("ppoh_MATHMP_put_data_3d, no such data name is defined in configuration file : "//trim(data_name))

end subroutine ppoh_MATHMP_put_data_vec_3d

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> get scalar data
subroutine ppoh_MATHMP_get_data(data_name, data)
  use ppoh_MATHMP_namelist, only : exchange_config_type, get_comp_config_ptr, get_num_of_comp_config
  use jcup_interface, only : jcup_get_value
  use ppoh_MATHMP_base, only : error
  implicit none
  character(len=*), intent(IN) :: data_name
  real(kind=8), intent(INOUT) :: data(*)
  type (exchange_config_type), pointer :: conf_ptr
  integer :: i, j

  do i = 1, get_num_of_comp_config()
    conf_ptr => get_comp_config_ptr(i)
    if (trim(my_name) == trim(conf_ptr%recv_comp)) then
      do j = 1, conf_ptr%num_of_data
        if (trim(data_name) == conf_ptr%ed(j)%recv_data) then
           call jcup_get_value(conf_ptr%ed(j)%varg_ptr, data)
           return
        end if
     end do
    end if
  end do

  call error("ppoh_MATHMP_get_data, no such data name is defined in configuration file : "//trim(data_name))

end subroutine ppoh_MATHMP_get_data

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> get vector 2D data
subroutine ppoh_MATHMP_get_data_vec_2d(data_name, data1, data2)
  use ppoh_MATHMP_namelist, only : vector_config_type, get_vector_config_ptr, get_num_of_vector_config
  use jcup_interface, only : jcup_get_value
  use ppoh_MATHMP_base, only : error
  implicit none
  character(len=*), intent(IN) :: data_name
  real(kind=8), intent(INOUT) :: data1(*), data2(*)
  type (vector_config_type), pointer :: conf_ptr
  integer :: i, j

  do i = 1, get_num_of_vector_config()
    conf_ptr => get_vector_config_ptr(i)
    if ((trim(my_name) == trim(conf_ptr%comp_name)).and.(trim(data_name)==trim(conf_ptr%data_name))) then
      call ppoh_MATHMP_get_data(conf_ptr%vector_data(1), data1)
      call ppoh_MATHMP_get_data(conf_ptr%vector_data(2), data2)
      return
    end if
  end do


  call error("ppoh_MATHMP_get_data_vec_2d, no such data name is defined in configuration file : "//trim(data_name))

end subroutine ppoh_MATHMP_get_data_vec_2d

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> get vector 3D data
subroutine ppoh_MATHMP_get_data_vec_3d(data_name, data1, data2, data3)
  use ppoh_MATHMP_namelist, only : vector_config_type, get_vector_config_ptr, get_num_of_vector_config
  use ppoh_MATHMP_base, only : error
  implicit none
  character(len=*), intent(IN) :: data_name
  real(kind=8), intent(INOUT) :: data1(*), data2(*), data3(*)
  type (vector_config_type), pointer :: conf_ptr
  integer :: i, j

  do i = 1, get_num_of_vector_config()
    conf_ptr => get_vector_config_ptr(i)
    if ((trim(my_name) == trim(conf_ptr%comp_name)).and.(trim(data_name)==trim(conf_ptr%data_name))) then
      call ppoh_MATHMP_get_data(conf_ptr%vector_data(1), data1)
      call ppoh_MATHMP_get_data(conf_ptr%vector_data(2), data2)
      call ppoh_MATHMP_get_data(conf_ptr%vector_data(3), data3)
      return
    end if
  end do

  call error("ppoh_MATHMP_get_data_vec_3d, no such data name is defined in configuration file : "//trim(data_name))

end subroutine ppoh_MATHMP_get_data_vec_3d

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> finalize ppOpen-MATH/MP
subroutine ppoh_MATHMP_finalize()
  use jcup_interface, only : jcup_coupling_end
  use ppoh_MATHMP_base, only : close_log_file
  implicit none

  call jcup_coupling_end(current_time)

  call close_log_file()

end subroutine ppoh_MATHMP_finalize

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> get mpi parameters 
subroutine ppoh_MATHMP_get_mpi_parameters(comm, size, rank)
  implicit none
  integer, intent(OUT) :: comm, size, rank

  comm  = my_comm
  size  = my_size
  rank  = my_rank

end subroutine ppoh_MATHMP_get_mpi_parameters


end module ppoh_MATHMP


subroutine interpolate_data(recv_model_name, send_model_name, mapping_tag, sn1, sn2, send_data, & 
                            rn1, rn2, recv_data, num_of_data, tn, exchange_tag)
  use ppoh_MATHMP_interpolation, only : interpolation
  implicit none
  character(len=*), intent(IN) :: recv_model_name, send_model_name
  integer, intent(IN) :: mapping_tag
  integer, intent(IN) :: sn1, sn2
  real(kind=8), intent(IN) :: send_data(sn1,sn2)
  integer, intent(IN) :: rn1, rn2
  real(kind=8), intent(INOUT) :: recv_data(rn1,rn2)
  integer, intent(IN) :: num_of_data
  integer, intent(IN) :: tn
  integer, intent(IN) :: exchange_tag(tn)

  call interpolation(recv_model_name, send_model_name, mapping_tag, send_data, recv_data, num_of_data, exchange_tag(1))

end subroutine interpolate_data

