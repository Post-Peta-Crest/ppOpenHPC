program comp2
  !use mpi
  use mod_common, only : NAME_LEN, STR_LEN
  use mod_field
  use ppoh_MATHMP
  implicit none
  character(len=STR_LEN) , parameter :: CONF_FILE = "coupling.nmlist"
  character(len=NAME_LEN), parameter :: MY_NAME = "FEM"
  character(len=NAME_LEN), parameter :: MY_GRID = "FEM_GRID"
  real(kind=8), allocatable :: data(:)
  integer :: t, i
  integer :: initial_time(4) = (/2015, 8, 10, 0/)
  type (field_type), pointer :: my_field


  call ppoh_MATHMP_init(MY_NAME, CONF_FILE, "SYSOUT.PE")

  call read_field_info(MY_NAME, CONF_FILE)

  my_field => get_field_ptr(MY_GRID)

  call ppoh_MATHMP_def_grid(MY_GRID, my_field%grid_index)

  call ppoh_MATHMP_end_init()

  allocate(data(size(my_field%grid_index)))

  do i = 1, size(data)
    data(i) = my_field%grid_index(i)*2
  end do

  !call ppoh_MATHMP_init_time(initial_time)

  call ppoh_MATHMP_put_data("FEM_T", data)  

  do t = 1, 2
  
    call ppoh_MATHMP_set_time(5)

    data(:) = 0.d0

    call ppoh_MATHMP_get_data("FDM_T", data)

    !write(0, *) data

    do i = 1, size(data)
      data(i) = my_field%grid_index(i)*2
    end do


    call ppoh_MATHMP_put_data("FEM_T", data)  

  end do

  call ppoh_MATHMP_finalize()

  stop 222

end program comp2
