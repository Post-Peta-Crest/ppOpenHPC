program comp1
  !use mpi
  use mod_common, only : NAME_LEN, STR_LEN 
  use mod_field
  use ppoh_MATHMP
  implicit none
  character(len=STR_LEN), parameter  :: CONF_FILE = "coupling.nmlist"
  character(len=NAME_LEN), parameter :: MY_NAME = "FDM"
  character(len=NAME_LEN), parameter :: MY_GRID = "FDM_GRID" 
  integer :: initial_time(4) = (/2015, 8, 10, 0/)
  type (field_type), pointer :: my_field
  real(kind=8), allocatable :: data_T(:,:,:)
  integer :: t, i, j, k

  
  call ppoh_MATHMP_init(MY_NAME, CONF_FILE, "SYSOUT.PE")

  call read_field_info(MY_NAME, CONF_FILE)

  my_field => get_field_ptr(MY_GRID)

  call ppoh_MATHMP_def_grid(MY_GRID, my_field%gnx, my_field%gny, my_field%gnz, &
                            my_field%is, my_field%ie, my_field%js, my_field%je, my_field%ks, my_field%ke)

  call ppoh_MATHMP_end_init()

  !call ppoh_MATHMP_init_time(initial_time)

  allocate(data_t(my_field%ie-my_field%is+1, my_field%je-my_field%js+1, my_field%ke-my_field%ks+1))

  do k = my_field%ks, my_field%ke
    do j = my_field%js, my_field%je
      do i = my_field%is, my_field%ie
        data_t(i-my_field%is+1, j-my_field%js+1, k-my_field%ks+1) = i + my_field%gnx*(j-1) + my_field%gnx*my_field%gny*(k-1)
      end do
    end do
  end do

  call ppoh_MATHMP_put_data("FDM_T", data_t)  

  
  do t = 1, 10

    call ppoh_MATHMP_set_time(1)

    data_t = 0.d0

    call ppoh_MATHMP_get_data("FEM_T", data_t)

    do k = my_field%ks, my_field%ke
      do j = my_field%js, my_field%je
        do i = my_field%is, my_field%ie
          data_t(i-my_field%is+1, j-my_field%js+1, k-my_field%ks+1) = i + my_field%gnx*(j-1) + my_field%gnx*my_field%gny*(k-1)
        end do
      end do
    end do

    call ppoh_MATHMP_put_data("FDM_T", data_t)  
    
  end do

  call ppoh_MATHMP_finalize()

  stop 111


end program
