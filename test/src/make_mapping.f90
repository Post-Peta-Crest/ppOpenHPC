program make_mapping
  use mod_common, only : NAME_LEN, STR_LEN
  use ppoh_MATHMP
  use mod_field
  implicit none
  character(len=STR_LEN) , parameter :: CONF_FILE = "coupling.nmlist"
  character(len=NAME_LEN), parameter :: COMP1 = "FDM"
  character(len=NAME_LEN), parameter :: COMP1_GRID = "FDM_GRID"
  character(len=NAME_LEN), parameter :: COMP2 = "FEM"
  character(len=NAME_LEN), parameter :: COMP2_GRID = "FEM_GRID"

  type (field_type), pointer :: comp1_field, comp2_field
  integer, pointer :: send_grid(:), recv_grid(:)
  real(kind=8), pointer :: coef(:)
  integer :: i

  call read_field_info(COMP1, CONF_FILE)
  comp1_field => get_field_ptr(COMP1_GRID)

  call read_field_info(COMP2, CONF_FILE)
  comp2_field => get_field_ptr(COMP2_GRID)

  write(0, *) "field size ", comp1_field%gnx, comp1_field%gny, comp2_field%gnx

  allocate(send_grid(comp1_field%gnx*comp1_field%gny))
  allocate(recv_grid(comp2_field%gnx))
  allocate(coef(size(recv_grid)))
  coef(:) = 1.d0
  do i = 1, size(send_grid)
    send_grid(i) = i
    recv_grid(i) = i
  end do

  call ppoh_MATHMP_write_mapping_binary("FDM_to_FEM_mapping.bin", send_grid, recv_grid, coef)  

end program make_mapping
