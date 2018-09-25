!=======+=========+=========+=========+=========+=========+=========+=========+
!> mapping table module
!! 
!! 
module ppoh_MATHMP_mapping
  implicit none
  include "mpif.h"
  private

!--------------------------------   public  ----------------------------------!

  public :: write_mapping_text   ! subroutine (file_name, send_grid, recv_grid, coef)
  public :: write_mapping_binary ! subroutine (file_name, send_grid, recv_grid, coef)
  public :: read_mapping_text    ! subroutine (file_name, send_grid, recv_grid, coef)
  public :: read_mapping_binary  ! subroutine (file_name, send_grid, recv_grid, coef)
  !public :: ppoh_MATHMP_set_default_mapping  ! subroutine (my_communicator, grid_pointer, src_address_all, dst_address_all)

!--------------------------------  private  ----------------------------------!

contains


!=======+=========+=========+=========+=========+=========+=========+=========+
!> write text format mapping table
!! @param[in] file_name file name
!! @param[in] send_grid(:) send grid index
!! @param[in] recv_grid(:) recv grid index
!! @param[in] coef(:,:) coefficients
subroutine write_mapping_text(file_name, send_grid, recv_grid, coef)
  use ppoh_MATHMP_base, only : LOG_FID
  implicit none
  character(len=*), intent(IN) :: file_name
  integer, intent(IN) :: send_grid(:)
  integer, intent(IN) :: recv_grid(:)
  real(kind=8), intent(IN) :: coef(:) ! coef
  integer :: num_scalar_coef = 1
  integer :: num_scoef, num_vcoef
  integer :: nf
  integer :: istat
  integer :: i, j

  nf = 543

 !     call ppoh_MATHMP_search_fileunit__run(nf) !OUT
  write(LOG_FID,*) "ppoh_MATHMP_write_mapping_table: open, unit=",nf,", file=",trim(file_name)
  write(LOG_FID,*) "                        write data for remapping"

  open(nf, file=trim(file_name), form='formatted', status='replace', iostat = istat)

  if (istat /= 0) then
    write(0, *) "ppoh_MATHMP_write_mapping_table_ascii, file open error !!! file = "//trim(file_name)
    stop 999
  end if

 
  write(nf, *) num_scalar_coef, size(coef,1)

  do i = 1, size(send_grid)
    write(nf, *) recv_grid(i), send_grid(i), coef(j)
  end do

  close(nf)

end subroutine write_mapping_text

!=======+=========+=========+=========+=========+=========+=========+=========+
!> write text format mapping table
!! @param[in] file_name file name
!! @param[in] send_grid(:) send grid index
!! @param[in] recv_grid(:) recv grid index
!! @param[in] coef(:,:) coefficients
subroutine write_mapping_text_v(file_name, send_grid, recv_grid, coef)
  use ppoh_MATHMP_base, only : LOG_FID
  implicit none
  character(len=*), intent(IN) :: file_name
  integer, intent(IN) :: send_grid(:)
  integer, intent(IN) :: recv_grid(:)
  real(kind=8), intent(IN) :: coef(:,:) ! coef(1 + num_of_vector_coef, num_of_links)
  integer :: num_scalar_coef = 1
  integer :: num_scoef, num_vcoef
  integer :: nf
  integer :: istat
  integer :: i, j

  nf = 543

 !     call ppoh_MATHMP_search_fileunit__run(nf) !OUT
  write(LOG_FID,*) "ppoh_MATHMP_write_mapping_table: open, unit=",nf,", file=",trim(file_name)
  write(LOG_FID,*) "                        write data for remapping"

  open(nf, file=trim(file_name), form='formatted', status='replace', iostat = istat)

  if (istat /= 0) then
    write(0, *) "ppoh_MATHMP_write_mapping_table_ascii, file open error !!! file = "//trim(file_name)
    stop 999
  end if

  write(nf, *) size(coef,1), size(coef,2)

  do i = 1, size(send_grid)
    write(nf, *) recv_grid(i), send_grid(i), (coef(j, i), j = 1, size(coef, 1))
  end do

  close(nf)

end subroutine write_mapping_text_v

!=======+=========+=========+=========+=========+=========+=========+=========+
!> write binary format mapping table
!! @param[in] file_name file name
!! @param[in] send_grid(:) send grid index
!! @param[in] recv_grid(:) recv grid index
!! @param[in] coef(:,:) coefficients
subroutine write_mapping_binary(file_name, send_grid, recv_grid, coef)
  use ppoh_MATHMP_base, only : LOG_FID
  implicit none
  character(len=*), intent(IN) :: file_name
  integer, intent(IN) :: send_grid(:)
  integer, intent(IN) :: recv_grid(:)
  real(kind=8), intent(IN) :: coef(:) 
  integer :: num_scalar_coef = 1
  integer :: num_scoef, num_vcoef
  integer :: nf
  integer :: istat
  integer :: i, j

  nf = 543

 !     call ppoh_MATHMP_search_fileunit__run(nf) !OUT
  write(LOG_FID,*) "ppoh_MATHMP_write_mapping_table: open, unit=",nf,", file=",trim(file_name)
  write(LOG_FID,*) "                        write data for remapping"

  open(nf, file=trim(file_name), form='unformatted', status='replace', iostat = istat)

  if (istat /= 0) then
    write(0, *) "ppoh_MATHMP_write_mapping_table_binary, file open error !!! file = "//trim(file_name)
    stop 999
  end if

  write(nf) num_scalar_coef, size(coef,1)

  write(nf) send_grid
  write(nf) recv_grid
  write(nf) coef

  close(nf)

end subroutine write_mapping_binary

!=======+=========+=========+=========+=========+=========+=========+=========+
!> write binary format mapping table
!! @param[in] file_name file name
!! @param[in] send_grid(:) send grid index
!! @param[in] recv_grid(:) recv grid index
!! @param[in] coef(:,:) coefficients
subroutine write_mapping_binary_v(file_name, send_grid, recv_grid, coef)
  use ppoh_MATHMP_base, only : LOG_FID
  implicit none
  character(len=*), intent(IN) :: file_name
  integer, intent(IN) :: send_grid(:)
  integer, intent(IN) :: recv_grid(:)
  real(kind=8), intent(IN) :: coef(:,:) ! coef(num_of_scalor_coef + num_of_vector_coef*2, num_of_links)
  integer :: num_scalar_coef = 1
  integer :: num_scoef, num_vcoef
  integer :: nf
  integer :: istat
  integer :: i, j

  nf = 543

 !     call ppoh_MATHMP_search_fileunit__run(nf) !OUT
  write(LOG_FID,*) "ppoh_MATHMP_write_mapping_table: open, unit=",nf,", file=",trim(file_name)
  write(LOG_FID,*) "                        write data for remapping"

  open(nf, file=trim(file_name), form='unformatted', status='replace', iostat = istat)

  if (istat /= 0) then
    write(0, *) "ppoh_MATHMP_write_mapping_table_binary, file open error !!! file = "//trim(file_name)
    stop 999
  end if

  write(nf) size(coef,1), size(coef,2)

  write(nf) send_grid
  write(nf) recv_grid
  write(nf) coef

  close(nf)

end subroutine write_mapping_binary_v

!=======+=========+=========+=========+=========+=========+=========+=========+
!> read text format mapping table
!! \protected
subroutine read_mapping_text(file_name, send_grid, recv_grid, coef)
  use ppoh_MATHMP_base, only : LOG_FID
  implicit none
  character(len=*), intent(IN) :: file_name
  integer, pointer :: send_grid(:)
  integer, pointer :: recv_grid(:)
  real(kind=8), pointer :: coef(:,:) ! coef(num_of_scalor_coef + num_of_vector_coef*2, num_of_links)
  integer :: num_coef, num_list
  integer :: nf
  integer :: i, j

  nf = 543

 !     call ppoh_MATHMP_search_fileunit__run(nf) !OUT
  write(LOG_FID,*) "ppoh_MATHMP_read_mapping_table: open, unit=",nf,", file=",trim(file_name)
  write(LOG_FID,*) "                        write data for remapping"

  open(nf, file=trim(file_name), form='formatted',action='read')

  read(nf, *) num_coef, num_list

  allocate(send_grid(num_list))
  allocate(recv_grid(num_list))
  allocate(coef(num_coef, num_list))

  do i = 1, num_list
    read(nf, *) recv_grid(i), send_grid(i), (coef(j, i), j = 1, num_coef)
  end do

  close(nf)

end subroutine read_mapping_text

!=======+=========+=========+=========+=========+=========+=========+=========+
!> read binary format mapping table
!! \protected
subroutine read_mapping_binary(file_name, send_grid, recv_grid, coef)
  use ppoh_MATHMP_base, only : LOG_FID
  implicit none
  character(len=*), intent(IN) :: file_name
  integer, pointer :: send_grid(:)
  integer, pointer :: recv_grid(:)
  real(kind=8), pointer :: coef(:,:) ! coef(1 + num_of_vector_coef, num_of_links)
  character(len=256) :: log_str
  integer :: num_scoef, num_vcoef
  integer :: num_coef, num_list
  integer :: nf
  integer :: i, j

  nf = 543

 !     call ppoh_MATHMP_search_fileunit__run(nf) !OUT
  write(LOG_FID, *) " ppoh_MATHMP_read_mapping_table: open, unit=",nf,", file=",trim(file_name)
  write(LOG_FID, *) "                        write data for remapping"

  open(nf, file=trim(file_name), form='unformatted',action='read')

  read(nf) num_coef, num_list

  allocate(send_grid(num_list))
  allocate(recv_grid(num_list))
  allocate(coef(num_coef, num_list))

  read(nf) send_grid
  read(nf) recv_grid
  read(nf) coef
  
  close(nf)

end subroutine read_mapping_binary

!=======+=========+=========+=========+=========+=========+=========+=========+
!> set default mapping table
!! \protected
subroutine set_default_mapping(global_field_size, send_grid, recv_grid)
  use ppoh_MATHMP_base, only : LOG_FID
  implicit none
  integer, intent(IN) :: global_field_size
  integer, pointer :: send_grid(:), recv_grid(:)
  integer :: i

  write(LOG_FID,*) " "
  write(LOG_FID,*) "ppoh_MATHMP_set_default_mapping"
  write(LOG_FID,*) "   global field  size = ", global_field_size

  allocate(send_grid(global_field_size), recv_grid(global_field_size))

  do i = 1, global_field_size
    send_grid(i) = i
    recv_grid(i) = i
  end do

end subroutine set_default_mapping

!=======+=========+=========+=========+=========+=========+=========+=========+

end module ppoh_MATHMP_mapping
