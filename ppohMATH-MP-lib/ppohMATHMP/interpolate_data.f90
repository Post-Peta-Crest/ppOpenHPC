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

  call interpolate_seism_to_fistr(mapping_tag, send_data, recv_data)

end subroutine interpolate_data
