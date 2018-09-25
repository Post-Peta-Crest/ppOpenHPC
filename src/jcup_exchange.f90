!====================================================================================================
!> @brief
!> jcup data exchange module

module jcup_exchange
  private

!--------------------------------   public  ----------------------------------!

  public :: send_data_scalar ! subroutine (varp, data)
  public :: recv_data_scalar ! subrouitne (varg, data)
  public :: write_all_scalar_data ! subroutine(fid, comp_id)
  public :: recv_all_scalar_data ! subroutine()

!--------------------------------   private  ---------------------------------!

contains

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> send scalar data 
!> @param[in] data_name name of data
!> @param[in] data 
! 2015/04/01 [NEW]
! 2015/05/18 [MOD] add call jml_ReduceMeanLocal
! 2015/06/22 [MOD] jml_ReduceMeanLocal -> jml_ReduceSumLocal
subroutine send_data_scalar(varp, data)
  use jcup_constant, only : NAME_LEN
  use jcup_data, only : varp_type, get_data_name
  use jcup_config, only : send_data_conf_type, get_send_data_conf_ptr
  use jcup_mpi_lib, only : jml_SendLeader, jml_isLocalLeader, jml_ReduceSumLocal
  use jcup_time, only : is_exchange_step
  use jcup_utils, only : put_log
  implicit none
  type(varp_type), pointer :: varp
  real(kind=8), intent(IN) :: data
  type(send_data_conf_type), pointer :: sd
  character(len=NAME_LEN) :: data_name
  real(kind=8) :: data_array(1)
  integer :: dest
  integer :: tag
  integer :: i

  data_name = get_data_name(varp)

  sd => get_send_data_conf_ptr(DATA_NAME = data_name)

  !!!!!!if (.not.jml_isLocalLeader(sd%model_id)) return ! 2015/06/17

  data_array(1) = data
  !data_buffer(1) = data
  do i = 1, sd%num_of_my_recv_data  
    if (.not.is_exchange_step(sd%model_id, 1, sd%my_recv_conf(i)%interval)) then
      !write(0,*) "send_data_scalar skipped ", sd%my_recv_conf(i)%interval
      cycle
    end if
   
    !call jml_ReduceMeanLocal(sd%model_id , data, data_array(1)) ! cal. mean value
    call jml_ReduceSumLocal(sd%model_id , data, data_array(1)) ! cal. sum value 2015/06/22 [MOD]

    dest = sd%my_recv_conf(i)%model_id - 1
    tag  = sd%my_recv_conf(i)%data_id
    call put_log("-------------------------------------------------------------------------------")
    call put_log("send gmean data, data name = "//trim(data_name))
    call jml_SendLeader(data_array, 1, 1, dest, tag)
  end do

end subroutine send_data_scalar

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> send scalar data 
!> @param[in] data_name name of data
!> @param[in] data 
! 2015/04/01 [NEW]
subroutine recv_data_scalar(varg, data)
  use jcup_constant, only : NAME_LEN, NO_DATA
  use jcup_mpi_lib, only : jml_ProbeLeader, jml_RecvLeader
  use jcup_data, only : varg_type, get_data_name
  use jcup_config, only : recv_data_conf_type, get_recv_data_conf_ptr
  use jcup_utils, only : put_log
  implicit none
  type(varg_type), pointer :: varg
  real(kind=8), intent(INOUT) :: data
  character(len=NAME_LEN) :: data_name
  real(kind=8) :: data_array(1)
  type(recv_data_conf_type), pointer :: rd
  integer :: source, tag

  data_name = get_data_name(varg)
  rd => get_recv_data_conf_ptr(DATA_NAME = data_name)

  source = rd%send_model_id - 1
  tag    = rd%data_id

  if (jml_ProbeLeader(source, tag)) then
    !write(0,*) "recv_data_scalar 1 ", source, tag
    call put_log("-------------------------------------------------------------------------------")
    call put_log("recv gmean data, data name = "//trim(data_name))
    call jml_RecvLeader(data_array, 1, 1, source, tag)
    !call jml_BcastLocal(rd%model_id, data_array, 1, 1)
    data = data_array(1)
  else
    data = NO_DATA
    !write(0,*) "recv_data_scalar 2 ", source, tag
  end if

end subroutine recv_data_scalar

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> write gmean data for restart
! 2015/04/06 [NEW]
subroutine write_all_scalar_data(fid, comp_id)
  use jcup_utils, only : put_log, IntToStr
  use jcup_constant, only : NAME_LEN
  use jcup_comp, only : get_num_of_total_component, is_my_component, get_component_name
  use jcup_config, only : recv_data_conf_type, get_num_of_recv_data, get_recv_data_conf_ptr_from_id
  use jcup_mpi_lib, only : jml_GetLeaderRank, jml_ProbeLeader, jml_RecvLeader
  implicit none
  integer, intent(IN) :: fid
  integer, intent(IN) :: comp_id
  type(recv_data_conf_type), pointer :: rd
  real(kind=8) :: data_array(1)
  character(len=NAME_LEN) :: comp_name
  integer :: source, tag
  integer :: j

    do j = 1, get_num_of_recv_data(comp_id)
       rd => get_recv_data_conf_ptr_from_id(comp_id, j)
       source = rd%send_model_id - 1
       tag    = rd%data_id
       if (jml_ProbeLeader(source, tag)) then
         call jml_RecvLeader(data_array, 1, 1, source, tag)
         comp_name = trim(get_component_name(rd%send_model_id))
         write(fid, *) comp_name
         write(fid, *) tag
         write(fid, *) data_array(1)
         call put_log("write restart gmean data to "//trim(comp_name)//", data tag = "//trim(IntToStr(tag)))
       end if
    end do  


end subroutine write_all_scalar_data

!=======+=========+=========+=========+=========+=========+=========+=========+
!> @breaf
!> clean up remained data
! 2015/04/03 [NEW]
! 2015/06/17 [MOD]
subroutine recv_all_scalar_data()
  use jcup_comp, only : get_num_of_total_component, is_my_component
  use jcup_config, only : recv_data_conf_type, get_num_of_recv_data, get_recv_data_conf_ptr_from_id
  use jcup_mpi_lib, only : jml_GetLeaderRank, jml_ProbeLeader, jml_RecvLeader, &
                           jml_ProbeAll, jml_RecvAll
  use mpi
  implicit none
  type(recv_data_conf_type), pointer :: rd
  real(kind=8) :: data_array(1)
  integer :: source, tag
  logical :: recv_flag
  integer :: i, j

  do i = 1, get_num_of_total_component()
    if (.not.is_my_component(i)) cycle
    do j = 1, get_num_of_recv_data(i)
         rd => get_recv_data_conf_ptr_from_id(i, j)
         source = rd%send_model_id - 1
         do  while(jml_ProbeAll(source))
             call jml_RecvAll(source) !Leader(data_array, 1, 1, source, MPI_ANY_TAG)
         end do
    end do  
  end do

  !do i = 1, get_num_of_total_component()
  !  if (.not.is_my_component(i)) cycle
  !  write(0, *) "recv_all_scalar_data of component loop ", i
  !  do 
  !    recv_flag = .false.
  !    do j = 1, get_num_of_recv_data(i)
  !       rd => get_recv_data_conf_ptr_from_id(i, j)
  !       source = rd%send_model_id - 1
  !       tag    = rd%data_id
  !      write(0,*) "recv_all_scalar_data ", i, j, source, tag
  !       if (jml_ProbeLeader(source, tag)) then
  !         write(0,*) "recv_all_scalar_data recv leader ", data_array(1)
  !         call jml_RecvLeader(data_array, 1, 1, source, tag)
  !         recv_flag = .true.
  !       end if
  !    end do  
  !    if (.not.recv_flag) exit
  !  end do
  !end do

end subroutine recv_all_scalar_data

end module jcup_exchange
