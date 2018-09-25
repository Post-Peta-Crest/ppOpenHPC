!
!Copyright (c) 2011, arakawa@rist.jp
!All rights reserved.
!
module jcup_mpi_lib
  implicit none
  include "mpif.h"

  private

  !--------------------------------   public  ----------------------------------!

  integer,public :: JML_ROOTS_TAG = 120
  integer,public :: JML_ANY_SOURCE = MPI_ANY_SOURCE

  public :: jml_init ! subroutine (isCallInit)
  public :: jml_create_communicator
  public :: jml_finalize
  public :: jml_GetCommGlobal     ! integer function ()
  public :: jml_GetMyrankGlobal   ! integer function ()
  public :: jml_GetCommSizeGlobal ! integer function ()
  public :: jml_GetComm       ! integer function (component_id)
  public :: jml_GetCommNULL   ! integer function ()
  public :: jml_isRoot        ! logical function ()
  public :: jml_GetMyGroup    ! integer function (component_id)
  public :: jml_GetMyrank     ! integer function (component_id)
  public :: jml_GetCommSizeLocal   ! integer funciion (component_id)
  public :: jml_GetModelRankOffset ! integer function (my_comp_id, target_comp_id)
  public :: jml_GetMyrankModel     ! integer function (my_comp_id, target_comp_id)
  public :: jml_GetLeaderRank ! integer function (component_id)
  public :: jml_isLocalLeader ! logical function (component_id)
  public :: jml_BcastGlobal   ! subroutine (string, source_rank) or (data_array, is, ie, source_rank)
  public :: jml_SendGlobal    ! subroutine (string, dest) or (string, str_len, dest) or (data, is, ie, dest)
  public :: jml_RecvGlobal    ! subroutine (string, source) or (string, str_len, source) or (data, is, ie, source)
  public :: jml_AllreduceMax  ! subroutine (data, is, ie, result)
  public :: jml_AllreduceMin  ! subroutine (data, is, ie, result)
  public :: jml_ReduceSumLocal
  public :: jml_ReduceMinLocal
  public :: jml_ReduceMaxLocal
  public :: jml_AllReduceMaxLocal
  public :: jml_BcastLocal
  public :: jml_GatherLocal
  public :: jml_ScatterLocal
  public :: jml_GatherVLocal
  public :: jml_ScatterVLocal
  public :: jml_SendLocal
  public :: jml_RecvLocal
  public :: jml_SendLeader
  public :: jml_RecvLeader
  public :: jml_SendModel
  public :: jml_RecvModel
  public :: jml_set_num_of_isend
  public :: jml_set_num_of_irecv
  public :: jml_ISendLocal
  public :: jml_IRecvLocal
  public :: jml_ISendModel
  public :: jml_IRecvModel
  public :: jml_send_waitall
  public :: jml_recv_waitall

  public :: jml_set_send_recv_buffer ! subroutine (buffer_size)

  public :: jml_send_local_test
  public :: jml_recv_local_test

!--------------------------------   private  ---------------------------------!

  integer, parameter, private :: MPI_MY_TAG = 0

  interface jml_BcastGlobal
    module procedure jml_bcast_string_global_1, jml_bcast_string_global_2
    module procedure jml_bcast_int_1d_global
  end interface

  interface jml_SendGlobal
    module procedure jml_send_string_global_1, jml_send_string_global_2
    module procedure jml_send_int_1d_global
  end interface

  interface jml_RecvGlobal
    module procedure jml_recv_string_global_1, jml_recv_string_global_2
    module procedure jml_recv_int_1d_global
  end interface
  
  interface jml_AllReduceSum
    module procedure jml_allreduce_sumint1d
  end interface

  interface jml_AllReduceMax
    module procedure jml_allreduce_maxint1d
  end interface

  interface jml_AllReduceMin
    module procedure jml_allreduce_minint1d
  end interface

  interface jml_ReduceSum
    module procedure jml_reduce_sum_int_1d
  end interface


  interface jml_ReduceSumLocal
    module procedure jml_reduce_sum_int_1d_local
  end interface

  interface jml_ReduceMinLocal
    module procedure jml_reduce_min_int_1d_local
  end interface

  interface jml_ReduceMaxLocal
    module procedure jml_reduce_max_int_1d_local
  end interface

  interface jml_AllReduceMaxLocal
    module procedure jml_allreduce_max_int_1d_local
  end interface

  interface jml_BcastLocal
    module procedure jml_bcast_string_local
    module procedure jml_bcast_int_1d_local, jml_bcast_real_1d_local, jml_bcast_double_1d_local
  end interface

  interface jml_GatherLocal
    module procedure jml_gather_int_1d_local, jml_gather_real_1d_local
  end interface

  interface jml_ScatterLocal
    module procedure jml_scatter_int_1d_local
  end interface

  interface jml_GatherVLocal
    module procedure jml_gatherv_int_1d_local, jml_gatherv_double_1d_local
  end interface

  interface jml_ScatterVLocal
    module procedure jml_scatterv_int_1d_local, jml_scatterv_double_1d_local
  end interface

  interface jml_SendLocal
    module procedure jml_send_int_1d_local, jml_send_int_2d_local, jml_send_int_3d_local
    module procedure jml_send_real_2d_local, jml_send_real_3d_local
    module procedure jml_send_double_1d_local, jml_send_double_2d_local, jml_send_double_3d_local
  end interface

  interface jml_RecvLocal
    module procedure jml_recv_int_1d_local, jml_recv_int_2d_local, jml_recv_int_3d_local
    module procedure jml_recv_real_2d_local, jml_recv_real_3d_local
    module procedure jml_recv_double_1d_local, jml_recv_double_2d_local, jml_recv_double_3d_local
  end interface

  interface jml_SendLeader
    module procedure jml_send_int_1d_leader, jml_send_int_2d_leader, jml_send_int_3d_leader
    module procedure jml_send_real_1d_leader, jml_send_real_2d_leader, jml_send_real_3d_leader
    module procedure jml_send_double_1d_leader, jml_send_double_2d_leader, jml_send_double_3d_leader
  end interface

  interface jml_RecvLeader
    module procedure jml_recv_int_1d_leader, jml_recv_int_2d_leader, jml_recv_int_3d_leader
    module procedure jml_recv_real_1d_leader, jml_recv_real_2d_leader, jml_recv_real_3d_leader
    module procedure jml_recv_double_1d_leader, jml_recv_double_2d_leader, jml_recv_double_3d_leader
  end interface

  interface jml_GatherLeader
    module procedure jml_gather_int_1d_leader, jml_gather_real_1d_leader
  end interface

  interface jml_BcastLeader
    module procedure jml_bcast_string_leader
    module procedure jml_bcast_int_1d_leader, jml_bcast_real_1d_leader
  end interface


  interface jml_SendModel
    module procedure jml_send_int_1d_model , jml_send_double_1d_model
    module procedure jml_send_real_2d_model, jml_send_double_2d_model
    module procedure jml_send_real_3d_model, jml_send_double_3d_model
  end interface

  interface jml_RecvModel
    module procedure jml_recv_int_1d_model , jml_recv_double_1d_model
    module procedure jml_recv_real_2d_model, jml_recv_double_2d_model
    module procedure jml_recv_real_3d_model, jml_recv_double_3d_model
  end interface

  interface jml_ISendLocal
    module procedure jml_isend_double_1d_local
  end interface

  interface jml_IRecvLocal
    module procedure jml_irecv_double_1d_local
  end interface

  interface jml_ISendModel
    module procedure jml_isend_double_1d_model
  end interface

  interface jml_IRecvModel
    module procedure jml_irecv_double_1d_model
  end interface


  type comm_type
    integer :: group_id
    integer :: group
    integer :: num_of_pe ! number of processors in this group
    integer :: root_rank ! local root rank ( = 0)
    integer :: my_rank ! my rank in this group
    integer :: leader_rank ! global rank of leader processor
    integer :: mpi_comm ! communicator
    integer :: pe_offset ! processor number offset on inter communication
    type(comm_type), pointer :: inter_comm(:)
  end type

  type(comm_type) :: global

  type(comm_type) :: leader        ! leader communicator
  integer, pointer :: leader_pe(:) ! conversion table from component id to leader pe
 
  integer :: num_of_total_component ! number of total component
  type(comm_type), pointer :: local(:)
  type(comm_type), pointer :: current_comp ! current_component

  integer :: ierror 
  logical :: isInitialized = .false. ! initialize flag
  integer :: num_of_models

  integer :: size_int, size_real, size_double

  integer :: buffer_size = 100
  real(kind=8), pointer :: local_buffer(:)


  integer, private          :: isend_counter
  integer, private, pointer :: isend_request(:)
  integer, private, pointer :: isend_status(:,:)

  integer, private          :: irecv_counter
  integer, private, pointer :: irecv_request(:)
  integer, private, pointer :: irecv_status(:,:)

contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_init(isCallInit)
    implicit none
    logical,intent(IN) :: isCallInit

    ! MPI Initialize
    if (isCallInit) call MPI_INIT(ierror)
    call MPI_COMM_GROUP(MPI_COMM_WORLD,global%group,ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,global%num_of_pe,ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD,global%my_rank,ierror)
    global%mpi_comm = MPI_COMM_WORLD
    global%root_rank = 0

   ! set initialize flag
   isInitialized = .true.

   call MPI_Type_size(MPI_INTEGER, size_int, ierror)
   call MPI_Type_size(MPI_REAL,    size_real, ierror)
   call MPI_Type_size(MPI_DOUBLE_PRECISION, size_double, ierror)

   if (.not.associated(local_buffer)) then
     allocate(local_buffer(buffer_size))
     call mpi_buffer_attach(local_buffer, 8*size(local_buffer), ierror)
   end if

end subroutine jml_init

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_comm(comm)
  implicit none
  type(comm_type), intent(INOUT) :: comm
  integer, allocatable :: dummy(:)
  integer :: i

  allocate(dummy(0))
  
  comm%group = maxval(dummy) ! set minimal value
  comm%group_id = MPI_UNDEFINED
  comm%num_of_pe = MPI_UNDEFINED
  comm%root_rank = MPI_UNDEFINED
  comm%my_rank = MPI_UNDEFINED
  comm%leader_rank = MPI_UNDEFINED
  comm%mpi_comm = MPI_COMM_NULL
  comm%pe_offset = 0
  
end subroutine init_comm
  
!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_create_communicator(my_comp_id)
    implicit none
    integer,intent(IN) :: my_comp_id(:) ! component id number of my PE
    integer :: send_buffer(1), recv_buffer(1)
    integer :: remote_leader
    integer :: color, key
    integer :: i, j
    integer :: istat

    send_buffer(1) = maxval(my_comp_id)

    call mpi_Allreduce(send_buffer, recv_buffer, 1, MPI_INTEGER, MPI_MAX, global%mpi_comm, ierror)
 
    num_of_total_component = recv_buffer(1)

    allocate(local(num_of_total_component))
    do i = 1, num_of_total_component
     call init_comm(local(i))
     allocate(local(i)%inter_comm(num_of_total_component))
     do j = 1, num_of_total_component
       call init_comm(local(i)%inter_comm(j))
     end do
    end do

    ! create new group
    ! set color
    key = global%my_rank
    do i = 1, num_of_total_component
      color = MPI_UNDEFINED
      do j = 1, size(my_comp_id)
        if (i == my_comp_id(j)) then
          color = i
          exit
        end if
      end do
      call jml_create_new_communicator(color, key, local(i)) ! set local communicator
    end do
  


    do i = 1, num_of_total_component
      send_buffer(1) = local(i)%leader_rank
      call mpi_Allreduce(send_buffer, recv_buffer, 1, MPI_INTEGER, MPI_MAX, global%mpi_comm, ierror)
      local(i)%leader_rank = recv_buffer(1)
    end do

    do i = 1, num_of_total_component
      send_buffer(1) = local(i)%num_of_pe
      call jml_BcastGlobal(send_buffer, 1, 1, local(i)%leader_rank)
      local(i)%num_of_pe = send_buffer(1)
    end do

    do i = 1, num_of_total_component
      send_buffer(1) = local(i)%group
      call jml_BcastGlobal(send_buffer, 1, 1, local(i)%leader_rank)
      local(i)%group = send_buffer(1)
    end do

    do i = 1, num_of_total_component
      send_buffer(1) = local(i)%group_id
      call jml_BcastGlobal(send_buffer, 1, 1, local(i)%leader_rank)
      local(i)%group_id = send_buffer(1)
    end do

    do i = 1, num_of_total_component
      send_buffer(1) = local(i)%leader_rank
      call jml_BcastGlobal(send_buffer, 1, 1, local(i)%leader_rank)
      local(i)%leader_rank = send_buffer(1)
    end do

    do i = 1, num_of_total_component
      do j = i+1, num_of_total_component
        call jml_create_intercomponent_communicator(local(i), local(j))
      end do
    end do

    !!!!do i = 1, num_of_total_component
    !!!!  if (is_my_component(local(i))) then
    !!!!    call write_comm_info(global%my_rank+100, local(i))
    !!!!  end if     
    !!!!end do


    call init_comm(leader)
    call jml_create_leader_communicator()

end subroutine jml_create_communicator

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function is_my_component(comm)
  implicit none
  type(comm_type), intent(IN) :: comm

  is_my_component = (comm%my_rank/=MPI_UNDEFINED)

end function is_my_component

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine set_current_component(component_id)
  implicit none
  integer, intent(IN) :: component_id
  integer :: i

  do i = 1, num_of_total_component
    if ((local(i)%group_id==component_id)) then
      current_comp => local(i)
      return
    end if
  end do

  write(0,'("set_current_component: not such component id ",I0)') component_id

  call mpi_finalize(i)
  stop

end subroutine set_current_component

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_create_new_communicator(color,key,comm)
  implicit none
  integer, intent(IN) :: color, key
  type(comm_type), intent(INOUT) :: comm

  comm%group_id = color

  call MPI_COMM_SPLIT(global%mpi_comm, color, key, comm%mpi_comm, ierror)
 
  ! reset local procnum, local rank
  if (comm%mpi_comm /= MPI_COMM_NULL) then
      comm%root_rank = 0
      call MPI_COMM_GROUP(comm%mpi_comm,comm%group, ierror)
      call MPI_COMM_SIZE(comm%mpi_comm,comm%num_of_pe,ierror)
      call MPI_COMM_RANK(comm%mpi_comm,comm%my_rank,ierror)
      if (comm%my_rank == 0) then
         comm%leader_rank = global%my_rank
      end if
      !write(0,*) "mpi local comm ",color,key,local%mpi_comm, local%group, local%num_of_pe, local%my_rank
   end if


end subroutine jml_create_new_communicator

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_create_leader_communicator()
  implicit none
  integer :: color, key
  integer :: res
  integer :: i

  allocate(leader_pe(0:num_of_total_component-1))
  leader_pe(:) = 0

  color = MPI_UNDEFINED
  key   = 0 !MPI_UNDEFINED
  do i = 1, num_of_total_component
    if (local(i)%leader_rank == global%my_rank) then
      color = 1 ! local leader
      !key   = global%my_rank
    end if
  end do

  call jml_create_new_communicator(color, key, leader)

  do i = 1, num_of_total_component
    if (local(i)%leader_rank == global%my_rank) then
      leader_pe(i-1) = leader%my_rank
    end if
    if (leader%mpi_comm/=MPI_COMM_NULL) then
      call MPI_ALLREDUCE(leader_pe(i-1:i-1),res,1,MPI_INTEGER,MPI_MAX,leader%mpi_comm,ierror)
      leader_pe(i-1) = res
    end if
  end do

end subroutine jml_create_leader_communicator

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_create_intercomponent_communicator(comm1, comm2)
  implicit none
  type(comm_type), intent(INOUT) :: comm1, comm2
  integer :: new_group
  integer :: new_comm
  integer :: new_size
  integer :: new_rank
  integer :: color
  integer :: target_id

  new_group = MPI_UNDEFINED
  color = MPI_UNDEFINED

  if (is_my_component(comm1).or.is_my_component(comm2)) then
    color = 1
  end if

  call mpi_comm_split(global%mpi_comm, color, 0, new_comm, ierror)

  if (is_my_component(comm1).or.is_my_component(comm2)) then

    call mpi_comm_size(new_comm, new_size, ierror)
    call mpi_comm_rank(new_comm, new_rank, ierror)

    !write(0,*) "create new_group ", global%my_rank, new_group, comm1%group_id, comm2%group_id, &
    !                                new_comm, new_size, new_rank

    if (is_my_component(comm1)) then
      target_id = comm2%group_id
      comm1%inter_comm(target_id)%group_id = target_id
      comm1%inter_comm(target_id)%mpi_comm = new_comm
      comm1%inter_comm(target_id)%my_rank = new_rank
      comm1%inter_comm(target_id)%pe_offset = cal_pe_offset(comm1%leader_rank, comm1%num_of_pe, comm2%leader_rank)
    end if
    if (is_my_component(comm2)) then
      target_id = comm1%group_id
      comm2%inter_comm(target_id)%group_id = target_id
      comm2%inter_comm(target_id)%mpi_comm = new_comm
      comm2%inter_comm(target_id)%my_rank = new_rank
      comm2%inter_comm(target_id)%pe_offset = cal_pe_offset(comm2%leader_rank, comm2%num_of_pe, comm1%leader_rank)
    end if
  end if

end subroutine jml_create_intercomponent_communicator

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function cal_pe_offset(my_leader_rank, my_size, target_leader_rank)
  implicit none
  integer, intent(IN) :: my_leader_rank, my_size
  integer, intent(IN) :: target_leader_rank
 
  cal_pe_offset = 0
  if (target_leader_rank>=my_leader_rank+my_size) then
    cal_pe_offset = my_size
  else
    cal_pe_offset = max(target_leader_rank-my_leader_rank,0)
  end if

end function cal_pe_offset

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine write_comm_info(unit, comm)
  implicit none
  integer, intent(IN) :: unit
  type(comm_type), intent(IN) :: comm
  integer :: i

  write(unit, *) "communicator information"  
  write(unit, '("  component id   =",I4)') comm%group_id
  write(unit, '("  mpi groupe  =",I0)') comm%group
  write(unit, '("  communicator=",I0)') comm%mpi_comm
  write(unit, '("  local size  =",I4)') comm%num_of_pe
  write(unit, '("  my rank     =",I4)') comm%my_rank
  write(unit, '("    inter communicator")') 
   
  do i = 1, num_of_total_component
    if (i/=comm%group_id) then
      write(unit, '("      target id    = ", I3)') comm%inter_comm(i)%group_id
      write(unit, '("      communicator = ", I0)')  comm%inter_comm(i)%mpi_comm
      write(unit, '("      my rank      = ", I0)')  comm%inter_comm(i)%my_rank
      write(unit, '("      pe offset    = ", I3)') comm%inter_comm(i)%pe_offset
      write(unit,*)
    end if 
  end do

end subroutine write_comm_info

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_finalize()
    implicit none

    call mpi_buffer_detach(local_buffer, 8*size(local_buffer), ierror)

    !!if (allocated(local_buffer)) deallocate(local_buffer)

    if (isInitialized) then
      call MPI_FINALIZE(ierror)
    end if
end subroutine jml_finalize

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function jml_GetCommGlobal()
  implicit none

  jml_GetCommGlobal = global%mpi_comm

end function jml_GetCommGlobal

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function jml_GetMyrankGlobal()
  implicit none

  jml_GetMyrankGlobal = global%my_rank

end function jml_GetMyrankGlobal

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function jml_GetCommSizeGlobal()
  implicit none
  
  jml_GetCommSizeGlobal = global%num_of_pe

end function jml_GetCommSizeGlobal

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function jml_isRoot()
    implicit none
    jml_isRoot=.false.
    if (global%root_rank==global%my_rank) jml_isRoot=.true.
end function jml_isRoot

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function jml_GetComm(component_id)
  implicit none
  integer, intent(IN) :: component_id

  call set_current_component(component_id)
  jml_GetComm = current_comp%mpi_comm

end function jml_GetComm

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function jml_GetCommNULL()
  implicit none
  
  jml_GetCommNULL = MPI_COMM_NULL

end function jml_GetCommNULL

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function jml_GetMyGroup(component_id)
  implicit none
  integer, intent(IN) :: component_id

  call set_current_component(component_id)
  jml_GetMyGroup = current_comp%group

end function jml_GetMyGroup

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function jml_GetMyrank(component_id)
  implicit none
  integer, intent(IN) :: component_id

  call set_current_component(component_id)
  jml_GetMyrank = current_comp%my_rank

end function jml_GetMyrank

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function jml_GetCommSizeLocal(component_id)
  implicit none
  integer, intent(IN) :: component_id

  call set_current_component(component_id)
  jml_GetCommSizeLocal = current_comp%num_of_pe

end function jml_GetCommSizeLocal

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function jml_GetModelRankOffset(my_comp_id, target_comp_id)
  implicit none
  integer, intent(IN) :: my_comp_id, target_comp_id

  jml_GetModelRankOffset = local(my_comp_id)%inter_comm(target_comp_id)%pe_offset

end function jml_GetModelRankOffset

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function jml_GetMyrankModel(my_comp_id, target_comp_id)
  implicit none
  integer, intent(IN) :: my_comp_id, target_comp_id

  jml_GetMyrankModel = local(my_comp_id)%inter_comm(target_comp_id)%my_rank

end function jml_GetMyrankModel

!=======+=========+=========+=========+=========+=========+=========+=========+

integer function jml_GetLeaderRank(component_id)
  implicit none
  integer, intent(IN) :: component_id

  call set_current_component(component_id)
  jml_GetLeaderRank = current_comp%leader_rank

end function jml_GetLeaderRank

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function jml_isLocalLeader(component_id)
  implicit none
  integer, intent(IN) :: component_id

  call set_current_component(component_id)
  jml_isLocalLeader = (0==current_comp%my_rank)

end function jml_isLocalLeader

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function jml_isLeader()
  implicit none
  
  jml_isLeader = (leader%my_rank/=MPI_UNDEFINED)

end function jml_isLeader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_bcast_string_global_1(string, source_rank)
  implicit none
  character(len=*), intent(INOUT) :: string
  integer, intent(IN) :: source_rank

  call MPI_Bcast(string,len(string),MPI_CHARACTER,source_rank,global%mpi_comm,ierror)

end subroutine jml_bcast_string_global_1

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_bcast_string_global_2(string, str_len, source_rank)
  implicit none
  integer, intent(IN) :: str_len
  character(len=str_len), intent(INOUT) :: string(:)
  integer, intent(IN) :: source_rank

  call MPI_Bcast(string, size(string)*str_len, MPI_CHARACTER,source_rank,global%mpi_comm,ierror)

end subroutine jml_bcast_string_global_2

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_bcast_int_1d_global(data,is,ie,source)
  implicit none
  integer, intent(INOUT) :: data(:)
  integer, intent(IN) :: is, ie
  integer, intent(IN), optional :: source

  integer :: source_rank

  if (present(source)) then
    source_rank = source
  else
    source_rank = global%root_rank
  end if

  call MPI_Bcast(data(is:),ie-is+1,MPI_INTEGER,source_rank,global%mpi_comm,ierror)

end subroutine jml_bcast_int_1d_global

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_string_global_1(string, dest)
  implicit none
  character(len=*), intent(IN) :: string
  integer, intent(IN) :: dest

  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  call MPI_ISEND(string,len(string),MPI_CHARACTER,dest,MPI_MY_TAG,global%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)
  
end subroutine jml_send_string_global_1

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_string_global_2(string, str_len, dest)
  implicit none
  integer, intent(IN) :: str_len
  character(len=str_len), intent(IN) :: string(:)
  integer, intent(IN) :: dest

  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  call MPI_ISEND(string,size(string)*str_len,MPI_CHARACTER,dest,MPI_MY_TAG,global%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)
  
end subroutine jml_send_string_global_2

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_string_global_1(string, source)
  implicit none
  character(len=*), intent(INOUT) :: string
  integer, intent(IN) :: source

  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  call MPI_IRECV(string,len(string),MPI_CHARACTER,source,MPI_MY_TAG,global%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

end subroutine jml_recv_string_global_1

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_string_global_2(string, str_len, source)
  implicit none
  integer, intent(IN) :: str_len
  character(len=str_len), intent(INOUT) :: string(:)
  integer, intent(IN) :: source

  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  call MPI_IRECV(string,size(string)*str_len,MPI_CHARACTER,source,MPI_MY_TAG,global%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

end subroutine jml_recv_string_global_2

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_int_1d_global(data,is,ie,dest)
  implicit none
  integer, intent(IN) :: data(:)
  integer, intent(IN) :: is, ie
  integer, intent(IN) :: dest

  integer :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  buffer(is:ie) = data(is:ie)
  call MPI_ISEND(buffer,ie-is+1,MPI_INTEGER,dest,MPI_MY_TAG,global%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

end subroutine jml_send_int_1d_global

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_int_1d_global(data,is,ie,source)
  implicit none
  integer, intent(OUT) :: data(:)
  integer, intent(IN)  :: is, ie
  integer, intent(IN)  :: source

  integer :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  call MPI_IRECV(buffer,ie-is+1,MPI_INTEGER,source,MPI_MY_TAG,global%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie) = buffer(is:ie)

end subroutine jml_recv_int_1d_global

!=======+=========+=========+=========+=========+=========+=========+=========+
! buffix 2014/06/18
!subroutine jml_reduce_sum_int_1d(data,is, ie, sum)
subroutine jml_reduce_sum_int_1d(d, sum)
  implicit none
  integer, intent(IN)  :: d
  integer, intent(INOUT) :: sum
  integer, parameter :: ONE = 1

  call MPI_REDUCE(d, sum, ONE, MPI_INTEGER,MPI_SUM,MPI_MY_TAG,global%mpi_comm,ierror)

end subroutine jml_reduce_sum_int_1d

!=======+=========+=========+=========+=========+=========+=========+=========+
! bugfix 2014/06/18
!subroutine jml_allreduce_sumint1d(data, is, ie ,sum)
subroutine jml_allreduce_sumint1d(d ,sum)
  implicit none
  integer, intent(IN)  :: d
  integer, intent(INOUT) :: sum
  integer, parameter :: ONE = 1

  call MPI_ALLREDUCE(d, sum, ONE, MPI_INTEGER,MPI_SUM,global%mpi_comm,ierror)

end subroutine jml_allreduce_sumint1d

!=======+=========+=========+=========+=========+=========+=========+=========+
! bugfix 2014/06/18
!!subroutine jml_allreduce_maxint1d(data,is,ie,res)
subroutine jml_allreduce_maxint1d(d, res)
  implicit none
  integer, intent(IN)  :: d
  integer, intent(INOUT) :: res
  integer, parameter :: ONE = 1

  call MPI_ALLREDUCE(d, res, ONE, MPI_INTEGER,MPI_MAX,global%mpi_comm,ierror)

end subroutine jml_allreduce_maxint1d

!=======+=========+=========+=========+=========+=========+=========+=========+
! bugfix 2014/06/18
!subroutine jml_allreduce_minint1d(data,is,ie,res)
subroutine jml_allreduce_minint1d(d,res)
  implicit none
  integer, intent(IN)  :: d
  integer, intent(INOUT) :: res
  integer, parameter :: ONE = 1

  call MPI_ALLREDUCE(d, res, ONE, MPI_INTEGER,MPI_MIN,global%mpi_comm,ierror)

end subroutine jml_allreduce_minint1d

!=======+=========+=========+=========+=========+=========+=========+=========+
! bugfix 2014/06/18
!subroutine jml_reduce_sum_int_1d_local(comp, data,is,ie,sum)
subroutine jml_reduce_sum_int_1d_local(comp, d, sum)
  implicit none
  integer, intent(IN)  :: comp
  integer, intent(IN)  :: d
  integer, intent(INOUT) :: sum
  integer, parameter :: ONE = 1

  call MPI_REDUCE(d, sum, ONE, MPI_INTEGER,MPI_SUM,MPI_MY_TAG,local(comp)%mpi_comm,ierror)

end subroutine jml_reduce_sum_int_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+
! bugfix 2014/08/18
!subroutine jml_reduce_min_int_1d_local(comp, data,is,ie,res)
subroutine jml_reduce_min_int_1d_local(comp, d, res)
  implicit none
  integer, intent(IN)  :: comp
  integer, intent(IN)  :: d
  integer, intent(INOUT) :: res
  integer, parameter :: ONE = 1

  call MPI_REDUCE(d ,res, ONE, MPI_INTEGER,MPI_MIN,0,local(comp)%mpi_comm,ierror)

end subroutine jml_reduce_min_int_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+
! bugfix 2014/06/18
!subroutine jml_reduce_max_int_1d_local(comp, data,is,ie,res)
subroutine jml_reduce_max_int_1d_local(comp, d, res)
  implicit none
  integer, intent(IN)  :: comp
  integer, intent(IN)  :: d
  integer, intent(INOUT) :: res
  integer, parameter :: ONE = 1

  call MPI_REDUCE(d, res, ONE, MPI_INTEGER,MPI_MAX,0,local(comp)%mpi_comm,ierror)

end subroutine jml_reduce_max_int_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+
! bugfix 2014/06/18
!subroutine jml_allreduce_max_int_1d_local(comp, data,is,ie,res)
subroutine jml_allreduce_max_int_1d_local(comp, d, res)
  implicit none
  integer, intent(IN)  :: comp
  integer, intent(IN)  :: d
  integer, intent(INOUT) :: res
  integer, parameter :: ONE = 1

  call MPI_ALLREDUCE(d, res, ONE, MPI_INTEGER,MPI_MAX,local(comp)%mpi_comm,ierror)

end subroutine jml_allreduce_max_int_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_bcast_string_local(comp, str,source)
  implicit none
  integer, intent(IN)  :: comp
  character(len=*), intent(INOUT) :: str
  integer, intent(IN), optional :: source

  integer :: source_rank

  if (present(source)) then
    source_rank = source
  else
    source_rank = local(comp)%root_rank
  end if

  call MPI_Bcast(str,len(str),MPI_CHARACTER,source_rank,local(comp)%mpi_comm,ierror)

end subroutine jml_bcast_string_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_bcast_int_1d_local(comp, data,is,ie,source)
  implicit none
  integer, intent(IN)    :: comp
  integer, intent(INOUT) :: data(:)
  integer, intent(IN) :: is, ie
  integer, intent(IN), optional :: source

  integer :: source_rank

  if (present(source)) then
    source_rank = source
  else
    source_rank = local(comp)%root_rank
  end if

  call MPI_Bcast(data(is:),ie-is+1,MPI_INTEGER,source_rank,local(comp)%mpi_comm,ierror)

end subroutine jml_bcast_int_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_bcast_real_1d_local(comp, data,is,ie,source)
  implicit none
  integer, intent(IN)  :: comp
  real(kind=4), intent(INOUT) :: data(:)
  integer, intent(IN) :: is, ie
  integer, intent(IN), optional :: source

  integer :: source_rank

  if (present(source)) then
    source_rank = source
  else
    source_rank = local(comp)%root_rank
  end if

  call MPI_Bcast(data(is:),ie-is+1,MPI_DOUBLE_PRECISION,source_rank,local(comp)%mpi_comm,ierror)

end subroutine jml_bcast_real_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_bcast_double_1d_local(comp, data,is,ie,source)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), intent(INOUT) :: data(:)
  integer, intent(IN) :: is, ie
  integer, intent(IN), optional :: source

  integer :: source_rank

  if (present(source)) then
    source_rank = source
  else
    source_rank = local(comp)%root_rank
  end if

  call MPI_Bcast(data(is:),ie-is+1,MPI_DOUBLE_PRECISION,source_rank,local(comp)%mpi_comm,ierror)

end subroutine jml_bcast_double_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_gather_int_1d_local(comp, data,is,ie,recv_data)
  implicit none
  integer, intent(IN)  :: comp
  integer, intent(IN)  :: data(:)
  integer, intent(IN)  :: is, ie
  integer, intent(OUT) :: recv_data(:)

  call MPI_Gather(data(is:),ie-is+1,MPI_INTEGER,recv_data,ie-is+1,MPI_INTEGER,0,local(comp)%mpi_comm,ierror)

end subroutine jml_gather_int_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_gather_real_1d_local(comp, data,is,ie,recv_data)
  implicit none
  integer, intent(IN) :: comp
  real(kind=4),    intent(IN)  :: data(:)
  integer, intent(IN)  :: is, ie
  real(kind=4),    intent(OUT) :: recv_data(:)

  call MPI_Gather(data(is:),ie-is+1,MPI_REAL,recv_data,ie-is+1,MPI_REAL,0,local(comp)%mpi_comm,ierror)

end subroutine jml_gather_real_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_scatter_int_1d_local(comp, send_data, num_of_data, recv_data)
  implicit none
  integer, intent(IN) :: comp
  integer, intent(IN) :: send_data(:)
  integer, intent(IN) :: num_of_data
  integer, intent(INOUT) :: recv_data(:)

  call MPI_Scatter(send_data, num_of_data, MPI_INTEGER, recv_data, num_of_data, MPI_INTEGER, 0, local(comp)%mpi_comm, ierror)

end subroutine jml_scatter_int_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_gatherv_int_1d_local(comp, send_data, num_of_my_data, recv_data, num_of_data, offset)
  implicit none
  integer, intent(IN) :: comp
  integer, intent(IN) :: send_data(:)
  integer, intent(IN) :: num_of_my_data
  integer, intent(INOUT) :: recv_data(:)
  integer, intent(IN)    :: num_of_data(:)
  integer, intent(IN)    :: offset(:)

  call MPI_GatherV(send_data, num_of_my_data, MPI_INTEGER, recv_data, num_of_data, offset, MPI_INTEGER, &
                   0, local(comp)%mpi_comm, ierror)

end subroutine jml_gatherv_int_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_gatherv_double_1d_local(comp, send_data, num_of_my_data, recv_data, num_of_data, offset)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), intent(IN) :: send_data(:)
  integer, intent(IN) :: num_of_my_data
  real(kind=8), intent(INOUT) :: recv_data(:)
  integer, intent(IN)         :: num_of_data(:)
  integer, intent(IN)         :: offset(:)

  call MPI_GatherV(send_data, num_of_my_data, MPI_DOUBLE_PRECISION, recv_data, num_of_data, offset, MPI_DOUBLE_PRECISION, &
                   0, local(comp)%mpi_comm, ierror)

end subroutine jml_gatherv_double_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_scatterv_int_1d_local(comp, send_data, num_of_data, offset, recv_data, num_of_my_data)
  implicit none
  integer, intent(IN) :: comp
  integer, intent(IN) :: send_data(:)
  integer, intent(IN) :: num_of_data(:)
  integer, intent(IN) :: offset(:)
  integer, intent(INOUT) :: recv_data(:)
  integer, intent(IN)    :: num_of_my_data

  call MPI_ScatterV(send_data, num_of_data, offset, MPI_INTEGER, recv_data, num_of_my_data, &
                    MPI_INTEGER, 0, local(comp)%mpi_comm, ierror)

end subroutine jml_scatterv_int_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_scatterv_double_1d_local(comp, send_data, num_of_data, offset, recv_data, num_of_my_data)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), intent(IN) :: send_data(:)
  integer, intent(IN) :: num_of_data(:)
  integer, intent(IN) :: offset(:)
  real(kind=8), intent(INOUT) :: recv_data(:)
  integer, intent(IN)    :: num_of_my_data

  call MPI_ScatterV(send_data, num_of_data, offset, MPI_DOUBLE_PRECISION, recv_data, num_of_my_data, &
                    MPI_DOUBLE_PRECISION, 0, local(comp)%mpi_comm, ierror)

end subroutine jml_scatterv_double_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_int_1d_local(comp, data,is,ie,dest)
  implicit none
  integer, intent(IN) :: comp
  integer, intent(IN) :: data(:)
  integer, intent(IN) :: is, ie
  integer, intent(IN) :: dest

  integer :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  buffer(is:ie) = data(is:ie)
  call MPI_ISEND(buffer,ie-is+1,MPI_INTEGER,dest,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

end subroutine jml_send_int_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_int_1d_local(comp, data,is,ie,source)
  implicit none
  integer, intent(IN) :: comp
  integer, intent(INOUT) :: data(:)
  integer, intent(IN)  :: is, ie
  integer, intent(IN)  :: source
  
  integer :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  call MPI_IRECV(buffer,ie-is+1,MPI_INTEGER,source,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie) = buffer(is:ie)

end subroutine jml_recv_int_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_local_test(comp, data,is,ie,dest)
  implicit none
  integer, intent(IN) :: comp
  integer, intent(IN) :: data(:)
  integer, intent(IN) :: is, ie
  integer, intent(IN) :: dest

  integer :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  if (dest == local(comp)%my_rank) then
    call check_buffer_size(ie-is+1)
    buffer(is:ie) = data(is:ie)
    call MPI_BSEND(buffer,ie-is+1,MPI_INTEGER,dest,MPI_MY_TAG,local(comp)%mpi_comm,ierror)
    !call MPI_WAIT(send_request,status,ierror)
    !local_buffer_int(1:ie-is+1) = data(is:ie)
  else
    buffer(is:ie) = data(is:ie)
    call MPI_ISEND(buffer,ie-is+1,MPI_INTEGER,dest,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_local_test

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_local_test(comp, data,is,ie,source)
  implicit none
  integer, intent(IN) :: comp
  integer, intent(INOUT) :: data(:)
  integer, intent(IN)  :: is, ie
  integer, intent(IN)  :: source
  
  integer :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  if (source == local(comp)%my_rank) then
    call MPI_IRECV(buffer,ie-is+1,MPI_INTEGER,source,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
    data(is:ie) = buffer(is:ie)
  else
    call MPI_IRECV(buffer,ie-is+1,MPI_INTEGER,source,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
    data(is:ie) = buffer(is:ie)
  end if

end subroutine jml_recv_local_test

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_int_2d_local(comp, data,is,ie,js,je,dest_pe)
  implicit none
  integer, intent(IN) :: comp
  integer, intent(IN) :: data(:,:)
  integer, intent(IN) :: is, ie, js, je
  integer, intent(IN) :: dest_pe

  integer :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  buffer(is:ie,js:je) = data(is:ie,js:je)

  call MPI_ISEND(buffer,(ie-is+1)*(je-js+1),MPI_INTEGER,dest_pe,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

end subroutine jml_send_int_2d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_int_2d_local(comp, data,is,ie,js,je,source_pe)
  implicit none
  integer, intent(IN) :: comp
  integer, intent(INOUT) :: data(:,:)
  integer, intent(IN) :: is, ie, js, je
  integer, intent(IN) :: source_pe

  integer :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1),MPI_INTEGER,source_pe,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je) = buffer(is:ie,js:je)

end subroutine jml_recv_int_2d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_int_3d_local(comp, data,is,ie,js,je,ks,ke,dest_pe)
  implicit none
  integer, intent(IN) :: comp
  integer, intent(IN) :: data(:,:,:)
  integer, intent(IN) :: is, ie, js, je, ks, ke
  integer, intent(IN) :: dest_pe

  integer :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  buffer(is:ie,js:je,ks:ke) = data(is:ie,js:je,ks:ke)

  call MPI_ISEND(buffer,(ie-is+1)*(je-js+1)*(ke-ks+1),MPI_INTEGER,dest_pe,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

end subroutine jml_send_int_3d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_int_3d_local(comp, data,is,ie,js,je,ks,ke,source_pe)
  implicit none
  integer, intent(IN) :: comp
  integer, intent(INOUT) :: data(:,:,:)
  integer, intent(IN) :: is, ie, js, je, ks, ke
  integer, intent(IN) :: source_pe

  integer :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1)*(ke-ks+1),MPI_INTEGER,source_pe,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je,ks:ke) = buffer(is:ie,js:je,ks:ke)

end subroutine jml_recv_int_3d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_real_2d_local(comp, data,is,ie,js,je,dest_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=4), intent(IN)    :: data(:,:)
  integer, intent(IN) :: is, ie, js, je
  integer, intent(IN) :: dest_pe

  real(kind=4) :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  buffer(is:ie,js:je) = data(is:ie,js:je)

  call MPI_ISEND(buffer,(ie-is+1)*(je-js+1),MPI_REAL,dest_pe,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

end subroutine jml_send_real_2d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_real_2d_local(comp, data,is,ie,js,je,source_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=4), intent(INOUT) :: data(:,:)
  integer, intent(IN) :: is, ie, js, je
  integer, intent(IN) :: source_pe

  real(kind=4) :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1),MPI_REAL,source_pe,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je) = buffer(is:ie,js:je)

end subroutine jml_recv_real_2d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_real_3d_local(comp, data,is,ie,js,je,ks,ke,dest_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=4), intent(IN)    :: data(:,:,:)
  integer, intent(IN) :: is, ie, js, je, ks, ke
  integer, intent(IN) :: dest_pe

  real(kind=4) :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  buffer(is:ie,js:je,ks:ke) = data(is:ie,js:je,ks:ke)

  call MPI_ISEND(buffer,(ie-is+1)*(je-js+1)*(ke-ks+1),MPI_REAL,dest_pe,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

end subroutine jml_send_real_3d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_real_3d_local(comp, data,is,ie,js,je,ks,ke,source_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=4), intent(INOUT) :: data(:,:,:)
  integer, intent(IN) :: is, ie, js, je, ks, ke
  integer, intent(IN) :: source_pe

  real(kind=4) :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1)*(ke-ks+1),MPI_REAL,source_pe,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je,ks:ke) = buffer(is:ie,js:je,ks:ke)

end subroutine jml_recv_real_3d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_double_1d_local(comp, data,is,ie,dest)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), intent(IN) :: data(:)
  integer, intent(IN) :: is, ie
  integer, intent(IN) :: dest

  real(kind=8) :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  buffer(is:ie) = data(is:ie)
  call MPI_ISEND(buffer,ie-is+1,MPI_DOUBLE_PRECISION,dest,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

end subroutine jml_send_double_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_double_1d_local(comp, data,is,ie,source)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), intent(INOUT) :: data(:)
  integer, intent(IN)  :: is, ie
  integer, intent(IN)  :: source
  
  real(kind=8) :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  call MPI_IRECV(buffer,ie-is+1,MPI_DOUBLE_PRECISION,source,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie) = buffer(is:ie)

end subroutine jml_recv_double_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_double_2d_local(comp, data,is,ie,js,je,dest_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), intent(IN)    :: data(:,:)
  integer, intent(IN) :: is, ie, js, je
  integer, intent(IN) :: dest_pe

  real(kind=8) :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  buffer(is:ie,js:je) = data(is:ie,js:je)

  call MPI_ISEND(buffer,(ie-is+1)*(je-js+1),MPI_DOUBLE_PRECISION, &
                 dest_pe,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

end subroutine jml_send_double_2d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_double_2d_local(comp, data,is,ie,js,je,source_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), intent(INOUT) :: data(:,:)
  integer, intent(IN) :: is, ie, js, je
  integer, intent(IN) :: source_pe

  real(kind=8) :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1),MPI_DOUBLE_PRECISiON, &
                 source_pe,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je) = buffer(is:ie,js:je)

end subroutine jml_recv_double_2d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_double_3d_local(comp, data,is,ie,js,je,ks,ke,dest_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), intent(IN)    :: data(:,:,:)
  integer, intent(IN) :: is, ie, js, je, ks, ke
  integer, intent(IN) :: dest_pe

  real(kind=8) :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  buffer(is:ie,js:je,ks:ke) = data(is:ie,js:je,ks:ke)

  call MPI_ISEND(buffer,(ie-is+1)*(je-js+1)*(ke-ks+1),MPI_DOUBLE_PRECISION, &
                 dest_pe,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

end subroutine jml_send_double_3d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_double_3d_local(comp,data,is,ie,js,je,ks,ke,source_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), intent(INOUT) :: data(:,:,:)
  integer, intent(IN) :: is, ie, js, je, ks, ke
  integer, intent(IN) :: source_pe

  real(kind=8) :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1)*(ke-ks+1),MPI_DOUBLE_PRECISiON, &
                 source_pe,MPI_MY_TAG,local(comp)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je,ks:ke) = buffer(is:ie,js:je,ks:ke)

end subroutine jml_recv_double_3d_local


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_int_1d_leader(data,is,ie,dest)
  implicit none
  integer, intent(IN)  :: data(:)
  integer, intent(IN)  :: is, ie
  integer, intent(IN)  :: dest

  integer :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: dest_rank
  integer :: data_size

  if (.not.jml_isLeader()) return

  buffer(is:ie) = data(is:ie)

  dest_rank = leader_pe(dest)
  data_size = ie-is+1

  !!write(0,*) "jml_send_int_1d_leader ", global%my_rank, dest_rank, data_size

  if (dest_rank == leader%my_rank) then
    call check_buffer_size(data_size)
    call MPI_BSEND(buffer,data_size,MPI_INTEGER,dest_rank,MPI_MY_TAG,leader%mpi_comm,ierror)
  else
    call MPI_ISEND(buffer,data_size,MPI_INTEGER,dest_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_int_1d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_int_1d_leader(data,is,ie,source)
  implicit none
  integer, intent(OUT) :: data(:)
  integer, intent(IN)  :: is, ie
  integer, intent(IN)  :: source

  integer :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: source_rank

  if (.not.jml_isLeader()) return

  source_rank = leader_pe(source)

  !!write(0,*) "jml_recv_int_1d_leader ", global%my_rank, source_rank, ie-is+1

  call MPI_IRECV(buffer,ie-is+1,MPI_INTEGER,source_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie) = buffer(is:ie)

end subroutine jml_recv_int_1d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_int_2d_leader(data,is,ie,js,je,dest)
  implicit none
  integer, intent(IN)  :: data(:,:)
  integer, intent(IN)  :: is, ie, js, je
  integer, intent(IN)  :: dest

  integer :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: dest_rank
  integer :: data_size

  if (.not.jml_isLeader()) return

  buffer(is:ie,js:je) = data(is:ie,js:je)

  dest_rank = leader_pe(dest)
  data_size = (ie-is+1)*(je-js+1)

  if (dest_rank == leader%my_rank) then
    call check_buffer_size(data_size)
    call MPI_BSEND(buffer,data_size,MPI_INTEGER,dest_rank,MPI_MY_TAG,leader%mpi_comm,ierror)
  else
    call MPI_ISEND(buffer,data_size,MPI_INTEGER,dest_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_int_2d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_int_2d_leader(data,is,ie,js,je,source)
  implicit none
  integer, intent(OUT) :: data(:,:)
  integer, intent(IN)  :: is, ie, js, je
  integer, intent(IN)  :: source

  integer :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: source_rank

  if (.not.jml_isLeader()) return

  source_rank = leader_pe(source)

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1),MPI_INTEGER,source_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je) = buffer(is:ie,js:je)

end subroutine jml_recv_int_2d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_int_3d_leader(data,is,ie,js,je,ks,ke,dest)
  implicit none
  integer, intent(IN)  :: data(:,:,:)
  integer, intent(IN)  :: is, ie, js, je, ks, ke
  integer, intent(IN)  :: dest

  integer :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: dest_rank
  integer :: data_size

  if (.not.jml_isLeader()) return

  buffer(is:ie,js:je,ks:ke) = data(is:ie,js:je,ks:ke)
  dest_rank = leader_pe(dest)
  data_size = (ie-is+1)*(je-js+1)*(ke-ks+1)

  if (dest_rank == leader%my_rank) then
    call check_buffer_size(data_size)
    call MPI_BSEND(buffer,data_size,MPI_INTEGER,dest_rank,MPI_MY_TAG,leader%mpi_comm,ierror)
  else
    call MPI_ISEND(buffer,data_size,MPI_INTEGER,dest_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_int_3d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_int_3d_leader(data,is,ie,js,je,ks,ke,source)
  implicit none
  integer, intent(OUT) :: data(:,:,:)
  integer, intent(IN)  :: is, ie, js, je, ks, ke
  integer, intent(IN)  :: source

  integer :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: source_rank

  if (.not.jml_isLeader()) return

  source_rank = leader_pe(source)

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1)*(ke-ks+1),MPI_INTEGER,source_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je,ks:ke) = buffer(is:ie,js:je,ks:ke)

end subroutine jml_recv_int_3d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_real_1d_leader(data,is,ie,dest)
  implicit none
  real(kind=4), intent(IN)  :: data(:)
  integer, intent(IN)  :: is, ie
  integer, intent(IN)  :: dest

  integer :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: dest_rank
  integer :: data_size

  if (.not.jml_isLeader()) return

  buffer(is:ie) = data(is:ie)
  dest_rank = leader_pe(dest)
  data_size = ie-is+1

  if (dest_rank == leader%my_rank) then
    call check_buffer_size(data_size)
    call MPI_BSEND(buffer,data_size,MPI_REAL,dest_rank,MPI_MY_TAG,leader%mpi_comm,ierror)
  else
    call MPI_ISEND(buffer,data_size,MPI_REAL,dest_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_real_1d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_real_1d_leader(data,is,ie,source)
  implicit none
  real(kind=4), intent(OUT) :: data(:)
  integer, intent(IN)  :: is, ie
  integer, intent(IN)  :: source

  integer :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: source_rank

  if (.not.jml_isLeader()) return

  source_rank = leader_pe(source)
  
  call MPI_IRECV(buffer,ie-is+1,MPI_INTEGER,source_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie) = buffer(is:ie)

end subroutine jml_recv_real_1d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_real_2d_leader(data,is,ie,js,je,dest)
  implicit none
  real(kind=4), intent(IN)    :: data(:,:)
  integer, intent(IN) :: is, ie, js, je
  integer, intent(IN) :: dest

  real(kind=4) :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: dest_rank
  integer :: data_size

  if (.not.jml_isLeader()) return

  buffer(is:ie,js:je) = data(is:ie,js:je)

  dest_rank = leader_pe(dest)
  data_size = (ie-is+1)*(je-js+1)

  if (dest_rank == leader%my_rank) then
    call check_buffer_size(data_size)
    call MPI_BSEND(buffer,data_size,MPI_REAL,dest_rank,MPI_MY_TAG,leader%mpi_comm,ierror)
  else
    call MPI_ISEND(buffer,data_size,MPI_REAL,dest_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_real_2d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_real_2d_leader(data,is,ie,js,je,source)
  implicit none
  real(kind=4), intent(INOUT) :: data(:,:)
  integer, intent(IN) :: is, ie, js, je
  integer, intent(IN) :: source

  real(kind=4) :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: source_rank

  if (.not.jml_isLeader()) return

  source_rank = leader_pe(source)

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1),MPI_REAL,source_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je) = buffer(is:ie,js:je)

end subroutine jml_recv_real_2d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_real_3d_leader(data,is,ie,js,je,ks,ke,dest)
  implicit none
  real(kind=4), intent(IN)    :: data(:,:,:)
  integer, intent(IN) :: is, ie, js, je, ks, ke
  integer, intent(IN) :: dest

  real(kind=4) :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: dest_rank
  integer :: data_size

  if (.not.jml_isLeader()) return

  buffer(is:ie,js:je,ks:ke) = data(is:ie,js:je,ks:ke)

  dest_rank = leader_pe(dest)
  data_size = (ie-is+1)*(je-js+1)*(ke-ks+1)

  if (dest_rank == leader%my_rank) then
    call check_buffer_size(data_size)
    call MPI_BSEND(buffer,data_size,MPI_REAL,dest_rank,MPI_MY_TAG,leader%mpi_comm,ierror)
  else
    call MPI_ISEND(buffer,data_size,MPI_REAL,dest_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_real_3d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_real_3d_leader(data,is,ie,js,je,ks,ke,source)
  implicit none
  real(kind=4), intent(INOUT) :: data(:,:,:)
  integer, intent(IN) :: is, ie, js, je, ks, ke
  integer, intent(IN) :: source

  real(kind=4) :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: source_rank

  if (.not.jml_isLeader()) return

  source_rank = leader_pe(source)

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1)*(ke-ks+1),MPI_REAL,source_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je,ks:ke) = buffer(is:ie,js:je,ks:ke)

end subroutine jml_recv_real_3d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_double_1d_leader(data,is,ie,dest)
  implicit none
  real(kind=8), intent(IN)  :: data(:)
  integer, intent(IN)  :: is, ie
  integer, intent(IN)  :: dest

  real(kind=8) :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: dest_rank
  integer :: data_size

  if (.not.jml_isLeader()) return

  buffer(is:ie) = data(is:ie)

  dest_rank = leader_pe(dest)
  data_size = ie-is+1

  if (dest_rank == leader%my_rank) then
    call check_buffer_size(data_size)
    call MPI_BSEND(buffer,data_size,MPI_DOUBLE_PRECISION,dest_rank,MPI_MY_TAG,leader%mpi_comm,ierror)
  else
    call MPI_ISEND(buffer,data_size,MPI_DOUBLE_PRECISION,dest_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_double_1d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_double_1d_leader(data,is,ie,source)
  implicit none
  real(kind=8), intent(OUT) :: data(:)
  integer, intent(IN)  :: is, ie
  integer, intent(IN)  :: source

  real(kind=8) :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: source_rank

  if (.not.jml_isLeader()) return

  source_rank = leader_pe(source)

  call MPI_IRECV(buffer,ie-is+1,MPI_DOUBLE_PRECISION,source_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie) = buffer(is:ie)

end subroutine jml_recv_double_1d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_double_2d_leader(data,is,ie,js,je,dest)
  implicit none
  real(kind=8), intent(IN)    :: data(:,:)
  integer, intent(IN) :: is, ie, js, je
  integer, intent(IN) :: dest

  real(kind=8) :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: dest_rank
  integer :: data_size

  if (.not.jml_isLeader()) return

  buffer(is:ie,js:je) = data(is:ie,js:je)

  dest_rank = leader_pe(dest)
  data_size = (ie-is+1)*(je-js+1)

  if (dest_rank == leader%my_rank) then
    call check_buffer_size(data_size)
    call MPI_BSEND(buffer,data_size,MPI_DOUBLE_PRECISION,dest_rank,MPI_MY_TAG,leader%mpi_comm,ierror)
  else
    call MPI_ISEND(buffer,data_size,MPI_DOUBLE_PRECISION,dest_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_double_2d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_double_2d_leader(data,is,ie,js,je,source)
  implicit none
  real(kind=8), intent(INOUT) :: data(:,:)
  integer, intent(IN) :: is, ie, js, je
  integer, intent(IN) :: source

  real(kind=8) :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: source_rank

  if (.not.jml_isLeader()) return

  source_rank = leader_pe(source)

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1),MPI_DOUBLE_PRECISION,source_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je) = buffer(is:ie,js:je)

end subroutine jml_recv_double_2d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_double_3d_leader(data,is,ie,js,je,ks,ke,dest)
  implicit none
  real(kind=8), intent(IN)    :: data(:,:,:)
  integer, intent(IN) :: is, ie, js, je, ks, ke
  integer, intent(IN) :: dest

  real(kind=8) :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: dest_rank
  integer :: data_size

  if (.not.jml_isLeader()) return

  buffer(is:ie,js:je,ks:ke) = data(is:ie,js:je,ks:ke)

  dest_rank = leader_pe(dest)
  data_size = (ie-is+1)*(je-js+1)*(ke-ks+1)

  if (dest_rank == leader%my_rank) then
    call check_buffer_size(data_size)
    call MPI_BSEND(buffer,data_size,MPI_DOUBLE_PRECISION,dest_rank,MPI_MY_TAG,leader%mpi_comm,ierror)
  else
    call MPI_ISEND(buffer,data_size,MPI_DOUBLE_PRECISION,dest_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_double_3d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_double_3d_leader(data,is,ie,js,je,ks,ke,source)
  implicit none
  real(kind=8), intent(INOUT) :: data(:,:,:)
  integer, intent(IN) :: is, ie, js, je, ks, ke
  integer, intent(IN) :: source

  real(kind=8) :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: source_rank

  if (.not.jml_isLeader()) return

  source_rank = leader_pe(source)

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1)*(ke-ks+1),MPI_DOUBLE_PRECISION,source_rank,MPI_MY_TAG,leader%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je,ks:ke) = buffer(is:ie,js:je,ks:ke)

end subroutine jml_recv_double_3d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_bcast_string_leader(str, source)
  implicit none
  character(len=*),intent(INOUT) :: str
  integer, intent(IN), optional :: source

  integer :: source_rank

  if (.not.jml_isLeader()) return

  if (present(source)) then
    source_rank = source
  else
    source_rank = leader%root_rank
  end if

  call MPI_Bcast(str,len(str),MPI_CHARACTER,source_rank,leader%mpi_comm,ierror)

end subroutine jml_bcast_string_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_bcast_int_1d_leader(data,is,ie,source)
  implicit none
  integer, intent(INOUT) :: data(:)
  integer, intent(IN) :: is, ie
  integer, intent(IN), optional :: source

  integer :: source_rank

  if (.not.jml_isLeader()) return

  if (present(source)) then
    source_rank = source
  else
    source_rank = leader%root_rank
  end if

  call MPI_Bcast(data(is:),ie-is+1,MPI_INTEGER,source_rank,leader%mpi_comm,ierror)

end subroutine jml_bcast_int_1d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_bcast_real_1d_leader(data,is,ie,source)
  implicit none
  real,    intent(INOUT) :: data(:)
  integer, intent(IN) :: is, ie
  integer, intent(IN), optional :: source

  integer :: source_rank

  if (.not.jml_isLeader()) return

  if (present(source)) then
    source_rank = source
  else
    source_rank = leader%root_rank
  end if

  call MPI_Bcast(data(is:),ie-is+1,MPI_REAL,source_rank,leader%mpi_comm,ierror)

end subroutine jml_bcast_real_1d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_gather_int_1d_leader(data,is,ie,recv_data)
  implicit none
  integer, intent(IN)  :: data(:)
  integer, intent(IN)  :: is, ie
  integer, intent(OUT) :: recv_data(:)

  if (.not.jml_isLeader()) return

  call MPI_Gather(data(is:),ie-is+1,MPI_INTEGER,recv_data,ie-is+1,MPI_INTEGER,0,leader%mpi_comm,ierror)

end subroutine jml_gather_int_1d_leader

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_gather_real_1d_leader(data,is,ie,recv_data)
  implicit none
  real,    intent(IN)  :: data(:)
  integer, intent(IN)  :: is, ie
  real,    intent(OUT) :: recv_data(:)

  if (.not.jml_isLeader()) return

  call MPI_Gather(data(is:),ie-is+1,MPI_REAL,recv_data,ie-is+1,MPI_REAL,0,leader%mpi_comm,ierror)

end subroutine jml_gather_real_1d_leader


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_int_1d_model(comp, data,is,ie,dest_model,dest_pe)
  implicit none
  integer, intent(IN) :: comp
  integer, intent(IN) :: data(:)
  integer, intent(IN) :: is, ie
  integer, intent(IN) :: dest_model, dest_pe

  integer :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: dest_rank
  integer :: data_size

  buffer(is:ie) = data(is:ie)

  dest_rank = dest_pe + local(comp)%inter_comm(dest_model)%pe_offset
  data_size = ie-is+1

  if (dest_rank == local(comp)%inter_comm(dest_model)%my_rank) then
    call check_buffer_size(data_size)
    call MPI_BSEND(buffer,data_size,MPI_INTEGER,dest_rank,MPI_MY_TAG,local(comp)%inter_comm(dest_model)%mpi_comm,ierror)
  else
    call MPI_ISEND(buffer,data_size,MPI_INTEGER,dest_rank,MPI_MY_TAG,local(comp)%inter_comm(dest_model)%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_int_1d_model

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_real_2d_model(comp,data,is,ie,js,je,dest_model,dest_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=4), intent(IN) :: data(:,:)
  integer, intent(IN) :: is, ie, js, je
  integer, intent(IN) :: dest_model, dest_pe

  real(kind=4) :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: dest_rank
  integer :: data_size

  buffer(is:ie,js:je) = data(is:ie,js:je)

  dest_rank = dest_pe + local(comp)%inter_comm(dest_model)%pe_offset
  data_size = (ie-is+1)*(je-js+1)

  if (dest_rank == local(comp)%my_rank) then
    call check_buffer_size(data_size)
    call MPI_BSEND(buffer,data_size,MPI_REAL,dest_rank,0,local(comp)%inter_comm(dest_model)%mpi_comm,ierror)
  else
    call MPI_ISEND(buffer,data_size,MPI_REAL,dest_rank,0,local(comp)%inter_comm(dest_model)%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_real_2d_model

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_real_3d_model(comp,data,is,ie,js,je,ks,ke,dest_model,dest_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=4), intent(IN) :: data(:,:,:)
  integer, intent(IN) :: is, ie, js, je, ks, ke
  integer, intent(IN) :: dest_model, dest_pe

  real(kind=4) :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: dest_rank
  integer :: data_size

  buffer(is:ie,js:je,ks:ke) = data(is:ie,js:je,ks:ke)

  dest_rank = dest_pe + local(comp)%inter_comm(dest_model)%pe_offset
  data_size = (ie-is+1)*(je-js+1)*(ke-ks+1)

  if (dest_rank == local(comp)%my_rank) then
    call check_buffer_size(data_size)
    call MPI_BSEND(buffer,data_size,MPI_REAL,dest_rank,0,local(comp)%inter_comm(dest_model)%mpi_comm,ierror)
  else
    call MPI_ISEND(buffer,data_size,MPI_REAL,dest_rank,0,local(comp)%inter_comm(dest_model)%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_real_3d_model

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_double_1d_model(comp,data,is,ie,dest_model,dest_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), intent(IN) :: data(:)
  real(kind=8) :: buffer(is:ie)
  integer, intent(IN) :: is, ie
  integer, intent(IN) :: dest_model, dest_pe

  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: dest_rank
  integer :: data_size

  buffer(is:ie) = data(is:ie)

  dest_rank = dest_pe + local(comp)%inter_comm(dest_model)%pe_offset
  data_size = ie-is+1

  if (dest_rank == local(comp)%my_rank) then
    call check_buffer_size(data_size)
    call MPI_BSEND(buffer,data_size,MPI_DOUBLE_PRECISION,dest_rank,0,local(comp)%inter_comm(dest_model)%mpi_comm,ierror)
  else
    call MPI_ISEND(buffer,data_size,MPI_DOUBLE_PRECISION,dest_rank,0,local(comp)%inter_comm(dest_model)%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_double_1d_model

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_double_2d_model(comp,data,is,ie,js,je,dest_model,dest_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), intent(IN) :: data(:,:)
  integer, intent(IN) :: is, ie, js, je
  integer, intent(IN) :: dest_model, dest_pe

  real(kind=8) :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: dest_rank
  integer :: data_size

  buffer(is:ie,js:je) = data(is:ie,js:je)

  dest_rank = dest_pe + local(comp)%inter_comm(dest_model)%pe_offset
  data_size = (ie-is+1)*(je-js+1)

  if (dest_rank == local(comp)%my_rank) then
    call check_buffer_size(data_size)
    call MPI_BSEND(buffer,data_size,MPI_DOUBLE_PRECISION,dest_rank,0,local(comp)%inter_comm(dest_model)%mpi_comm,ierror)
  else
    call MPI_ISEND(buffer,data_size,MPI_DOUBLE_PRECISION,dest_rank,0,local(comp)%inter_comm(dest_model)%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_double_2d_model

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_double_3d_model(comp,data,is,ie,js,je,ks,ke,dest_model,dest_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), intent(IN) :: data(:,:,:)
  integer, intent(IN) :: is, ie, js, je, ks, ke
  integer, intent(IN) :: dest_model, dest_pe

  real(kind=8) :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: dest_rank
  integer :: data_size

  buffer(is:ie,js:je,ks:ke) = data(is:ie,js:je,ks:ke)

  dest_rank = dest_pe + local(comp)%inter_comm(dest_model)%pe_offset
  data_size = (ie-is+1)*(je-js+1)*(ke-ks+1)

  if (dest_rank == local(comp)%my_rank) then
    call check_buffer_size(data_size)
    call MPI_BSEND(buffer,data_size,MPI_DOUBLE_PRECISION,dest_rank,0,local(comp)%inter_comm(dest_model)%mpi_comm,ierror)
  else
    call MPI_ISEND(buffer,data_size,MPI_DOUBLE_PRECISION,dest_rank,0,local(comp)%inter_comm(dest_model)%mpi_comm,request,ierror)
    call MPI_WAIT(request,status,ierror)
  end if

end subroutine jml_send_double_3d_model

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_int_1d_model(comp,data,is,ie,source_model,source_pe)
  implicit none
  integer, intent(IN) :: comp
  integer, intent(INOUT) :: data(:)
  integer, intent(IN) :: is, ie
  integer, intent(IN) :: source_model, source_pe

  integer :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: source_rank

  !!!write(0,*) "RecvInt1DModel, ", source_model, source_pe

  source_rank = source_pe + local(comp)%inter_comm(source_model)%pe_offset

  call MPI_IRECV(buffer,ie-is+1,MPI_INTEGER,source_rank,MPI_MY_TAG,local(comp)%inter_comm(source_model)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie) = buffer(is:ie)

end subroutine jml_recv_int_1d_model

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_real_2d_model(comp,data,is,ie,js,je,source_model,source_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=4), intent(INOUT) :: data(:,:)
  integer, intent(IN) :: is, ie, js, je
  integer, intent(IN) :: source_model, source_pe

  real(kind=4) :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: source_rank

  source_rank = source_pe + local(comp)%inter_comm(source_model)%pe_offset

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1),MPI_REAL,source_rank,0, &
                 local(comp)%inter_comm(source_model)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je) = buffer(is:ie,js:je)

end subroutine jml_recv_real_2d_model

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_real_3d_model(comp,data,is,ie,js,je,ks,ke,source_model,source_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=4), intent(INOUT) :: data(:,:,:)
  integer, intent(IN) :: is, ie, js, je, ks, ke
  integer, intent(IN) :: source_model, source_pe

  real(kind=4) :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: source_rank

  source_rank = source_pe + local(comp)%inter_comm(source_model)%pe_offset

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1)*(ke-ks+1),MPI_REAL,source_rank,0, &
                 local(comp)%inter_comm(source_model)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je,ks:ke) = buffer(is:ie,js:je,ks:ke)

end subroutine jml_recv_real_3d_model

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_double_1d_model(comp,data,is,ie,source_model,source_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), intent(INOUT) :: data(:)
  integer, intent(IN) :: is, ie
  integer, intent(IN) :: source_model, source_pe
  real(kind=8) :: buffer(is:ie)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: source_rank

  source_rank = source_pe + local(comp)%inter_comm(source_model)%pe_offset

  call MPI_IRECV(buffer,ie-is+1,MPI_DOUBLE_PRECISION,source_rank,0,&
                 local(comp)%inter_comm(source_model)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie) = buffer(is:ie)

end subroutine jml_recv_double_1d_model

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_double_2d_model(comp,data,is,ie,js,je,source_model,source_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), intent(INOUT) :: data(:,:)
  integer, intent(IN) :: is, ie, js, je
  integer, intent(IN) :: source_model, source_pe

  real(kind=8) :: buffer(is:ie,js:je)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: source_rank

  source_rank = source_pe + local(comp)%inter_comm(source_model)%pe_offset

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1),MPI_DOUBLE_PRECISION,source_rank,0, &
                 local(comp)%inter_comm(source_model)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je) = buffer(is:ie,js:je)

end subroutine jml_recv_double_2d_model

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_double_3d_model(comp,data,is,ie,js,je,ks,ke,source_model,source_pe)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), intent(INOUT) :: data(:,:,:)
  integer, intent(IN) :: is, ie, js, je, ks, ke
  integer, intent(IN) :: source_model, source_pe

  real(kind=8) :: buffer(is:ie,js:je,ks:ke)
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  integer :: source_rank

  source_rank = source_pe + local(comp)%inter_comm(source_model)%pe_offset

  call MPI_IRECV(buffer,(ie-is+1)*(je-js+1)*(ke-ks+1),MPI_DOUBLE_PRECISION,source_rank,0, &
                 local(comp)%inter_comm(source_model)%mpi_comm,request,ierror)
  call MPI_WAIT(request,status,ierror)

  data(is:ie,js:je,ks:ke) = buffer(is:ie,js:je,ks:ke)

end subroutine jml_recv_double_3d_model


!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_set_num_of_isend(num_of_isend)
  implicit none
  integer, intent(IN) :: num_of_isend

  if (num_of_isend<=0) return
  if (associated(isend_request)) then
    if (size(isend_request) >= num_of_isend) return
  end if

  if (associated(isend_request)) deallocate(isend_request)
  if (associated(isend_status))  deallocate(isend_status)

  allocate(isend_request(num_of_isend))
  allocate(isend_status(MPI_STATUS_SIZE, num_of_isend))
  isend_counter = 0

end subroutine jml_set_num_of_isend

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_set_num_of_irecv(num_of_irecv)
  implicit none
  integer, intent(IN) :: num_of_irecv

  if (num_of_irecv<=0) return
  if (associated(irecv_request)) then
    if (size(irecv_request) >= num_of_irecv) return
  end if

  if (associated(irecv_request)) deallocate(irecv_request)
  if (associated(irecv_status))  deallocate(irecv_status)

  allocate(irecv_request(num_of_irecv))
  allocate(irecv_status(MPI_STATUS_SIZE, num_of_irecv))
  irecv_counter = 0

end subroutine jml_set_num_of_irecv

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_isend_double_1d_local(comp, data,is,ie,dest_model,dest_pe, exchange_tag)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), pointer :: data
  integer, intent(IN) :: is, ie
  integer, intent(IN) :: dest_model, dest_pe
  integer, optional, intent(IN) :: exchange_tag
 
  integer :: tag

  if (present(exchange_tag)) then
    tag = exchange_tag
  else 
    tag = 0
  end if

  isend_counter = isend_counter + 1

  call MPI_ISEND(data,ie-is+1,MPI_DOUBLE_PRECISION,dest_pe,tag, &
                 local(comp)%mpi_comm,isend_request(isend_counter),ierror)


end subroutine jml_isend_double_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_irecv_double_1d_local(comp, data,is,ie,source_model,source_pe, exchange_tag)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), pointer :: data
  integer, intent(IN) :: is, ie
  integer, intent(IN) :: source_model, source_pe
  integer, optional, intent(IN) :: exchange_tag

  integer :: tag
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  
  if (present(exchange_tag)) then
    tag = exchange_tag
  else
    tag = 0
  end if

  irecv_counter = irecv_counter + 1

  call MPI_IRECV(data,ie-is+1,MPI_DOUBLE_PRECISION,source_pe,tag, &
                 local(comp)%mpi_comm,irecv_request(irecv_counter),ierror)

end subroutine jml_irecv_double_1d_local

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_isend_double_1d_model(comp, data,is,ie,dest_model,dest_pe, exchange_tag)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), pointer :: data
  integer, intent(IN) :: is, ie
  integer, intent(IN) :: dest_model, dest_pe
  integer, optional, intent(IN) :: exchange_tag
  integer :: dest_rank
  integer :: tag
  integer :: data_size
  integer :: i

  if (present(exchange_tag)) then
    tag = exchange_tag
  else 
    tag = 0
  end if

  dest_rank = dest_pe + local(comp)%inter_comm(dest_model)%pe_offset
  data_size = ie-is+1

  if (dest_rank == jml_GetMyrankModel(comp, dest_model)) then !local(comp)%my_rank) then
    write(0,*) "mpi_IBsend called ", comp, dest_model, dest_pe, exchange_tag
    call check_buffer_size(data_size)
    call MPI_BSEND(data,data_size,MPI_DOUBLE_PRECISION,dest_rank,tag,local(comp)%inter_comm(dest_model)%mpi_comm,ierror)
  else
    isend_counter = isend_counter + 1
    call MPI_ISEND(data,data_size,MPI_DOUBLE_PRECISION,dest_rank,tag, &
                   local(comp)%inter_comm(dest_model)%mpi_comm, isend_request(isend_counter),ierror)
  end if

end subroutine jml_isend_double_1d_model

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_irecv_double_1d_model(comp, data,is,ie,source_model,source_pe, exchange_tag)
  implicit none
  integer, intent(IN) :: comp
  real(kind=8), pointer :: data
  integer, intent(IN) :: is, ie
  integer, intent(IN) :: source_model, source_pe
  integer, optional, intent(IN) :: exchange_tag
  integer :: source_rank
  integer :: tag
  integer :: request
  integer :: status(MPI_STATUS_SIZE)
  
  if (present(exchange_tag)) then
    tag = exchange_tag
  else
    tag = 0
  end if

  irecv_counter = irecv_counter + 1

  source_rank = source_pe + local(comp)%inter_comm(source_model)%pe_offset

  call MPI_IRECV(data,ie-is+1,MPI_DOUBLE_PRECISION,source_rank,tag, &
                 local(comp)%inter_comm(source_model)%mpi_comm, irecv_request(irecv_counter),ierror)


end subroutine jml_irecv_double_1d_model

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_send_waitall()
  implicit none

  if (isend_counter==0) return
  call mpi_WaitAll(isend_counter, isend_request, isend_status, ierror)
  isend_counter = 0

end subroutine jml_send_waitall

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_recv_waitall()
  implicit none

  if (irecv_counter==0) return
  call mpi_WaitAll(irecv_counter, irecv_request, irecv_status, ierror)
  irecv_counter = 0

end subroutine jml_recv_waitall

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine jml_set_send_recv_buffer(bsize)
  implicit none
  integer, intent(IN) :: bsize
  
  call check_buffer_size(bsize)

end subroutine jml_set_send_recv_buffer

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_buffer_size(bsize)
  implicit none
  integer, intent(IN) :: bsize

  if (bsize>buffer_size) then
    call mpi_buffer_detach(local_buffer, 8*size(local_buffer), ierror)

    deallocate(local_buffer, STAT = ierror)

    allocate(local_buffer(bsize*2))
    call mpi_buffer_attach(local_buffer, 8*size(local_buffer*2), ierror)
    buffer_size = bsize
  end if

end subroutine check_buffer_size



end module jcup_mpi_lib

