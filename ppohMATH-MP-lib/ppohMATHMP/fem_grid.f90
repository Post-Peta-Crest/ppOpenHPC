!=======+=========+=========+=========+=========+=========+=========+=========+
!>
!! fem_grid defines mesh structure of FrontISTR
!! 
module fem_grid
  use grid_base, only : node_type
 implicit none
  private
  
!--------------------------------   public  ----------------------------------!
  public :: fem_grid_type
  public :: fi_grid
  public :: init_fem_grid ! subroutine (coupling.nmlst)
  public :: make_coupling_info_file ! subroutine (total_rank, coupling_node_file, coupling_mesh_file)
  public :: read_coupling_info ! subroutine (my_rank)
  public :: get_num_of_exchange_node ! integer function
  public :: get_exchange_node_ptr ! type(node_type), pointer function (node_num)

!--------------------------------  private  ----------------------------------!

  real(kind=8), parameter :: EPSILON = 1.d-10

  integer, parameter :: STR_LEN = 128

  type fem_grid_type
    integer :: my_rank 
    integer :: num_of_node ! the number of exchange node (global or local)
    real(kind=8) :: xorg, yorg
    real(kind=8) :: angle
    real(kind=8) :: cr ! cos(angle)
    real(kind=8) :: sr ! sin(angle)
    type(node_type), pointer :: node(:) ! exchange node
  end type 

  type (fem_grid_type) :: fi_grid

  integer :: total_rank
  character(len=STR_LEN) :: global_node_file
  character(len=STR_LEN) :: coupling_mesh_file
  character(len=STR_LEN) :: coupling_info_file

contains


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! init fistr grid info
subroutine init_fem_grid(log_fid, num_of_total_rank, xorg, yorg, angle, node_file, mesh_file, info_file)
  implicit none
  integer, intent(IN) :: log_fid
  integer, intent(IN) :: num_of_total_rank
  integer, intent(IN) :: xorg, yorg
  real(kind=8), intent(IN) :: angle 
  character(len=*), intent(IN) :: node_file, mesh_file, info_file
  real(kind=8), parameter :: PI = 3.14159265d0

  total_rank = num_of_total_rank
  global_node_file = trim(node_file)
  coupling_mesh_file = trim(mesh_file)
  coupling_info_file = trim(info_file)

  write(LOG_FID, *)
  write(LOG_FID, *) "init_fem_grid"
  write(LOG_FID,*) "total_rank = ", total_rank
  write(LOG_FID,*) "xorg = ", xorg
  write(LOG_FID,*) "yorg = ", yorg
  write(LOG_FID,*) "angle = ", angle
  write(LOG_FID,*) "global_node_file   = "//trim(global_node_file)
  write(LOG_FID,*) "coupling_mesh_file = "//trim(coupling_mesh_file)
  write(LOG_FID,*) "coupling_info_file = "//trim(coupling_info_file)

  fi_grid%xorg = xorg
  fi_grid%yorg = yorg
  fi_grid%angle = angle

  fi_grid%cr = cos(angle*PI/180.d0)
  fi_grid%sr = sin(angle*PI/180.d0)
  
end subroutine init_fem_grid


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! init fistr grid info
subroutine make_coupling_info_file()
  implicit none
  integer, pointer :: coupling_node(:)
  character(len=128) :: mesh_file_name
  integer :: i

  call read_coupling_node_file(global_node_file, coupling_node)
  fi_grid%num_of_node = size(coupling_node)
  allocate(fi_grid%node(fi_grid%num_of_node))
  do i = 1, fi_grid%num_of_node
    fi_grid%node(i)%index = coupling_node(i)
    fi_grid%node(i)%rank = -1
    fi_grid%node(i)%local_index = -1
  end do

  do i =1, total_rank
    call read_and_set_coupling_info(coupling_mesh_file, i-1, fi_grid%node)
  end do

  call write_coupling_info(coupling_info_file, fi_grid%node)


end subroutine make_coupling_info_file

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! read coupling_global_node.txt
subroutine read_coupling_node_file(file_name, coupling_node)
  implicit none
  character(len=*), intent(IN) :: file_name
  integer, pointer :: coupling_node(:)
  integer, parameter :: FID = 321
  integer :: istat
  character(len=128) :: file_str
  integer :: file_int
  integer :: counter 

  open(unit=FID, file = trim(file_name), status = "old", action = "read", iostat = istat)
  if (istat /= 0) then
    write(0,*) "file : "//trim(file_name)//" open error"
    stop 999
  end if

  read(FID, *) file_str
  counter  = 0
  do 
    read(FID, *, iostat = istat), file_int
    if (istat /= 0) exit
    counter = counter + 1
  end do

  allocate(coupling_node(counter))

  rewind(FID)

  read(FID, *) file_str

  counter  = 0
  do 
    read(FID, *, iostat = istat) file_int
    if (istat /= 0) exit
    counter = counter + 1
    coupling_node(counter) = file_int
  end do

  close(FID)
  
end subroutine read_coupling_node_file


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! read Jcup_mesh.x file and set exchange node
subroutine read_and_set_coupling_info(file_name, rank, node)
  implicit none
  character(len=*), intent(IN) :: file_name
  integer, intent(IN) :: rank
  type(node_type), pointer :: node(:)
  integer :: num_of_elem ! number of coupled element
  integer, allocatable :: elem_num(:) ! id number of coupled elements
  integer :: ierror
  integer, parameter :: FID = 129
  character(len=128) :: mesh_file_name
  character(len=128) :: file_str
  integer :: file_int
  integer :: index
  real(kind=4) :: x, y, z
  integer :: local_index
  integer :: istat
  integer :: counter
  integer :: i
  
  write(mesh_file_name, '(A,I0)') trim(file_name)//".",rank
 
  open(unit = FID, file  = trim(mesh_file_name), status = "old", action = "read", iostat = istat) 
  if (istat /= 0) then
    write(0,*) "file open error ! file name : "//trim(mesh_file_name)
    stop 999
  end if

  read(FID, *) file_str, file_int
  read(FID, *) file_str
  local_index = 0
  do 
    read(FID, *, iostat = istat) index, x, y, z
    if (istat /= 0) exit
    local_index = local_index + 1
    do i = 1, size(node)
      if (node(i)%index == index) then
        node(i)%local_index = local_index
        node(i)%x = x
        node(i)%y = y
        node(i)%z = z
        node(i)%rank = rank
        exit
      end if
    end do
  end do

  close(FID) 

end subroutine read_and_set_coupling_info


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! write coupling node information file
subroutine write_coupling_info(file_name, node)
  implicit none
  character(len=*), intent(IN) :: file_name
  type(node_type), pointer :: node(:)
  integer, parameter :: FID = 129
  integer :: istat
  integer :: i

  open(unit = FID, file  = trim(file_name), action = "write", iostat = istat) 
  if (istat /= 0) then
    write(0,*) "file open error ! file name : "//trim(file_name)
    stop 999
  end if

  write(FID, *) "coupling_node_num", size(node)

  do i = 1, size(node)
    if (node(i)%rank >= 0) then
      write(FID, *) node(i)%index, node(i)%rank, node(i)%local_index, node(i)%x, node(i)%y, node(i)%z
    end if
  end do

  close(FID)

end subroutine write_coupling_info



!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! read fistr exchange node info
subroutine read_coupling_info(my_rank)
  implicit none
  integer, intent(IN) :: my_rank ! if my_rank < 0 all rank
  type(node_type), pointer :: node(:)
  integer, parameter :: FID = 129
  character(len=128) :: file_str
  integer :: coupling_node_num
  integer :: index, rank, local_index
  real(kind=8) :: x, y, z
  integer :: counter
  integer :: istat
  integer :: i

  open(unit = FID, file  = trim(coupling_info_file), action = "read", iostat = istat) 
  if (istat /= 0) then
    write(0,*) "subroutine read_coupling_infor, file open error ! file name : "//trim(coupling_info_file)
    stop 999
  end if


  if (my_rank < 0) then
    read(FID, *) file_str, coupling_node_num
    allocate(fi_grid%node(coupling_node_num))
    fi_grid%num_of_node = coupling_node_num
    fi_grid%my_rank = my_rank
    node => fi_grid%node
    do i = 1, coupling_node_num
      read(FID, *) node(i)%index, node(i)%rank, node(i)%local_index, node(i)%x, node(i)%y, node(i)%z
    end do

  else

    counter = 0
    read(FID, *) file_str, coupling_node_num
    counter = 0
    do 
      read(FID, *, end = 100) index, rank, local_index, x, y, z
      if (rank == my_rank) counter = counter + 1
    end do

  100 continue
    
    allocate(fi_grid%node(counter))
  
    node => fi_grid%node

    fi_grid%num_of_node = counter
    fi_grid%my_rank = my_rank

    rewind(FID)
    counter = 0
    read(FID, *) file_str, coupling_node_num
    do
      read(FID, *, end = 200) index, rank, local_index, x, y, z
      if (rank == my_rank) then
        counter = counter + 1
        node(counter)%index = index
        node(counter)%local_index = local_index
        node(counter)%rank = rank
        node(counter)%x = x
        node(counter)%y = y
        node(counter)%z = z
      end if
    end do

  200 continue

  end if

  close(FID)

end subroutine read_coupling_info

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get number of exchange node
integer function get_num_of_exchange_node()
  implicit none

  get_num_of_exchange_node = fi_grid%num_of_node

end function get_num_of_exchange_node


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! get exchange node pointer
function get_exchange_node_ptr(node_num) result(node_ptr)
  implicit none
  type(node_type), pointer :: node_ptr
  integer, intent(IN) :: node_num

  node_ptr => fi_grid%node(node_num)

end function get_exchange_node_ptr

end module fem_grid
