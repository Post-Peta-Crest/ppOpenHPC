module field_common
public


character(len=3),  parameter :: ATM = "atm"
character(len=4),  parameter :: LAND = "land"
character(len=3),  parameter :: OCN = "ocn"
character(len=3),  parameter :: ICE = "ice"
character(len=3),  parameter :: CHM = "chm"
character(len=11), parameter :: ATM_GRID_2D = "atm_grid_2d"
character(len=11), parameter :: ATM_GRID_3D = "atm_grid_3d"
character(len=9),  parameter :: LAND_GRID = "land_grid"
character(len=8),  parameter :: OCN_GRID = "ocn_grid"
character(len=8),  parameter :: ICE_GRID = "ice_grid"
character(len=8),  parameter :: CHM_GRID = "chm_grid"

integer, parameter :: GNXA = 12
integer, parameter :: GNYA = 15
integer, parameter :: GNZA =  8

integer, parameter :: GNXL = 24
integer, parameter :: GNYL = 30
integer, parameter :: GNZL =  1

integer, parameter :: GNXO =  8
integer, parameter :: GNYO =  6
integer, parameter :: GNZO =  6

integer, parameter :: GNXI = 16
integer, parameter :: GNYI = 12
integer, parameter :: GNZI =  1

integer, parameter :: GNXC = 12
integer, parameter :: GNYC = 15
integer, parameter :: GNZC =  8

integer, parameter :: GN25 =  6


end module field_common

!======================================================================================================
!======================================================================================================
!======================================================================================================

module component_field
  use jcup_interface, only : jcup_varp_type, jcup_varg_type
  implicit none
  public

type varp_pointer_type
  type(jcup_varp_type), pointer :: varp_ptr
end type

type varg_pointer_type
  type(jcup_varg_type), pointer :: varg_ptr
end type

type component_field_type
  character(len=32) :: field_name
  integer :: lnx, lny, lnz
  integer :: lis, lie, ljs, lje, lks, lke
  integer :: data_25d
  integer :: num_of_varp
  integer :: num_of_varg

  real(kind=8), pointer :: send_2d(:,:)
  real(kind=8), pointer :: send_25d(:,:,:)
  real(kind=8), pointer :: send_3d(:,:,:)
  real(kind=8), pointer :: recv_2d(:,:)
  real(kind=8), pointer :: recv_25d(:,:,:)
  real(kind=8), pointer :: recv_3d(:,:,:)

  real(kind=8), pointer :: buffer1d(:)
  real(kind=8), pointer :: buffer25d(:,:)

  integer, pointer :: grid_index(:)

  logical, pointer      :: mask2d(:,:)
  logical, pointer      :: mask3d(:,:,:)

  type(varp_pointer_type), pointer :: varp(:)
  type(varg_pointer_type), pointer :: varg(:)

end type


!integer, parameter, public :: DP = 8

!interface my_interface
!  subroutine test(x, y)
!    real(kind=DP), intent(IN) :: x, y
!  end subroutine test
!end interface my_interface

contains

!======================================================================================================

subroutine init_field(field, field_name, is, ie, js, je, ks, ke)
  implicit none
  type(component_field_type), intent(INOUT) :: field
  character(len=*), intent(IN) :: field_name
  integer, intent(IN) :: is, ie, js, je, ks, ke

  field%field_name = trim(field_name)

  field%lis = is ; field%lie = ie 
  field%ljs = js ; field%lje = je 
  field%lks = ks ; field%lke = ke

  field%lnx = ie-is+1 
  field%lny = je-js+1 
  field%lnz = ke-ks+1

  allocate(field%send_2d(field%lnx, field%lny), field%send_3d(field%lnx, field%lny, field%lnz))
  allocate(field%recv_2d(field%lnx, field%lny), field%recv_3d(field%lnx, field%lny, field%lnz))
  allocate(field%buffer1d(field%lnx*field%lny*field%lnz))
  allocate(field%grid_index(field%lnx*field%lny*field%lnz))

  allocate(field%mask2d(field%lnx, field%lny))
  allocate(field%mask3d(field%lnx, field%lny, field%lnz))

  field%mask2d(:,:) = .true.
  field%mask3d(:,:,:) = .true.

end subroutine

!======================================================================================================

subroutine init_field_data(field, num_of_25d, num_of_varp, num_of_varg)
  implicit none
  type(component_field_type), intent(INOUT) :: field
  integer, intent(IN) :: num_of_25d, num_of_varp, num_of_varg

  field%data_25d = num_of_25d
  field%num_of_varp = num_of_varp
  field%num_of_varg = num_of_varg

  allocate(field%send_25d(field%lnx, field%lny, field%data_25d))
  allocate(field%recv_25d(field%lnx, field%lny, field%data_25d))
  allocate(field%buffer25d(field%lnx*field%lny, field%data_25d))

  allocate(field%varp(field%num_of_varp))
  allocate(field%varg(field%num_of_varg))

end subroutine


!======================================================================================================

end module component_field

!======================================================================================================
!======================================================================================================
!======================================================================================================

module field_def
use jcup_constant, only : NAME_LEN, MAX_GRID
private

public :: init_mpi
public :: get_mpi_rank
public :: get_mpi_size
public :: is_FirstHalf
public :: init_field_def
public :: set_field_def
public :: set_grid_mapping
public :: set_grid_mapping_3d
public :: get_local_field
public :: global_to_local
public :: local_to_global
public :: cal_mn
public :: cal_grid_index
public :: set_send_data_2d
public :: set_send_data_3d
public :: write_data_2d
public :: write_data_3d


interface global_to_local
  module procedure global_to_local_2d, global_to_local_3d
end interface

interface local_to_global
  module procedure local_to_global_2d, local_to_global_3d
end interface


type field_def_type
  character(len = NAME_LEN) :: component_name
  character(len = NAME_LEN) :: grid_name
  integer :: component_id

  integer :: GNX, GNY, GNZ 
  integer :: LNX, LNY, LNZ
  integer :: HALLO_WIDTH
  integer :: DIVX, DIVY

  integer, pointer :: lnx_field(:), lny_field(:)

  integer :: my_comm, my_group, my_rank, my_size
  integer :: MY_X, MY_Y
  integer :: IS, IE, JS, JE, KS, KE

end type


integer :: num_of_comp

type(field_def_type), pointer :: field(:,:) ! num_of_component*MAX_GRID

type(field_def_type), pointer :: current_field

integer :: mpi_size ! all size
integer :: mpi_rank ! my rank

contains

!======================================================================================================

subroutine init_mpi()
  use jcup_mpi_lib, only : jml_init, jml_GetCommsizeGlobal, jml_GetmyrankGlobal
  implicit none
  integer :: ierror

  call jml_Init(.true.)
  mpi_size = jml_GetCommsizeGlobal()
  mpi_rank = jml_GetMyrankGlobal()

end subroutine init_mpi

!======================================================================================================

integer function get_mpi_rank()
  implicit none

  get_mpi_rank = mpi_rank

end function get_mpi_rank

!======================================================================================================

integer function get_mpi_size()
  implicit none

  get_mpi_size = mpi_size

end function get_mpi_size

!======================================================================================================

logical function is_FirstHalf()
  implicit none

  is_FirstHalf = (mpi_rank+1<=mpi_size/2)

end function is_FirstHalf

!======================================================================================================

subroutine init_field_def(num_of_component)
  implicit none
  integer, intent(IN) :: num_of_component
  integer :: i, j

  num_of_comp = num_of_component
  allocate(field(num_of_comp, MAX_GRID))

  do i = 1, num_of_comp
    do j = 1, MAX_GRID
      field(i,j)%component_id = i
      field(i,j)%component_name = "NO_COMPONENT_NAME_ASSIGNED"
      field(i,j)%grid_name = "NO_GRID_NAME_ASSIGNED"
    end do
  end do

end subroutine init_field_def

!======================================================================================================

subroutine set_current_field(component_name, grid_name)
  use jcup_interface, only : jcup_error
  implicit none
  character(len=*), intent(IN) :: component_name
  character(len=*), intent(IN) :: grid_name
  integer :: i, j

  do i = 1, num_of_comp
    if (trim(component_name)==trim(field(i,1)%component_name)) then
        do j = 1, MAX_GRID
          if (trim(grid_name)==trim(field(i,j)%grid_name)) then
            current_field => field(i,j)
            return
          end if
        end do
        do j = 1, MAX_GRID
          if (trim(field(i,j)%grid_name)=="NO_GRID_NAME_ASSIGNED") then
            field(i,j)%grid_name = trim(grid_name)
            current_field => field(i,j)
            return
          end if
        end do
    end if
  end do

  do i = 1, num_of_comp
    if (trim(field(i,1)%component_name)=="NO_COMPONENT_NAME_ASSIGNED") then
      current_field => field(i,1)
      current_field%component_name = trim(component_name)
      current_field%grid_name = trim(grid_name)
      return
    end if
  end do

  call jcup_error("set_current_field", "no such component "//trim(component_name)//" assigned")
  
end subroutine set_current_field

!======================================================================================================

subroutine set_field_def(component_name, grid_name, &
                         g_nx, g_ny, g_nz, hallo, div_x, div_y)
  use jcup_interface, only : jcup_get_mpi_parameter
  implicit none
  character(len=*), intent(IN) :: component_name
  character(len=*), intent(IN) :: grid_name
  integer, intent(IN) :: g_nx, g_ny, g_nz, hallo, div_x, div_y
  integer :: i

  call set_current_field(component_name, grid_name)

  current_field%GNX = g_nx ; current_field%GNY = g_ny ; current_field%GNZ = g_nz ; current_field%HALLO_WIDTH = hallo
  current_field%DIVX = div_x ; current_field%DIVY = div_y

  allocate(current_field%lnx_field(current_field%DIVX), current_field%lny_field(current_field%DIVY))

  call cal_local_size(current_field%GNX, current_field%DIVX, current_field%lnx_field)
  call cal_local_size(current_field%GNY, current_field%DIVY, current_field%lny_field)


  call jcup_get_mpi_parameter(component_name, &
                              current_field%my_comm, current_field%my_group, current_field%my_size, current_field%my_rank)

  current_field%MY_X = mod(current_field%my_rank, current_field%DIVX)+ 1
  current_field%MY_Y = int(current_field%my_rank/current_field%DIVX) + 1

  current_field%IS = 1
  do i = 1, current_field%MY_X - 1
    current_field%IS = current_field%IS + current_field%lnx_field(i)
  end do
  current_field%IE = current_field%IS + current_field%lnx_field(current_field%MY_X) - 1

  current_field%lnx = current_field%IE-current_field%IS+1

  current_field%JS = 1
  do i = 1, current_field%MY_Y - 1
    current_field%JS = current_field%JS + current_field%lny_field(i)
  end do
  current_field%JE = current_field%JS + current_field%lny_field(current_field%MY_Y) - 1
 

  current_field%KS = 1
  current_field%KE = g_nz

  current_field%lny = current_field%JE-current_field%JS+1

end subroutine set_field_def

!======================================================================================================

subroutine cal_local_size(gn, divn, local_size)
  implicit none
  integer, intent(IN) :: gn, divn
  integer, intent(INOUT) :: local_size(:)
  integer :: i

  do i = 1, divn
    local_size(i) = int(gn/divn)
  end do

  do i = 1, mod(gn, divn)
    local_size(i) = local_size(i)+1
  end do

end subroutine cal_local_size

!======================================================================================================

subroutine set_grid_mapping(send_comp_name, send_grid_name, SNX, SNY, &
                            recv_comp_name, recv_grid_name, RNX, RNY, &
                            send_grid, recv_grid)
  implicit none
  character(len=*), intent(IN) :: send_comp_name, send_grid_name
  integer, intent(IN) :: SNX, SNY
  character(len=*), intent(IN) :: recv_comp_name, recv_grid_name
  integer, intent(IN) :: RNX, RNY
  integer, intent(INOUT) :: send_grid(:)
  integer, intent(INOUT) :: recv_grid(:)
  integer :: ri, rj, si, sj
  integer :: rij, sij

  rij = 0
  do rj = 1, RNY
    sj = mod(rj-1, SNY) + 1
    do ri = 1, RNX
      si = mod(ri-1, SNX) + 1
      rij = rij+1
      recv_grid(rij) = rij
      sij = (sj-1)*SNX+si
      send_grid(rij) = sij
    end do
  end do

end subroutine set_grid_mapping

!======================================================================================================

subroutine set_grid_mapping_3d(send_comp_name, send_grid_name, SNX, SNY, SNZ, &
                               recv_comp_name, recv_grid_name, RNX, RNY, RNZ, &
                               send_grid, recv_grid)
  implicit none
  character(len=*), intent(IN) :: send_comp_name, send_grid_name
  integer, intent(IN) :: SNX, SNY, SNZ
  character(len=*), intent(IN) :: recv_comp_name, recv_grid_name
  integer, intent(IN) :: RNX, RNY, RNZ
  integer, intent(INOUT) :: send_grid(:)
  integer, intent(INOUT) :: recv_grid(:)
  integer :: ri, rj, rk, si, sj, sk
  integer :: rijk, sijk

  rijk = 0
  do rk = 1, RNZ
    sk = mod(rk-1, SNZ) + 1
    do rj = 1, RNY
      sj = mod(rj-1, SNY) + 1
      do ri = 1, RNX
        si = mod(ri-1, SNX) + 1
        rijk = rijk+1
        recv_grid(rijk) = rijk
        sijk = (sk-1)*SNX*SNY+(sj-1)*SNX+si
        send_grid(rijk) = sijk
      end do
    end do
  end do

end subroutine set_grid_mapping_3d

!======================================================================================================

subroutine get_local_field(component_name, grid_name, local_is, local_ie, local_js, local_je)
  implicit none
  character(len=*), intent(IN) :: component_name
  character(len=*), intent(IN) :: grid_name
  integer, intent(OUT) :: local_is, local_ie, local_js, local_je

  call set_current_field(component_name, grid_name)

  local_is = current_field%is
  local_ie = current_field%ie
  local_js = current_field%js
  local_je = current_field%je


end subroutine get_local_field

!======================================================================================================

subroutine global_to_local_2d(component_name, grid_name, global_field, local_field)
  implicit none
  character(len=*), intent(IN) :: component_name
  character(len=*), intent(IN) :: grid_name
  real(kind=8), intent(IN) :: global_field(:,:)
  real(kind=8), intent(INOUT) :: local_field(:,:)
  integer :: i, j

  call set_current_field(component_name, grid_name)

  do j = current_field%js, current_field%je
    do i = current_field%is, current_field%ie
      local_field(i-current_field%is+1, j-current_field%js+1) = global_field(i,j)
    end do
  end do

end subroutine global_to_local_2d

!======================================================================================================

subroutine global_to_local_3d(component_name, grid_name, global_field, local_field)
  implicit none
  character(len=*), intent(IN) :: component_name
  character(len=*), intent(IN) :: grid_name
  real(kind=8), intent(IN) :: global_field(:,:,:)
  real(kind=8), intent(INOUT) :: local_field(:,:,:)
  integer :: k

  do k = 1, size(local_field,3)
    call global_to_local_2d(component_name, grid_name, global_field(:,:,k), local_field(:,:,k))
  end do

end subroutine global_to_local_3d

!======================================================================================================

subroutine local_to_global_2d(component_name, grid_name, local_field, global_field)
  use jcup_mpi_lib, only : jml_isLocalLeader, jml_SendLocal, jml_RecvLocal
  use jcup_interface, only : jcup_get_model_id
  implicit none
  character(len=*), intent(IN) :: component_name
  character(len=*), intent(IN) :: grid_name
  real(kind=8), intent(IN) :: local_field(:,:)
  real(kind=8), intent(INOUT) :: global_field(:,:)
  integer :: comp_id
  integer :: is, ie, js, je
  integer :: pe, i, j

  call set_current_field(component_name, grid_name)

  call jcup_get_model_id(component_name, comp_id)

  if (jml_isLocalLeader(comp_id)) then
    is = 1 ; ie = current_field%lnx_field(1)
    js = 1 ; je = current_field%lny_field(1)
    global_field(is:ie,js:je) = local_field(is:ie,js:je) ! my pe
 
    pe = 0
    js = 0 ; je = 0 ; 
    do j = 1, current_field%DIVY
      js = je + 1
      je = js + current_field%lny_field(j) - 1
      is = 0 ; ie = 0 
      do i = 1, current_field%DIVX
        is = ie + 1
        ie = is + current_field%lnx_field(i) -1
        if (i==1.and.j==1) cycle
        pe = pe + 1
        call jml_RecvLocal(comp_id, global_field, is, ie, js, je, pe)

      end do
    end do
  else
    call jml_SendLocal(comp_id, local_field, 1, current_field%lnx_field(current_field%MY_X), &
                                    1, current_field%lny_field(current_field%MY_Y), 0)
  end if

end subroutine local_to_global_2d

!======================================================================================================

subroutine local_to_global_3d(component_name, grid_name, local_field, global_field)
  implicit none
  character(len=*), intent(IN) :: component_name
  character(len=*), intent(IN) :: grid_name
  real(kind=8), intent(IN) :: local_field(:,:,:)
  real(kind=8), intent(INOUT) :: global_field(:,:,:)
  integer :: k

  do k = 1, size(local_field,3)
    call local_to_global_2d(component_name, grid_name, local_field(:,:,k), global_field(:,:,k))
  end do

end subroutine local_to_global_3d

!======================================================================================================

subroutine cal_mn(total_proc_num, m, n)
  implicit none
  integer, intent(IN) :: total_proc_num
  integer, intent(OUT) :: m, n
  
  n = 1
  do m = aint(sqrt(dble(total_proc_num))+0.999999999d0), total_proc_num
    if (mod(total_proc_num,m)==0) then
      n = total_proc_num/m
      return
    end if
  end do

end subroutine cal_mn

!======================================================================================================

subroutine cal_my_area(m,n,my_rank,nx,ny,is,ie,js,je)
  implicit none
  integer, intent(IN) :: m,n,my_rank
  integer, intent(IN) :: nx, ny
  integer, intent(OUT) :: is, ie, js, je
  integer :: my_m, my_n

  my_m = mod(my_rank,m)+1
  my_n = aint(my_rank/real(m))+1

  is = aint(nx/real(m))*(my_m-1)+1
  ie = is+aint(nx/real(m))-1
  if (my_m==m) ie = nx

  js = aint(ny/real(n))*(my_n-1)+1
  je = js+aint(ny/real(n))-1
  if (my_n == n) je = ny
end subroutine cal_my_area

!======================================================================================================

subroutine set_send_data_2d(component_name, grid_name, data, step, code)
  implicit none
  character(len=*), intent(IN) :: component_name, grid_name
  real(kind=8), intent(INOUT) :: data(:,:)
  integer, intent(IN) :: step, code

  integer :: i, j

  call set_current_field(component_name, grid_name)
    do j = current_field%js, current_field%je
      do i = current_field%is, current_field%ie
        data(i-current_field%is+1,j-current_field%js+1) = i+j*100+code*10000+step*1000000! +k*10000 !+step*1000000+code*10000000
      end do
    end do

end subroutine set_send_data_2d

!======================================================================================================

subroutine set_send_data_3d(component_name, grid_name, data, step, code)
  implicit none
  character(len=*), intent(IN) :: component_name, grid_name
  real(kind=8), intent(INOUT) :: data(:,:,:)
  integer, intent(IN) :: step, code

  integer :: i, j, k

  call set_current_field(component_name, grid_name)

  do k = 1, current_field%GNZ
    call set_send_data_2d(component_name, grid_name, data(:,:,k), step, code)
  end do

end subroutine set_send_data_3d

!======================================================================================================

subroutine cal_grid_index(component_name, grid_name, grid_index)
  implicit none
  character(len=*), intent(IN) :: component_name
  character(len=*), intent(IN) :: grid_name
  integer, pointer :: grid_index(:)
  integer :: i, j, k
  integer :: counter

  call set_current_field(component_name, grid_name)

  counter = 0
  do k = current_field%ks, current_field%ke
    do j = current_field%js, current_field%je
      do i = current_field%is, current_field%ie
        counter = counter+1
        grid_index(counter) = current_field%GNX*current_field%GNY*(k-1)+current_field%GNX*(j-1)+i
      end do
    end do
  end do

end subroutine cal_grid_index

!======================================================================================================

subroutine write_data_2d(component_name, grid_name, data_name, dt)
  use jcup_mpi_lib, only : jml_isLocalLeader
  use jcup_interface, only : jcup_get_model_id
  implicit none
  character(len=*), intent(IN) :: component_name, grid_name
  character(len=*), intent(IN) :: data_name
  real(kind=8), intent(IN) :: dt(:,:)

  real(kind=8), allocatable :: data(:,:)
  integer :: comp_id
  integer :: unit
  integer :: i, j

  call set_current_field(component_name, grid_name)

  call jcup_get_model_id(component_name, comp_id)

  if (jml_isLocalLeader(comp_id)) then
    allocate(data(current_field%GNX, current_field%GNY))
  else
    allocate(data(1,1))
  end if

  call local_to_global(component_name, grid_name, dt, data)

  if (jml_isLocalLeader(comp_id)) then
    call jcup_get_model_id(component_name, unit)

    unit = unit*100+200

    write(unit,*) trim(data_name)
      do j = 1, current_field%GNY
        write(unit,'(35I9)') (int(data(i,j)),i=1, current_field%GNX)
      end do
      write(unit,*)
  end if

end subroutine write_data_2d

!======================================================================================================


subroutine write_data_3d(component_name, grid_name, data_name, dt)
  use jcup_mpi_lib, only : jml_isLocalLeader
  use jcup_interface, only : jcup_get_model_id
  implicit none
  character(len=*), intent(IN) :: component_name, grid_name
  character(len=*), intent(IN) :: data_name
  real(kind=8), pointer :: dt(:,:,:)
  real(kind=8), allocatable :: data(:,:,:)
  integer :: comp_id
  integer :: unit
  integer :: i, j, k

  call set_current_field(component_name, grid_name)

  call jcup_get_model_id(component_name, comp_id)

  if (jml_isLocalLeader(comp_id)) then
    allocate(data(current_field%GNX, current_field%GNY, current_field%GNZ))
  else
    allocate(data(1,1,1))
  end if

  call local_to_global(component_name, grid_name, dt, data)

  if (jml_isLocalLeader(comp_id)) then
    call jcup_get_model_id(component_name, unit)

    unit = unit*100+200

    write(unit,*) trim(data_name)
      do k = 1, current_field%GNZ
        do j = 1, current_field%GNY
          write(unit,'(35I9)') (int(data(i,j,k)),i=1, current_field%GNX)
        end do
        write(unit,*)
      end do
      write(unit,*)
  end if

end subroutine write_data_3d

!======================================================================================================

end module field_def
