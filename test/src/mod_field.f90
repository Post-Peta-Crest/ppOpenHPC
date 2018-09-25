module mod_field
  use mod_common, only : NAME_LEN, STR_LEN
  implicit none
  private
 
!--------------------------------   public  ----------------------------------!

  integer, parameter, public :: DIV_1D = 1
  integer, parameter, public :: DIV_2D = 2
  integer, parameter, public :: DIV_3D = 3
  
  public :: field_type
  public :: read_field_info    ! subroutine (config_file_name, component_name)
  public :: get_field_ptr ! field_type, pointer function (field_name) 

!--------------------------------  private  ----------------------------------!

  type field_type
    character(len=NAME_LEN) :: name
    integer :: gnx, gny, gnz          ! global field
    real(kind=8) :: ox, oy, oz         ! origin of global field
    real(kind=8) :: dx, dy, dz         
    integer :: is, ie, js, je, ks, ke ! local array
    integer, pointer :: grid_index(:)
    type (field_type), pointer :: next_ptr => null()
  end type

  integer :: num_of_field  = 0
  type (field_type), pointer :: start_ptr => null()
  type (field_type), pointer :: current_ptr => null()

contains

!=======+=========+=========+=========+=========+=========+=========+=========+
!格子情報の設定
subroutine read_field_info(component_name, conf_file_name)
  use mod_common, only : MAX_GRID, NAME_LEN
  implicit none
  character(len=*), intent(IN) :: component_name 
  character(len=*), intent(IN) :: conf_file_name ! config file name
  integer, parameter :: FID = 77
  character(len=NAME_LEN) :: comp_name
  character(len=NAME_LEN) :: grid_name(MAX_GRID)
  integer, dimension(MAX_GRID) :: nx, ny, nz
  real(kind=8), dimension(MAX_GRID) :: ox, oy, oz
  real(kind=8), dimension(MAX_GRID) :: dx, dy, dz
  namelist / nmcomp / comp_name, grid_name, nx, ny, nz, ox, oy, oz, dx, dy, dz
  integer :: i
  integer :: istat

  open(FID, file=trim(conf_file_name), action='read', iostat = istat)

  if (istat /= 0) then
     write(0, *) "read_coupling_config, File Open error ! file name = "//trim(conf_file_name)
     stop 666
  end if

  rewind(FID)

  do 
    comp_name = ""
    grid_name(:) = ""
    read(FID, nml=nmcomp, iostat = istat, end = 7000)
    if (trim(comp_name) == trim(component_name)) then
      do i = 1, MAX_GRID
        if (trim(grid_name(i)) /= "") then
          call set_field(grid_name(i), nx(i), ny(i), nz(i), ox(i), oy(i), oz(i), dx(i), dy(i), dz(i))
        end if            
      end do
      write(0, *) "num_of_field = ", num_of_field
      close(FID)
      return
    end if

  end do

  7000 continue

  close(FID)


  return



end subroutine read_field_info


!=======+=========+=========+=========+=========+=========+=========+=========+
!格子情報の設定
subroutine set_field(field_name, nx, ny, nz, ox, oy, oz, dx, dy, dz)
  implicit none
  character(len=*), intent(IN) :: field_name
  integer, intent(IN) :: nx, ny, nz
  real(kind=8), intent(IN) :: ox, oy, oz
  real(kind=8), intent(IN) :: dx, dy, dz

  if (num_of_field == 0) then
    num_of_field = 1
    allocate(start_ptr)
    current_ptr => start_ptr
    call set_my_field(current_ptr, field_name, nx, ny, nz, ox, oy, oz, dx, dy, dz)
    return
  end if

  current_ptr => start_ptr
  do while(associated(current_ptr%next_ptr))
    current_ptr => current_ptr%next_ptr
  end do

  allocate(current_ptr%next_ptr)
  current_ptr => current_ptr%next_ptr

  num_of_field = num_of_field + 1

  call set_my_field(current_ptr, field_name, nx, ny, nz, ox, oy, oz, dx, dy, dz)

end subroutine set_field

!=======+=========+=========+=========+=========+=========+=========+=========+
!ny = 0 and nz = 0なら非構造格子とし、格子点番号はサイクリックに分配する
!ny /= 0　なら2次元分割とする。z方向の3次元分割は考えない。
subroutine set_my_field(field_ptr, field_name, nx, ny, nz, ox, oy, oz, dx, dy, dz)
  implicit none
  type (field_type), pointer :: field_ptr
  character(len=*), intent(IN) :: field_name
  integer, intent(IN) :: nx, ny, nz
  real(kind=8), intent(IN) :: ox, oy, oz
  real(kind=8), intent(IN) :: dx, dy, dz
  logical :: is_init
  integer :: ierror

  field_ptr%name = trim(field_name)
  field_ptr%gnx = nx
  field_ptr%gny = ny
  field_ptr%gnz = nz
  field_ptr%ox  = ox
  field_ptr%oy  = oy
  field_ptr%oz  = oz
  field_ptr%dx  = dx
  field_ptr%dy  = dy
  field_ptr%dz  = dz

  field_ptr%next_ptr => null()

  call mpi_initialized(is_init, ierror)

  if (.not.is_init) return

  if ((field_ptr%gny == 0).and.(field_ptr%gnz == 0)) then
    call set_unstructured_field(field_ptr%gnx, field_ptr%grid_index)
    return
  else
    call set_structured_field(field_ptr%gnx, field_ptr%gny, field_ptr%gnz, &
                              field_ptr%is, field_ptr%ie, &
                              field_ptr%js, field_ptr%je, &
                              field_ptr%ks, field_ptr%ke)
  end if

end subroutine set_my_field

!=======+=========+=========+=========+=========+=========+=========+=========+
!2次元分割された構造格子にでの格子点番号設定
subroutine set_structured_field(nx, ny, nz, is, ie, js, je, ks, ke)
  use ppoh_MATHMP, only : ppoh_MATHMP_get_mpi_parameters
  implicit none
  integer, intent(IN) :: nx, ny, nz
  integer, intent(OUT) :: is, ie, js, je, ks, ke
  integer :: my_comm, my_group, my_size, my_rank
  integer :: div_x, div_y
  integer :: my_x, my_y

  ks = 1 ; ke = nz

  call ppoh_MATHMP_get_mpi_parameters(my_comm, my_size, my_rank)

  call cal_mn(my_size, div_x, div_y)
  call cal_my_area(div_x, div_y, my_rank, nx, ny, is, ie, js, je, my_x, my_y)

  write(0, *) "set_structured_field ", is, ie, js, je

end subroutine set_structured_field

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

subroutine cal_my_area(m,n,my_rank,nx,ny,is,ie,js,je, my_m, my_n)
  implicit none
  integer, intent(IN) :: m,n,my_rank
  integer, intent(IN) :: nx, ny
  integer, intent(OUT) :: is, ie, js, je
  integer, intent(OUT) :: my_m, my_n

  my_m = mod(my_rank,m)+1
  my_n = aint(my_rank/real(m))+1

  is = aint(nx/real(m))*(my_m-1)+1
  ie = is+aint(nx/real(m))-1
  if (my_m==m) ie = nx

  js = aint(ny/real(n))*(my_n-1)+1
  je = js+aint(ny/real(n))-1
  if (my_n == n) je = ny

end subroutine cal_my_area

!=======+=========+=========+=========+=========+=========+=========+=========+
!サイクリック分割における格子点番号設定
subroutine set_unstructured_field(nx, grid_index)
  use ppoh_MATHMP, only : ppoh_MATHMP_get_mpi_parameters
  implicit none
  integer, intent(IN) :: nx
  integer, pointer :: grid_index(:)
  integer :: my_comm, my_group, my_size, my_rank
  integer :: my_grid_size, is
  integer :: i

  call ppoh_MATHMP_get_mpi_parameters(my_comm, my_size, my_rank)

  my_grid_size = nx/my_size + 1*int(mod(nx, my_size)/(my_rank + 1))
  is = my_rank + 1

  allocate(grid_index(my_grid_size))
  
  do i = 1, my_grid_size
    grid_index(i) = is + (i-1)*my_size
  end do

  write(0, *) grid_index

end subroutine set_unstructured_field

!=======+=========+=========+=========+=========+=========+=========+=========+
!fieldのポインタを返す

function get_field_ptr(field_name) result (res)
  implicit none
  character(len=*), intent(IN) :: field_name
  type (field_type), pointer :: res

  current_ptr => start_ptr

  do while(associated(current_ptr))
    if (trim(field_name) == trim(current_ptr%name)) then
       res => current_ptr
       return
    end if
    current_ptr => current_ptr%next_ptr
  end do

  res => null()

end function get_field_ptr

end module mod_field
