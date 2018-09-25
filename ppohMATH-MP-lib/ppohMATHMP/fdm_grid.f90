!=======+=========+=========+=========+=========+=========+=========+=========+
!>
!! fistr_grid defines mesh structure of FrontISTR
!! 
module fdm_grid
 implicit none
  private
  
!--------------------------------   public  ----------------------------------!

  public :: fdm_grid_type
  public :: grid_s
  public :: grid_u
  public :: grid_v
  public :: grid_w


  public :: STAG_S
  public :: STAG_X
  public :: STAG_Y
  public :: STAG_Z
  public :: STAG_XY
  public :: STAG_XZ
  public :: STAG_YZ
  public :: STAG_XYZ

  public :: init_fdm_grid ! subroutine (coupling.nmlst)
  public :: get_grid_size_from_cod_file ! subroutine (cod_file_name, nx, ny, nz)
  public :: get_grid_info_from_cod_file ! subroutine (cod_file_name, num_of_rank, is, ie, js, je, ks, ke)
  public :: get_global_index ! integer function (fdm_grid, i, j, k)
 
!--------------------------------  private  ----------------------------------!
  
  integer, parameter :: STAG_S = 0
  integer, parameter :: STAG_X = 1
  integer, parameter :: STAG_Y = 2
  integer, parameter :: STAG_Z = 3
  integer, parameter :: STAG_XY = 4
  integer, parameter :: STAG_XZ = 5
  integer, parameter :: STAG_YZ = 6
  integer, parameter :: STAG_XYZ = 7


  type fdm_grid_type
    integer :: stag_type
    integer :: nx, ny, nz
    integer :: kfs ! surface grid
    real(kind=8) :: dx, dy, dz
    real(kind=8) :: ox, oy, oz
  end type 

  type(fdm_grid_type) :: grid_s ! scalar grid
  type(fdm_grid_type) :: grid_u 
  type(fdm_grid_type) :: grid_v
  type(fdm_grid_type) :: grid_w

contains

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! init seism grid
subroutine init_fdm_grid(xo, yo, dx, dy, dz, kfs, cod_file, index_file)
  implicit none
  integer, intent(IN) :: xo, yo
  integer, intent(IN) :: dx, dy, dz
  integer, intent(IN) :: kfs
  character(len=*), intent(IN) :: cod_file
  character(len=*), intent(IN) :: index_file

  real(kind=8) :: xorg, yorg
  real(kind=8) :: delta_x, delta_y, delta_z
  integer :: nx, ny, nz
  real(kind=8) :: zorg

  xorg = xo ; yorg = yo
  delta_x = dx ; delta_y = dy ; delta_z = dz

  call get_grid_size_from_cod_file(trim(cod_file), nx, ny, nz)

  zorg = -1*delta_z*(kfs-1)

  write(*,*) 
  write(*,*) "fdm_grid configuration"
  write(*,*) "xorg = ", xorg
  write(*,*) "yorg = ", yorg
  write(*,*) "zorg = ", zorg
  write(*,*) "delta_x = ", delta_x
  write(*,*) "delta_y = ", delta_y
  write(*,*) "delta_z = ", delta_z
  write(*,*) "nx = ", nx
  write(*,*) "ny = ", ny
  write(*,*) "nz = ", nz
  write(*,*) "ksf = ", kfs


  grid_s%stag_type = STAG_S
  call set_fdm_grid(grid_s, nx, ny, nz, kfs, delta_x, delta_y, delta_z, xorg, yorg, zorg)
  write(0,*) "stag s ", grid_s%stag_type

  grid_u%stag_type = STAG_X 
  call set_fdm_grid(grid_u, nx, ny, nz, kfs, delta_x, delta_y, delta_z, xorg, yorg, zorg)
  write(0,*) "stag u ", grid_u%stag_type

  grid_v%stag_type = STAG_Y 
  call set_fdm_grid(grid_v, nx, ny, nz, kfs, delta_x, delta_y, delta_z, xorg, yorg, zorg)
  write(0,*) "stag v ", grid_v%stag_type

  grid_w%stag_type = STAG_Z 
  call set_fdm_grid(grid_w, nx, ny, nz, kfs, delta_x, delta_y, delta_z, xorg, yorg, zorg)
  write(0,*) "stag w ", grid_w%stag_type

end subroutine init_fdm_grid

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! cal grid size from cod file
subroutine get_grid_size_from_cod_file(cod_file_name, nx, ny, nz)
  use jcup_interface, only : jcup_error
  implicit none
  character(len=*), intent(IN) :: cod_file_name
  integer, intent(OUT) ::  nx, ny, nz
  integer, parameter :: FID = 128
  integer :: ierror
  integer :: xmin, xmax, ymin, ymax, zmin, zmax
  integer :: int1, int2, int3, int4, int5, int6
  integer :: i

  open(unit = FID, file = trim(cod_file_name), status="OLD", iostat=ierror)
  if (ierror /= 0) then
     write(0,*) "module jcup_seism_io, subroutine read_cod_file: Cannot open the file! file name = "//trim(cod_file_name)
     stop 999
  end if
  rewind(FID)

  xmin = 99999 ; xmax = -99999
  ymin = 99999 ; ymax = -99999
  zmin = 99999 ; zmax = -99999

  do 
    read(FID,*, end = 100) int1, int2, int3

    xmin = min(xmin, int1)
    ymin = min(ymin, int2)
    zmin = min(zmin, int3)

    read(FID,*, end = 100) int4, int5, int6

    xmax = max(xmax, int4)
    ymax = max(ymax, int5)
    zmax = max(zmax, int6)    

  end do

  100 continue

  close(FID)


  nx = xmax - xmin + 1
  ny = ymax - ymin + 1
  nz = zmax - zmin + 1

end subroutine get_grid_size_from_cod_file

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! cal grid size from cod file
subroutine get_grid_info_from_cod_file(cod_file_name, num_of_rank, is, ie, js, je, ks, ke)
  use jcup_interface, only : jcup_error
  implicit none
  character(len=*), intent(IN) :: cod_file_name
  integer, intent(OUT) ::  num_of_rank
  integer, pointer :: is(:), ie(:), js(:), je(:), ks(:), ke(:)
  integer, parameter :: FID = 128
  integer :: ierror
  integer :: xmin, xmax, ymin, ymax, zmin, zmax
  integer :: int1, int2, int3, int4, int5, int6
  integer :: i

  open(unit = FID, file = trim(cod_file_name), status="OLD", iostat=ierror)
  if (ierror /= 0) then
     write(0,*) "module jcup_seism_io, subroutine read_cod_file: Cannot open the file! file name = "//trim(cod_file_name)
     stop 999
  end if
  rewind(FID)

  num_of_rank = 0

  do 
    read(FID, *, end = 100) int1, int2, int3
    read(FID, *, end = 100) int4, int5, int6
    num_of_rank = num_of_rank + 1
  end do

  100 continue

  allocate(is(num_of_rank), ie(num_of_rank), js(num_of_rank), je(num_of_rank), ks(num_of_rank), ke(num_of_rank))

  rewind(FID)

  i = 0
  do 
    i = i + 1
    read(FID,*, end = 200) is(i), js(i), ks(i)
    read(FID,*, end = 200) ie(i), je(i), ke(i)
  end do

  200 continue

  close(FID)

end subroutine get_grid_info_from_cod_file


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! read mesh definition file
subroutine set_fdm_grid(grid, nx, ny, nz, kfs, dx, dy, dz, ox, oy, oz)
  implicit none
  type(fdm_grid_type), intent(INOUT) :: grid
  integer, intent(IN) :: nx, ny, nz
  integer, intent(IN) :: kfs
  real(kind=8), intent(IN) :: dx, dy, dz
  real(kind=8), intent(IN) :: ox, oy, oz

  grid%kfs = kfs
  grid%nx = nx ; grid%ny = ny ; grid%nz = nz
  grid%dx = dx ; grid%dy = dy ; grid%dz = dz
  grid%ox = ox ; grid%oy = oy ; grid%oz = oz

end subroutine set_fdm_grid

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! return 1D index from 3D index I,J,K
integer function get_global_index(seism, i, j, k)
  implicit none
  type(fdm_grid_type), intent(IN) :: seism
  integer, intent(IN) :: i, j, k

  get_global_index = i + seism%nx*(j-1)+seism%nx*seism%ny*(k-1)

end function get_global_index
  
end module fdm_grid


