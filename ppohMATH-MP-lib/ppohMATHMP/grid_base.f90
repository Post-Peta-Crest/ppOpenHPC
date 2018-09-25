!=======+=========+=========+=========+=========+=========+=========+=========+
!>
!! fistr_grid defines mesh structure of FrontISTR
!! 
module grid_base
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: node_type

  public :: cal_global_point_from_local ! subroutine (ox, oy, cr, sr, lx, ly, gx, gy)
  public :: cal_local_point_from_global ! subroutine (ox, oy, cr, sr, gx, gy, lx, ly)
  public :: cal_latlon_from_global_xy   ! subroutine (olon, olat, gx, gy, olon, glon, glat)
 
!--------------------------------  private  ----------------------------------!

  type node_type
    integer :: index ! global index of the node
    integer :: local_index ! local index of the node
    integer :: rank ! 
    logical :: is_coupled ! flag whether this node is coupled or not 
    real(kind=8) :: x, y, z 
    real(kind=8) :: data 
    real(kind=8) :: coef ! coef of target_element%node
  end type

contains


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! calculate (x, y) of global coordinate from (x, y) of local coordinate
!!
!! |gx|   |cos(angle) -sin(angle)||lx|   |ox|
!! |  | = |                      ||  | + |  |
!! |gy|   |sin(angle)  cos(angle)||ly|   |oy|
!!
subroutine cal_global_point_from_local(ox, oy, cr, sr, lx, ly, gx, gy)
  implicit none
  real(kind=8), intent(IN) :: ox, oy ! origin of local grid
  real(kind=8), intent(IN) :: cr, sr ! cos(angle), sin(angle)
  real(kind=8), intent(IN) :: lx, ly ! (x, y) of local grid
  real(kind=8), intent(OUT) :: gx, gy ! (x, y) of global grid

  gx = cr*lx-sr*ly + ox
  gy = sr*lx+cr*ly + oy

end subroutine cal_global_point_from_local

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! calculate (x, y) of local coordinate from (x, y) of globall coordinate
!!
!! |lx|   | cos(angle) sin(angle)||gx-ox|
!! |  | = |                      ||     |
!! |ly|   |-sin(angle) cos(angle)||gy-oy|
!!
subroutine cal_local_point_from_global(ox, oy, cr, sr, gx, gy, lx, ly)
  implicit none
  real(kind=8), intent(IN) :: ox, oy ! origin of local grid
  real(kind=8), intent(IN) :: cr, sr ! cos(angle), sin(angle)
  real(kind=8), intent(IN) :: gx, gy ! (x, y) of local grid
  real(kind=8), intent(OUT) :: lx, ly ! (x, y) of global grid

  lx =  cr*(gx-ox)+sr*(gy-oy)
  ly = -sr*(gx-ox)+cr*(gy-oy)

end subroutine cal_local_point_from_global

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! calculate (lat, lon) from (x, y) of globall coordinate
!!
!! |lx|   | cos(angle) sin(angle)||gx-ox|
!! |  | = |                      ||     |
!! |ly|   |-sin(angle) cos(angle)||gy-oy|
!!
subroutine cal_latlon_from_global_xy(olon, olat, gx, gy, glon, glat)
  implicit none
  real(kind=8), intent(IN) :: olon, olat, gx, gy
  real(kind=8), intent(OUT) :: glon, glat
  real(kind=8), parameter :: PI = 3.141592653579
  real(kind=8), parameter :: R = 6370000.d0
  real(kind=8) :: rr

  glat = olat + gy/R*180.d0/PI
  rr = R*cos(olat*PI/180.d0)
  glon = olon + gx/rr*180.d0/PI

end subroutine cal_latlon_from_global_xy

end module grid_base

