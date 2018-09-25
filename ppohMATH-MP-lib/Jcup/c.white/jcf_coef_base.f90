!=======+=========+=========+=========+=========+=========+=========+=========+
!------------------------------------------------------------------------------------
!! This module difines API for interpolation coefficient calculation.
!!
!> @author Takashi ARAKAWA <arakawa@rist.jp>
!------------------------------------------------------------------------------------
module jcf_coef_base
  use jcf_mesh_base, only : polygon_type
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: cal_coefficient ! subroutine (my_polygon, is_check_coef)
  !public :: cal_coefficient_without_mask ! subroutine (my_polygon)

!--------------------------------   private  ---------------------------------!

  integer, private, parameter :: MAX_POINT = 30

  type coef_type
    integer :: skip_side
    logical :: is_checked(MAX_POINT) !
    logical :: is_valid(MAX_POINT) ! 
    logical :: is_inner(MAX_POINT) !
    logical :: have_singular_point !
    integer :: side_index(MAX_POINT) ! target_polygon side index on my singular point
    integer :: overlap_index(MAX_POINT) ! target_polygon side index of my overlap point(side)
    integer :: num_of_point ! = p%num_of_point
    real(kind=8) :: x(MAX_POINT) ! p%ptr%x
    real(kind=8) :: y(MAX_POINT) ! p%ptr%y
    type(polygon_type), pointer :: p
  end type


contains

!=======+=========+=========+=========+=========+=========+=========+=========+
!===================================================================================
!> @breaf
!> calculate interpolation coefficient on my_polygon
!> @param[inout] my_polygon pointer of my polygon
!> @param[in] is_check_coef flag whether validate the coefficient 
subroutine cal_coefficient(my_polygon, is_check_coef)
  use jcf_sphere_lib, only : cal_great_circle_area
  use jcf_mesh_base, only : get_target_polygon_by_num, set_target_polygon_coef_by_num, get_target_polygon_coef_by_num
  implicit none
  type(polygon_type), pointer :: my_polygon
  logical, intent(IN) :: is_check_coef
  type(polygon_type), pointer :: target_polygon
  real(kind=8) :: my_area
  real(kind=8) :: overlap_area, overlap_sum
  real(kind=8) :: point_x(MAX_POINT), point_y(MAX_POINT)
  real(kind=8) :: temp_coef
  integer      :: num_of_point
  integer      :: area_counter
  integer :: i, j

  if (my_polygon%is_included) then
    if (my_polygon%num_of_target > 1) then
      write(0,*) "cal_coefficient num_of_target error "
      stop
    end if
    if (target_polygon%mask) then
      call set_target_polygon_coef_by_num(my_polygon, 1, 1.d0)
    else
      my_polygon%mask = .false.
      call set_target_polygon_coef_by_num(my_polygon, 1, 0.d0)
    end if
    return
  end if

  if (is_check_coef) then
    num_of_point = my_polygon%num_of_point
    do i = 1, num_of_point
      point_x(i) = my_polygon%point(i)%ptr%x
      point_y(i) = my_polygon%point(i)%ptr%y
    end do
    my_area = cal_great_circle_area(num_of_point, point_y, point_x)
  end if

  !write(0,*) "cal_coefficent, my_area1 ", my_area

  !write(0,*) "num of target polygon ", my_polygon%num_of_target

  overlap_sum = 0.d0
  area_counter = 0
  do i = 1, my_polygon%num_of_target
    target_polygon => get_target_polygon_by_num(my_polygon, i)
    if ((.not.is_check_coef).and.(.not.target_polygon%mask)) cycle ! skip mask data
    area_counter = area_counter + 1
    if (target_polygon%is_included) then
      num_of_point = target_polygon%num_of_point
      do j = 1, num_of_point
        point_x(j) = target_polygon%point(j)%ptr%x
        point_y(j) = target_polygon%point(j)%ptr%y
      end do
      overlap_area = cal_great_circle_area(num_of_point, point_y, point_x)
    else
      call cal_overlap_area(overlap_area, my_polygon, target_polygon)
    end if
    overlap_sum = overlap_sum + overlap_area
    call set_target_polygon_coef_by_num(my_polygon, i, overlap_area)
  end do  
  
  ! check coef
  if (is_check_coef) then
    if (abs(my_area - overlap_sum)/my_area > 0.01d0) then
      write(0,*) "cal_coefficent (area ratio), area sum check error ", my_polygon%index, my_area, overlap_sum
      stop
    end if
  end if

  if (.not.is_check_coef) then
    if (overlap_sum <= 0.d0) then
      my_polygon%mask = .false.
      return
    end if
  end if

  if (is_check_coef) then
    my_polygon%mask = .true.
    do i = 1, my_polygon%num_of_target
      temp_coef =  get_target_polygon_coef_by_num(my_polygon, i)
      call set_target_polygon_coef_by_num(my_polygon, i, temp_coef/overlap_sum)
    end do
  else
    ! recompute coefficient
    if (area_counter == 0) then
      !write(0,*) "area counter == 0, set mask = .false. "
      my_polygon%mask = .false.
    else
      my_polygon%mask = .true.
      do i = 1, my_polygon%num_of_target
        temp_coef =  get_target_polygon_coef_by_num(my_polygon, i)
        call set_target_polygon_coef_by_num(my_polygon, i, temp_coef/overlap_sum)
      end do
    end if
  end if

end subroutine cal_coefficient

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_coefficient_with_monte_carlo(my_polygon, is_check_coef, coef_cal_type, side_div_num)
  use jcf_sphere_lib, only : cal_great_circle_area
  use jcf_mesh_base, only : get_target_polygon_by_num, set_target_polygon_coef_by_num, get_target_polygon_coef_by_num
  implicit none
  type(polygon_type), pointer :: my_polygon
  logical, intent(IN) :: is_check_coef
  character(len=*), intent(IN) :: coef_cal_type
  integer, intent(IN) :: side_div_num
  type(polygon_type), pointer :: target_polygon
  real(kind=8) :: my_area
  real(kind=8) :: overlap_area, overlap_sum
  real(kind=8) :: point_x(MAX_POINT), point_y(MAX_POINT)
  real(kind=8) :: temp_coef
  integer      :: num_of_point
  integer      :: area_counter
  integer :: i, j
  logical :: is_monte_carlo

  is_monte_carlo = (trim(coef_cal_type) == "MONTE_CARLO")

  if (my_polygon%is_included) then
    if (my_polygon%num_of_target > 1) then
      write(0,*) "cal_coefficient num_of_target error "
      stop
    end if
    call set_target_polygon_coef_by_num(my_polygon, 1, 1.d0)
    return
  end if

  if (is_check_coef) then
    num_of_point = my_polygon%num_of_point
    do i = 1, num_of_point
      point_x(i) = my_polygon%point(i)%ptr%x
      point_y(i) = my_polygon%point(i)%ptr%y
    end do
    my_area = cal_great_circle_area(num_of_point, point_y, point_x)
  end if

  !write(0,*) "cal_coefficent, my_area1 ", my_area

  !write(0,*) "num of target polygon ", my_polygon%num_of_target

  overlap_sum = 0.d0
  area_counter = 0
  do i = 1, my_polygon%num_of_target
    target_polygon => get_target_polygon_by_num(my_polygon, i)
    if ((.not.is_check_coef).and.(.not.target_polygon%mask)) cycle ! skip mask data
    area_counter = area_counter + 1
    if (target_polygon%is_included) then
      num_of_point = target_polygon%num_of_point
      do j = 1, num_of_point
        point_x(j) = target_polygon%point(j)%ptr%x
        point_y(j) = target_polygon%point(j)%ptr%y
      end do
      if (is_monte_carlo) then
        overlap_area = side_div_num*side_div_num
      else
        overlap_area = cal_great_circle_area(num_of_point, point_y, point_x)
      end if
    else
      if (is_monte_carlo) then
        call cal_overlap_area_monte_carlo(overlap_area, my_polygon, target_polygon, side_div_num)
      else
        call cal_overlap_area(overlap_area, my_polygon, target_polygon)
      end if
    end if
    overlap_sum = overlap_sum + overlap_area
    call set_target_polygon_coef_by_num(my_polygon, i, overlap_area)
  end do  
  
  ! check coef
  if (is_check_coef) then
    if (is_monte_carlo) then
      !write(0,*) my_polygon%index, overlap_sum
      !write(432,*) my_polygon%index, overlap_sum
      if (overlap_sum == 0.d0) then
         write(0,*) "cal_coefficient (monte carlo), area sum check error ", my_polygon%index
         do i = 1, my_polygon%num_of_point
            write(0,*) my_polygon%point(i)%ptr%x, my_polygon%point(i)%ptr%y
         end do
         stop
       end if
    else
      if (abs(my_area - overlap_sum)/my_area > 0.01d0) then
        write(0,*) "cal_coefficent (area ratio), area sum check error ", my_polygon%index, my_area, overlap_sum
        stop
      end if
    end if
  end if

  if (.not.is_check_coef) then
    if (overlap_sum <= 0.d0) then
      my_polygon%mask = .false.
      return
    end if
  end if

  if (is_check_coef) then
    my_polygon%mask = .true.
    do i = 1, my_polygon%num_of_target
      temp_coef =  get_target_polygon_coef_by_num(my_polygon, i)
      call set_target_polygon_coef_by_num(my_polygon, i, temp_coef/overlap_sum)
    end do
  else
    ! recompute coefficient
    if (area_counter == 0) then
      !write(0,*) "area counter == 0, set mask = .false. "
      my_polygon%mask = .false.
    else
      my_polygon%mask = .true.
      do i = 1, my_polygon%num_of_target
        temp_coef =  get_target_polygon_coef_by_num(my_polygon, i)
        call set_target_polygon_coef_by_num(my_polygon, i, temp_coef/overlap_sum)
      end do
    end if
  end if

end subroutine cal_coefficient_with_monte_carlo

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_coefficient_with_mask(my_polygon, coef_cal_type, side_div_num)
  use jcf_sphere_lib, only : cal_great_circle_area
  use jcf_mesh_base, only : get_target_polygon_by_num, set_target_polygon_coef_by_num, get_target_polygon_coef_by_num
  implicit none
  type(polygon_type), pointer :: my_polygon
  character(len=*), intent(IN) :: coef_cal_type
  integer, intent(IN) :: side_div_num
  type(polygon_type), pointer :: target_polygon
  real(kind=8) :: my_area
  real(kind=8) :: overlap_area, overlap_sum
  real(kind=8) :: point_x(MAX_POINT), point_y(MAX_POINT)
  real(kind=8) :: temp_coef
  integer      :: num_of_point
  integer      :: area_counter
  integer :: i, j

  if (my_polygon%is_included) then
    if (my_polygon%num_of_target > 1) then
      write(0,*) "cal_coefficient num_of_target error "
      stop
    end if
    call set_target_polygon_coef_by_num(my_polygon, 1, 1.d0)
    return
  end if

  !num_of_point = my_polygon%num_of_point
  !do i = 1, num_of_point
  !  point_x(i) = my_polygon%point(i)%ptr%x
  !  point_y(i) = my_polygon%point(i)%ptr%y
  !end do

  !my_area = cal_great_circle_area(num_of_point, point_y, point_x)


  !write(0,*) "cal_coefficent, my_area1 ", my_area

  !write(0,*) "num of target polygon ", my_polygon%num_of_target

  overlap_sum = 0.d0
  area_counter = 0
  do i = 1, my_polygon%num_of_target
    target_polygon => get_target_polygon_by_num(my_polygon, i)
    if (.not.target_polygon%mask) cycle ! skip mask data
    area_counter = area_counter + 1
    if (target_polygon%is_included) then
      num_of_point = target_polygon%num_of_point
      do j = 1, num_of_point
        point_x(j) = target_polygon%point(j)%ptr%x
        point_y(j) = target_polygon%point(j)%ptr%y
      end do
      if (trim(coef_cal_type) == "MONTE_CARLO") then
        overlap_area = side_div_num*side_div_num
      else
        overlap_area = cal_great_circle_area(num_of_point, point_y, point_x)
      end if
    else
      if (trim(coef_cal_type) == "MONTE_CARLO") then
        call cal_overlap_area_monte_carlo(overlap_area, my_polygon, target_polygon, side_div_num)
      else
        call cal_overlap_area(overlap_area, my_polygon, target_polygon)
      end if
    end if
    overlap_sum = overlap_sum + overlap_area
    call set_target_polygon_coef_by_num(my_polygon, i, overlap_area)
  end do  
  
  !if (abs(my_area - overlap_sum)/my_area > 0.01d0) then
  !  write(0,*) "cal_coefficent, area sum check error ", my_polygon%index, my_area, overlap_sum
  !  stop
  !end if

  if (overlap_sum <= 0.d0) then
    my_polygon%mask = .false.
    return
  end if

  ! recompute coefficient
  if (area_counter == 0) then
    !write(0,*) "area counter == 0, set mask = .false. "
    my_polygon%mask = .false.
  else
    my_polygon%mask = .true.
    do i = 1, my_polygon%num_of_target
      temp_coef =  get_target_polygon_coef_by_num(my_polygon, i)
      call set_target_polygon_coef_by_num(my_polygon, i, temp_coef/overlap_sum)
    end do
  end if

end subroutine cal_coefficient_with_mask

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_coefficient_without_mask(my_polygon, coef_cal_type, side_div_num)
  use jcf_sphere_lib, only : cal_great_circle_area
  use jcf_mesh_base, only : get_target_polygon_by_num, set_target_polygon_coef_by_num, get_target_polygon_coef_by_num
   implicit none
  type(polygon_type), pointer :: my_polygon
  character(len=*), intent(IN) :: coef_cal_type
  integer, intent(IN) :: side_div_num  
  type(polygon_type), pointer :: target_polygon
  real(kind=8) :: my_area, filarea
  real(kind=8) :: overlap_area, overlap_sum
  real(kind=8) :: point_x(MAX_POINT), point_y(MAX_POINT)
  real(kind=8) :: temp_coef
  integer      :: num_of_point
  integer      :: area_counter
  integer :: i, j

  if (my_polygon%is_included) then
    if (my_polygon%num_of_target > 1) then
      write(0,*) "cal_coefficient num_of_target error "
      stop
    end if
    call set_target_polygon_coef_by_num(my_polygon, 1, 1.d0)
    return
  end if

  num_of_point = my_polygon%num_of_point
  do i = 1, num_of_point
    point_x(i) = my_polygon%point(i)%ptr%x
    point_y(i) = my_polygon%point(i)%ptr%y
  end do

  my_area = cal_great_circle_area(num_of_point, point_y, point_x)

  write(0,*) "num of target polygon ", my_polygon%num_of_target

  overlap_sum = 0.d0
  area_counter = 0
  do i = 1, my_polygon%num_of_target
    target_polygon => get_target_polygon_by_num(my_polygon, i)
    area_counter = area_counter + 1
    if (target_polygon%is_included) then
      num_of_point = target_polygon%num_of_point
      do j = 1, num_of_point
        point_x(j) = target_polygon%point(j)%ptr%x
        point_y(j) = target_polygon%point(j)%ptr%y
      end do
      overlap_area = cal_great_circle_area(num_of_point, point_y, point_x)
      overlap_area = 10*10
    else
      !call cal_overlap_area(overlap_area, my_polygon, target_polygon)
      call cal_overlap_area_monte_carlo(overlap_area, my_polygon, target_polygon, 10)
      write(0,*) "cal_coefficient_witout_mask, area ", overlap_area
    end if
    overlap_sum = overlap_sum + overlap_area
    call set_target_polygon_coef_by_num(my_polygon, i, overlap_area)
  end do  
  
  !if (abs(my_area - overlap_sum)/overlap_sum > 0.01d0) then
  !  write(0,*) "cal_coefficent, area sum check error ", my_polygon%index, my_area, overlap_sum
  !  stop
  !end if

  ! recompute coefficient
    my_polygon%mask = .true.
    do i = 1, my_polygon%num_of_target
      temp_coef =  get_target_polygon_coef_by_num(my_polygon, i)
      write(0,*) "cal_coef ", i, temp_coef/overlap_sum
      call set_target_polygon_coef_by_num(my_polygon, i, temp_coef/overlap_sum)
    end do

end subroutine cal_coefficient_without_mask


!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_overlap_area_monte_carlo(area, pa, pb, div_num)
  use jcf_sphere_lib, only : is_inner
  implicit none
  real(kind=8), intent(OUT) :: area
  type(polygon_type), pointer :: pa, pb
  integer, intent(IN) :: div_num
  real(kind=8) :: point_pos_x, point_pos_y
  integer :: i, j, p
  real(kind=8) :: lat_diff, lon_diff
  type(polygon_type), pointer :: rect_polygon, target_polygon
  real(kind=8), dimension(div_num+1) :: x12, x23, x43, x14, y12, y23, y43, y14
  real(kind=8) :: x_pos(4), y_pos(4), x1, y1, x2, y2, x3, y3
  real(kind=8) :: px1, px2, px3, px4, py1, py2, py3, py4
  real(kind=8) :: x_diff, y_diff

  ! set rectangular polygon
  if (pa%num_of_point == 4) then ! rectangular polygon
    rect_polygon => pa
    target_polygon => pb
  else if (pb%num_of_point == 4) then
    rect_polygon => pb
    target_polygon => pa
  else
    write(0,*) "cal_overlap_area_1, num_of_point error"
    stop
  end if

  ! set point position
  do i = 1, 4
    x_pos(i) = rect_polygon%point(i)%ptr%x
    y_pos(i) = rect_polygon%point(i)%ptr%y
  end do

  if (minval(x_pos) == 0.d0) then
    if (maxval(x_pos) >= 180.d0) then
      do i = 1, 4
        if (x_pos(i) == 0.d0) x_pos(i) = 360.d0
      end do
    end if
  end if
  
  if (maxval(x_pos) == 360.d0) then
    if (minval(x_pos) <= 180.d0) then
      do i = 1, 4
        if (x_pos(i) == 360.d0) x_pos(i) = 0.d0
      end do
    end if
  end if

  if (maxval(x_pos)-minval(x_pos) >= 180.d0) then
    do i = 1, 4
      if (x_pos(i) > 180.d0) x_pos(i) = x_pos(i) - 360.d0
    end do
  end if

  !do i = 1, target_polygon%num_of_point
  !  write(0,*) "target points " 
  !  write(0,*) i, target_polygon%index, target_polygon%point(i)%ptr%x, target_polygon%point(i)%ptr%y
  !end do

  x12(1) = x_pos(1)
  x23(1) = x_pos(2)
  x43(1) = x_pos(4)
  x14(1) = x_pos(1)

  y12(1) = y_pos(1)
  y23(1) = y_pos(2)
  y43(1) = y_pos(4)
  y14(1) = y_pos(1)

  !write(0,*) "points "
  !write(0,*) x12(1), y12(1)
  !write(0,*) x23(1), y23(1)
  !write(0,*) x43(1), y43(1)
  !write(0,*) x14(1), y14(1)

  do i = 1, div_num
    x12(i+1) = x12(i) + (x_pos(2)-x_pos(1))/div_num
    x23(i+1) = x23(i) + (x_pos(3)-x_pos(2))/div_num    
    x43(i+1) = x43(i) + (x_pos(3)-x_pos(4))/div_num    
    x14(i+1) = x14(i) + (x_pos(4)-x_pos(1))/div_num
    y12(i+1) = y12(i) + (y_pos(2)-y_pos(1))/div_num
    y23(i+1) = y23(i) + (y_pos(3)-y_pos(2))/div_num    
    y43(i+1) = y43(i) + (y_pos(3)-y_pos(4))/div_num    
    y14(i+1) = y14(i) + (y_pos(4)-y_pos(1))/div_num
  end do
    
  area = 0

  do j = 1, div_num
    do i = 1, div_num
      call cal_cross_line(x12(i  ), y12(i  ), x43(i  ), y43(i  ), x14(j  ), y14(j  ), x23(j  ), y23(j  ), px1, py1)
      call cal_cross_line(x12(i+1), y12(i+1), x43(i+1), y43(i+1), x14(j  ), y14(j  ), x23(j  ), y23(j  ), px2, py2)
      call cal_cross_line(x12(i  ), y12(i  ), x43(i  ), y43(i  ), x14(j+1), y14(j+1), x23(j+1), y23(j+1), px3, py3)
      call cal_cross_line(x12(i+1), y12(i+1), x43(i+1), y43(i+1), x14(j+1), y14(j+1), x23(j+1), y23(j+1), px4, py4)

      point_pos_x = (px1+px2+px3+px4)/4
      point_pos_y = (py1+py3+py3+py4)/4
  
      !write(432,*) point_pos_x, point_pos_y

      x1 = target_polygon%point(1)%ptr%x
      y1 = target_polygon%point(1)%ptr%y
      do p = 2, target_polygon%num_of_point-1
        x2 = target_polygon%point(p)%ptr%x
        y2 = target_polygon%point(p)%ptr%y
        x3 = target_polygon%point(p+1)%ptr%x
        y3 = target_polygon%point(p+1)%ptr%y

        !write(432, *) x1, y1
        !write(432, *) x2, y2
        !write(432, *) x3, y3
        ! write(432, *) is_inner(x1, y1, x2, y2, x3, y3, point_pos_x, point_pos_y)
        ! write(432, *)

        if (is_inner(x1, y1, x2, y2, x3, y3, point_pos_x, point_pos_y)) then
          area = area + 1
          exit
        end if

      end do

    end do
  end do
 

end subroutine cal_overlap_area_monte_carlo

!=======+=========+=========+=========+=========+=========+=========+=========+
!
!        (x3, y3)     / (x2,y2)
!                    /
!           --------/-------    
!                  /      (x4,y4)
!                 /
!          (x1, y1)
subroutine cal_cross_line(x1, y1, x2, y2, x3, y3, x4, y4, cx1, cy1)
  implicit none
  real(kind=8), intent(IN) :: x1, y1, x2, y2, x3, y3, x4, y4
  real(kind=8), intent(OUT) :: cx1, cy1
  real(kind=8) :: alpha

  alpha = ((x3-x1)*(y4-y3)-(y3-y1)*(x4-x3))/((x2-x1)*(y4-y3)-(x4-x3)*(y2-y1))
  cx1 = x1 + alpha*(x2-x1)
  cy1 = y1 + alpha*(y2-y1)

end subroutine cal_cross_line
  

!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+
!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_overlap_area(area, pa, pb)
  use jcf_sphere_lib, only : is_same_point, cal_great_circle_area
  implicit none
  real(kind=8), intent(OUT) :: area
  type(polygon_type), pointer :: pa, pb
  type(coef_type), target :: ca, cb
  type(coef_type), pointer :: target_polygon, my_polygon, temp_polygon
  integer :: current_index, skip_index, next_index
  integer :: my_start_index, target_start_index
  real(kind=8) :: xa1, ya1, xa2, ya2, xb1, by1, xb2, yb2
  real(kind=8) :: point_x(MAX_POINT), point_y(MAX_POINT)
  real(kind=8) :: xs, ys, xn, yn
  integer :: point_counter
  integer :: point_index
  integer :: overlap_side_index
  integer :: i1, i2
 
  ca%skip_side = -1
  ca%p => pa
  cb%skip_side = -1
  cb%p => pb

  call init_coef_var(ca)
  call init_coef_var(cb)
  call check_singularity(ca, cb)
  call check_singularity(cb, ca)

  !write(0,*) "cal_overlap_area ", pa%index, pb%index
  !do i1 = 1, pa%num_of_point
  !  write(0,'(A,I3,F,F,I3)') "pa point ", i1, ca%x(i1), ca%y(i1), ca%side_index(i1)
  !end do
  !do i1 = 1, pb%num_of_point
  !  write(0,'(A,I3,F,F,I3)') "pb point ", i1, cb%x(i1), cb%y(i1), cb%side_index(i1)
  !end do

  ! search start point

  if ((ca%have_singular_point).or.(cb%have_singular_point)) then
    call select_valid_side(ca, cb)

    point_counter = 0
    do i1 = 1, ca%num_of_point
      if (ca%is_valid(i1)) point_counter = point_counter + 1
    end do
    do i1 = 1, cb%num_of_point
      if (cb%is_valid(i1)) point_counter = point_counter + 1
    end do

    if (point_counter < 3) then
      area = 0.d0
      return
    end if

    call cal_start_point(ca, cb, xs, ys, current_index, point_index)
  else
    call cal_start_point_no_singularity(ca, cb, xs, ys, current_index, point_index)
  end if
  
  !write(0,*) "set start point ", current_index, xs, ys, point_index
  
  my_start_index = current_index
  target_start_index  = point_index  

  point_x(1) = xs
  point_y(1) = ys

  if (point_index < 0) then
    area = 0.d0
    return
  end if

  point_counter = 1
  i1 = current_index
  i2 = mod(i1, pa%num_of_point) + 1
  xa1 = point_x(1)
  ya1 = point_y(1)
  xa2 = pa%point(i2)%ptr%x
  ya2 = pa%point(i2)%ptr%y
  target_polygon => cb
  my_polygon => ca
  skip_index = point_index
  do 
    point_counter = point_counter + 1

    !write(0,*) "cal next_point", xa1, ya1
    !write(0,*) "cal next_point", xa2, ya2
    !write(0,*) "cal nest_point, current_index", current_index
    !write(0,*) "cal next_point, skip_index", skip_index
    !write(0,*) "cal next_point, polygon index ", my_polygon%p%index, target_polygon%p%index

    overlap_side_index = my_polygon%overlap_index(current_index)
    if ((overlap_side_index > 0)) then 
      !write(0,*) "cal overlap side line ", current_index, overlap_side_index
      if (my_polygon%side_index(current_index+1) == overlap_side_index) then
         !              /
         !             / my_polygon
         !            /
         !  ---------- 
         !    --------------- target_polygon
         point_index = -1
         xn = xa2
         yn = ya2
      else
         !              /
         !             / target_polygon
         !            /
         !  ---------- 
         !----------------- my_polygon
         point_index = mod(overlap_side_index, target_polygon%num_of_point) + 1
         xn = target_polygon%x(point_index)
         yn = target_polygon%y(point_index)
      end if
    else
      call cal_nearest_cross_point(xa1, ya1, xa2, ya2, current_index, xn, yn, target_polygon, &
                                   skip_index, .false., point_index)
    end if

    !              target_polygpon
    !                    |
    !                    |  /my_polygon(next_index)
    !                    | /
    !                    |/
    !my_polygon ---------
    !  current_index     |
    !                    |
    if (point_index <= 0) then ! no cross point
      next_index = mod(current_index, my_polygon%num_of_point) + 1
      if (my_polygon%side_index(next_index) > 0) then ! side index + 1
        if (my_polygon%is_valid(next_index) == .false.) then
          point_index = my_polygon%side_index(next_index)          
        end if
      end if
    end if

    !write(0,*) "next point ", point_counter, xn, yn, point_index

    if (point_counter > MAX_POINT) then      
      write(0,*) "cal_overlap_area, point calculation error ", pa%index, pb%index
      call mpi_finalize(i1)
      stop
    end if

    ! detact same cross line
    if (my_polygon%p%index == pa%index) then
      if ((current_index == my_start_index).and.(point_index == target_start_index).and.(point_counter > 3)) then
         exit ! write(0,*) "same cross line 1 search end"
      end if
    else
      if ((current_index == target_start_index).and.(point_index == my_start_index).and.(point_counter > 3)) then
         exit ! write(0,*) "same cross line 2 search end"
      end if
    end if



    if (is_same_point(ys, xs, yn, xn).and.point_counter > 3) exit ! if (next point == start point) exit

    point_x(point_counter) = xn
    point_y(point_counter) = yn

    ! set next points
    xa1 = xn
    ya1 = yn
 
    if (point_index > 0) then ! swap target_polygon
      temp_polygon => target_polygon
      target_polygon => my_polygon
      my_polygon => temp_polygon
      skip_index = current_index
      current_index = point_index
    else
      current_index = mod(current_index, my_polygon%p%num_of_point) + 1
      skip_index = my_polygon%side_index(current_index) !-1
    end if
    i2 = mod(current_index, my_polygon%p%num_of_point) + 1
    xa2 = my_polygon%p%point(i2)%ptr%x
    ya2 = my_polygon%p%point(i2)%ptr%y

  end do   

  !write(0,*) "cal_overlap_area, point_counter ", point_counter
  area = 0.d0
  if (point_counter-1 < 3) return
  !do i1 = 1, point_counter-1
  !  write(0,*) point_y(i1), point_x(i1)
  !end do
  area = cal_great_circle_area(point_counter-1, point_y, point_x)

  !write(0,*) "cal_area ", area, point_counter-1
  !write(0,*)
  
end subroutine cal_overlap_area

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_coef_var(c)
  implicit none
  type(coef_type), intent(INOUT) :: c
  integer :: i

  c%is_checked(:) = .false.
  c%is_valid(:) = .true.
  c%is_inner(:) = .false.
  c%have_singular_point = .false.
  c%side_index(:) = 0
  c%overlap_index(:) = 0

  c%num_of_point = c%p%num_of_point

  do i = 1, c%num_of_point
    c%x(i) = c%p%point(i)%ptr%x
    c%y(i) = c%p%point(i)%ptr%y
  end do
  c%x(c%num_of_point+1) = c%x(1)
  c%y(c%num_of_point+1) = c%y(1)

end subroutine init_coef_var

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_singularity(ca, cb)
  use jcf_sphere_lib, only : is_on_line
  implicit none
  type(coef_type), intent(INOUT) :: ca
  type(coef_type), intent(IN)    :: cb
  integer :: i, j

  do i = 1, ca%num_of_point
    if (ca%p%point(i)%ptr%target_polygon%index == cb%p%index) then
      ca%is_inner(i) = .true.
    end if
    bloop: do j = 1, cb%num_of_point
      if (is_on_line(cb%y(j), cb%x(j), cb%y(j+1), cb%x(j+1), ca%y(i), ca%x(i))) then
        ca%is_inner(i) = .true.
        ca%have_singular_point = .true.
        ca%side_index(i) = j
        exit bloop
      end if
    end do bloop
  end do

  ca%is_inner(ca%num_of_point+1) = ca%is_inner(1)
  ca%side_index(ca%num_of_point+1) = ca%side_index(1)

end subroutine check_singularity

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine select_valid_side(ca, cb)
  implicit none
  type(coef_type), intent(INOUT) :: ca, cb
  integer :: i

  call check_both_point_in(ca)
  call check_both_point_in(cb)
  call check_validity_by_cross_line(ca, cb)
  call check_validity_by_cross_line(cb, ca)
  call check_overlap_line(ca, cb)

  !write(0,*) "select valid side "
  !do i = 1, ca%num_of_point+1
  !  write(0,*) i, ca%is_valid(i), ca%is_checked(i), ca%side_index(i), ca%overlap_index(i)
  !end do
  !do i = 1, cb%num_of_point+1
  !  write(0,*) i, cb%is_valid(i), cb%is_checked(i), cb%side_index(i), cb%overlap_index(i)
  !end do

end subroutine select_valid_side

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_both_point_in(ca)
  implicit none
  type(coef_type), intent(INOUT) :: ca
  integer :: i

  do i = 1, ca%num_of_point
    if (ca%is_inner(i).and.ca%is_inner(i+1)) then
      ca%is_valid(i) = .true.
      ca%is_checked(i) = .true.
    end if
  end do

end subroutine check_both_point_in

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_overlap_line(ca, cb)
  implicit none
  type(coef_type), intent(INOUT) :: ca, cb
  integer :: i, j
  integer :: ia, ib

  do i = 1, ca%num_of_point
    !  ai--------ai+1
    !       bi-----------bi+1
    ia = ca%side_index(i+1)
    if (ia > 0) then
      if (i == cb%side_index(ia)) then
        ca%is_valid(i) = .true.
        ca%overlap_index(i) = ia
        !ca%overlap_index(mod(i, ca%num_of_point)+1) = ia
        cb%is_valid(ia) = .false.
        cycle
      end if
    end if

    !     ai----------ai+1
    !  bi------bi+1
    ia = ca%side_index(i)
    if (ia > 0) then
      if (i == cb%side_index(ia+1)) then
        ca%is_valid(i) = .true.
        ca%overlap_index(i) = ia
        cb%is_valid(ia) = .false.
        cycle
      end if
    end if

    !     ai----------ai+1
    !  bi--------------------bi+1
    if (ia > 0) then
      if (ia == ca%side_index(i+1)) then
        ca%is_valid(i) = .true.
        ca%overlap_index(i) = ia
        !ca%overlap_index(mod(i, ca%num_of_point)+1) = ia
        cb%is_valid(ia) = .false.
        cycle
      end if
    end if
  end do

    ! ai---------------ai+1
    !    bi-------bi+1
  do j= 1, cb%num_of_point
    ib = cb%side_index(j)
    if (ib > 0) then
      if (ib == cb%side_index(j+1)) then
        ca%is_valid(ib) = .true.
        ca%overlap_index(ib) = j
        cb%is_valid(j) = .false.
      end if
    end if
  end do

  ca%is_valid(ca%num_of_point+1) = ca%is_valid(1)
  ca%overlap_index(ca%num_of_point+1) = ca%overlap_index(1)
  cb%is_valid(cb%num_of_point+1) = cb%is_valid(1)

end subroutine check_overlap_line

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine check_validity_by_cross_line(ca, cb)
  use jcf_sphere_lib, only : is_cross_line
  implicit none
  type(coef_type), intent(INOUT) :: ca
  type(coef_type), intent(IN)    :: cb
  real(kind=8) :: xc, yc
  integer :: i, j

  aloop: do i = 1, ca%num_of_point
    if (ca%is_checked(i)) cycle ! skip inner line
    ca%is_valid(i) = .false.
    ca%is_checked(i) = .true.
    bloop: do j = 1, cb%num_of_point
      if (ca%side_index(i) == j) cycle ! skip online side
      if (ca%side_index(i+1) == j) cycle ! skip online side
      if (is_cross_line(ca%y(i), ca%x(i), ca%y(i+1), ca%x(i+1), &
                        cb%y(j), cb%x(j), cb%y(j+1), cb%x(j+1), yc, xc)) then
        ca%is_valid(i) = .true.
        ca%is_checked(i) = .true.
        exit bloop
      end if
    end do bloop
  end do aloop

end subroutine check_validity_by_cross_line

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_start_point_no_singularity(ca, cb, xs, ys, my_point_index, target_point_index)
  implicit none
  type(coef_type), target, intent(INOUT) :: ca, cb !my_polygon, target_polygon
  real(kind=8), intent(OUT) :: xs, ys ! start point
  integer, intent(OUT) :: my_point_index, target_point_index
  real(kind=8) :: xa1, ya1, xa2, ya2
  type(coef_type), pointer :: c_ptr
  integer :: target_polygon_index
  integer :: i1, i2

  c_ptr => cb

  do i1 = 1, ca%p%num_of_point
    if (.not.ca%is_inner(i1)) then
      !i2 = mod(i1, ca%p%num_of_point) + 1
      xa1 = ca%x(i1)
      ya1 = ca%y(i1)
      xa2 = ca%x(i1+1)
      ya2 = ca%y(i1+1)

      !write(0,*)
      !write(0,*)
      !write(0,*) "cal_start_point_no_singularity ", i1, xa1, ya1, xa2, ya2

      call cal_nearest_cross_point(xa1, ya1, xa2, ya2, i1, xs, ys, c_ptr, -1, .true., target_point_index)

      if (target_point_index > 0) then
        !write(0,*) "start_point ", target_point_index
        my_point_index = i1
        return
      end if
    end if
  end do 

  xs = 999.d0
  ys = 998.d0
  my_point_index = -1
  target_point_index = -1

end subroutine cal_start_point_no_singularity

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_start_point(ca, cb, xs, ys, my_point_index, target_point_index)
  implicit none
  type(coef_type), target, intent(INOUT) :: ca, cb !my_polygon, target_polygon
  real(kind=8), intent(OUT) :: xs, ys ! start point
  integer, intent(OUT) :: my_point_index, target_point_index
  real(kind=8) :: xa1, ya1, xa2, ya2
  type(coef_type), pointer :: c_ptr
  integer :: target_polygon_index
  integer :: i1, i2


  c_ptr => cb

  do i1 = 1, ca%p%num_of_point
    if (.not.ca%is_valid(i1)) cycle
    if (ca%side_index(i1) > 0) then
      my_point_index = i1
      target_point_index = ca%side_index(i1)
      xs = ca%x(i1)
      ys = ca%y(i1)
      return
    end if
    if (.not.ca%is_inner(i1)) then
      !i2 = mod(i1, ca%p%num_of_point) + 1
      xa1 = ca%x(i1)
      ya1 = ca%y(i1)
      xa2 = ca%x(i1+1)
      ya2 = ca%y(i1+1)
      call cal_nearest_cross_point(xa1, ya1, xa2, ya2, i1, xs, ys, c_ptr, -1, .true., target_point_index)
      if (target_point_index > 0) then
        !write(0,*) "start_point ", target_point_index
        my_point_index = i1
        return
      end if
    end if
  end do 

  xs = 999.d0
  ys = 998.d0
  my_point_index = -1
  target_point_index = -1

end subroutine cal_start_point

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine cal_nearest_cross_point(xs, ys, xe, ye, my_index, xn, yn, c, skip_point, is_start, polygon_point_index)
  use jcf_sphere_lib, only : is_same_point, is_cross_line, get_length
  implicit none
  real(kind=8), intent(IN) :: xs, ys, xe, ye 
  integer, intent(IN) :: my_index
  real(kind=8), intent(OUT) :: xn, yn
  type(coef_type), pointer :: c
  integer, intent(IN) :: skip_point
  logical, intent(IN) :: is_start
  integer, intent(OUT) :: polygon_point_index
  real(kind=8) :: x1, y1, x2, y2, xc, yc
  integer :: i1
  real(kind=8) :: min_length, current_length
   
  min_length = 9.99d0+32
  polygon_point_index = -1

  !write(0,*) "cncp start ", xs, ys, xe, ye

  do i1 = 1, c%num_of_point
    if (i1 == skip_point) cycle
    if ((.not.is_start).and.(.not.c%is_valid(i1))) cycle

    !      /
    !     /
    !    /i1
    ! ------- my_line
    if (my_index == c%side_index(i1)) then
      current_length = get_length(ys, xs, c%y(i1), c%x(i1))
      if (current_length <= min_length) then
        min_length = current_length
        polygon_point_index = i1
        xn = c%x(i1)
        yn = c%y(i1)
      end if
      cycle
    end if

    x1 = c%x(i1)
    y1 = c%y(i1)
    x2 = c%x(i1+1)
    y2 = c%y(i1+1)

    !write(0,*) "cal_nearest_cross_point ", xs, ys, xe, ye
    !write(0,*) "cal_nearest_cross_point ", x1, y1, x2, y2
    !write(0,*) is_cross_line(ys, xs, ye, xe, y1, x1, y2, x2, yc, xc)
    !write(0,*)
  
    if (is_cross_line(ys, xs, ye, xe, y1, x1, y2, x2, yc, xc)) then
      current_length =  get_length(ys, xs, yc, xc)     
      !write(0,*) "length ", current_length, min_length
      !write(0,*) xs, ys, xe, ye
      !write(0,*) x1, y1, x2, y2
      !write(0,*) xc, yc
      if (current_length <= min_length) then
        min_length = current_length
        polygon_point_index = i1
        xn = xc
        yn = yc
      end if
    end if
  end do

  if (polygon_point_index == -1) then ! no cross line
    xn = xe
    yn = ye
  end if
     
end subroutine cal_nearest_cross_point

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine search_overlap_line(xs, ys, xe, ye, polygon, polygon_point_index)
  use jcf_sphere_lib, only : is_same_line, is_cross_line, get_length
  implicit none
  real(kind=8), intent(IN) :: xs, ys, xe, ye 
  type(polygon_type), pointer :: polygon
  integer, intent(OUT) :: polygon_point_index
  real(kind=8) :: x1, y1, x2, y2
  integer :: i1, i2
   
  polygon_point_index = -1

  do i1 = 1, polygon%num_of_point
    i2 = mod(i1, polygon%num_of_point) + 1
    x1 = polygon%point(i1)%ptr%x
    y1 = polygon%point(i1)%ptr%y
    x2 = polygon%point(i2)%ptr%x
    y2 = polygon%point(i2)%ptr%y
    if (is_same_line(ys, xs, ye, xe, y1, x1, y2, x2)) then
      if (((min(ys,ye) <= y1).and.(y1 <= max(ys,ye))).or. &
       ((min(ys,ye) <= y2).and.(y2 <= max(ys,ye)))) then
        polygon_point_index = i1
        return
      end if
    end if
  end do
     
end subroutine search_overlap_line

!=======+=========+=========+=========+=========+=========+=========+=========+

logical function check_cross_line(xs, ys, xe, ye, polygon, skip_point)
  use jcf_sphere_lib, only : is_cross_line
  implicit none
  real(kind=8), intent(IN) :: xs, ys, xe, ye
  type(polygon_type), pointer :: polygon
  integer, intent(IN) :: skip_point
  real(kind=8) :: x1, y1, x2, y2, xc, yc
  integer :: i1, i2

  !write(0,*) "check_cross_line ", xs, ys, xe, ye, skip_point
    
  do i1 = 1, polygon%num_of_point
    if (i1 == skip_point) cycle
    i2 = mod(i1, polygon%num_of_point) + 1
    x1 = polygon%point(i1)%ptr%x
    y1 = polygon%point(i1)%ptr%y
    x2 = polygon%point(i2)%ptr%x
    y2 = polygon%point(i2)%ptr%y

    if (is_cross_line(ys, xs, ye, xe, y1, x1, y2, x2, yc, xc)) then
      !write(0,*) "check_cross_line true ", i1
      check_cross_line = .true.
      return
    end if
  end do

  check_cross_line = .false.

end function check_cross_line

!=======+=========+=========+=========+=========+=========+=========+=========+

end module jcf_coef_base
