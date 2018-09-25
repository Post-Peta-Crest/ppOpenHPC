!=======+=========+=========+=========+=========+=========+=========+=========+
!>
!! fem_grid defines mesh structure of FrontISTR
!! 
module fs_grid
 implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: write_grid_kml ! subroutine()  
  public :: cal_mapping_table ! subroutine ()
  public :: read_mapping_table ! subroutine (file_name, send_grid, recv_grid)

!--------------------------------  private  ----------------------------------!

  real(kind=8), parameter :: EPSILON = 1.d-10
  real(kind=8), parameter :: OLAT =  34.65409016806023d0
  real(kind=8), parameter :: OLON = 135.22066470807215d0


contains


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! write kml file
subroutine write_grid_kml(file_name, fistr, seism)
  use grid_base, only : cal_latlon_from_global_xy, cal_global_point_from_local
  use fem_grid, only : fem_grid_type
  use fdm_grid, only : fdm_grid_type
  use mod_kml, only : kml_init_polygon, kml_init_point, kml_init, set_kml_name, dump_poly_to_kml, dump_point_to_kml, kml_final
  implicit none
  character(len=*), intent(IN) :: file_name
  type(fem_grid_type), intent(IN) :: fistr
  type(fdm_grid_type), intent(IN) :: seism
  real(kind=8) :: polygon(8)
  real(kind=8) :: gx, gy
  real(kind=8) :: glon, glat
  integer :: i, j

  call kml_init_point("FISTR", 1.d0, 0, 255, 0)
  call kml_init_polygon("SEISM", 100.d0, 0, 0, 255, 2, 0.d0, 0, 0, 255)
   
  call kml_init(file_name)

  call set_kml_name("SEISM")

  do j = 1, seism%ny
    do i = 1, seism%nx
      call cal_latlon_from_global_xy(OLON, OLAT, seism%ox+seism%dx*(i-1), seism%oy-seism%dy*(j-1), polygon(1), polygon(2))
      call cal_latlon_from_global_xy(OLON, OLAT, seism%ox+seism%dx*(i)  , seism%oy-seism%dy*(j-1), polygon(3), polygon(4))
      call cal_latlon_from_global_xy(OLON, OLAT, seism%ox+seism%dx*(i)  , seism%oy-seism%dy*(j)  , polygon(5), polygon(6))
      call cal_latlon_from_global_xy(OLON, OLAT, seism%ox+seism%dx*(i-1), seism%oy-seism%dy*(j)  , polygon(7), polygon(8))
      call dump_poly_to_kml("SEISM", i, j, polygon)
    end do
  end do

  call set_kml_name("FISTR")
  do i = 1, fistr%num_of_node
    if (fistr%node(i)%z /= 0.d0) cycle
    call cal_global_point_from_local(fistr%xorg, fistr%yorg, fistr%cr, fistr%sr, fistr%node(i)%x, fistr%node(i)%y, gx, gy)
    call cal_latlon_from_global_xy(OLON, OLAT, gx, gy, glon, glat)
    call dump_point_to_kml("FISTR", i, glon, glat)
  end do

  call kml_final()

end subroutine write_grid_kml

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! calculate mapping table
!!    
!!
!!(x1,y1,z1)
!!     ___________
!!    /          /|
!!   /          / |
!!  /__________/  |
!!  |          |  |
!!  |          |  /
!!  |          | /
!!  |__________|/ 
!!           (x7,y7,z7)
!!

subroutine cal_mapping_table(FID, fistr, seism)
  use grid_base, only : cal_global_point_from_local, cal_local_point_from_global
  use fem_grid, only : fem_grid_type
  use fdm_grid, only : fdm_grid_type, get_global_index, STAG_S, STAG_X, STAG_Y, STAG_Z
  
  implicit none
  integer, intent(IN) :: FID
  type(fem_grid_type), intent(IN) :: fistr
  type(fdm_grid_type), intent(IN) :: seism
  real(kind=8) :: gx, gy ! x, y of global
  real(kind=8) :: sx, sy, sz ! x, y, z of fdm_grid
  real(kind=8) :: coef(8)
  real(kind=8) :: seism_ox, seism_oy, seism_oz
  integer :: si, sj, sk
  real(kind=8) :: x1, y1, z1, x7, y7, z7
  integer :: global_index
  integer :: i, j

  write(0,*) "cal_mapping_table ", fistr%num_of_node
  do i = 1, fistr%num_of_node

    sz = -1.d0*fistr%node(i)%z ! from upward to downward

    call cal_global_point_from_local(fistr%xorg, fistr%yorg, fistr%cr, fistr%sr, fistr%node(i)%x, fistr%node(i)%y, gx, gy)
    call cal_local_point_from_global(seism%ox, seism%oy, 1.d0, 0.d0, gx, gy, sx, sy) 
    
    !write(0,*) "fistr x, y ", fistr%node(i)%x, fistr%node(i)%y
    !write(0,*) "gx, gy ", gx, gy
    !write(0,*) "seism x, y, z", seism%ox, seism%oy, seism%oz

    sy = -1.d0*sy ! from northward grid to southward grid

    !write(0,*) sx, sy
    !stop

    seism_ox = 0 ; seism_oy = 0 ; seism_oz = seism%oz
    select case(seism%stag_type)
    case(STAG_S)
    case(STAG_X)
      seism_ox = seism%dx*0.5d0
    case(STAG_Y)
      seism_oy = seism%dy*0.5d0
    case(STAG_Z)
      seism_oz = seism%dz*0.5d0+seism%oz
    case default
       write(0,*) "cal_mapping_table, staggerding error ", seism%stag_type
       stop 999
    end select
   
                                                        
    if ((sz == 0).and.(seism%stag_type == STAG_Z)) then !  .    z = 0
      sz = seism%dz*0.5d0                               !------ z = 0.5*dz
    end if                                              !  .
                                                        
    call cal_target_element(sx, sy, sz, seism_ox, seism_oy, seism_oz, seism%dx, seism%dy, seism%dz, si, sj, sk)

    if ((si<1).or.(si>seism%nx)) cycle
    if ((sj<1).or.(sj>seism%ny)) cycle
    if ((sk<1).or.(sk>seism%nz)) cycle

    x1 = seism_ox + seism%dx*(si-1) ; x7 = x1 + seism%dx
    y1 = seism_oy + seism%dy*(sj-1) ; y7 = y1 + seism%dy
    z1 = seism_oz + seism%dz*(sk-1) ; z7 = z1 + seism%dz

    call cal_coefficient(sx, sy, sz, x1, y1, z1, x7, y7, z7, coef)
    !write(0,*) "cal_mapping_table ", sx, sy, sz
    !write(0,*) "cal_mapping_table ", si, sj, sk
    !write(0,*) "cal_mapping_table ", x1, y1, z1
    !write(0,*) coef

    if (coef(1) > 0) call write_mapping_info(FID, fistr%node(i)%index, fistr%node(i)%rank, si  , sj  , sk  , &
                                             get_global_index(seism, si  , sj  , sk  ), coef(1))
    if (coef(2) > 0) call write_mapping_info(FID, fistr%node(i)%index, fistr%node(i)%rank, si+1, sj  , sk  , &
                                             get_global_index(seism, si+1, sj  , sk  ), coef(2))
    if (coef(3) > 0) call write_mapping_info(FID, fistr%node(i)%index, fistr%node(i)%rank, si+1, sj+1, sk  , &
                                             get_global_index(seism, si+1, sj+1, sk  ), coef(3))
    if (coef(4) > 0) call write_mapping_info(FID, fistr%node(i)%index, fistr%node(i)%rank, si  , sj+1, sk  , &
                                             get_global_index(seism, si  , sj+1, sk  ), coef(4))
    if (coef(5) > 0) call write_mapping_info(FID, fistr%node(i)%index, fistr%node(i)%rank, si  , sj  , sk+1, & 
                                             get_global_index(seism, si  , sj  , sk+1), coef(5))
    if (coef(6) > 0) call write_mapping_info(FID, fistr%node(i)%index, fistr%node(i)%rank, si+1, sj  , sk+1, &
                                             get_global_index(seism, si+1, sj  , sk+1), coef(6))
    if (coef(7) > 0) call write_mapping_info(FID, fistr%node(i)%index, fistr%node(i)%rank, si+1, sj+1, sk+1, &
                                             get_global_index(seism, si+1, sj+1, sk+1), coef(7))
    if (coef(8) > 0) call write_mapping_info(FID, fistr%node(i)%index, fistr%node(i)%rank, si  , sj+1, sk+1, &
                                             get_global_index(seism, si  , sj+1, sk+1), coef(8))
    !stop 666

  end do

end subroutine cal_mapping_table

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! calculate target element
subroutine write_mapping_info(FID, recv_index, recv_rank, send_i, send_j, send_k, send_global, coef)
  implicit none
  integer, intent(IN) :: FID
  integer, intent(IN) :: recv_index, recv_rank, send_i, send_j, send_k, send_global
  real(kind=8), intent(IN) :: coef

  if (recv_rank >= 0) then
    write(FID, '(I8,I5,I7,I7,I7,I12,F12.8)') recv_index, recv_rank, send_i, send_j, send_k, send_global, coef
  end if

end subroutine write_mapping_info

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! calculate target element
subroutine cal_target_element(cx, cy, cz, ox, oy, oz, dx, dy, dz, si, sj, sk)
  implicit none
  real(kind=8), intent(IN) :: cx, cy, cz
  real(kind=8), intent(IN) :: ox, oy, oz
  real(kind=8), intent(IN) :: dx, dy, dz
  integer, intent(OUT) :: si, sj, sk

  si = int((cx-ox+0.000001d0)/dx) + 1
  sj = int((cy-oy+0.000001d0)/dy) + 1
  sk = int((cz-oz+0.000001d0)/dz) + 1

end subroutine cal_target_element


!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! calculate coefficient
!!
!! (x1,y1)  
!! d1__________________d2 
!!  |                  |
!!  |                  | beta1
!!  |         x        |_
!!  |      (cx,cy)     |
!!  |                  | beta2
!!  |__________________|
!! d4 alpha1  | alpha2  d3 
!!                     (x7,y7)
!!
!!
!!           _ (z1)
!!
!!              gamma1
!!           x  ___
!!          (cz)
!!              gamma2
!!
!!           _
!!          (zy)
!!   
subroutine cal_coefficient(cx, cy, cz, x1, y1, z1, x7, y7, z7, coef)
  implicit none
  real(kind=8), intent(IN) :: cx, cy, cz
  real(kind=8), intent(IN) :: x1, y1, z1
  real(kind=8), intent(IN) :: x7, y7, z7
  real(kind=8), intent(OUT) :: coef(:)
  real(kind=8) :: alpha1, alpha2, beta1, beta2, gamma1, gamma2
  integer :: i

  alpha1 = (cx-x1)/(x7-x1)
  alpha2 = (x7-cx)/(x7-x1)
  beta1  = (cy-y1)/(y7-y1)
  beta2  = (y7-cy)/(y7-y1)
  gamma1 = (cz-z1)/(z7-z1)
  gamma2 = (z7-cz)/(z7-z1)

  coef(1) = alpha2*beta2*gamma2
  coef(2) = alpha1*beta2*gamma2
  coef(3) = alpha1*beta1*gamma2
  coef(4) = alpha2*beta1*gamma2
  coef(5) = alpha2*beta2*gamma1
  coef(6) = alpha1*beta2*gamma1
  coef(7) = alpha1*beta1*gamma1
  coef(8) = alpha2*beta1*gamma1

  do i = 1, 8
    if (abs(coef(i)) <= EPSILON) coef(i) = 0.d0
  end do

end subroutine cal_coefficient

!=======+=========+=========+=========+=========+=========+=========+=========+
!> 
!! calculate target element
subroutine read_mapping_table(file_name, send_grid, recv_grid, coef)
  implicit none
  character(len=*), intent(IN) :: file_name
  integer, pointer :: send_grid(:)
  integer, pointer :: recv_grid(:)
  real(kind=8), pointer :: coef(:)
  integer, parameter :: FID = 228
  character(len=128) :: file_str
  integer :: r_grid, r_rank, s_grid, si, sj, sk
  real(kind=8) :: fcoef
  integer :: counter
  integer :: istat

  open(unit=FID, file=trim(file_name))
  read(FID,*) file_str
  counter = 0
  do 
    read(FID, *, iostat = istat) r_grid, r_rank, si, sj, sk, s_grid, fcoef
    if (istat /= 0) exit
    counter = counter + 1
  end do

200 continue
  rewind(FID)

  allocate(send_grid(counter), recv_grid(counter), coef(counter))

  read(FID, *) file_str
  counter = 0
  do 
    read(FID, *, iostat = istat) r_grid, r_rank, si, sj, sk, s_grid, fcoef
    if (istat /= 0) exit
    counter = counter + 1

    recv_grid(counter) = r_grid
    send_grid(counter) = s_grid
    coef(counter) = fcoef
  end do
  
  close(FID)

end subroutine read_mapping_table

end module fs_grid

