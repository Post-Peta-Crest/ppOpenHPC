module ppoh_MATHMP_rotation
  use ppoh_MATHMP_base, only : NAME_LEN, STR_LEN
  implicit none
  private

!--------------------------------   public  ----------------------------------!

  public :: init_rotation ! subroutine (my_name)

!--------------------------------  private  ----------------------------------!

  character(len=NAME_LEN) :: my_name   ! my component name

  type rotate_coef_type
     character(len=NAME_LEN) :: data_name
     integer :: my_size   ! data size of my grid
     real(kind=8) :: theta = 0.d0 ! rotation angle of 2D case
     real(kind=8) :: alpha = 0.d0 ! rotation angle of X axis
     real(kind=8) :: beta  = 0.d0 ! rotation angle of y axis
     real(kind=8) :: gamma = 0.d0 ! rotation angle of z axis
  end type rotate_coef_type

  integer :: num_of_coef = 0
  type (rotate_coef_type), pointer :: rot(:)

contains

!=======+=========+=========+=========+=========+=========+=========+=========+

subroutine init_rotation(my_comp_name)
  use ppoh_MATHMP_namelist, only : vector_config_type, get_num_of_vector_config, get_vector_config_ptr
  implicit none
  character(len=*), intent(IN) :: my_comp_name
  type (vector_config_type), pointer :: conf_ptr
  integer :: i

  my_name = my_comp_name

  do i = 1, get_num_of_vector_config()
    conf_ptr => get_vector_config_ptr(i)
    if (trim(my_name) == trim(conf_ptr%comp_name)) then
      num_of_coef = num_of_coef + 1
    end if
  end do

  allocate(rot(num_of_coef))

  num_of_coef = 0

  do i = 1, get_num_of_vector_config()
    conf_ptr => get_vector_config_ptr(i)
    if (trim(my_name) == trim(conf_ptr%comp_name)) then
      num_of_coef = num_of_coef + 1
      rot(num_of_coef)%data_name = conf_ptr%data_name
    end if
  end do


end subroutine init_rotation

!=======+=========+=========+=========+=========+=========+=========+=========+
! 
! 2D
! c1 = cos(theta)
! c2 = sin(theta)
!
! |x'|  | c1  -c2 ||x|
! |  |= |         || |
! |y'|  | c2   c1 ||y|
!
subroutine rotate_data_2d(data_name, data_size, data1, data2)
  implicit none
  character(len=*), intent(IN) :: data_name
  integer, intent(IN) :: data_size
  real(kind=8), intent(INOUT) :: data1(*)
  real(kind=8), intent(INOUT) :: data2(*)
  real(kind=8) :: d1, d2
  real(kind=8) :: c1, c2
  integer :: i

  c1 = 0.d0 ; c2 = 0.d0

  do i = 1, num_of_coef
    if (trim(data_name) == trim(rot(i)%data_name)) then
      c1 = cos(rot(i)%theta)
      c2 = sin(rot(i)%theta)
      exit
    end if
  end do

  do i = 1, data_size
    d1 = data1(i)
    d2 = data2(i)

    data1(i) = c1*d1 - c2*d2
    data2(i) = c2*d1 + c1*d2
  end do 

end subroutine rotate_data_2d

!=======+=========+=========+=========+=========+=========+=========+=========+
! 
! 3D
! c1 = cos(alpha)
! c2 = cos(beta)
! c3 = cos(gamma)
! s1 = sin(alpha)
! s2 = sin(beta)
! s3 = sin(gamma)
!
! |x'|  | c1c2c3-s1s3 -c1c2s3-s1c3 c1s2 0 ||x|
! |y'|= | s1c2c3+c1s3 -s1c2s3+c1c3 s1s2 0 ||y|
! |z'|  |     -s2c3         s2s3    c2  0 ||z|
! |1 |  |       0             0     0   1 ||1|
!
subroutine rotate_data_3d(data_name, data_size, data1, data2, data3)
  implicit none
  character(len=*), intent(IN) :: data_name
  integer, intent(IN) :: data_size
  real(kind=8), intent(INOUT) :: data1(*)
  real(kind=8), intent(INOUT) :: data2(*)
  real(kind=8), intent(INOUT) :: data3(*)
  real(kind=8) :: d1, d2, d3
  real(kind=8) :: c1, c2, c3, s1, s2, s3
  integer :: i

  c1 = 0.d0 ; c2 = 0.d0 ; c3 = 0
  s1 = 0.d0 ; s2 = 0.d0 ; s3 = 0

  do i = 1, num_of_coef
    if (trim(data_name) == trim(rot(i)%data_name)) then
      c1 = cos(rot(i)%alpha)
      c2 = cos(rot(i)%beta)
      c3 = cos(rot(i)%gamma)
      s1 = sin(rot(i)%alpha)
      s2 = sin(rot(i)%beta)
      s3 = sin(rot(i)%gamma)
      exit
    end if
  end do

  do i = 1, data_size

    d1 = data1(i)
    d2 = data2(i)
    d3 = data3(i)

    data1(i) = (c1*c2*c3-s1*s3)*d1 + (-c1*c2*s3-s1*c3)*d2 + (c1*s2)*d3
    data2(i) = (s1*c2*c3+c1*s3)*d1 + (-s1*c2*s3+c1*c3)*d2 + (s1*s2)*d3
    data3(i) = (        -s2*c3)*d1 + (          s2*s3)*d2 + (   c2)*d3

  end do 

end subroutine rotate_data_3d


end module ppoh_MATHMP_rotation
