!=====================================================================*
!                                                                     *
!   Software Name : ppohBEM                                           *
!         Version : 0.5.0                                             *
!                                                                     *
!   License                                                           *
!     This file is part of ppohBEM.                                   *
!     ppohBEM is a free software, you can use it under the terms      *
!     of The MIT License (MIT). See LICENSE file and User's guide     *
!     for more details.                                               *
!                                                                     *
!   ppOpen-HPC project:                                               *
!     Open Source Infrastructure for Development and Execution of     *
!     Large-Scale Scientific Applications on Post-Peta-Scale          *
!     Supercomputers with Automatic Tuning (AT).                      *
!                                                                     *
!   Organizations:                                                    *
!     The University of Tokyo                                         *
!       - Information Technology Center                               *
!       - Atmosphere and Ocean Research Institute (AORI)              *
!       - Interfaculty Initiative in Information Studies              *
!         /Earthquake Research Institute (ERI)                        *
!       - Graduate School of Frontier Science                         *
!     Kyoto University                                                *
!       - Academic Center for Computing and Media Studies             *
!     Hokkaido University                                             *
!       - Information Initiative Center                               *
!     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  *
!                                                                     *
!   Sponsorship:                                                      *
!     Japan Science and Technology Agency (JST), Basic Research       *
!     Programs: CREST, Development of System Software Technologies    *
!     for post-Peta Scale High Performance Computing.                 *
!                                                                     *
!   Copyright (c) 2012 <Takeshi Iwashita, Takeshi Mifune, Yuki Noseda,*
!                    Yasuhito Takahashi, Masatoshi Kawai, Akihiro Ida>*
!                                                                     *
!=====================================================================*
!
module user_func
  integer, parameter :: KIND_REAL = kind(1d0)
  real(kind = KIND_REAL), parameter :: PI = 3.141592653589793
  real(kind = KIND_REAL), parameter :: EPSILON_0 = 8.854187818 * 1d-12
  
  public face_integral

  contains

    real(kind = KIND_REAL) function face_integral(xs, ys, zs, x, y, z)
    real(kind = KIND_REAL), intent(in) :: xs(3), ys(3), zs(3)
    real(kind = KIND_REAL), intent(in) :: x, y, z
    
      real(kind = KIND_REAL) :: r(3)
      real(kind = KIND_REAL) :: xi, xj, yi, dx, dy, t, l, m, d, ti, tj
      real(kind = KIND_REAL) :: theta, omega, q, g, zp, zpabs
      
      integer :: i, j
      real(kind = KIND_REAL) :: u(3), v(3), w(3) 
      real(kind = KIND_REAL) :: ox, oy, oz 
      
      r(:) = sqrt( (xs(:) - x)**2 + (ys(:) - y)**2 + (zs(:) - z)**2 )
      
      u(1) = xs(2) - xs(1);  v(1) = xs(3) - xs(2)
      u(2) = ys(2) - ys(1);  v(2) = ys(3) - ys(2)
      u(3) = zs(2) - zs(1);  v(3) = zs(3) - zs(2)
      call cross_product(u, v, w)
      w(:) = w(:) / sqrt( dot_product(w, w) )
      
      u(1) = x - xs(1);  u(2) = y - ys(1);  u(3) = z - zs(1)
      zp = dot_product(u, w)
      ox = x - zp * w(1);  oy = y - zp * w(2);  oz = z - zp * w(3)
      zpabs = abs(zp)
      
      face_integral = 0d0
      do i = 1, 3
         j = mod(i, 3) + 1
         u(1) = xs(j) - ox;  u(2) = ys(j) - oy; u(3) = zs(j) - oz;
               xj = sqrt( dot_product(u, u) )
         u(:) = u(:) / xj
         call cross_product(w, u, v) 
         xi = (xs(i) - ox) * u(1) + (ys(i) - oy) * u(2) + (zs(i) - oz) * u(3)
         yi = (xs(i) - ox) * v(1) + (ys(i) - oy) * v(2) + (zs(i) - oz) * v(3)

         dx = xj - xi;  dy = - yi ! (yj = 0)
         t = sqrt( dx**2 + dy**2 )
         l = dx/t;  m = dy/t
         d = l * yi - m * xi
         ti = l * xi + m * yi;  tj = l * xj ! (yj = 0)

         !            theta = sign(1d0, yi) * acos( xi / sqrt( xi**2 + yi**2 ) ) ! (xj > 0, yj = 0)
         theta = atan2(yi, xi)
         omega = theta - atan2( r(i) * d, zpabs * ti ) + atan2( r(j) * d, zpabs * tj )
         q = log( (r(j) + tj) / ( r(i) + ti ) )
         g = d * q - zpabs * omega 
         face_integral = face_integral + g
      enddo

      face_integral = abs(face_integral) / (4d0 * PI * EPSILON_0)       

    end function

!***********************************************************************
    subroutine cross_product(u, v, w)
      real(kind = KIND_REAL) :: u(3), v(3), w(3)

      w(1) = u(2) * v(3) - u(3) * v(2)
      w(2) = u(3) * v(1) - u(1) * v(3)
      w(3) = u(1) * v(2) - u(2) * v(1)

    end subroutine cross_product

  end module user_func

!****************************************************************************
  real(8) function ppohBEM_matrix_element_ij(i, j, nond, nofc, nond_on_fc, np,int_para_fc, nint_para_fc, dble_para_fc, ndble_para_fc, face2node)
  use user_func
 
  type :: coordinate
     real(8) :: x ,y ,z
  end type coordinate
 
   integer ,intent(in) :: i, j, nond, nofc, nond_on_fc, nint_para_fc, ndble_para_fc
  type(coordinate), intent(in) :: np(*)
  integer, intent(in) :: face2node(3, *), int_para_fc(nint_para_fc,*)
  real(8), intent(in) :: dble_para_fc(ndble_para_fc,*)
  
  integer :: n(3)
  real(8) :: xf(3), yf(3), zf(3)
  real(8) :: xp, yp, zp

  n(1:3) = face2node(1:3, i) + 1
  xf(1:3) = np( n(1:3) )%x
  yf(1:3) = np( n(1:3) )%y
  zf(1:3) = np( n(1:3) )%z

  xp = sum( xf(1:3) ) / 3d0
  yp = sum( yf(1:3) ) / 3d0
  zp = sum( zf(1:3) ) / 3d0

  n(1:3) = face2node(1:3, j) + 1
  xf(1:3) = np( n(1:3) )%x
  yf(1:3) = np( n(1:3) )%y
  zf(1:3) = np( n(1:3) )%z
  
  ppohBEM_matrix_element_ij = face_integral(xf, yf, zf, xp, yp, zp)
  
end function ppohBEM_matrix_element_ij

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!    ppohBEM_right_hand_side_vector_element_i    !!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real(8) function ppohBEM_right_hand_side_vector_element_i(i, nond, nofc, nond_on_fc, np, int_para_fc, nint_para_fc, &
  dble_para_fc, ndble_para_fc, face2node)
  use user_func

  type :: coordinate
     real(8) :: x ,y ,z
  end type coordinate

  integer ,intent(in) :: i, nond, nofc, nond_on_fc, nint_para_fc, ndble_para_fc  !!!! call by value
  type(coordinate), intent(in) :: np(*)
  integer, intent(in) :: face2node(3, *), int_para_fc(nint_para_fc,*)
  real(8), intent(in) :: dble_para_fc( ndble_para_fc, * )
  
  ppohBEM_right_hand_side_vector_element_i = dble_para_fc(1,i)
  
end function ppohBEM_right_hand_side_vector_element_i
