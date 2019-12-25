!=====================================================================*
!                                                                     *
!   Software Name : ppohBEM                                           *
!         Version : 0.1                                               *
!                                                                     *
!   License                                                           *
!     This file is part of ppohBEM.                                   *
!     ppohBEM is a free software, you can use it under the terms   *
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
module m_ppohBEM_matrix_element_ij
  use m_ppohBEM_user_func
  
!*** type :: coordinate
  type :: coordinate
    real(8) :: x ,y ,z
  end type coordinate
  
contains
  real(8) function ppohBEM_matrix_element_ij(i, j, nond, nofc, nond_on_fc, np,int_para_fc, nint_para_fc, dble_para_fc, ndble_para_fc, face2node)
 
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

  integer ,intent(in) :: i, nond, nofc, nond_on_fc, nint_para_fc, ndble_para_fc  !!!! call by value
  type(coordinate), intent(in) :: np(*)
  integer, intent(in) :: face2node(3, *), int_para_fc(nint_para_fc,*)
  real(8), intent(in) :: dble_para_fc( ndble_para_fc, * )
  
  ppohBEM_right_hand_side_vector_element_i = dble_para_fc(1,i)
  
end function ppohBEM_right_hand_side_vector_element_i

end module m_ppohBEM_matrix_element_ij
