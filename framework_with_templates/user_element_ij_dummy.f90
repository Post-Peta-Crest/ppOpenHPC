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
!****************************************************************************
  real(8) function element_ij(i, j, nond, nofc, nond_on_fc, np,int_para_fc, nint_para_fc, dble_para_fc, ndble_para_fc, face2node)
!  use user_func
 
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

      if (i.eq.1.and.j.eq.1) then
        print*, 'Dummy user element_ij function was used.'
      endif
      
  if (i.eq.j) then
    element_ij = 1.1
  elseif (i.eq.j+2) then
    element_ij=-0.001
  else
    element_ij=0.0
  endif
  
end function element_ij
