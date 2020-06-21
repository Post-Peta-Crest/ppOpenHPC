!=====================================================================!!
!!                                                                    !!
!!   Software Name : ppOpen-APPL/AMR-FDM (ppohAMRFDM)                 !!
!!         Version : 0.3.0                                            !!
!!                                                                    !!
!!   License:                                                         !!
!!     This file is part of ppohAMRFDM.                               !!
!!     ppohAMRFDM is a free software, you can use it under the terms  !!
!!     of The MIT License (MIT). See LICENSE file and User's guide    !!
!!     for more details.                                              !!
!!                                                                    !!
!!   ppOpen-HPC project:                                              !!
!!     Open Source Infrastructure for Development and Execution of    !!
!!     Large-Scale Scientific Applications on Post-Peta-Scale         !!
!!     Supercomputers with Automatic Tuning (AT).                     !!
!!                                                                    !!
!!   Organizations:                                                   !!
!!     The University of Tokyo                                        !!
!!       - Information Technology Center                              !!
!!       - Atmosphere and Ocean Research Institute (AORI)             !!
!!       - Interfaculty Initiative in Information Studies/            !!
!!         Earthquake Research Institute (ERI)                        !!
!!       - Graduate School of Frontier Science                        !!
!!     Kyoto University                                               !!
!!       - Academic Center for Computing and Media Studies            !!
!!     Japan Agency for Marine-Earth Science and Technology (JAMSTEC) !!
!!                                                                    !!
!!   Sponsorship:                                                     !!
!!     Japan Science and Technology Agency (JST), Basic Research      !!
!!     Programs: CREST, Development of System Software Technologies   !!
!!     for Post-Peta Scale High Performance Computing.                !!
!!                                                                    !!
!!   Copyright (c) 2014 <Masaharu Matsumoto, The University of Tokyo  !!
!!                       matsumoto(at)cc.u-tokyo.ac.jp           >    !!
!!                                                                    !!
!!====================================================================!!

subroutine ppohAMRFDM_MrtN_nghbrs(iPOS,Mnarr,st_param)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  integer,intent(in)::iPOS(1:3)
  integer,intent(out)::Mnarr(1:27)
  ! local
  integer,parameter::max=20
  integer::nx(1:max),ny(1:max),nz(1:max)
  integer::ix,iy,iz,ix2,iy2,iz2
  integer::i,j,k,m
  integer::ixx(1:3),iyy(1:3),izz(1:3)
  ! initialize
  Mnarr(:)=0
  nx(:)=0; ny(:)=0; nz(:)=0; m=1
  ! Convert iPOS system from hieralchial to Normal ordering
  ix=(iPOS(1)+st_param%Nint2)/(2*st_param%Nint2); ix=ix-1
  iy=(iPOS(2)+st_param%Nint2)/(2*st_param%Nint2); iy=iy-1
  iz=(iPOS(3)+st_param%Nint2)/(2*st_param%Nint2); iz=iz-1
  ixx=(/1, 0, 0/); iyy=(/0, 1, 0/); izz=(/0, 0, 1/)
  do j=-1,1,2
     do k=1,3 
        ix2=ix+ixx(k)*j; iy2=iy+iyy(k)*j; iz2=iz+izz(k)*j
        if(ix2<0) ix2=st_param%pxmax*st_param%ixmax-1
        if(iy2<0) iy2=st_param%pymax*st_param%iymax-1
        if(iz2<0) iz2=st_param%pzmax*st_param%izmax-1
        if(ix2>st_param%pxmax*st_param%ixmax-1) ix2=0
        if(iy2>st_param%pymax*st_param%iymax-1) iy2=0
        if(iz2>st_param%pzmax*st_param%izmax-1) iz2=0
        do i=1,max
           nx(i)=mod(ix2,2); ny(i)=mod(iy2,2); nz(i)=mod(iz2,2)
           ix2=ix2/2; iy2=iy2/2; iz2=iz2/2
        enddo
        do i=1,max
           Mnarr(m)=Mnarr(m) &
                +nx(i)*(2**(3*i-3)) &
                +ny(i)*(2**(3*i-2)) &
                +nz(i)*(2**(3*i-1))    
        enddo
        Mnarr(m)=Mnarr(m)+1
        m=m+1
     enddo
  enddo
  return
end subroutine PpohAMRFDM_MrtN_nghbrs
