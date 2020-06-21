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

subroutine ppohAMRFDM_interpolate(iLv,iF,iK,iN,st_param,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::iLv,iF,iK,iN
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  integer(kind=ppohAMRFDM_kint)::index,ID,is,ie
  real(kind=ppohAMRFDM_kdbl)::b1,b2,b3,b4,c1,c2
  type(st_ppohAMRFDM_octset),pointer::p0,new1,new2,new3,new4,new5,new6,new7,new8
  if(iLv==st_param%LvMax) return
  if(st_meshset%MaxID(iN,iLv)<=st_meshset%MinID(iN,iLv)) return
  is=1; ie=st_param%npy
  !$omp parallel do default(none) &
  !$omp& private(index,p0,new1,new2,new3,new4,new5,new6,new7,new8,&
  !$omp& ID,b1,b2,b3,b4,c1,c2) &
  !$omp& shared(st_param,st_meshset,iLv,iF,iK,iN,is,ie)
  do index=st_meshset%MinID(iN,iLv),st_meshset%MaxID(iN,iLv)
     if(IN==1.or.IN==2) then
        p0 => st_meshset%Mesh(index)
     else if(IN==3.or.IN==4) then
        p0 => st_meshset%GMesh(index)
     endif
     if(p0%iFLG(iK)==iF) then
        new1 => p0
        new2 => p0              %octNb1
        new3 => p0       %octNb3
        new4 => p0       %octNb3%octNb1
        new5 => p0%octNb5
        new6 => p0%octNb5       %octNb1
        new7 => p0%octNb5%octNb3
        new8 => p0%octNb5%octNb3%octNb1
        do ID=is,ie
           b1=0.75d0*new1%F(ID)+0.25d0*new2%F(ID)
           b2=0.75d0*new3%F(ID)+0.25d0*new4%F(ID)
           b3=0.75d0*new5%F(ID)+0.25d0*new6%F(ID)
           b4=0.75d0*new7%F(ID)+0.25d0*new8%F(ID)
           c1=0.75d0*b1       +0.25d0*b2
           c2=0.75d0*b3       +0.25d0*b4
           p0%octCh1%F(ID)=0.75d0*c1+0.25d0*c2
        enddo
        new1 => p0
        new2 => p0              %octNb2
        new3 => p0       %octNb3
        new4 => p0       %octNb3%octNb2
        new5 => p0%octNb5
        new6 => p0%octNb5       %octNb2
        new7 => p0%octNb5%octNb3
        new8 => p0%octNb5%octNb3%octNb2
        do ID=is,ie
           b1=0.75d0*new1%F(ID)+0.25d0*new2%F(ID)
           b2=0.75d0*new3%F(ID)+0.25d0*new4%F(ID)
           b3=0.75d0*new5%F(ID)+0.25d0*new6%F(ID)
           b4=0.75d0*new7%F(ID)+0.25d0*new8%F(ID)
           c1=0.75d0*b1       +0.25d0*b2
           c2=0.75d0*b3       +0.25d0*b4
           p0%octCh2%F(ID)=0.75d0*c1+0.25d0*c2
        enddo
        new1 => p0
        new2 => p0              %octNb1
        new3 => p0       %octNb4
        new4 => p0       %octNb4%octNb1
        new5 => p0%octNb5
        new6 => p0%octNb5       %octNb1
        new7 => p0%octNb5%octNb4
        new8 => p0%octNb5%octNb4%octNb1
        do ID=is,ie
           b1=0.75d0*new1%F(ID)+0.25d0*new2%F(ID)
           b2=0.75d0*new3%F(ID)+0.25d0*new4%F(ID)
           b3=0.75d0*new5%F(ID)+0.25d0*new6%F(ID)
           b4=0.75d0*new7%F(ID)+0.25d0*new8%F(ID)
           c1=0.75d0*b1       +0.25d0*b2
           c2=0.75d0*b3       +0.25d0*b4
           p0%octCh3%F(ID)=0.75d0*c1+0.25d0*c2
        enddo
        new1 => p0
        new2 => p0              %octNb2
        new3 => p0       %octNb4
        new4 => p0       %octNb4%octNb2
        new5 => p0%octNb5
        new6 => p0%octNb5       %octNb2
        new7 => p0%octNb5%octNb4
        new8 => p0%octNb5%octNb4%octNb2
        do ID=is,ie
           b1=0.75d0*new1%F(ID)+0.25d0*new2%F(ID)
           b2=0.75d0*new3%F(ID)+0.25d0*new4%F(ID)
           b3=0.75d0*new5%F(ID)+0.25d0*new6%F(ID)
           b4=0.75d0*new7%F(ID)+0.25d0*new8%F(ID)
           c1=0.75d0*b1       +0.25d0*b2
           c2=0.75d0*b3       +0.25d0*b4
           p0%octCh4%F(ID)=0.75d0*c1+0.25d0*c2
        enddo
        new1 => p0
        new2 => p0              %octNb1
        new3 => p0       %octNb3
        new4 => p0       %octNb3%octNb1
        new5 => p0%octNb6
        new6 => p0%octNb6       %octNb1
        new7 => p0%octNb6%octNb3
        new8 => p0%octNb6%octNb3%octNb1
        do ID=is,ie
           b1=0.75d0*new1%F(ID)+0.25d0*new2%F(ID)
           b2=0.75d0*new3%F(ID)+0.25d0*new4%F(ID)
           b3=0.75d0*new5%F(ID)+0.25d0*new6%F(ID)
           b4=0.75d0*new7%F(ID)+0.25d0*new8%F(ID)
           c1=0.75d0*b1       +0.25d0*b2
           c2=0.75d0*b3       +0.25d0*b4
           p0%octCh5%F(ID)=0.75d0*c1+0.25d0*c2
        enddo
        new1 => p0
        new2 => p0              %octNb2
        new3 => p0       %octNb3
        new4 => p0       %octNb3%octNb2
        new5 => p0%octNb6
        new6 => p0%octNb6       %octNb2
        new7 => p0%octNb6%octNb3
        new8 => p0%octNb6%octNb3%octNb2
        do ID=is,ie
           b1=0.75d0*new1%F(ID)+0.25d0*new2%F(ID)
           b2=0.75d0*new3%F(ID)+0.25d0*new4%F(ID)
           b3=0.75d0*new5%F(ID)+0.25d0*new6%F(ID)
           b4=0.75d0*new7%F(ID)+0.25d0*new8%F(ID)
           c1=0.75d0*b1       +0.25d0*b2
           c2=0.75d0*b3       +0.25d0*b4
           p0%octCh6%F(ID)=0.75d0*c1+0.25d0*c2
        enddo
        new1 => p0
        new2 => p0              %octNb1
        new3 => p0       %octNb4
        new4 => p0       %octNb4%octNb1
        new5 => p0%octNb6
        new6 => p0%octNb6       %octNb1
        new7 => p0%octNb6%octNb4
        new8 => p0%octNb6%octNb4%octNb1
        do ID=is,ie
           b1=0.75d0*new1%F(ID)+0.25d0*new2%F(ID)
           b2=0.75d0*new3%F(ID)+0.25d0*new4%F(ID)
           b3=0.75d0*new5%F(ID)+0.25d0*new6%F(ID)
           b4=0.75d0*new7%F(ID)+0.25d0*new8%F(ID)
           c1=0.75d0*b1       +0.25d0*b2
           c2=0.75d0*b3       +0.25d0*b4
           p0%octCh7%F(ID)=0.75d0*c1+0.25d0*c2
        enddo
        new1 => p0
        new2 => p0              %octNb2
        new3 => p0       %octNb4
        new4 => p0       %octNb4%octNb2
        new5 => p0%octNb6
        new6 => p0%octNb6       %octNb2
        new7 => p0%octNb6%octNb4
        new8 => p0%octNb6%octNb4%octNb2
        do ID=is,ie
           b1=0.75d0*new1%F(ID)+0.25d0*new2%F(ID)
           b2=0.75d0*new3%F(ID)+0.25d0*new4%F(ID)
           b3=0.75d0*new5%F(ID)+0.25d0*new6%F(ID)
           b4=0.75d0*new7%F(ID)+0.25d0*new8%F(ID)
           c1=0.75d0*b1       +0.25d0*b2
           c2=0.75d0*b3       +0.25d0*b4
           p0%octCh8%F(ID)=0.75d0*c1+0.25d0*c2
        enddo
     endif
  enddo
  !$omp end parallel do
  return
end subroutine ppohAMRFDM_interpolate
