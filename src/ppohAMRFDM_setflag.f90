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

subroutine ppohAMRFDM_setflag(iLv,st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::iLv
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  type(st_ppohAMRFDM_octset),pointer::p0
  integer(kind=ppohAMRFDM_kint)::index,ID,JD,rfnflg,addR,addG,RSize,GSize
  if(iLv==0) then
     ! --- set iFLG(3) ---
     !$omp parallel do default(none) &
     !$omp& private(index,p0,rfnflg) &
     !$omp& shared(st_param,st_meshset,iLv)
     do index=st_meshset%MinID(1,iLv),st_meshset%MaxID(1,iLv)
        p0 => st_meshset%Mesh(index)
        if(p0%iFLG(3)==1) then
           if(p0%octCh1%iFLG(1)>=1) cycle; if(p0%octCh2%iFLG(1)>=1) cycle
           if(p0%octCh3%iFLG(1)>=1) cycle; if(p0%octCh4%iFLG(1)>=1) cycle
           if(p0%octCh5%iFLG(1)>=1) cycle; if(p0%octCh6%iFLG(1)>=1) cycle
           if(p0%octCh7%iFLG(1)>=1) cycle; if(p0%octCh8%iFLG(1)>=1) cycle
           call ppohAMRFDM_set_unrefine_criteria(index,iLv,rfnflg,st_param,st_meshset)
           if(rfnflg==1) p0%iFLG(3)=0
        endif
        call ppohAMRFDM_set_refine_criteria(index,iLv,rfnflg,st_param,st_meshset)
        if(rfnflg==1) p0%iFLG(3)=1
     enddo
     !$omp end parallel do
     ! --- set iFLG(1) ---
     !$omp parallel default(none) &
     !$omp& private(index,p0) &
     !$omp& shared(st_meshset,st_param,iLv)
     !$omp do
     do index=st_meshset%MinID(1,iLv),st_meshset%MaxID(1,iLv)
        p0 => st_meshset%Mesh(index)
        p0%iFLG(2)=min(p0%iFLG(1),st_param%nfg)
        p0%iFLG(1)=0
     enddo
     !$omp end do
     !$omp do
     do index=st_meshset%MinID(3,iLv),st_meshset%MaxID(3,iLv)
        p0 => st_meshset%GMesh(index)
        p0%iFLG(2)=min(p0%iFLG(1),st_param%nfg)
        p0%iFLG(1)=0
     enddo
     !$omp end do
     !$omp end parallel
     ! --- layout iFLG(1)=nfg ---
     call ppohAMRFDM_flg_layout(1,iLv,st_param,st_meshset)
  else if(iLv==st_param%LvMax) then 
     do ID=1,4
        if(st_meshset%MinID(ID,iLv).lt.st_meshset%MaxID(ID,iLv)) then
           !$omp parallel do default(none) &
           !$omp& private(index,p0) &
           !$omp& shared(st_meshset,st_param,ID,iLv)
           do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
              if(ID==1.or.ID==2) then
                 p0 => st_meshset%Mesh(index)
              else if(ID==3.or.ID==4) then
                 p0 => st_meshset%GMesh(index)
              endif
              p0%iFLG(1)=p0%octPrt%iFLG(1)-st_param%nfg
           enddo
           !$omp end parallel do
        endif
     enddo
     return
  else
     do ID=1,2
        if(st_meshset%MinID(ID,iLv)<st_meshset%MaxID(ID,iLv)) then
           !$omp parallel do default(none) &
           !$omp& private(index,p0,rfnflg) &
           !$omp& shared(st_meshset,st_param,ID,iLv)
           do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
              p0 => st_meshset%Mesh(index)
              if(p0%octPrt%iFLG(3)==1) then
                 if(p0%iFLG(3)==1) then
                    if(p0%octCh1%iFLG(1)>=1) cycle; if(p0%octCh2%iFLG(1)>=1) cycle
                    if(p0%octCh3%iFLG(1)>=1) cycle; if(p0%octCh4%iFLG(1)>=1) cycle
                    if(p0%octCh5%iFLG(1)>=1) cycle; if(p0%octCh6%iFLG(1)>=1) cycle
                    if(p0%octCh7%iFLG(1)>=1) cycle; if(p0%octCh8%iFLG(1)>=1) cycle
                    call ppohAMRFDM_set_unrefine_criteria(index,iLv,rfnflg,st_param,st_meshset)
                    if(rfnflg==1) p0%iFLG(3)=0
                 endif
                 call ppohAMRFDM_set_refine_criteria(index,iLv,rfnflg,st_param,st_meshset)
                 if(rfnflg==1) p0%iFLG(3)=1
              endif
           enddo
           !$omp end parallel do
        endif
     enddo
     do ID=1,4
        if(st_meshset%MinID(ID,iLv)<st_meshset%MaxID(ID,iLv)) then
           ! --- set iFLG(1) ---
           !$omp parallel do default(none) &
           !$omp& private(index,p0) &
           !$omp& shared(st_meshset,st_param,ID,iLv)
           do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
              if(ID==1.or.ID==2) then
                 p0 => st_meshset%Mesh(index)
              else if(ID==3.or.ID==4) then
                 p0 => st_meshset%GMesh(index)
              endif
              p0%iFLG(2)=min(p0%iFLG(1),st_param%nfg)
              p0%iFLG(1)=p0%octPrt%iFLG(1)-st_param%nfg
           enddo
           !$omp end parallel do
        endif
     enddo
     do ID=1,2
        ! --- layout iFLG(1)=nfg ---
        call ppohAMRFDM_flg_layout(ID,iLv,st_param,st_meshset)
     enddo
  endif
  call ppohAMRFDM_passing_iFLG(iLv,st_param,st_meshset,st_comm_info)
  ! --- layout iFLG(1)=nfg for Goct ---
  do ID=3,4
     call ppohAMRFDM_flg_layout(ID,iLv,st_param,st_meshset)
  enddo
  call ppohAMRFDM_passing_iFLG(iLv,st_param,st_meshset,st_comm_info)
  do ID=st_param%nfg-1,1,-1
     call ppohAMRFDM_set_buffer_out(iLv,ID+1,ID,st_meshset)
  enddo
  call ppohAMRFDM_passing_iFLG(iLv,st_param,st_meshset,st_comm_info)
  ! -- for OpenMP parallelization at add_oct --
  allocate(st_meshset%rfnoct1(st_meshset%MinID(1,iLv):st_meshset%MaxID(1,iLv)))
  allocate(st_meshset%rfnoct2(st_meshset%MinID(2,iLv):st_meshset%MaxID(2,iLv)))
  allocate(st_meshset%rfnoct3(st_meshset%MinID(3,iLv):st_meshset%MaxID(3,iLv)))
  allocate(st_meshset%rfnoct4(st_meshset%MinID(4,iLv):st_meshset%MaxID(4,iLv)))
  st_meshset%rfnoct1(:)=0; st_meshset%rfnoct2(:)=0
  st_meshset%rfnoct3(:)=0; st_meshset%rfnoct4(:)=0
  ! --- set iFLG(2) ---
  addR=0; addG=0
  do ID=1,4
     ! -- for oct --
     if(st_meshset%MinID(ID,iLv).lt.st_meshset%MaxID(ID,iLv)) then 
        !$omp parallel do default(none) &
        !$omp& private(index,p0,JD) &
        !$omp& shared(st_param,st_meshset,ID,iLv) &
        !$omp& reduction(+:addR,addG)
        do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
           if(ID==1.or.ID==2) then
              p0 => st_meshset%Mesh(index)
           else if(ID==3.or.ID==4) then
              p0 => st_meshset%GMesh(index)
           endif
           JD=p0%iFLG(2) ! previous iFLG(1)
           p0%iFLG(2)=0
           if(JD>=st_param%nfg) then 
              if((p0%iFLG(1)<=st_param%nfg-1).and.(p0%iFLG(1)>=0)) p0%iFLG(2)=-1
           else if(JD<=0) then
              if(p0%iFLG(1)>=1) then
                 p0%iFLG(2)=2
                 if(ID==1) then
                    st_meshset%rfnoct1(index)=8
                    addR=addR+8
                 else if(ID==2) then
                    st_meshset%rfnoct2(index)=8
                    addR=addR+8
                 else if(ID==3) then
                    st_meshset%rfnoct3(index)=8
                    addG=addG+8
                 else if(ID==4) then
                    st_meshset%rfnoct4(index)=8
                    addG=addG+8
                 endif
              endif
           else if(JD<=st_param%nfg-1) then 
              if(p0%iFLG(1)>=1) p0%iFLG(2)=1
           endif
        enddo
        !$omp end parallel do
     endif
  enddo
  ! -- resize Mesh and GMesh --
  RSize=size(st_meshset%Mesh)+addR
  GSize=size(st_meshset%GMesh)+addG
  call ppohAMRFDM_resize_Mesh(RSize,GSize,st_meshset,st_comm_info)
  return
end subroutine ppohAMRFDM_setflag

subroutine ppohAMRFDM_set_refine_criteria(index,iLv,rfnflg,st_param,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::index,iLv
  integer(kind=ppohAMRFDM_kint),intent(inout)::rfnflg
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  real(kind=ppohAMRFDM_kdbl)::Fp1,Fp2,Fp3
  type(st_ppohAMRFDM_octset),pointer::p0
  rfnflg=0
  p0 => st_meshset%Mesh(index)
  Fp1=dabs(p0%octNb2%F(1)-2.0d0*p0%F(1)+p0%octNb1%F(1)) &
       /(dabs(p0%octNb2%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb1%F(1)) &
       +0.001d0*(dabs(p0%octNb2%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb1%F(1))))
  Fp2=dabs(p0%octNb4%F(1)-2.0d0*p0%F(1)+p0%octNb3%F(1)) &
       /(dabs(p0%octNb4%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb3%F(1)) &
       +0.001d0*(dabs(p0%octNb4%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb3%F(1))))
  Fp3=dabs(p0%octNb6%F(1)-2.0d0*p0%F(1)+p0%octNb5%F(1)) &
       /(dabs(p0%octNb6%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb5%F(1)) &
       +0.001d0*(dabs(p0%octNb6%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb5%F(1))))
  if(Fp1>st_param%crtr_r(iLv).or.Fp2>st_param%crtr_r(iLv).or.Fp3>st_param%crtr_r(iLv)) rfnflg=1
  return
end subroutine ppohAMRFDM_set_refine_criteria

subroutine ppohAMRFDM_set_unrefine_criteria(index,iLv,rfnflg,st_param,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::index,iLv
  integer(kind=ppohAMRFDM_kint),intent(inout)::rfnflg
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  real(kind=ppohAMRFDM_kdbl)::Fp1,Fp2,Fp3
  type(st_ppohAMRFDM_octset),pointer::p0
  rfnflg=1
  ! -- for Ch1 --
  p0 => st_meshset%Mesh(index)%octCh1
  Fp1=dabs(p0%octNb2%F(1)-2.0d0*p0%F(1)+p0%octNb1%F(1)) &
       /(dabs(p0%octNb2%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb1%F(1)) &
       +0.001d0*(dabs(p0%octNb2%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb1%F(1))))
  Fp2=dabs(p0%octNb4%F(1)-2.0d0*p0%F(1)+p0%octNb3%F(1)) &
       /(dabs(p0%octNb4%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb3%F(1)) &
       +0.001d0*(dabs(p0%octNb4%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb3%F(1))))
  Fp3=dabs(p0%octNb6%F(1)-2.0d0*p0%F(1)+p0%octNb5%F(1)) &
       /(dabs(p0%octNb6%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb5%F(1)) &
       +0.001d0*(dabs(p0%octNb6%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb5%F(1))))
  if(Fp1>st_param%crtr_d(iLv).or.Fp2>st_param%crtr_d(iLv).or.Fp3>st_param%crtr_d(iLv)) then
     rfnflg=0
     return
  endif
  ! -- for Ch2 --
  p0 => st_meshset%Mesh(index)%octCh2
  Fp1=dabs(p0%octNb2%F(1)-2.0d0*p0%F(1)+p0%octNb1%F(1)) &
       /(dabs(p0%octNb2%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb1%F(1)) &
       +0.001d0*(dabs(p0%octNb2%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb1%F(1))))
  Fp2=dabs(p0%octNb4%F(1)-2.0d0*p0%F(1)+p0%octNb3%F(1)) &
       /(dabs(p0%octNb4%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb3%F(1)) &
       +0.001d0*(dabs(p0%octNb4%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb3%F(1))))
  Fp3=dabs(p0%octNb6%F(1)-2.0d0*p0%F(1)+p0%octNb5%F(1)) &
       /(dabs(p0%octNb6%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb5%F(1)) &
       +0.001d0*(dabs(p0%octNb6%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb5%F(1))))
  if(Fp1>st_param%crtr_d(iLv).and.Fp2>st_param%crtr_d(iLv).or.Fp3>st_param%crtr_d(iLv)) then
     rfnflg=0
     return
  endif
  ! -- for Ch3 --
  p0 => st_meshset%Mesh(index)%octCh3
  Fp1=dabs(p0%octNb2%F(1)-2.0d0*p0%F(1)+p0%octNb1%F(1)) &
       /(dabs(p0%octNb2%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb1%F(1)) &
       +0.001d0*(dabs(p0%octNb2%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb1%F(1))))
  Fp2=dabs(p0%octNb4%F(1)-2.0d0*p0%F(1)+p0%octNb3%F(1)) &
       /(dabs(p0%octNb4%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb3%F(1)) &
       +0.001d0*(dabs(p0%octNb4%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb3%F(1))))
  Fp3=dabs(p0%octNb6%F(1)-2.0d0*p0%F(1)+p0%octNb5%F(1)) &
       /(dabs(p0%octNb6%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb5%F(1)) &
       +0.001d0*(dabs(p0%octNb6%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb5%F(1))))
  if(Fp1>st_param%crtr_d(iLv).or.Fp2>st_param%crtr_d(iLv).or.Fp3>st_param%crtr_d(iLv)) then
     rfnflg=0
     return
  endif
  ! -- for Ch4 --
  p0 => st_meshset%Mesh(index)%octCh4
  Fp1=dabs(p0%octNb2%F(1)-2.0d0*p0%F(1)+p0%octNb1%F(1)) &
       /(dabs(p0%octNb2%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb1%F(1)) &
       +0.001d0*(dabs(p0%octNb2%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb1%F(1))))
  Fp2=dabs(p0%octNb4%F(1)-2.0d0*p0%F(1)+p0%octNb3%F(1)) &
       /(dabs(p0%octNb4%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb3%F(1)) &
       +0.001d0*(dabs(p0%octNb4%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb3%F(1))))
  Fp3=dabs(p0%octNb6%F(1)-2.0d0*p0%F(1)+p0%octNb5%F(1)) &
       /(dabs(p0%octNb6%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb5%F(1)) &
       +0.001d0*(dabs(p0%octNb6%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb5%F(1))))
  if(Fp1>st_param%crtr_d(iLv).and.Fp2>st_param%crtr_d(iLv).or.Fp3>st_param%crtr_d(iLv)) then
     rfnflg=0
     return
  endif
  ! -- for Ch5 --
  p0 => st_meshset%Mesh(index)%octCh5
  Fp1=dabs(p0%octNb2%F(1)-2.0d0*p0%F(1)+p0%octNb1%F(1)) &
       /(dabs(p0%octNb2%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb1%F(1)) &
       +0.001d0*(dabs(p0%octNb2%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb1%F(1))))
  Fp2=dabs(p0%octNb4%F(1)-2.0d0*p0%F(1)+p0%octNb3%F(1)) &
       /(dabs(p0%octNb4%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb3%F(1)) &
       +0.001d0*(dabs(p0%octNb4%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb3%F(1))))
  Fp3=dabs(p0%octNb6%F(1)-2.0d0*p0%F(1)+p0%octNb5%F(1)) &
       /(dabs(p0%octNb6%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb5%F(1)) &
       +0.001d0*(dabs(p0%octNb6%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb5%F(1))))
  if(Fp1>st_param%crtr_d(iLv).and.Fp2>st_param%crtr_d(iLv).or.Fp3>st_param%crtr_d(iLv)) then
     rfnflg=0
     return
  endif
  ! -- for Ch6 --
  p0 => st_meshset%Mesh(index)%octCh6
  Fp1=dabs(p0%octNb2%F(1)-2.0d0*p0%F(1)+p0%octNb1%F(1)) &
       /(dabs(p0%octNb2%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb1%F(1)) &
       +0.001d0*(dabs(p0%octNb2%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb1%F(1))))
  Fp2=dabs(p0%octNb4%F(1)-2.0d0*p0%F(1)+p0%octNb3%F(1)) &
       /(dabs(p0%octNb4%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb3%F(1)) &
       +0.001d0*(dabs(p0%octNb4%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb3%F(1))))
  Fp3=dabs(p0%octNb6%F(1)-2.0d0*p0%F(1)+p0%octNb5%F(1)) &
       /(dabs(p0%octNb6%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb5%F(1)) &
       +0.001d0*(dabs(p0%octNb6%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb5%F(1))))
  if(Fp1>st_param%crtr_d(iLv).and.Fp2>st_param%crtr_d(iLv).or.Fp3>st_param%crtr_d(iLv)) then
     rfnflg=0
     return
  endif
  ! -- for Ch7 --
  p0 => st_meshset%Mesh(index)%octCh7
  Fp1=dabs(p0%octNb2%F(1)-2.0d0*p0%F(1)+p0%octNb1%F(1)) &
       /(dabs(p0%octNb2%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb1%F(1)) &
       +0.001d0*(dabs(p0%octNb2%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb1%F(1))))
  Fp2=dabs(p0%octNb4%F(1)-2.0d0*p0%F(1)+p0%octNb3%F(1)) &
       /(dabs(p0%octNb4%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb3%F(1)) &
       +0.001d0*(dabs(p0%octNb4%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb3%F(1))))
  Fp3=dabs(p0%octNb6%F(1)-2.0d0*p0%F(1)+p0%octNb5%F(1)) &
       /(dabs(p0%octNb6%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb5%F(1)) &
       +0.001d0*(dabs(p0%octNb6%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb5%F(1))))
  if(Fp1>st_param%crtr_d(iLv).and.Fp2>st_param%crtr_d(iLv).or.Fp3>st_param%crtr_d(iLv)) then
     rfnflg=0
     return
  endif
  ! -- for Ch8 --
  p0 => st_meshset%Mesh(index)%octCh8
  Fp1=dabs(p0%octNb2%F(1)-2.0d0*p0%F(1)+p0%octNb1%F(1)) &
       /(dabs(p0%octNb2%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb1%F(1)) &
       +0.001d0*(dabs(p0%octNb2%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb1%F(1))))
  Fp2=dabs(p0%octNb4%F(1)-2.0d0*p0%F(1)+p0%octNb3%F(1)) &
       /(dabs(p0%octNb4%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb3%F(1)) &
       +0.001d0*(dabs(p0%octNb4%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb3%F(1))))
  Fp3=dabs(p0%octNb6%F(1)-2.0d0*p0%F(1)+p0%octNb5%F(1)) &
       /(dabs(p0%octNb6%F(1)-p0%F(1))+dabs(p0%F(1)-p0%octNb5%F(1)) &
       +0.001d0*(dabs(p0%octNb6%F(1))+2.0d0*dabs(p0%F(1))+dabs(p0%octNb5%F(1))))
  if(Fp1>st_param%crtr_d(iLv).and.Fp2>st_param%crtr_d(iLv).or.Fp3>st_param%crtr_d(iLv)) then
     rfnflg=0
     return
  endif
  return
end subroutine ppohAMRFDM_set_unrefine_criteria

subroutine ppohAMRFDM_flg_layout(ID,iLv,st_param,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::ID,iLv
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  integer(kind=ppohAMRFDM_kint)::index,x,y,z
  type(st_ppohAMRFDM_octset),pointer::p0,p1,p2,p3
  if(st_meshset%MinID(ID,iLv)<st_meshset%MaxID(ID,iLv)) then
     !$omp parallel do default(none) &
     !$omp& private(index,x,y,z,p0,p1,p2,p3) &
     !$omp& shared(st_param,st_meshset,ID,iLv)
     do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
        if(ID==1.or.ID==2) then
           p0 => st_meshset%Mesh(index)
        else if(ID==3.or.ID==4) then
           p0 => st_meshset%GMesh(index)
        endif
        if(p0%iFLG(3)/=0) then
           !1: -x -y -z
           p1 => p0
           do z=1,st_param%nfg
              p2 => p1
              do y=1,st_param%nfg
                 p3 => p2
                 do x=1,st_param%nfg
                    p3%iFLG(1)=st_param%nfg
                    p3 => p3%octNb1
                 enddo
                 p2 => p2%octNb3
              enddo
              p1 => p1%octNb5
           enddo
           !2: +x -y -z
           p1 => p0
           do z=1,st_param%nfg
              p2 => p1
              do y=1,st_param%nfg
                 p3 => p2
                 do x=1,st_param%nfg
                    p3%iFLG(1)=st_param%nfg
                    p3 => p3%octNb2
                 enddo
                 p2 => p2%octNb3
              enddo
              p1 => p1%octNb5
           enddo
           !3: -x +y -z
           p1 => p0
           do z=1,st_param%nfg
              p2 => p1
              do y=1,st_param%nfg
                 p3 => p2
                 do x=1,st_param%nfg
                    p3%iFLG(1)=st_param%nfg
                    p3 => p3%octNb1
                 enddo
                 p2 => p2%octNb4
              enddo
              p1 => p1%octNb5
           enddo
           !4: +x +y -z
           p1 => p0
           do z=1,st_param%nfg
              p2 => p1
              do y=1,st_param%nfg
                 p3 => p2
                 do x=1,st_param%nfg
                    p3%iFLG(1)=st_param%nfg
                    p3 => p3%octNb2
                 enddo
                 p2 => p2%octNb4
              enddo
              p1 => p1%octNb5
           enddo
           !5: -x -y +z
           p1 => p0
           do z=1,st_param%nfg
              p2 => p1
              do y=1,st_param%nfg
                 p3 => p2
                 do x=1,st_param%nfg
                    p3%iFLG(1)=st_param%nfg
                    p3 => p3%octNb1
                 enddo
                 p2 => p2%octNb3
              enddo
              p1 => p1%octNb6
           enddo
           !6: +x -y +z
           p1 => p0
           do z=1,st_param%nfg
              p2 => p1
              do y=1,st_param%nfg
                 p3 => p2
                 do x=1,st_param%nfg
                    p3%iFLG(1)=st_param%nfg
                    p3 => p3%octNb2
                 enddo
                 p2 => p2%octNb3
              enddo
              p1 => p1%octNb6
           enddo
           !7: -x +y +z
           p1 => p0
           do z=1,st_param%nfg
              p2 => p1
              do y=1,st_param%nfg
                 p3 => p2
                 do x=1,st_param%nfg
                    p3%iFLG(1)=st_param%nfg
                    p3 => p3%octNb1
                 enddo
                 p2 => p2%octNb4
              enddo
              p1 => p1%octNb6
           enddo
           !8: +x +y +z
           p1 => p0
           do z=1,st_param%nfg
              p2 => p1
              do y=1,st_param%nfg
                 p3 => p2
                 do x=1,st_param%nfg
                    p3%iFLG(1)=st_param%nfg
                    p3 => p3%octNb2
                 enddo
                 p2 => p2%octNb4
              enddo
              p1 => p1%octNb6
           enddo
        endif
     enddo
     !$omp end parallel do
  endif
  return
end subroutine ppohAMRFDM_flg_layout

subroutine ppohAMRFDM_set_buffer_out(iLv,iF1,iF2,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::iLv,iF1,iF2
  type(st_ppohAMRFDM_meshset)::st_meshset
  integer(kind=ppohAMRFDM_kint)::ID,index
  type(st_ppohAMRFDM_octset),pointer::p0,p1
  do ID=1,4
     if(st_meshset%MinID(ID,iLv).lt.st_meshset%MaxID(ID,iLv)) then
        !$omp parallel do default(none) &
        !$omp& private(index,p0,p1) &
        !$omp& shared(st_meshset,ID,iLv,iF1,iF2)
        do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
           if(ID==1.or.ID==2) then
              p0 => st_meshset%Mesh(index)
           else if(ID==3.or.ID==4) then
              p0 => st_meshset%GMesh(index)
           endif
           if(p0%iFLG(1) >= iF1) then
              if(p0%octNb1%iFLG(1)<iF2) then
                 p0%octNb1%iFLG(1)=iF2
              endif
              if(p0%octNb2%iFLG(1)<iF2) then
                 p0%octNb2%iFLG(1)=iF2
              endif
              if(p0%octNb3%iFLG(1)<iF2) then
                 p0%octNb3%iFLG(1)=iF2
              endif
              if(p0%octNb4%iFLG(1)<iF2) then
                 p0%octNb4%iFLG(1)=iF2
              endif
              if(p0%octNb1%octNb3%iFLG(1)<iF2) then
                 p0%octNb1%octNb3%iFLG(1)=iF2
              endif
              if(p0%octNb2%octNb4%iFLG(1)<iF2) then
                 p0%octNb2%octNb4%iFLG(1)=iF2
              endif
              if(p0%octNb3%octNb2%iFLG(1)<iF2) then
                 p0%octNb3%octNb2%iFLG(1)=iF2
              endif
              if(p0%octNb4%octNb1%iFLG(1)<iF2) then
                 p0%octNb4%octNb1%iFLG(1)=iF2
              endif
              !
              p1 => p0%octNb5
              !
              if(p1%iFLG(1)<iF2) then
                 p1%iFLG(1)=iF2
              endif
              if(p1%octNb1%iFLG(1)<iF2) then
                 p1%octNb1%iFLG(1)=iF2
              endif
              if(p1%octNb2%iFLG(1)<iF2) then
                 p1%octNb2%iFLG(1)=iF2
              endif
              if(p1%octNb3%iFLG(1)<iF2) then
                 p1%octNb3%iFLG(1)=iF2
              endif
              if(p1%octNb4%iFLG(1)<iF2) then
                 p1%octNb4%iFLG(1)=iF2
              endif
              if(p1%octNb1%octNb3%iFLG(1)<iF2) then
                 p1%octNb1%octNb3%iFLG(1)=iF2
              endif
              if(p1%octNb2%octNb4%iFLG(1)<iF2) then
                 p1%octNb2%octNb4%iFLG(1)=iF2
              endif
              if(p1%octNb3%octNb2%iFLG(1)<iF2) then
                 p1%octNb3%octNb2%iFLG(1)=iF2
              endif
              if(p1%octNb4%octNb1%iFLG(1)<iF2) then
                 p1%octNb4%octNb1%iFLG(1)=iF2
              endif
              !
              p1 => p0%octNb6
              !
              if(p1%iFLG(1)<iF2) then
                 p1%iFLG(1)=iF2
              endif
              if(p1%octNb1%iFLG(1)<iF2) then
                 p1%octNb1%iFLG(1)=iF2
              endif
              if(p1%octNb2%iFLG(1)<iF2) then
                 p1%octNb2%iFLG(1)=iF2
              endif
              if(p1%octNb3%iFLG(1)<iF2) then
                 p1%octNb3%iFLG(1)=iF2
              endif
              if(p1%octNb4%iFLG(1)<iF2) then
                 p1%octNb4%iFLG(1)=iF2
              endif
              if(p1%octNb1%octNb3%iFLG(1)<iF2) then
                 p1%octNb1%octNb3%iFLG(1)=iF2
              endif
              if(p1%octNb2%octNb4%iFLG(1)<iF2) then
                 p1%octNb2%octNb4%iFLG(1)=iF2
              endif
              if(p1%octNb3%octNb2%iFLG(1)<iF2) then
                 p1%octNb3%octNb2%iFLG(1)=iF2
              endif
              if(p1%octNb4%octNb1%iFLG(1)<iF2) then
                 p1%octNb4%octNb1%iFLG(1)=iF2
              endif
           endif
        enddo
        !$omp end parallel do
     endif
  enddo
  return
end subroutine ppohAMRFDM_set_buffer_out
