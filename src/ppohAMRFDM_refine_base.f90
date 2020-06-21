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

subroutine ppohAMRFDM_refine_base(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::iLv,ID
  if(st_param%LvMax<1) return
  do iLv=0,st_param%initLv-1
     call ppohAMRFDM_setflag_base(iLv,st_param,st_meshset,st_comm_info)     
     call ppohAMRFDM_addRoct(iLv,st_param,st_meshset,st_comm_info)
     call ppohAMRFDM_addGoct(iLv,st_param,st_meshset,st_comm_info)
     call ppohAMRFDM_connect_oct(iLv,st_param,st_meshset)
  enddo
  if(st_param%initLv==st_param%LvMax) then
     call ppohAMRFDM_setflag_base(st_param%initLv,st_param,st_meshset,st_comm_info)
  endif
  do iLv=0,st_param%initLv-1
     do ID=st_param%nfg,st_param%nfg+st_param%nlv-1
        call ppohAMRFDM_set_buffer_ave(iLv,ID,ID+1,st_meshset)
     enddo
     call ppohAMRFDM_passing_iFLG(iLv,st_param,st_meshset,st_comm_info)
  enddo
  call ppohAMRFDM_sortoct(st_param,st_meshset,st_comm_info)
  call ppohAMRFDM_get_ID_order(st_param,st_meshset)
  return
end subroutine ppohAMRFDM_refine_base

subroutine ppohAMRFDM_setflag_base(iLv,st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::iLv
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  type(st_ppohAMRFDM_octset),pointer::p0
  integer(kind=ppohAMRFDM_kint)::index,ID,JD,addR,addG,RSize,GSize
  if(iLv==0) then
     ! --- set iFLG(3) ---
     !$omp parallel do default(none) &
     !$omp& private(index,p0) &
     !$omp& shared(st_param,st_meshset,iLv)
     do index=st_meshset%MinID(1,iLv),st_meshset%MaxID(1,iLv)
        p0 => st_meshset%Mesh(index)
        if(p0%iFLG(3)==1) then
           if(p0%octCh1%iFLG(1)>=1) cycle; if(p0%octCh2%iFLG(1)>=1) cycle
           if(p0%octCh3%iFLG(1)>=1) cycle; if(p0%octCh4%iFLG(1)>=1) cycle
           if(p0%octCh5%iFLG(1)>=1) cycle; if(p0%octCh6%iFLG(1)>=1) cycle
           if(p0%octCh7%iFLG(1)>=1) cycle; if(p0%octCh8%iFLG(1)>=1) cycle
        endif
        p0%iFLG(3)=1
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
           !$omp& private(index,p0) &
           !$omp& shared(st_meshset,st_param,ID,iLv)
           do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
              p0 => st_meshset%Mesh(index)
              if(p0%octPrt%iFLG(3)==1) then
                 if(p0%iFLG(3)==1) then
                    if(p0%octCh1%iFLG(1)>=1) cycle; if(p0%octCh2%iFLG(1)>=1) cycle
                    if(p0%octCh3%iFLG(1)>=1) cycle; if(p0%octCh4%iFLG(1)>=1) cycle
                    if(p0%octCh5%iFLG(1)>=1) cycle; if(p0%octCh6%iFLG(1)>=1) cycle
                    if(p0%octCh7%iFLG(1)>=1) cycle; if(p0%octCh8%iFLG(1)>=1) cycle
                 endif
                 p0%iFLG(3)=1
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
end subroutine ppohAMRFDM_setflag_base
