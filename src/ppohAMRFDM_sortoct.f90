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

subroutine ppohAMRFDM_sortoct(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::iLv,ID,JD,JD0,JDG,JDG0
  integer(kind=ppohAMRFDM_kint)::index,nprt
  integer(kind=ppohAMRFDM_kint):: MinIF(0:st_param%Lvmax), MaxIF(0:st_param%Lvmax)
  integer(kind=ppohAMRFDM_kint)::GMinIF(0:st_param%Lvmax),GMaxIF(0:st_param%Lvmax)
  type(st_ppohAMRFDM_octset),pointer::p0,p1
  ! -- allocate Mesh2 arr --
  allocate(st_meshset% Mesh2(1:size(st_meshset% Mesh)))
  allocate(st_meshset%GMesh2(1:size(st_meshset%GMesh)))
  ! -- substitute Mesh arr --
  JD=0; JDG=0
  do iLv=0,st_param%LvMax 
     JD0=JD; JDG0=JDG
     do ID=1,2
        if(st_meshset%MinID(ID,iLv)<st_meshset%MaxID(ID,iLv)) then
           do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
              p0 => st_meshset%Mesh(index)
              if(p0%iFLG(1)>=-(st_param%nfg-1)) then
                 JD=JD+1
                 p0%Psort => st_meshset%Mesh(JD)
                 st_meshset%Mesh2(JD)=st_meshset%Mesh(index)
              endif
           enddo
        endif
        if(st_meshset%MinID(ID+2,iLv)<st_meshset%MaxID(ID+2,iLv)) then
           do index=st_meshset%MinID(ID+2,iLv),st_meshset%MaxID(ID+2,iLv)
              p0 => st_meshset%GMesh(index)
              if(p0%iFLG(1)>=-(st_param%nfg-1)) then
                 JDG=JDG+1 
                 p0%Psort => st_meshset%GMesh(JDG)
                 st_meshset%GMesh2(JDG)=st_meshset%GMesh(index)
              endif
           enddo
        endif
     enddo
     ! -- sorting for RMesh --
     if(JD.eq.JD0) then
        MinIF(iLv)=JD0; MaxIF(iLv)=JD0
     else
        !$omp parallel do default(none) &
        !$omp& private(index,p0,p1,nprt) &
        !$omp& shared(st_param,st_meshset,JD0,JD,iLv)
        do index=JD0+1,JD
           p0 => st_meshset%Mesh2(index)
           p0%octN=index
           p0%octNb1 => p0%octNb1%Psort
           p0%octNb2 => p0%octNb2%Psort
           p0%octNb3 => p0%octNb3%Psort
           p0%octNb4 => p0%octNb4%Psort
           p0%octNb5 => p0%octNb5%Psort
           p0%octNb6 => p0%octNb6%Psort
           if(iLv>0) then
              p1 => p0%octPrt
              nprt=p1%octN
              p1 => st_meshset%Mesh2(nprt)
              if(p0%Csort<=4) then 
                 if(p0%Csort==1) then 
                    p1%octCh1 => p0%Psort
                 else if(p0%Csort==2) then 
                    p1%octCh2 => p0%Psort
                 else if(p0%Csort==3) then 
                    p1%octCh3 => p0%Psort
                 else 
                    p1%octCh4 => p0%Psort
                 endif
              else
                 if(p0%Csort==5) then 
                    p1%octCh5 => p0%Psort
                 else if(p0%Csort==6) then 
                    p1%octCh6 => p0%Psort
                 else if(p0%Csort==7) then 
                    p1%octCh7 => p0%Psort
                 else 
                    p1%octCh8 => p0%Psort
                 end if
              endif
           endif
           if(iLv<st_param%LvMax) then
              if(p0%iFLG(1)>0) then 
                 p0%octCh1%octPrt => p0%Psort; p0%octCh2%octPrt => p0%Psort
                 p0%octCh3%octPrt => p0%Psort; p0%octCh4%octPrt => p0%Psort
                 p0%octCh5%octPrt => p0%Psort; p0%octCh6%octPrt => p0%Psort
                 p0%octCh7%octPrt => p0%Psort; p0%octCh8%octPrt => p0%Psort
              endif
           endif
        enddo
        !$omp end parallel do
        MinIF(iLv)=JD0+1; MaxIF(iLv)=JD
     endif
     ! -- sorting for GMesh --
     if(JDG.eq.JDG0) then
        GMinIF(iLv)=JDG0; GMaxIF(iLv)=JDG0
     else
        !$omp parallel do default(none) &
        !$omp& private(index,p0,p1,nprt) &
        !$omp& shared(st_param,st_meshset,JDG0,JDG,iLv)
        do index=JDG0+1,JDG
           p0 => st_meshset%GMesh2(index)
           p0%octN=index
           if(associated(p0%octNb1)) p0%octNb1 => p0%octNb1%Psort
           if(associated(p0%octNb2)) p0%octNb2 => p0%octNb2%Psort
           if(associated(p0%octNb3)) p0%octNb3 => p0%octNb3%Psort
           if(associated(p0%octNb4)) p0%octNb4 => p0%octNb4%Psort
           if(associated(p0%octNb5)) p0%octNb5 => p0%octNb5%Psort
           if(associated(p0%octNb6)) p0%octNb6 => p0%octNb6%Psort
           if(iLv>0) then
              p1 => p0%octPrt
              nprt=p1%octN
              p1 => st_meshset%GMesh2(nprt)
              if(p0%Csort<=4) then 
                 if(p0%Csort==1) then 
                    p1%octCh1 => p0%Psort
                 else if(p0%Csort==2) then 
                    p1%octCh2 => p0%Psort
                 else if(p0%Csort==3) then 
                    p1%octCh3 => p0%Psort
                 else 
                    p1%octCh4 => p0%Psort
                 endif
              else
                 if(p0%Csort==5) then 
                    p1%octCh5 => p0%Psort
                 else if(p0%Csort==6) then 
                    p1%octCh6 => p0%Psort
                 else if(p0%Csort==7) then 
                    p1%octCh7 => p0%Psort
                 else 
                    p1%octCh8 => p0%Psort
                 endif
              endif
           endif
           if(iLv<st_param%LvMax) then
              if(p0%iFLG(1)>0 ) then 
                 p0%octCh1%octPrt => p0%Psort; p0%octCh2%octPrt => p0%Psort
                 p0%octCh3%octPrt => p0%Psort; p0%octCh4%octPrt => p0%Psort
                 p0%octCh5%octPrt => p0%Psort; p0%octCh6%octPrt => p0%Psort
                 p0%octCh7%octPrt => p0%Psort; p0%octCh8%octPrt => p0%Psort
              endif
           endif
        enddo
        !$omp end parallel do
        GMinIF(iLv)=JDG0+1; GMaxIF(iLv)=JDG
     endif
  enddo
  ! -- overwrite Mesh arr --
  !$omp parallel default(none) &
  !$omp& private(index) shared(st_meshset,JD,JDG)
  !$omp do
  do index=1,JD
     st_meshset%Mesh(index)=st_meshset%Mesh2(index)
  enddo
  !$omp end do
  !$omp do
  do index=1,JDG
     st_meshset%GMesh(index)=st_meshset%GMesh2(index)
  enddo
  !$omp end do
  !$omp end parallel
  ! -- reset MeshID --
  do iLv=0,st_param%LvMax
     st_meshset%MinID(1,iLv)=MinIF(iLv)
     st_meshset%MaxID(1,iLv)=MaxIF(iLv)
  enddo
  do iLv=0,st_param%LvMax
     st_meshset%MinID(3,iLv)=GMinIF(iLv)
     st_meshset%MaxID(3,iLv)=GMaxIF(iLv)
  enddo
  do iLv=0,st_param%LvMax
     st_meshset%MinID(2,iLv)=MaxIF(st_param%LvMax)
     st_meshset%MaxID(2,iLv)=MaxIF(st_param%LvMax)
  enddo
  do iLv=0,st_param%LvMax
     st_meshset%MinID(4,iLv)=GMaxIF(st_param%LvMax)
     st_meshset%MaxID(4,iLv)=GMaxIF(st_param%LvMax)
  enddo
  ! -- deallocate Mesh2 arr --
  deallocate(st_meshset% Mesh2)
  deallocate(st_meshset%GMesh2)
  ! -- resize Mesh --
  call ppohAMRFDM_resize_Mesh(JD,JDG,st_meshset,st_comm_info)
  return
end subroutine ppohAMRFDM_sortoct
