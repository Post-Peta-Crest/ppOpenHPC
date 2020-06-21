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

subroutine ppohAMRFDM_addRoct_init(iLv,st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::iLv
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::index,index2,ID,ich,n1,n2,ix,iy,iz
  integer(kind=ppohAMRFDM_kint)::load,octLv,Nint,MrtN,bndry
  integer(kind=ppohAMRFDM_kint)::iFLG(1:3),iPOS(1:3)
  integer(kind=ppohAMRFDM_kint),allocatable::add(:)
  real(kind=ppohAMRFDM_kdbl)::F(1:st_param%npy),C(1:st_param%npy)
  type(st_ppohAMRFDM_octset),pointer::p0,newp
  st_meshset%MinID(2,iLv+1)=max(st_meshset%MaxID(1,st_param%LvMax),st_meshset%MaxID(2,iLv))
  index2=st_meshset%MinID(2,iLv+1)
  load=2**iLv; F(:)=0.0d0; C(:)=0.0d0
  nullify(newp)
  do ID=1,2
     if(st_meshset%MinID(ID,iLv)<st_meshset%MaxID(ID,iLv)) then
        ! -- for OpenMP --
        allocate(add(st_meshset%MinID(ID,iLv):st_meshset%MaxID(ID,iLv)))
        add(st_meshset%MinID(ID,iLv))=0
        do index=st_meshset%MinID(ID,iLv)+1,st_meshset%MaxID(ID,iLv)
           if(ID==1) then
              add(index)=add(index-1)+st_meshset%rfnoct1(index-1)
           else
              add(index)=add(index-1)+st_meshset%rfnoct2(index-1)
           endif
        enddo
        add(:)=add(:)+index2
        !$omp parallel do default(none) &
        !$omp& private(index,p0,iFLG,octLv,Nint,ich,MrtN,bndry,n1,n2,ix,iy,iz,iPOS) &
        !$omp& lastprivate(index2) &
        !$omp& shared(st_param,st_meshset,st_comm_info,ID,iLv,add,F,C,load,newp)
        do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
           p0 => st_meshset%Mesh(index)
           index2=add(index)
           if(p0%iFLG(2)==2) then
              iFLG(:)=0
              iFLG(1)=p0%iFLG(1)-st_param%nfg
              octLv=p0%octLv+1
              Nint=2**(st_param%LvMax-octLv)
              ! --- for each directed child octs ---
              do ich=1,8
                 index2=index2+1
                 if(index2>size(st_meshset%Mesh)) then
                    print *,"Mesh overflow, size(Mesh)=",size(st_meshset%Mesh),st_comm_info%rank
                    call ppohAMRFDM_abort(st_comm_info)
                 endif
                 MrtN=p0%MrtN; bndry=p0%bndry
                 ! -- Extract position index for child-octs --
                 n1=ich; iz=int((n1-1)/4)+1; n2=n1-4*(iz-1)
                 iy=int((n2-1)/2)+1; ix=n2-2*(iy-1)
                 ! -- Position index --
                 ix=p0%iPOS(1)+(2*ix-3)*Nint
                 iy=p0%iPOS(2)+(2*iy-3)*Nint
                 iz=p0%iPOS(3)+(2*iz-3)*Nint
                 iPOS(1)=ix; iPOS(2)=iy; iPOS(3)=iz
                 ! -- Input informaions into a target variable Mesh(:) --
                 st_meshset%Mesh(index2)=&
                      st_ppohAMRFDM_octset(index2,octLv,ich,iFLG,&
                      iPOS,MrtN,bndry,F,C,load, &
                      p0, &
                      newp,newp,newp, &
                      newp,newp,newp, &
                      newp,newp,newp, &
                      newp,newp,newp, &
                      newp,newp,p0)
                 ! -- Connect eight child-octs of parent oct --
                 if(ich==1) then
                    p0%octCh1 => st_meshset%Mesh(index2)
                 else if(ich==2) then
                    p0%octCh2 => st_meshset%Mesh(index2)
                 else if(ich==3) then
                    p0%octCh3 => st_meshset%Mesh(index2)
                 else if(ich==4) then
                    p0%octCh4 => st_meshset%Mesh(index2)
                 else if(ich==5) then
                    p0%octCh5 => st_meshset%Mesh(index2)
                 else if(ich==6) then
                    p0%octCh6 => st_meshset%Mesh(index2)
                 else if(ich==7) then
                    p0%octCh7 => st_meshset%Mesh(index2)
                 else if(ich==8) then 
                    p0%octCh8 => st_meshset%Mesh(index2)
                 endif
              enddo
           endif
        enddo
        !$omp end parallel do
        deallocate(add)
     endif
     ! -- add MeshID for new octs --
     if(index2==st_meshset%MinID(2,iLv+1)) then 
        st_meshset%MinID(2,iLv+1)=st_meshset%MaxID(2,iLv)
        st_meshset%MaxID(2,iLv+1)=st_meshset%MaxID(2,iLv)
     else
        st_meshset%MinID(2,iLv+1)=st_meshset%MaxID(2,iLv)+1
        st_meshset%MaxID(2,iLv+1)=index2
     endif
  enddo
  ! -- interpolate physical variables for new octs --
  do ID=1,2
     call ppohAMRFDM_copy(iLv,2,2,ID,st_param,st_meshset)
  enddo
  ! -- reset iFLG(2) --
  do ID=1,2
     if(st_meshset%MinID(ID,iLv).lt.st_meshset%MaxID(ID,iLv)) then
        !$omp parallel do default(none) &
        !$omp& private(index,p0) &
        !$omp& shared(st_param,st_meshset,ID,iLv)
        do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
           p0 => st_meshset%Mesh(index)
           if((p0%iFLG(1).ge.1).and.(p0%iFLG(1).le.st_param%nfg-1)) then 
              p0%iFLG(2)=5
           else
              p0%iFLG(2)=0
           endif
        enddo
        !$omp end parallel do
     endif
  enddo
  deallocate(st_meshset%rfnoct1); deallocate(st_meshset%rfnoct2)
  return
end subroutine ppohAMRFDM_addRoct_init
