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

subroutine ppohAMRFDM_allocate_Buf(ch,iLv,st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  character(kind=1,len=1),intent(in)::ch
  integer(kind=ppohAMRFDM_kint),intent(in)::iLv
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::ID,sdest,rdest
  ! -- count RMesh and GMesh corresponding to communications --
  st_comm_info%snum(:,:)=0; st_comm_info%rnum(:,:)=0
  call ppohAMRFDM_count_sBuf(iLv,st_meshset,st_comm_info)
  call ppohAMRFDM_count_rBuf(iLv,st_meshset,st_comm_info)
  ! -- allocate buffer arr. --
  if(ch=="I") then
     do ID=1,st_comm_info%nprocs-1
        sdest=mod(st_comm_info%rank+ID,st_comm_info%nprocs)
        rdest=mod(st_comm_info%rank-ID+st_comm_info%nprocs,st_comm_info%nprocs)
        ! - allocation check -
        if(allocated(st_meshset%sBuf(sdest+1)%bufi)) then
           deallocate(st_meshset%sBuf(sdest+1)%bufi)
        endif
        if(allocated(st_meshset%rBuf(rdest+1)%bufi)) then
           deallocate(st_meshset%rBuf(rdest+1)%bufi)
        endif
        ! - allocation -
        allocate(st_meshset%sBuf(sdest+1)%bufi(st_comm_info%snum(iLv,sdest+1)*2))
        allocate(st_meshset%rBuf(rdest+1)%bufi(st_comm_info%rnum(iLv,rdest+1)*2))
     enddo
  else if(ch=="R") then
     do ID=1,st_comm_info%nprocs-1
        sdest=mod(st_comm_info%rank+ID,st_comm_info%nprocs)
        rdest=mod(st_comm_info%rank-ID+st_comm_info%nprocs,st_comm_info%nprocs)
        ! - allocation check -
        if(allocated(st_meshset%sBuf(sdest+1)%bufr)) then
           deallocate(st_meshset%sBuf(sdest+1)%bufr)
        endif
        if(allocated(st_meshset%rBuf(rdest+1)%bufr)) then
           deallocate(st_meshset%rBuf(rdest+1)%bufr)
        endif
        ! - allocation -
        allocate(st_meshset%sBuf(sdest+1)%bufr(st_comm_info%snum(iLv,sdest+1)*st_param%npy))
        allocate(st_meshset%rBuf(rdest+1)%bufr(st_comm_info%rnum(iLv,rdest+1)*st_param%npy))
     enddo
  endif
  return
end subroutine ppohAMRFDM_allocate_Buf

subroutine ppohAMRFDM_count_sBuf(sLv,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::sLv
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::np,sdest,ID,sID,eID,index,iLv,count
  do np=1,st_comm_info%nprocs-1
     sdest=mod(st_comm_info%rank+np,st_comm_info%nprocs)
     if(st_comm_info%nrproc(sdest+1)==0) cycle
     if(sdest==0) then
        sID=1
     else
        sID=sum(st_comm_info%nrproc(1:sdest))+1
     endif
     eID=sum(st_comm_info%nrproc(1:sdest+1))
     ! - survey each iLv oct -
     if(sLv==0) then
        count=eID-sID+1
     else if(sLv>0) then
        count=0; iLv=0
        do ID=sID,eID
           index=st_comm_info%iMesh(ID)
           call ppohAMRFDM_count_sBuf_child(index,sLv,count,iLv+1,st_meshset)
        enddo
     endif
     st_comm_info%snum(sLv,sdest+1)=count
  enddo
  return
end subroutine ppohAMRFDM_count_sBuf

recursive subroutine ppohAMRFDM_count_sBuf_child(index,sLv,count,iLv,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::index,sLv,iLv
  integer(kind=ppohAMRFDM_kint),intent(inout)::count
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_octset),pointer::p0
  p0 => st_meshset%Mesh(index)
  if(p0%iFLG(1)>0) then
     if(iLv==sLv) then
        count=count+8
     else
        call ppohAMRFDM_count_sBuf_child(p0%octCh1%octN,sLv,count,iLv+1,st_meshset)
        call ppohAMRFDM_count_sBuf_child(p0%octCh2%octN,sLv,count,iLv+1,st_meshset)
        call ppohAMRFDM_count_sBuf_child(p0%octCh3%octN,sLv,count,iLv+1,st_meshset)
        call ppohAMRFDM_count_sBuf_child(p0%octCh4%octN,sLv,count,iLv+1,st_meshset)
        call ppohAMRFDM_count_sBuf_child(p0%octCh5%octN,sLv,count,iLv+1,st_meshset)
        call ppohAMRFDM_count_sBuf_child(p0%octCh6%octN,sLv,count,iLv+1,st_meshset)
        call ppohAMRFDM_count_sBuf_child(p0%octCh7%octN,sLv,count,iLv+1,st_meshset)
        call ppohAMRFDM_count_sBuf_child(p0%octCh8%octN,sLv,count,iLv+1,st_meshset)
     endif
  endif
  return
end subroutine ppohAMRFDM_count_sBuf_child

subroutine ppohAMRFDM_count_rBuf(rLv,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::rLv
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::index,rdest,sID,eID,count,np,iLv
  do np=1,st_comm_info%nprocs-1
     rdest=mod(st_comm_info%rank-np+st_comm_info%nprocs,st_comm_info%nprocs)
     if(st_comm_info%ngproc(rdest+1)==0) cycle
     if(rdest==0) then
        sID=1
     else
        sID=sum(st_comm_info%ngproc(1:rdest))+1
     endif
     eID=sum(st_comm_info%ngproc(1:rdest+1))
     ! - survey each iLv oct -
     if(rLv==0) then
        count=eID-sID+1
     else if(rLv>0) then
        count=0; iLv=0
        do index=sID,eID
           call ppohAMRFDM_count_rBuf_child(index,rLv,count,iLv+1,st_meshset)
        enddo
     endif
     st_comm_info%rnum(rLv,rdest+1)=count
  enddo
  return
end subroutine ppohAMRFDM_count_rBuf

recursive subroutine ppohAMRFDM_count_rBuf_child(index,rLv,count,iLv,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::index,rLv,iLv
  integer(kind=ppohAMRFDM_kint),intent(inout)::count
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_octset),pointer::p0
  p0 => st_meshset%GMesh(index)
  if(p0%iFLG(1)>0) then
     if(iLv==rLv) then
        count=count+8
     else
        call ppohAMRFDM_count_rBuf_child(p0%octCh1%octN,rLv,count,iLv+1,st_meshset)
        call ppohAMRFDM_count_rBuf_child(p0%octCh2%octN,rLv,count,iLv+1,st_meshset)
        call ppohAMRFDM_count_rBuf_child(p0%octCh3%octN,rLv,count,iLv+1,st_meshset)
        call ppohAMRFDM_count_rBuf_child(p0%octCh4%octN,rLv,count,iLv+1,st_meshset)
        call ppohAMRFDM_count_rBuf_child(p0%octCh5%octN,rLv,count,iLv+1,st_meshset)
        call ppohAMRFDM_count_rBuf_child(p0%octCh6%octN,rLv,count,iLv+1,st_meshset)
        call ppohAMRFDM_count_rBuf_child(p0%octCh7%octN,rLv,count,iLv+1,st_meshset)
        call ppohAMRFDM_count_rBuf_child(p0%octCh8%octN,rLv,count,iLv+1,st_meshset)
     endif
  endif
  return
end subroutine ppohAMRFDM_count_rBuf_child
