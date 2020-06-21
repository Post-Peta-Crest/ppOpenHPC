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

subroutine ppohAMRFDM_passing_fields(iLv,st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::iLv
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::ID,sdest,rdest,ir,is,im
  integer(kind=ppohAMRFDM_kint)::ireq(1:st_comm_info%nprocs*2),istat(mpi_status_size,1:st_comm_info%nprocs*2),ierr
  ! -- count and allocate RMesh and GMesh corresponding to communications --
  call ppohAMRFDM_allocate_Buf("R",iLv,st_param,st_meshset,st_comm_info)
  ! -- data packing --
  call ppohAMRFDM_pack_flds(iLv,st_param,st_meshset,st_comm_info)
  ! -- send the fields --
  ir=0; is=0; im=0
  do ID=1,st_comm_info%nprocs-1
     sdest=mod(st_comm_info%rank+ID,st_comm_info%nprocs)
     rdest=mod(st_comm_info%rank-ID+st_comm_info%nprocs,st_comm_info%nprocs)
     ir=ir+1; is=is+1
     if(st_comm_info%ngproc(rdest+1)==0.and.st_comm_info%nrproc(sdest+1)==0) cycle
     if(st_comm_info%rnum(iLv,rdest+1)>0) then 
        im=im+1
        call mpi_irecv(st_meshset%rBuf(rdest+1)%bufr,&
             st_comm_info%rnum(iLv,rdest+1)*st_param%npy,&
             mpi_double_precision,rdest,ir,st_comm_info%comm,ireq(im),ierr)
     endif
     if(st_comm_info%snum(iLv,sdest+1)>0) then 
        im=im+1
        call mpi_isend(st_meshset%sBuf(sdest+1)%bufr,&
             st_comm_info%snum(iLv,sdest+1)*st_param%npy,&
             mpi_double_precision,sdest,is,st_comm_info%comm,ireq(im),ierr)
     endif
  enddo
  call mpi_waitall(im,ireq(1:im),istat(:,1:im),ierr)
  ! -- data unpacking --
  call ppohAMRFDM_unpack_flds(iLv,st_param,st_meshset,st_comm_info)
  return
end subroutine ppohAMRFDM_passing_fields

subroutine ppohAMRFDM_pack_flds(sLv,st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::sLv
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::np,index,sdest,ID,sID,eID,count,iLv
  type(st_ppohAMRFDM_octset),pointer::p0
  do np=1,st_comm_info%nprocs-1
     sdest=mod(st_comm_info%rank+np,st_comm_info%nprocs)
     if(st_comm_info%nrproc(sdest+1)==0) cycle
     if(sdest==0) then
        sID=1
     else
        sID=sum(st_comm_info%nrproc(1:sdest))+1
     endif
     eID=sum(st_comm_info%nrproc(1:sdest+1))
     iLv=0; count=1
     do ID=sID,eID
        index=st_comm_info%iMesh(ID)
        if(iLv==sLv) then
           ! --- for Lv.0 ---
           p0 => st_meshset%Mesh(index)
           st_meshset%sBuf(sdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy) &
                =p0%F(1:st_param%npy)
           count=count+1
        else
           call ppohAMRFDM_pack_flds_child(index,sLv,count,iLv+1,sdest,st_param,st_meshset)
        endif
     enddo
  enddo
  return
end subroutine ppohAMRFDM_pack_flds

recursive subroutine ppohAMRFDM_pack_flds_child(index,sLv,count,iLv,sdest,st_param,st_meshset)
  use m_ppohAMRFDM_util
  integer(kind=ppohAMRFDM_kint),intent(in)::index,sLv,iLv,sdest
  integer(kind=ppohAMRFDM_kint),intent(inout)::count
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_octset),pointer::p0
  p0 => st_meshset%Mesh(index)
  if(p0%iFLG(1)>0) then
     if(iLv==sLv) then
        ! -- Ch1 --
        st_meshset%sBuf(sdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy) &
             =p0%octCh1%F(1:st_param%npy)
        count=count+1
        ! -- Ch2 --
        st_meshset%sBuf(sdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy) &
             =p0%octCh2%F(1:st_param%npy)
        count=count+1
        ! -- Ch3 --
        st_meshset%sBuf(sdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy) &
             =p0%octCh3%F(1:st_param%npy)
        count=count+1
        ! -- Ch4 --
        st_meshset%sBuf(sdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy) &
             =p0%octCh4%F(1:st_param%npy)
        count=count+1
        ! -- Ch5 --
        st_meshset%sBuf(sdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy) &
             =p0%octCh5%F(1:st_param%npy)
        count=count+1
        ! -- Ch6 --
        st_meshset%sBuf(sdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy) &
             =p0%octCh6%F(1:st_param%npy)
        count=count+1
        ! -- Ch7 --
        st_meshset%sBuf(sdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy) &
             =p0%octCh7%F(1:st_param%npy)
        count=count+1
        ! -- Ch8 --
        st_meshset%sBuf(sdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy) &
             =p0%octCh8%F(1:st_param%npy)
        count=count+1
     else
        call ppohAMRFDM_pack_flds_child(p0%octCh1%octN,sLv,count,iLv+1,sdest,st_param,st_meshset)
        call ppohAMRFDM_pack_flds_child(p0%octCh2%octN,sLv,count,iLv+1,sdest,st_param,st_meshset)
        call ppohAMRFDM_pack_flds_child(p0%octCh3%octN,sLv,count,iLv+1,sdest,st_param,st_meshset)
        call ppohAMRFDM_pack_flds_child(p0%octCh4%octN,sLv,count,iLv+1,sdest,st_param,st_meshset)
        call ppohAMRFDM_pack_flds_child(p0%octCh5%octN,sLv,count,iLv+1,sdest,st_param,st_meshset)
        call ppohAMRFDM_pack_flds_child(p0%octCh6%octN,sLv,count,iLv+1,sdest,st_param,st_meshset)
        call ppohAMRFDM_pack_flds_child(p0%octCh7%octN,sLv,count,iLv+1,sdest,st_param,st_meshset)
        call ppohAMRFDM_pack_flds_child(p0%octCh8%octN,sLv,count,iLv+1,sdest,st_param,st_meshset)
     endif
  endif
  return
end subroutine ppohAMRFDM_pack_flds_child

subroutine ppohAMRFDM_unpack_flds(rLv,st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::rLv
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  type(st_ppohAMRFDM_octset),pointer::p0
  integer(kind=ppohAMRFDM_kint)::np,index,iLv,count,sID,eID,rdest
  do np=1,st_comm_info%nprocs-1
     rdest=mod(st_comm_info%rank-np+st_comm_info%nprocs,st_comm_info%nprocs)
     if(st_comm_info%ngproc(rdest+1)==0) cycle
     if(rdest==0)then
        sID=1
     else
        sID=sum(st_comm_info%ngproc(1:rdest))+1
     endif
     eID=sum(st_comm_info%ngproc(1:rdest+1))
     iLv=0; count=1
     do index=sID,eID
        if(iLv==rLv) then
           ! --- for Lv.0 ---
           p0 => st_meshset%GMesh(index)
           p0%F(1:st_param%npy)&
                =st_meshset%rBuf(rdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy)
           count=count+1
        else
           call ppohAMRFDM_unpack_flds_child(index,rLv,count,iLv+1,rdest,st_param,st_meshset)
        endif
     enddo
  enddo
  return
end subroutine ppohAMRFDM_unpack_flds

recursive subroutine ppohAMRFDM_unpack_flds_child(index,rLv,count,iLv,rdest,st_param,st_meshset)
  use m_ppohAMRFDM_util
  integer(kind=ppohAMRFDM_kint),intent(in)::index,rLv,iLv,rdest
  integer(kind=ppohAMRFDM_kint),intent(inout)::count
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_octset),pointer::p0
  p0 => st_meshset%GMesh(index)
  if(p0%iFLG(1)>0) then
     if(iLv==rLv) then
        ! -- Ch1 --
        p0%octCh1%F(1:st_param%npy)=st_meshset%rBuf(rdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy)
        count=count+1
        ! -- Ch2 --
        p0%octCh2%F(1:st_param%npy)=st_meshset%rBuf(rdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy)
        count=count+1
        ! -- Ch3 --
        p0%octCh3%F(1:st_param%npy)=st_meshset%rBuf(rdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy)
        count=count+1
        ! -- Ch4 --
        p0%octCh4%F(1:st_param%npy)=st_meshset%rBuf(rdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy)
        count=count+1
        ! -- Ch5 --
        p0%octCh5%F(1:st_param%npy)=st_meshset%rBuf(rdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy)
        count=count+1
        ! -- Ch6 --
        p0%octCh6%F(1:st_param%npy)=st_meshset%rBuf(rdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy)
        count=count+1
        ! -- Ch7 --
        p0%octCh7%F(1:st_param%npy)=st_meshset%rBuf(rdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy)
        count=count+1
        ! -- Ch8 --
        p0%octCh8%F(1:st_param%npy)=st_meshset%rBuf(rdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy)
        count=count+1
     else
        call ppohAMRFDM_unpack_flds_child(p0%octCh1%octN,rLv,count,iLv+1,rdest,st_param,st_meshset)
        call ppohAMRFDM_unpack_flds_child(p0%octCh2%octN,rLv,count,iLv+1,rdest,st_param,st_meshset)
        call ppohAMRFDM_unpack_flds_child(p0%octCh3%octN,rLv,count,iLv+1,rdest,st_param,st_meshset)
        call ppohAMRFDM_unpack_flds_child(p0%octCh4%octN,rLv,count,iLv+1,rdest,st_param,st_meshset)
        call ppohAMRFDM_unpack_flds_child(p0%octCh5%octN,rLv,count,iLv+1,rdest,st_param,st_meshset)
        call ppohAMRFDM_unpack_flds_child(p0%octCh6%octN,rLv,count,iLv+1,rdest,st_param,st_meshset)
        call ppohAMRFDM_unpack_flds_child(p0%octCh7%octN,rLv,count,iLv+1,rdest,st_param,st_meshset)
        call ppohAMRFDM_unpack_flds_child(p0%octCh8%octN,rLv,count,iLv+1,rdest,st_param,st_meshset)
     endif
  endif
  return
end subroutine ppohAMRFDM_unpack_flds_child
