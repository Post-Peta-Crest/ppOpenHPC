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

subroutine ppohAMRFDM_make_GMesh(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::MeshSize,GMeshSize,comrange,ngst,GMn2Min,GMn2Max
  integer(kind=ppohAMRFDM_kint),allocatable::nrflg(:),nrsum(:)
  integer(kind=ppohAMRFDM_kint),allocatable::GFLG(:),GCPU(:),GIDX(:),GCHK(:)
  integer(kind=ppohAMRFDM_kint),allocatable::GFLGSort(:),GCPUSort(:)
  ! --- start procedure ---
  ! -- initialize --
  MeshSize=st_meshset%MaxID(1,0)-st_meshset%MinID(1,0)+1
  comrange=(st_param%nbf*2+1)**3
  allocate(nrflg(1:st_comm_info%nprocs))
  allocate(nrsum(1:st_comm_info%nprocs))
  allocate(GFLG(1:MeshSize*comrange)); allocate(GCPU(1:MeshSize*comrange))
  allocate(GIDX(1:MeshSize*comrange)); allocate(GCHK(1:MeshSize*comrange))
  GMeshSize=0
  st_comm_info%nrproc=0; st_comm_info%ngproc=0; nrsum=0
  ! -- specify GMesh region --
  call ppohAMRFDM_xpnd_gflg
  if(allocated(st_comm_info%iMesh)) deallocate(st_comm_info%iMesh)
  allocate(st_comm_info%iMesh(1:sum(st_comm_info%nrproc)))
  allocate(GFLGSort(1:ngst)); allocate(GCPUSort(1:ngst))
  st_comm_info%iMesh=-1; nrflg=0
  call ppohAMRFDM_set_iMesh_and_sort
  deallocate(nrflg); deallocate(nrsum)
  deallocate(GIDX); deallocate(GCHK)
  deallocate(Gflg); deallocate(GCPU)
  if(associated(st_meshset%GMesh)) deallocate(st_meshset%GMesh)
  allocate(st_meshset%GMesh(1:GMeshSize))
  call ppohAMRFDM_set_Goct
  deallocate(GCPUSort)
  call ppohAMRFDM_Gconnect
  deallocate(GflgSort)

contains
  
  subroutine ppohAMRFDM_xpnd_gflg
    implicit none
    integer(kind=ppohAMRFDM_kint)::index,CPU,ix,iy,iz
    integer(kind=ppohAMRFDM_kint)::GMrtN
    integer(kind=ppohAMRFDM_kint)::spt(1:3),ept(1:3),iPOSN(1:3)
    type(st_ppohAMRFDM_octset),pointer::p0
    ! -- expand Gflg and get Morton number of new ghost oct --
    GMn2Min=(st_param%pxmax*st_param%ixmax) &
           *(st_param%pymax*st_param%iymax) &
           *(st_param%pzmax*st_param%izmax)
    GMn2Max=-1
    ngst=0; GCHK(:)=0
    do index=st_meshset%MinID(1,0),st_meshset%MaxID(1,0)
       p0 => st_meshset%Mesh(index)
       nrflg(:)=0
       spt(:)=(p0%iPOS(:)+st_param%Nint2)/(2*st_param%Nint2)-st_param%nbf
       ept(:)=(p0%iPOS(:)+st_param%Nint2)/(2*st_param%Nint2)+st_param%nbf
       do iz=spt(3),ept(3)
          do iy=spt(2),ept(2)
             do ix=spt(1),ept(1)
                iPOSN(1)=ix; iPOSN(2)=iy; iPOSN(3)=iz
                call ppohAMRFDM_Idxn2MrtN(iPOSN,GMrtN,st_param)
                if(GMrtN<st_comm_info%MinMn.or.st_comm_info%MaxMn<GMrtN) then
                   GMn2Min=min(GMn2Min,GMrtN)
                   GMn2Max=max(GMn2Max,GMrtN)
                   call ppohAMRFDM_findCPU(GMrtN,CPU,st_comm_info)
                   ngst=ngst+1
                   Gflg(ngst)=GMrtN
                   GCPU(ngst)=CPU+1
                   GIDX(ngst)=index
                   if(nrflg(CPU+1)==0) then
                      GCHK(ngst)=1
                   endif
                   nrflg(CPU+1)=1
                endif
             enddo
          enddo
       enddo
       st_comm_info%nrproc(1:st_comm_info%nprocs)&
            =st_comm_info%nrproc(1:st_comm_info%nprocs)&
            +nrflg(1:st_comm_info%nprocs)
    enddo
    ! -- create nrsum --
    nrsum(1)=0
    do CPU=2,st_comm_info%nprocs
       nrsum(CPU)=sum(st_comm_info%nrproc(1:CPU-1))
    enddo
    return
  end subroutine ppohAMRFDM_xpnd_gflg
  
  subroutine ppohAMRFDM_set_iMesh_and_sort
    implicit none
    integer(kind=ppohAMRFDM_kint)::index,index2,CPU,ID
    ! -- set iMesh --
    do index=1,ngst
       if(GCHK(index)==1) then
          ! -- set iBMesh_arr --
          CPU=GCPU(index)
          st_comm_info%iMesh(nrsum(CPU)+nrflg(CPU)+1)=GIDX(index)
          nrflg(CPU)=nrflg(CPU)+1
       endif
    enddo
    ! -- quick sort --
    call ppohAMRFDM_qs(GFLG,GCPU,1,MeshSize*comrange,1,ngst)
    ! -- remove duplicate element --
    index2=1; GFLGSort(1)=GFLG(1); GCPUSort(1)=GCPU(1)
    do index=2,ngst
       call ppohAMRFDM_bins(GFLGSort,ngst,index2,GFLG(index),ID)
       if(ID/=-1) cycle
       index2=index2+1
       GFLGSort(index2)=GFLG(index)
       GCPUSort(index2)=GCPU(index)
    enddo
    GMeshSize=index2
    return
  end subroutine ppohAMRFDM_set_iMesh_and_sort

  subroutine ppohAMRFDM_set_Goct
    implicit none
    integer(kind=ppohAMRFDM_kint)::index,iLv,GMrtN,CPU
    integer(kind=ppohAMRFDM_kint)::octLv,Csort,bndry,load
    integer(kind=ppohAMRFDM_kint)::iFLG(1:3),iPOS(1:3),iPOSN(1:3)
    integer(kind=ppohAMRFDM_kint)::ngproc(1:st_comm_info%nprocs)
    real(kind=ppohAMRFDM_kdbl)::F(1:st_param%npy),C(1:st_param%npy)
    type(st_ppohAMRFDM_octset),pointer::newp
    ngproc(:)=0; nullify(newp)
    octLv=0; Csort=0; iFLG(:)=0; bndry=1; load=1
    F(:)=0.0d0; C(:)=0.0d0
    !$omp parallel do default(none) &
    !$omp& private(index,GMrtN,CPU,iPOSN,iPOS) &
    !$omp& shared(GMeshSize,GflgSort,GCPUSort,octLv,Csort,iFLG,F,C,bndry,load, &
    !$omp& newp,st_param,st_comm_info,st_meshset) &
    !$omp& reduction(+:ngproc)
    do index=1,GMeshSize
       GMrtN=GflgSort(index); CPU=GCPUSort(index)
       ! - add number to n_gcells_proc -
       ngproc(CPU)=ngproc(CPU)+1
       ! -- from MrtN to normal index --
       call ppohAMRFDM_MrtN2Idxn(GMrtN,iPOSN)
       ! - add Lv.0 Goct -
       iPOS(:)=2*iPOSN(:)*st_param%Nint2-st_param%Nint2
       st_meshset%GMesh(index)=&
            st_ppohAMRFDM_octset(index,octLv,Csort,iFLG, &
            iPOS,GMrtN,bndry,F,C,load, & 
            newp, &
            newp,newp,newp,&
            newp,newp,newp,&
            newp,newp,newp,newp,&
            newp,newp,newp,newp,&
            newp)
    enddo
    !$omp end parallel do
    st_comm_info%ngproc=ngproc
    ! -- set MeshID --
    st_meshset%MinID(3,0)=1; st_meshset%MaxID(3,0)=GMeshSize
    if(st_param%LvMax>0) then
       do iLv=1,st_param%LvMax
          st_meshset%MinID(3,iLv)=st_meshset%MaxID(3,iLv-1)
          st_meshset%MaxID(3,iLv)=st_meshset%MaxID(3,iLv-1)
       enddo
    endif
    do iLv=0,st_param%LvMax
       st_meshset%MinID(4,iLv)=st_meshset%MaxID(3,st_param%LvMax)
       st_meshset%MaxID(4,iLv)=st_meshset%MaxID(3,st_param%LvMax)
    enddo
    return
  end subroutine ppohAMRFDM_set_Goct

  subroutine ppohAMRFDM_Gconnect
    implicit none
    integer(kind=ppohAMRFDM_kint)::index,ID
    integer(kind=ppohAMRFDM_kint)::nbindx(1:6),Mnarr(1:27)
    integer(kind=ppohAMRFDM_kint)::iPOS(1:3)
    type(st_ppohAMRFDM_octset),pointer::p0,p1
    !$omp parallel do default(none) &
    !$omp& private(index,p0,p1,iPOS,Mnarr,nbindx,ID) &
    !$omp& shared(GMn2Min,GMn2Max,GflgSort,ngst,GMeshSize,& 
    !$omp& st_param,st_comm_info,st_meshset)
    do index=st_meshset%MinID(3,0),st_meshset%MaxID(3,0)
       p0 => st_meshset%GMesh(index)
       iPOS(:)=p0%iPOS(:)
       call ppohAMRFDM_MrtN_nghbrs(iPOS,Mnarr,st_param)
       nbindx(:)=-1
       if(Mnarr(1)>=GMn2Min.and.Mnarr(1)<=GMn2Max) then
          call ppohAMRFDM_bins(GflgSort,ngst,GMeshSize,Mnarr(1),ID)
          if(ID/=-1) nbindx(1)=ID
       endif
       if(Mnarr(4)>=GMn2Min.and.Mnarr(4)<=GMn2Max) then
          call ppohAMRFDM_bins(GflgSort,ngst,GMeshSize,Mnarr(4),ID)
          if(ID/=-1) nbindx(2)=ID
       endif
       if(Mnarr(2)>=GMn2Min.and.Mnarr(2)<=GMn2Max) then
          call ppohAMRFDM_bins(GflgSort,ngst,GMeshSize,Mnarr(2),ID)
          if(ID/=-1) nbindx(3)=ID
       endif
       if(Mnarr(5)>=GMn2Min.and.Mnarr(5)<=GMn2Max) then
          call ppohAMRFDM_bins(GflgSort,ngst,GMeshSize,Mnarr(5),ID)
          if(ID/=-1) nbindx(4)=ID
       endif
       if(Mnarr(3)>=GMn2Min.and.Mnarr(3)<=GMn2Max) then
          call ppohAMRFDM_bins(GflgSort,ngst,GMeshSize,Mnarr(3),ID)
          if(ID/=-1) nbindx(5)=ID
       endif
       if(Mnarr(6)>=GMn2Min.and.Mnarr(6)<=GMn2Max) then
          call ppohAMRFDM_bins(GflgSort,ngst,GMeshSize,Mnarr(6),ID)
          if(ID/=-1) nbindx(6)=ID
       endif
       ! -- connect process --
       if(nbindx(1)>=st_meshset%MinID(3,0).and.nbindx(1)<=st_meshset%MaxID(3,0)) then !nb is ghost
          p0%octNb1 => st_meshset%GMesh(nbindx(1))
       else if(Mnarr(1)>=st_comm_info%MinMn.and.Mnarr(1)<=st_comm_info%MaxMn) then !nb is oct
          p1 => st_meshset%Mesh(Mnarr(1)-st_comm_info%MnDisp)
          p0%octNb1 => p1; p1%octNb2 => p0
       else
          nullify(p0%octNb1)
       endif
       if(nbindx(2)>=st_meshset%MinID(3,0).and.nbindx(2)<=st_meshset%MaxID(3,0)) then
          p0%octNb2 => st_meshset%GMesh(nbindx(2))
       else if(Mnarr(4)>=st_comm_info%MinMn.and.Mnarr(4)<=st_comm_info%MaxMn) then
          p1 => st_meshset%Mesh(Mnarr(4)-st_comm_info%MnDisp)
          p0%octNb2 => p1; p1%octNb1 => p0
       else
          nullify(p0%octNb2)
       endif
       if(nbindx(3)>=st_meshset%MinID(3,0).and.nbindx(3)<=st_meshset%MaxID(3,0)) then
          p0%octNb3 => st_meshset%GMesh(nbindx(3))
       else if(Mnarr(2)>=st_comm_info%MinMn.and.Mnarr(2)<=st_comm_info%MaxMn) then
          p1 => st_meshset%Mesh(Mnarr(2)-st_comm_info%MnDisp)
          p0%octNb3 => p1; p1%octNb4 => p0
       else
          nullify(p0%octNb3)
       endif
       if(nbindx(4)>=st_meshset%MinID(3,0).and.nbindx(4)<=st_meshset%MaxID(3,0)) then
          p0%octNb4 => st_meshset%GMesh(nbindx(4))
       else if(Mnarr(5)>=st_comm_info%MinMn.and.Mnarr(5)<=st_comm_info%MaxMn) then
          p1 => st_meshset%Mesh(Mnarr(5)-st_comm_info%MnDisp)
          p0%octNb4 => p1; p1%octNb3 => p0
       else
          nullify(p0%octNb4)
       endif
       if(nbindx(5)>=st_meshset%MinID(3,0).and.nbindx(5)<=st_meshset%MaxID(3,0)) then
          p0%octNb5 => st_meshset%GMesh(nbindx(5))
       else if(Mnarr(3)>=st_comm_info%MinMn.and.Mnarr(3)<=st_comm_info%MaxMn) then
          p1 => st_meshset%Mesh(Mnarr(3)-st_comm_info%MnDisp)
          p0%octNb5 => p1; p1%octNb6 => p0
       else
          nullify(p0%octNb5)
       endif
       if(nbindx(6)>=st_meshset%MinID(3,0).and.nbindx(6)<=st_meshset%MaxID(3,0)) then
          p0%octNb6 => st_meshset%GMesh(nbindx(6))
       else if(Mnarr(6)>=st_comm_info%MinMn.and.Mnarr(6)<=st_comm_info%MaxMn) then
          p1 => st_meshset%Mesh(Mnarr(6)-st_comm_info%MnDisp)
          p0%octNb6 => p1; p1%octNb5 => p0
       else
          nullify(p0%octNb6)
       endif
    enddo
    !$omp end parallel do
    ! -- self-connect edge of Lv.0 Goct --
    !$omp parallel do default(none) &
    !$omp& private(index,p0) &
    !$omp& shared(st_meshset)
    do index=st_meshset%MinID(3,0),st_meshset%MaxID(3,0)
       p0 => st_meshset%GMesh(index)
       if(.not.associated(p0%octNb1)) p0%octNb1 => p0
       if(.not.associated(p0%octNb2)) p0%octNb2 => p0
       if(.not.associated(p0%octNb3)) p0%octNb3 => p0
       if(.not.associated(p0%octNb4)) p0%octNb4 => p0
       if(.not.associated(p0%octNb5)) p0%octNb5 => p0
       if(.not.associated(p0%octNb6)) p0%octNb6 => p0
    enddo
    !$omp end parallel do
    return
  end subroutine ppohAMRFDM_Gconnect

end subroutine ppohAMRFDM_make_GMesh

subroutine ppohAMRFDM_Idxn2MrtN(iPOS,MrtN,st_param)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  integer,parameter::max=10
  integer,intent(in)::iPOS(1:3)
  integer,intent(out)::MrtN
  integer::index,ix,iy,iz
  integer::nx(1:max),ny(1:max),nz(1:max)
  ! -- initialize --
  MrtN=0; nx=0; ny=0; nz=0
  ix=iPOS(1)-1; iy=iPOS(2)-1; iz=iPOS(3)-1
  ! -- Periodic B.C. --
  if(ix<0) then
     ix=ix+(st_param%pxmax*st_param%ixmax)
  else if(ix>=st_param%pxmax*st_param%ixmax) then
     ix=ix-(st_param%pxmax*st_param%ixmax)
  endif
  if(iy<0) then
     iy=iy+(st_param%pymax*st_param%iymax)
  else if(iy>=st_param%pymax*st_param%iymax) then
     iy=iy-(st_param%pymax*st_param%iymax)
  endif
  if(iz<0) then
     iz=iz+(st_param%pzmax*st_param%izmax)
  else if(iz>=st_param%pzmax*st_param%izmax) then
     iz=iz-(st_param%pzmax*st_param%izmax)
  endif
  ! -------------------
  do index=1,max
     nx(index)=mod(ix,2); ny(index)=mod(iy,2); nz(index)=mod(iz,2)
     ix=ix/2; iy=iy/2; iz=iz/2
  enddo
  do index=1,max
     MrtN=MrtN &
          +nx(index)*(2**(3*index-3)) &
          +ny(index)*(2**(3*index-2)) &
          +nz(index)*(2**(3*index-1))    
  enddo
  MrtN=MrtN+1
  return
end subroutine ppohAMRFDM_Idxn2MrtN

recursive subroutine ppohAMRFDM_qs(a,b,sID,eID,first,last)
  implicit none
  integer::sID,eID
  integer::first,last,a(sID:eID),b(sID:eID)
  integer::ID,JD
  integer::x,t1,t2
  x=a((first+last)/2)
  ID=first; JD=last
  do
     do while(a(ID)<x)
        ID=ID+1
     enddo
     do while(x<a(JD))
        JD=JD-1
     enddo
     if(ID>=JD) exit
     t1=a(ID); a(ID)=a(JD); a(JD)=t1
     t2=b(ID); b(ID)=b(JD); b(JD)=t2
     ID=ID+1; JD=JD-1
  enddo
  if(first<ID-1) call ppohAMRFDM_qs(a,b,sID,eID,first,ID-1)
  if(JD+1<last)  call ppohAMRFDM_qs(a,b,sID,eID,JD+1,last)
  return
end subroutine ppohAMRFDM_qs

subroutine ppohAMRFDM_bins(A,fullsize,size,val,ID)
  implicit none
  integer,intent(in)::fullsize,size,val
  integer,intent(out)::ID
  integer::A(1:fullsize)
  integer::lower,upper,mid
  lower=1; upper=size
  do while(lower<=upper)
     mid=(lower+upper)/2
     if(A(mid)==val) then
        ID=mid; return
     else if(A(mid)<val) then
        lower=mid+1
     else
        upper=mid-1
     endif
  enddo
  ID=-1
  return
end subroutine ppohAMRFDM_bins
