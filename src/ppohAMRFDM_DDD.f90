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

subroutine ppohAMRFDM_DDD(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::iLv
  call ppohAMRFDM_DDD_check(st_param,st_meshset,st_comm_info)
  if(st_param%DDD==1) then
     if(st_param%DDDflg/=0) then
        if(st_comm_info%rank==0) print *,'### DDD start ###', st_param%it
        call ppohAMRFDM_loadbalance(st_param,st_meshset,st_comm_info)
        call ppohAMRFDM_reset_Gst(st_param,st_meshset)
        call ppohAMRFDM_passing_alldomain(st_param,st_meshset,st_comm_info)
        call ppohAMRFDM_connect_newocts(st_param,st_meshset,st_comm_info)
        call ppohAMRFDM_make_GMesh(st_param,st_meshset,st_comm_info)
        if(st_param%LvMax>0) then
           call ppohAMRFDM_refine_DDD(st_param,st_meshset,st_comm_info)
           call ppohAMRFDM_sortoct(st_param,st_meshset,st_comm_info)
        endif
        do iLv=0,st_param%LvMax
           call ppohAMRFDM_passing_fields(iLv,st_param,st_meshset,st_comm_info)
        enddo
     endif
  endif
  return
end subroutine ppohAMRFDM_DDD

subroutine ppohAMRFDM_DDD_check(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(St_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::loadsum,flgsum,ierr
  real(kind=ppohAMRFDM_kdbl)::average_ratio,load_ratio
  st_param%DDDflg=0; st_param%nloop=0
  loadsum=0; flgsum=0
  call ppohAMRFDM_loadcount(st_param%nloop,st_param,st_meshset)
  call mpi_allreduce(st_param%nloop,loadsum,1,mpi_integer,mpi_sum,st_comm_info%comm,ierr)
  average_ratio=1.0d0/dble(st_comm_info%nprocs)
  load_ratio=dble(st_param%nloop)/dble(loadsum)
  if(average_ratio*st_param%DDD_crtr<load_ratio) st_param%DDDflg=1
  call mpi_allreduce(st_param%DDDflg,flgsum,1,mpi_integer,mpi_sum,st_comm_info%comm,ierr)
  st_param%DDDflg=flgsum
  return
end subroutine ppohAMRFDM_DDD_check

subroutine ppohAMRFDM_loadcount(nloop,st_param,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(out)::nloop
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  integer(kind=ppohAMRFDM_kint)::index,iLv
  type(st_ppohAMRFDM_octset),pointer::p0
  nloop=0
  do iLv=st_param%LvMax,0,-1
     if(st_meshset%MinID(1,iLv)<st_meshset%MaxID(1,iLv)) then
        !$omp parallel do default(none) &
        !$omp& private(index,p0) &
        !$omp& shared(st_param,st_meshset,iLv) &
        !$omp& reduction(+:nloop)
        do index=st_meshset%MinID(1,iLv),st_meshset%MaxID(1,iLv)
           p0 => st_meshset%Mesh(index)
           p0%load=0
           if(p0%iFLG(1)>=0.and.p0%iFLG(1)<=st_param%nfg-1) then
              p0%load=2**iLv
           else if(p0%iFLG(1)>st_param%nfg-1) then
              p0%load=p0%load &
                   +p0%octCh1%load+p0%octCh2%load &
                   +p0%octCh3%load+p0%octCh4%load &
                   +p0%octCh5%load+p0%octCh6%load &
                   +p0%octCh7%load+p0%octCh8%load
           endif
           if(iLv==0) nloop=nloop+p0%load
        enddo
        !$omp end parallel do
     endif
  enddo
  return
end subroutine ppohAMRFDM_loadcount

subroutine ppohAMRFDM_loadbalance(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::equal_loop,equal_loop2,whole_ncells,whole_loops
  integer(kind=ppohAMRFDM_kint)::i,j,k,Nall,ierr
  integer(kind=ppohAMRFDM_kint),allocatable::Nall_arr(:)
  integer(kind=ppohAMRFDM_kint),allocatable::loop(:),proc_total_loops(:),rdispls(:)
  integer(kind=ppohAMRFDM_kint),allocatable::accum_loops(:)
  type(st_ppohAMRFDM_octset),pointer::p0
  ! -- collect the loads of each level and bring them up to Lv=0 --
  call mpi_allreduce(st_param%nloop,whole_loops,1,mpi_integer,mpi_sum,st_comm_info%comm,ierr)
  ! -- total oct number in Lv=0 --
  whole_ncells=(st_param%pxmax*st_param%ixmax) &
       *(st_param%pymax*st_param%iymax) &
       *(st_param%pzmax*st_param%izmax)
  Nall=st_meshset%MaxID(1,0)-st_meshset%MinID(1,0)+1 ! oct number in the local process
  ! -- allocation --
  allocate(Nall_arr(1:st_comm_info%nprocs))
  allocate(rdispls(1:st_comm_info%nprocs))
  allocate(accum_loops(1:whole_ncells))
  allocate(loop(1:Nall)) !loop is the number of ptcl loops for each cell in the local process
  allocate(proc_total_loops(1:Nall)) !total_number of accum_loops in the local process
  k=1
  do j=st_meshset%MinID(1,0),st_meshset%MaxID(1,0)
     p0 => st_meshset%Mesh(j)
     loop(k)=p0%load
     k=k+1
  enddo
  call MPI_allgather(Nall,1,mpi_integer,Nall_arr,1,mpi_integer,mpi_comm_world,ierr)
  ! -- Nall_arr records the number of Lv=0 cells in each process --
  rdispls(1)=0
  do i=2,st_comm_info%nprocs
     rdispls(i)=rdispls(i-1)+Nall_arr(i-1)
  enddo
  proc_total_loops(:)=loop(:)
  do i=2,Nall
     proc_total_loops(i)=proc_total_loops(i)+proc_total_loops(i-1)
  enddo
  call MPI_allgatherV(proc_total_loops,Nall,mpi_integer,accum_loops,Nall_arr,rdispls,mpi_integer,st_comm_info%comm,ierr)
  equal_loop=whole_loops/st_comm_info%nprocs
  st_comm_info%Mn2CPU(:)=Nall_arr(:) ! initially, Mn2CPU is the same as Nall_arr
  do i=2,st_comm_info%nprocs
     st_comm_info%Mn2CPU(i)=st_comm_info%Mn2CPU(i)+st_comm_info%Mn2CPU(i-1)
     accum_loops(st_comm_info%Mn2CPU(i-1)+1:st_comm_info%Mn2CPU(i))= &
          accum_loops(st_comm_info%Mn2CPU(i-1)+1:st_comm_info%Mn2CPU(i)) &
          +accum_loops(st_comm_info%Mn2CPU(i-1))
  enddo
  !----Now Slice accum_loops by equal_loop 
  st_comm_info%Mn2CPU(:)=0
  equal_loop2=equal_loop
  j=1
  do i=1,whole_ncells
     st_comm_info%Mn2CPU(j)=st_comm_info%Mn2CPU(j)+1  ! add up cell number in each index which corresponds to process number
     if(accum_loops(i)>=equal_loop2) then
        j=j+1
        equal_loop2=equal_loop2+equal_loop
     endif
     if(j>st_comm_info%nprocs) exit
  enddo
  do i=2,st_comm_info%nprocs-1
     if(st_comm_info%Mn2CPU(i)<4) st_comm_info%Mn2CPU(i)=4
     st_comm_info%Mn2CPU(i)=st_comm_info%Mn2CPU(i)+st_comm_info%Mn2CPU(i-1) !Remember, Mn2CPU is cumulative.
  enddo
  st_comm_info%Mn2CPU(st_comm_info%nprocs)=whole_ncells ! The end guy takes care of the rest of the cells unconditionally.
  !Mn2CPU=Mn2CPU-1 ! remember, Morton number starts from zero, so it has to be subtracted by 1.
  ! -- Specify the Min and Max Morton on each process --
  if(st_comm_info%rank==0)then
     st_comm_info%MinMn=1
  else
     st_comm_info%MinMn=st_comm_info%Mn2CPU(st_comm_info%rank)+1
  endif
  st_comm_info%MaxMn=st_comm_info%Mn2CPU(st_comm_info%rank+1)
  ! -- deallocation --
  deallocate(Nall_arr)
  deallocate(accum_loops)
  deallocate(rdispls)
  deallocate(loop)
  deallocate(proc_total_loops)
  return
end subroutine ppohAMRFDM_loadbalance

subroutine ppohAMRFDM_reset_Gst(st_param,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  integer(kind=ppohAMRFDM_kint)::iLv,index,ID
  type(st_ppohAMRFDM_octset),pointer::p0
  ! -- reset pointers --
  do iLv=0,st_param%LvMax
     do ID=3,4
        if(st_meshset%MinID(ID,iLv)>=st_meshset%MaxID(ID,iLv)) cycle
        do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
           p0 => st_meshset%GMesh(index)
           if(associated(p0%octNb1)) nullify(p0%octNb1%octNb2)
           if(associated(p0%octNb2)) nullify(p0%octNb2%octNb1)
           if(associated(p0%octNb3)) nullify(p0%octNb3%octNb4)
           if(associated(p0%octNb4)) nullify(p0%octNb4%octNb3)
           if(associated(p0%octNb5)) nullify(p0%octNb5%octNb6)
           if(associated(p0%octNb6)) nullify(p0%octNb6%octNb5)
           nullify(p0%octPrt)
           nullify(p0%octCh1); nullify(p0%octCh2)
           nullify(p0%octCh3); nullify(p0%octCh4)
           nullify(p0%octCh5); nullify(p0%octCh6)
           nullify(p0%octCh7); nullify(p0%octCh8)
        enddo
     enddo
  enddo
  ! -- reset MeshID --
  st_meshset%MinID(3,0)=1; st_meshset%MaxID(3,0)=0
  if(st_param%LvMax>0) then
     do iLv=1,st_param%LvMax
        st_meshset%MinID(3,iLv)=st_meshset%MaxID(3,iLv-1)+1
        st_meshset%MaxID(3,iLv)=st_meshset%MaxID(3,iLv-1)
     enddo
  endif
  do iLv=0,st_param%LvMax
     st_meshset%MinID(4,iLv)=st_meshset%MaxID(3,st_param%LvMax)+1
     st_meshset%MaxID(4,iLv)=st_meshset%MaxID(3,st_param%LvMax)
  enddo
  return
end subroutine ppohAMRFDM_reset_Gst

subroutine ppohAMRFDM_passing_alldomain(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::ID,ir,is,im,iLv
  integer(kind=ppohAMRFDM_kint)::sdest,rdest,sendnum,recvnum
  integer(kind=ppohAMRFDM_kint)::ireq(1:st_comm_info%nprocs*2),istat(mpi_status_size,1:st_comm_info%nprocs*2),ierr
  type(st_ppohAMRFDM_octset),pointer::p0
  st_comm_info%snum(:,:)=0; st_comm_info%rnum(:,:)=0
  ! -- count all send numbers --
  call ppohAMRFDM_count_snum(st_meshset,st_comm_info)
  ! -- send snum and receive rnum --
  ir=0; is=0; im=0
  do ID=0,st_comm_info%nprocs-1
     sdest=mod(st_comm_info%rank+ID,st_comm_info%nprocs)
     rdest=mod(st_comm_info%rank-ID+st_comm_info%nprocs,st_comm_info%nprocs)
     ir=ir+1; im=im+1
     call mpi_irecv(st_comm_info%rnum(0,rdest+1),st_param%LvMax+1,mpi_integer,rdest,ir,st_comm_info%comm,ireq(im),ierr)
     is=is+1; im=im+1
     call mpi_isend(st_comm_info%snum(0,sdest+1),st_param%LvMax+1,mpi_integer,sdest,is,st_comm_info%comm,ireq(im),ierr)
  enddo
  ! -- allocate send buffer --
  call ppohAMRFDM_allocate_sendbuf(st_param,st_meshset,st_comm_info)
  call mpi_waitall(im,ireq(1:im),istat(:,1:im),ierr)
  ! -- allocate recv buffer --
  call ppohAMRFDM_allocate_recvbuf(st_param,st_meshset,st_comm_info)
  ! -- refresh MinID and MaxID --
  call ppohAMRFDM_reset_ID(st_param,st_meshset,st_comm_info)
  ! -- data packing --
  call ppohAMRFDM_pack_domains(st_param,st_meshset,st_comm_info)
  ! -- send and recv --
  ir=0; is=0; im=0
  do ID=0,st_comm_info%nprocs-1
     sdest=mod(st_comm_info%rank+ID,st_comm_info%nprocs)
     rdest=mod(st_comm_info%rank-ID+st_comm_info%nprocs,st_comm_info%nprocs)
     sendnum=0; recvnum=0
     do iLv=0,st_param%LvMax
        sendnum=sendnum+st_comm_info%snum(iLv,sdest+1)
        recvnum=recvnum+st_comm_info%rnum(iLv,rdest+1)
     enddo
     ir=ir+2; is=is+2
     if(sendnum==0.and.recvnum==0) cycle
     ! -- recv --
     if(recvnum>0) then
        im=im+2
        call mpi_irecv(st_meshset%rBuf(rdest+1)%bufr,recvnum*st_param%npy,&
             mpi_double_precision,rdest,ir-1,st_comm_info%comm,ireq(im-1),ierr)
        call mpi_irecv(st_meshset%rBuf(rdest+1)%bufi,recvnum*2           ,&
             mpi_integer         ,rdest,ir  ,st_comm_info%comm,ireq(im  ),ierr)
     endif
     ! -- send --
     if(sendnum>0) then
        im=im+2
        call mpi_isend(st_meshset%sBuf(sdest+1)%bufr,sendnum*st_param%npy,&
             mpi_double_precision,sdest,is-1,st_comm_info%comm,ireq(im-1),ierr)
        call mpi_isend(st_meshset%sBuf(sdest+1)%bufi,sendnum*2           ,&
             mpi_integer         ,sdest,is  ,st_comm_info%comm,ireq(im  ),ierr)
     endif
  enddo
  ! -- re-allocate Mesh --
  deallocate(st_meshset%Mesh)
  recvnum=0
  do ID=0,st_comm_info%nprocs-1
     do iLv=0,st_param%LvMax
        recvnum=recvnum+st_comm_info%rnum(iLv,ID+1)
     enddo
  enddo
  allocate(st_meshset%Mesh(1:recvnum))
  ! -- initialize --
  !$omp parallel do default(none) &
  !$omp& private(ID,p0) &
  !$omp& shared(st_param,st_meshset,recvnum)
  do ID=1,recvnum
     p0 => st_meshset%Mesh(ID)
     p0%octN=-1; p0%octLv=-1; p0%Csort=0
     p0%iFLG(:)=-st_param%nfg; p0%iPOS(:)=-1; p0%MrtN=-1; p0%bndry=-1
     allocate(p0%F(1:st_param%npy))
     allocate(p0%C(1:st_param%npy))
     p0%F(:)=0.0d0; p0%C(:)=0.0d0; p0%load=0
     nullify(p0%octPrt)
     nullify(p0%octNb1); nullify(p0%octNb2)
     nullify(p0%octNb3); nullify(p0%octNb4)
     nullify(p0%octNb5); nullify(p0%octNb6)
     nullify(p0%octCh1); nullify(p0%octCh2)
     nullify(p0%octCh3); nullify(p0%octCh4)
     nullify(p0%octCh5); nullify(p0%octCh6)
     nullify(p0%octCh7); nullify(p0%octCh8)
     nullify(p0%Psort)
  enddo
  !$omp end parallel do
  call mpi_waitall(im,ireq(1:im),istat(:,1:im),ierr)
  ! -- data unpacking --
  call ppohAMRFDM_unpack_domains(st_param,st_meshset,st_comm_info)
  return
end subroutine ppohAMRFDM_passing_alldomain

subroutine ppohAMRFDM_count_snum(st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::index,CPU,iLv
  type(st_ppohAMRFDM_octset),pointer::p0
  iLv=0
  do index=st_meshset%MinID(1,iLv),st_meshset%MaxID(1,iLv)
     p0 => st_meshset%Mesh(index)
     call ppohAMRFDM_findCPU(p0%MrtN,CPU,st_comm_info)
     st_comm_info%snum(iLv,CPU+1)=st_comm_info%snum(iLv,CPU+1)+1
     if(p0%iFLG(1)>0) then
        call ppohAMRFDM_count_snum_child(p0%octCh1%octN,iLv+1,CPU,st_meshset,st_comm_info)
        call ppohAMRFDM_count_snum_child(p0%octCh2%octN,iLv+1,CPU,st_meshset,st_comm_info)
        call ppohAMRFDM_count_snum_child(p0%octCh3%octN,iLv+1,CPU,st_meshset,st_comm_info)
        call ppohAMRFDM_count_snum_child(p0%octCh4%octN,iLv+1,CPU,st_meshset,st_comm_info)
        call ppohAMRFDM_count_snum_child(p0%octCh5%octN,iLv+1,CPU,st_meshset,st_comm_info)
        call ppohAMRFDM_count_snum_child(p0%octCh6%octN,iLv+1,CPU,st_meshset,st_comm_info)
        call ppohAMRFDM_count_snum_child(p0%octCh7%octN,iLv+1,CPU,st_meshset,st_comm_info)
        call ppohAMRFDM_count_snum_child(p0%octCh8%octN,iLv+1,CPU,st_meshset,st_comm_info)
     endif
  enddo
  return
end subroutine ppohAMRFDM_count_snum

recursive subroutine ppohAMRFDM_count_snum_child(index,iLv,CPU,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::index,iLv,CPU
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  type(st_ppohAMRFDM_octset),pointer::p0
  st_comm_info%snum(iLv,CPU+1)=st_comm_info%snum(iLv,CPU+1)+1
  p0 => st_meshset%Mesh(index)
  if(p0%iFLG(1)>0) then
     call ppohAMRFDM_count_snum_child(p0%octCh1%octN,iLv+1,CPU,st_meshset,st_comm_info)
     call ppohAMRFDM_count_snum_child(p0%octCh2%octN,iLv+1,CPU,st_meshset,st_comm_info)
     call ppohAMRFDM_count_snum_child(p0%octCh3%octN,iLv+1,CPU,st_meshset,st_comm_info)
     call ppohAMRFDM_count_snum_child(p0%octCh4%octN,iLv+1,CPU,st_meshset,st_comm_info)
     call ppohAMRFDM_count_snum_child(p0%octCh5%octN,iLv+1,CPU,st_meshset,st_comm_info)
     call ppohAMRFDM_count_snum_child(p0%octCh6%octN,iLv+1,CPU,st_meshset,st_comm_info)
     call ppohAMRFDM_count_snum_child(p0%octCh7%octN,iLv+1,CPU,st_meshset,st_comm_info)
     call ppohAMRFDM_count_snum_child(p0%octCh8%octN,iLv+1,CPU,st_meshset,st_comm_info)
  endif
  return
end subroutine ppohAMRFDM_count_snum_child

subroutine ppohAMRFDM_allocate_sendbuf(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::ID,iLv,sdest,sendnum
  do ID=0,st_comm_info%nprocs-1
     sdest=mod(st_comm_info%rank+ID,st_comm_info%nprocs)
     if(allocated(st_meshset%sBuf(sdest+1)%bufi)) then
        deallocate(st_meshset%sBuf(sdest+1)%bufi)
     endif
     if(allocated(st_meshset%sBuf(sdest+1)%bufr)) then
        deallocate(st_meshset%sBuf(sdest+1)%bufr)
     endif
     sendnum=0
     do iLv=0,st_param%LvMax
        sendnum=sendnum+st_comm_info%snum(iLv,sdest+1)
     enddo
     if(sendnum>0) then
        allocate(st_meshset%sBuf(sdest+1)%bufr(1:sendnum*st_param%npy))
        allocate(st_meshset%sBuf(sdest+1)%bufi(1:sendnum*2))
     endif
  enddo
  return
end subroutine ppohAMRFDM_allocate_sendbuf

subroutine ppohAMRFDM_allocate_recvbuf(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::ID,iLv,rdest,recvnum
  do ID=0,st_comm_info%nprocs-1
     rdest=mod(st_comm_info%rank-ID+st_comm_info%nprocs,st_comm_info%nprocs)
     if(allocated(st_meshset%rBuf(rdest+1)%bufi)) then
        deallocate(st_meshset%rBuf(rdest+1)%bufi)
     endif
     if(allocated(st_meshset%rBuf(rdest+1)%bufr)) then
        deallocate(st_meshset%rBuf(rdest+1)%bufr)
     endif
     recvnum=0
     do iLv=0,st_param%LvMax
        recvnum=recvnum+st_comm_info%rnum(iLv,rdest+1)
     enddo
     if(recvnum>0) then
        allocate(st_meshset%rBuf(rdest+1)%bufr(1:recvnum*st_param%npy))
        allocate(st_meshset%rBuf(rdest+1)%bufi(1:recvnum*2))
     endif
  enddo
  return
end subroutine ppohAMRFDM_allocate_recvbuf

subroutine ppohAMRFDM_reset_ID(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::ID,iLv
  integer(kind=ppohAMRFDM_kint)::rsum(0:st_param%LvMax)
  rsum(:)=0
  ! -- MinID(1,:), MaxID(1,:) --
  st_meshset%MinID(1,0)=1
  do iLv=0,st_param%LvMax
     do ID=1,st_comm_info%nprocs
        rsum(iLv)=rsum(iLv)+st_comm_info%rnum(iLv,ID)
     enddo
  enddo
  st_meshset%MaxID(1,0)=rsum(0)
  if(st_param%LvMax>0) then
     do iLv=1,st_param%LvMax
        st_meshset%MinID(1,iLv)=st_meshset%MaxID(1,iLv-1)+1
        st_meshset%MaxID(1,iLv)=st_meshset%MaxID(1,iLv-1)+rsum(iLv)
        if(st_meshset%MinID(1,iLv)>st_meshset%MaxID(1,iLv)) then
           st_meshset%MinID(1,iLv)=st_meshset%MaxID(1,iLv)
        endif
     enddo
  endif
  ! -- MinID(2,:), MaxID(2,:) --
  do iLv=0,st_param%LvMax
     st_meshset%MinID(2,iLv)=st_meshset%MaxID(1,st_param%LvMax)
     st_meshset%MaxID(2,iLv)=st_meshset%MaxID(1,st_param%LvMax)
  enddo
  ! -- MnDisp --
  st_comm_info%MnDisp=st_comm_info%MinMn-1
  return
end subroutine ppohAMRFDM_reset_ID

subroutine ppohAMRFDM_pack_domains(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::index,ID,sID,eID,sdest,count
  type(st_ppohAMRFDM_octset),pointer::p0
  do ID=0,st_comm_info%nprocs-1
     sdest=mod(st_comm_info%rank+ID,st_comm_info%nprocs)
     if(st_comm_info%snum(0,sdest+1)==0) cycle
     ! -- set sID and eID --
     if(sdest==0) then
        sID=1
     else
        sID=0
        do index=1,sdest
           sID=sID+st_comm_info%snum(0,index)
        enddo
        sID=sID+1
     endif
     eID=0
     do index=1,sdest+1
        eID=eID+st_comm_info%snum(0,index)
     enddo
     count=1
     do index=sID,eID
        p0 => st_meshset%Mesh(index)
        st_meshset%sBuf(sdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy)&
             =p0%F(1:st_param%npy)
        st_meshset%sBuf(sdest+1)%bufi(count*2-1)=p0%iFLG(3)
        st_meshset%sBuf(sdest+1)%bufi(count*2  )=p0%iFLG(1)
        count=count+1
        if(p0%iFLG(1)>0) then
           call ppohAMRFDM_pack_domains_child(p0%octCh1%octN,count,sdest,st_param,st_meshset)
           call ppohAMRFDM_pack_domains_child(p0%octCh2%octN,count,sdest,st_param,st_meshset)
           call ppohAMRFDM_pack_domains_child(p0%octCh3%octN,count,sdest,st_param,st_meshset)
           call ppohAMRFDM_pack_domains_child(p0%octCh4%octN,count,sdest,st_param,st_meshset)
           call ppohAMRFDM_pack_domains_child(p0%octCh5%octN,count,sdest,st_param,st_meshset)
           call ppohAMRFDM_pack_domains_child(p0%octCh6%octN,count,sdest,st_param,st_meshset)
           call ppohAMRFDM_pack_domains_child(p0%octCh7%octN,count,sdest,st_param,st_meshset)
           call ppohAMRFDM_pack_domains_child(p0%octCh8%octN,count,sdest,st_param,st_meshset)
        endif
     enddo
  enddo
  return
end subroutine ppohAMRFDM_pack_domains

recursive subroutine ppohAMRFDM_pack_domains_child(index,count,sdest,st_param,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::index,sdest
  integer(kind=ppohAMRFDM_kint),intent(inout)::count
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_octset),pointer::p0
  p0 => st_meshset%Mesh(index)
  st_meshset%sBuf(sdest+1)%bufr(1+(count-1)*st_param%npy:count*st_param%npy)&
       =p0%F(1:st_param%npy)
  st_meshset%sBuf(sdest+1)%bufi(count*2-1)=p0%iFLG(3)
  st_meshset%sBuf(sdest+1)%bufi(count*2  )=p0%iFLG(1)
  count=count+1
  if(p0%iFLG(1)>0) then
     call ppohAMRFDM_pack_domains_child(p0%octCh1%octN,count,sdest,st_param,st_meshset)
     call ppohAMRFDM_pack_domains_child(p0%octCh2%octN,count,sdest,st_param,st_meshset)
     call ppohAMRFDM_pack_domains_child(p0%octCh3%octN,count,sdest,st_param,st_meshset)
     call ppohAMRFDM_pack_domains_child(p0%octCh4%octN,count,sdest,st_param,st_meshset)
     call ppohAMRFDM_pack_domains_child(p0%octCh5%octN,count,sdest,st_param,st_meshset)
     call ppohAMRFDM_pack_domains_child(p0%octCh6%octN,count,sdest,st_param,st_meshset)
     call ppohAMRFDM_pack_domains_child(p0%octCh7%octN,count,sdest,st_param,st_meshset)
     call ppohAMRFDM_pack_domains_child(p0%octCh8%octN,count,sdest,st_param,st_meshset)
  endif
  return
end subroutine ppohAMRFDM_pack_domains_child

subroutine ppohAMRFDM_unpack_domains(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::index,ID,sID,eID,iLv,count,rdest
  integer(kind=ppohAMRFDM_kint)::MrtN,iPOS(1:3)
  integer(kind=ppohAMRFDM_kint)::num(1:st_param%LvMax)
  type(st_ppohAMRFDM_octset),pointer::p0
  num(:)=0
  do ID=0,st_comm_info%nprocs-1
     rdest=mod(st_comm_info%rank-ID+st_comm_info%nprocs,st_comm_info%nprocs)
     if(st_comm_info%rnum(0,rdest+1)==0) cycle
     ! -- set sID and eID --
     if(rdest==0) then
        sID=1
     else
        sID=0
        do index=1,rdest
           sID=sID+st_comm_info%rnum(0,index)
        enddo
        sID=sID+1
     endif
     eID=0
     do index=1,rdest+1
        eID=eID+st_comm_info%rnum(0,index)
     enddo
     ! -- unpack --
     count=1; iLv=0
     do index=sID,eID
        p0 => st_meshset%Mesh(index)
        p0%octN=index
        p0%octLv=0
        p0%MrtN=index+st_comm_info%MnDisp
        MrtN=index+st_comm_info%MnDisp
        p0%Psort => st_meshset%Mesh(index)
        p0%iFLG(3)=st_meshset%rBuf(rdest+1)%bufi(count*2-1)
        p0%iFLG(1)=st_meshset%rBuf(rdest+1)%bufi(count*2  )
        if((p0%iFLG(1)>=1).and.(p0%iFLG(1)<=st_param%nfg-1)) then
           p0%iFLG(2)=5
        else
           p0%iFLG(2)=0
        endif
        ! -- set field parameter --
        p0%F(1:st_param%npy)=st_meshset%rBuf(rdest+1)%&
             bufr(1+(count-1)*st_param%npy:count*st_param%npy)
        count=count+1
        call ppohAMRFDM_MrtN2Idxn(p0%MrtN,iPOS)
        ! -- from normal index to hierarchy index --
        p0%iPOS(1)=2*iPOS(1)*st_param%Nint2-st_param%Nint2
        p0%iPOS(2)=2*iPOS(2)*st_param%Nint2-st_param%Nint2
        p0%iPOS(3)=2*iPOS(3)*st_param%Nint2-st_param%Nint2
        if(p0%iFLG(1)>0) then
           p0%octCh1 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+0)
           p0%octCh2 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+1)
           p0%octCh3 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+2)
           p0%octCh4 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+3)
           p0%octCh5 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+4)
           p0%octCh6 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+5)
           p0%octCh7 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+6)
           p0%octCh8 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+7)
           st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+0)%octPrt => p0
           st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+1)%octPrt => p0
           st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+2)%octPrt => p0
           st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+3)%octPrt => p0
           st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+4)%octPrt => p0
           st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+5)%octPrt => p0
           st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+6)%octPrt => p0
           st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+7)%octPrt => p0
           call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
                iLv+1,1,MrtN,p0%iPOS,rdest,st_param,st_meshset)
           call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
                iLv+1,2,MrtN,p0%iPOS,rdest,st_param,st_meshset)
           call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
                iLv+1,3,MrtN,p0%iPOS,rdest,st_param,st_meshset)
           call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
                iLv+1,4,MrtN,p0%iPOS,rdest,st_param,st_meshset)
           call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
                iLv+1,5,MrtN,p0%iPOS,rdest,st_param,st_meshset)
           call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
                iLv+1,6,MrtN,p0%iPOS,rdest,st_param,st_meshset)
           call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
                iLv+1,7,MrtN,p0%iPOS,rdest,st_param,st_meshset)
           call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
                iLv+1,8,MrtN,p0%iPOS,rdest,st_param,st_meshset)
        endif
     enddo
  enddo
  return
end subroutine ppohAMRFDM_unpack_domains

recursive subroutine ppohAMRFDM_unpack_domains_child(count,num,sID,iLv,Cs,MrtN,iPOS,rdest,&
     st_param,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::iLv,Cs,sID,rdest
  integer(kind=ppohAMRFDM_kint),intent(in)::iPOS(1:3)
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  integer(kind=ppohAMRFDM_kint)::count,MrtN
  integer(kind=ppohAMRFDM_kint)::num(1:st_param%LvMax)
  integer(kind=ppohAMRFDM_kint)::index,ix,iy,iz,n1,n2,Nint
  type(st_ppohAMRFDM_octset),pointer::p0
  index=sID+num(iLv)
  num(iLv)=num(iLv)+1
  p0 => st_meshset%Mesh(index)
  p0%octN=index
  p0%Csort=Cs
  p0%octLv=iLv
  p0%MrtN=MrtN
  p0%Psort => st_meshset%Mesh(index)
  ! -- set iFLG --
  p0%iFLG(3)=st_meshset%rBuf(rdest+1)%bufi(count*2-1)
  p0%iFLG(1)=st_meshset%rBuf(rdest+1)%bufi(count*2  )
  if((p0%iFLG(1)>=1).and.(p0%iFLG(1)<=st_param%nfg-1)) then 
     p0%iFLG(2)=5
  else
     p0%iFLG(2)=0
  endif
  ! -- set iPOS --
  Nint=2**(st_param%LvMax-iLv)
  n1=CS; iz=int((n1-1)/4)+1
  n2=n1-4*(iz-1); iy=int((n2-1)/2)+1; ix=n2-2*(iy-1)
  p0%iPOS(1)=iPOS(1)+(2*ix-3)*Nint
  p0%iPOS(2)=iPOS(2)+(2*iy-3)*Nint
  p0%iPOS(3)=iPOS(3)+(2*iz-3)*Nint
  ! -- set field parameter --
  p0%F(1:st_param%npy)=st_meshset%rBuf(rdest+1)%&
       bufr(1+(count-1)*st_param%npy:count*st_param%npy)
  count=count+1 ! care to change
  ! -- connect pointer --
  if(p0%iFLG(1)>0) then
     p0%octCh1 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+0)
     p0%octCh2 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+1)
     p0%octCh3 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+2)
     p0%octCh4 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+3)
     p0%octCh5 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+4)
     p0%octCh6 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+5)
     p0%octCh7 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+6)
     p0%octCh8 => st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+7)
     st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+0)%octPrt => p0
     st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+1)%octPrt => p0
     st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+2)%octPrt => p0
     st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+3)%octPrt => p0
     st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+4)%octPrt => p0
     st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+5)%octPrt => p0
     st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+6)%octPrt => p0
     st_meshset%Mesh(st_meshset%MinID(1,iLv+1)+num(iLv+1)+7)%octPrt => p0
     call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
          iLv+1,1,MrtN,p0%iPOS,rdest,st_param,st_meshset)
     call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
          iLv+1,2,MrtN,p0%iPOS,rdest,st_param,st_meshset)
     call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
          iLv+1,3,MrtN,p0%iPOS,rdest,st_param,st_meshset)
     call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
          iLv+1,4,MrtN,p0%iPOS,rdest,st_param,st_meshset)
     call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
          iLv+1,5,MrtN,p0%iPOS,rdest,st_param,st_meshset)
     call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
          iLv+1,6,MrtN,p0%iPOS,rdest,st_param,st_meshset)
     call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
          iLv+1,7,MrtN,p0%iPOS,rdest,st_param,st_meshset)
     call ppohAMRFDM_unpack_domains_child(count,num,st_meshset%MinID(1,iLv+1),&
          iLv+1,8,MrtN,p0%iPOS,rdest,st_param,st_meshset)
  endif
  return
end subroutine ppohAMRFDM_unpack_domains_child

subroutine ppohAMRFDM_connect_newocts(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::index,iLv,MtnNB(1:27),tNBIndex(1:6)
  type(st_ppohAMRFDM_octset),pointer::p0
  ! --- connect new left octs ---
  !$omp parallel do default(none) &
  !$omp& private(index,p0,tNbIndex,MtnNB) &
  !$omp& shared(st_param,st_meshset,st_comm_info)
  do index=st_meshset%MinID(1,0),st_meshset%MaxID(1,0)
     p0 => st_meshset%Mesh(index)
     call ppohAMRFDM_MrtN_nghbrs(p0%iPOS,MtnNB,st_param)
     tNbIndex(1:6)=MtnNB(1:6)-st_comm_info%MnDisp
     if(MtnNB(1)>=st_comm_info%MinMn.and.MtnNB(1)<=st_comm_info%MaxMn) then
        p0%octNb1 => st_meshset%Mesh(tNbIndex(1))
     else
        nullify(p0%octNb1)
     endif
     if(MtnNB(4)>=st_comm_info%MinMn.and.MtnNB(4)<=st_comm_info%MaxMn) then
        p0%octNb2 => st_meshset%Mesh(tNbIndex(4))
     else
        nullify(p0%octNb2)
     endif
     if(MtnNB(2)>=st_comm_info%MinMn.and.MtnNB(2)<=st_comm_info%MaxMn) then
        p0%octNb3 => st_meshset%Mesh(tNbIndex(2))
     else
        nullify(p0%octNb3)
     endif
     if(MtnNB(5)>=st_comm_info%MinMn.and.MtnNB(5)<=st_comm_info%MaxMn) then
        p0%octNb4 => st_meshset%Mesh(tNbIndex(5))
     else
        nullify(p0%octNb4)
     endif
     if(MtnNB(3)>=st_comm_info%MinMn.and.MtnNB(3)<=st_comm_info%MaxMn) then
        p0%octNb5 => st_meshset%Mesh(tNbIndex(3))
     else
        nullify(p0%octNb5)
     endif
     if(MtnNB(6)>=st_comm_info%MinMn.and.MtnNB(6)<=st_comm_info%MaxMn) then
        p0%octNb6 => st_meshset%Mesh(tNbIndex(6))
     else
        nullify(p0%octNb6)
     endif
  enddo
  !$omp end parallel do
  ! -- connect children --
  do iLv=0,st_param%LvMax-1
     call ppohAMRFDM_re_Gconnect(iLv,st_param,st_meshset)
  enddo
  return
end subroutine ppohAMRFDM_connect_newocts

subroutine ppohAMRFDM_re_Gconnect(iLv,st_param,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::iLv
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  integer(kind=ppohAMRFDM_kint)::ID,index
  type(st_ppohAMRFDM_octset),pointer::p0
  if(iLv>=st_param%LvMax) return
  do ID=1,2
     if(st_meshset%MinID(ID,iLv)<st_meshset%MaxID(ID,iLv)) then
        !$omp parallel do default(none) &
        !$omp& private(index,p0) &
        !$omp& shared(st_meshset,ID,iLv)
        do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
           p0 => st_meshset%Mesh(index)
           if(p0%iFLG(1)>0) then
              if(p0%iFLG(1)>=2) then ! This means that the cell is not at the edge.
                 ! - for octNb1 -
                 if(associated(p0%octNb1)) then
                    p0%octCh1%octNb1 => p0%octNb1%octCh2
                    p0%octCh3%octNb1 => p0%octNb1%octCh4
                    p0%octCh5%octNb1 => p0%octNb1%octCh6
                    p0%octCh7%octNb1 => p0%octNb1%octCh8
                 endif
                 ! - for octNb2 -
                 if(associated(p0%octNb2)) then
                    p0%octCh2%octNb2 => p0%octNb2%octCh1
                    p0%octCh4%octNb2 => p0%octNb2%octCh3
                    p0%octCh6%octNb2 => p0%octNb2%octCh5
                    p0%octCh8%octNb2 => p0%octNb2%octCh7
                 endif
                 ! - for octNb3 -
                 if(associated(p0%octNb3)) then
                    p0%octCh1%octNb3 => p0%octNb3%octCh3
                    p0%octCh2%octNb3 => p0%octNb3%octCh4
                    p0%octCh5%octNb3 => p0%octNb3%octCh7
                    p0%octCh6%octNb3 => p0%octNb3%octCh8
                 endif
                 ! - for octNb4 -
                 if(associated(p0%octNb4)) then
                    p0%octCh3%octNb4 => p0%octNb4%octCh1
                    p0%octCh4%octNb4 => p0%octNb4%octCh2
                    p0%octCh7%octNb4 => p0%octNb4%octCh5
                    p0%octCh8%octNb4 => p0%octNb4%octCh6
                 endif
                 ! - for octNb5 -
                 if(associated(p0%octNb5)) then
                    p0%octCh1%octNb5 => p0%octNb5%octCh5
                    p0%octCh2%octNb5 => p0%octNb5%octCh6
                    p0%octCh3%octNb5 => p0%octNb5%octCh7
                    p0%octCh4%octNb5 => p0%octNb5%octCh8
                 endif
                 ! - for octNb6 -
                 if(associated(p0%octNb6)) then
                    p0%octCh5%octNb6 => p0%octNb6%octCh1
                    p0%octCh6%octNb6 => p0%octNb6%octCh2
                    p0%octCh7%octNb6 => p0%octNb6%octCh3
                    p0%octCh8%octNb6 => p0%octNb6%octCh4
                 endif
                 ! - for octCh1 -
                 p0%octCh1%octNb2 => p0%octCh2
                 p0%octCh1%octNb4 => p0%octCh3
                 p0%octCh1%octNb6 => p0%octCh5
                 ! - for octCh2 -
                 p0%octCh2%octNb1 => p0%octCh1
                 p0%octCh2%octNb4 => p0%octCh4
                 p0%octCh2%octNb6 => p0%octCh6
                 ! - for octCh3 -
                 p0%octCh3%octNb2 => p0%octCh4
                 p0%octCh3%octNb3 => p0%octCh1
                 p0%octCh3%octNb6 => p0%octCh7
                 ! - for octCh4 -
                 p0%octCh4%octNb1 => p0%octCh3
                 p0%octCh4%octNb3 => p0%octCh2
                 p0%octCh4%octNb6 => p0%octCh8
                 ! - for octCh5 -
                 p0%octCh5%octNb2 => p0%octCh6
                 p0%octCh5%octNb4 => p0%octCh7
                 p0%octCh5%octNb5 => p0%octCh1
                 ! - for octCh6 -
                 p0%octCh6%octNb1 => p0%octCh5           
                 p0%octCh6%octNb4 => p0%octCh8
                 p0%octCh6%octNb5 => p0%octCh2
                 ! - for octCh7 -
                 p0%octCh7%octNb2 => p0%octCh8
                 p0%octCh7%octNb3 => p0%octCh5
                 p0%octCh7%octNb5 => p0%octCh3
                 ! - for octCh8 -
                 p0%octCh8%octNb1 => p0%octCh7
                 p0%octCh8%octNb3 => p0%octCh6
                 p0%octCh8%octNb5 => p0%octCh4
              endif
              if(p0%iFLG(1)==1) then !This is at the edge of overlap region
                 ! - for octCh1 -
                 p0%octCh1%octNb1 => p0%octCh1 
                 p0%octCh1%octNb2 => p0%octCh2 !
                 p0%octCh1%octNb3 => p0%octCh1 
                 p0%octCh1%octNb4 => p0%octCh3 !
                 p0%octCh1%octNb5 => p0%octCh1 
                 p0%octCh1%octNb6 => p0%octCh5 !
                 ! - for octCh2 -
                 p0%octCh2%octNb1 => p0%octCh1 !
                 p0%octCh2%octNb2 => p0%octCh2
                 p0%octCh2%octNb3 => p0%octCh2
                 p0%octCh2%octNb4 => p0%octCh4 !
                 p0%octCh2%octNb5 => p0%octCh2
                 p0%octCh2%octNb6 => p0%octCh6 !
                 ! - for octCh3 -
                 p0%octCh3%octNb1 => p0%octCh3 
                 p0%octCh3%octNb2 => p0%octCh4 !
                 p0%octCh3%octNb3 => p0%octCh1 !
                 p0%octCh3%octNb4 => p0%octCh3
                 p0%octCh3%octNb5 => p0%octCh3
                 p0%octCh3%octNb6 => p0%octCh7 !
                 ! - for octCh4 -
                 p0%octCh4%octNb1 => p0%octCh3 !
                 p0%octCh4%octNb2 => p0%octCh4
                 p0%octCh4%octNb3 => p0%octCh2 !
                 p0%octCh4%octNb4 => p0%octCh4
                 p0%octCh4%octNb5 => p0%octCh4
                 p0%octCh4%octNb6 => p0%octCh8 !
                 ! - for octCh5 -
                 p0%octCh5%octNb1 => p0%octCh5   
                 p0%octCh5%octNb2 => p0%octCh6 !
                 p0%octCh5%octNb3 => p0%octCh5
                 p0%octCh5%octNb4 => p0%octCh7 !
                 p0%octCh5%octNb5 => p0%octCh1 !
                 p0%octCh5%octNb6 => p0%octCh5
                 ! - for octCh6 -
                 p0%octCh6%octNb1 => p0%octCh5 !
                 p0%octCh6%octNb2 => p0%octCh6
                 p0%octCh6%octNb3 => p0%octCh6
                 p0%octCh6%octNb4 => p0%octCh8 !
                 p0%octCh6%octNb5 => p0%octCh2 !
                 p0%octCh6%octNb6 => p0%octCh6
                 ! - for octCh7 -
                 p0%octCh7%octNb1 => p0%octCh7 
                 p0%octCh7%octNb2 => p0%octCh8 !
                 p0%octCh7%octNb3 => p0%octCh5 !
                 p0%octCh7%octNb4 => p0%octCh7
                 p0%octCh7%octNb5 => p0%octCh3 !
                 p0%octCh7%octNb6 => p0%octCh7
                 ! - for octCh8 -
                 p0%octCh8%octNb1 => p0%octCh7 !
                 p0%octCh8%octNb2 => p0%octCh8
                 p0%octCh8%octNb3 => p0%octCh6 !
                 p0%octCh8%octNb4 => p0%octCh8
                 p0%octCh8%octNb5 => p0%octCh4 !
                 p0%octCh8%octNb6 => p0%octCh8
                 if(associated(p0%octNb1)) then
                    if(p0%octNb1%iFLG(1) >= 1) then ! In case there are children in octNb1
                       p0%octCh1%octNb1 => p0%octNb1%octCh2
                       p0%octCh3%octNb1 => p0%octNb1%octCh4
                       p0%octCh5%octNb1 => p0%octNb1%octCh6
                       p0%octCh7%octNb1 => p0%octNb1%octCh8
                    endif
                 endif
                 if(associated(p0%octNb2)) then
                    if(p0%octNb2%iFLG(1) >= 1) then !
                       p0%octCh2%octNb2 => p0%octNb2%octCh1
                       p0%octCh4%octNb2 => p0%octNb2%octCh3
                       p0%octCh6%octNb2 => p0%octNb2%octCh5
                       p0%octCh8%octNb2 => p0%octNb2%octCh7
                    endif
                 endif
                 if(associated(p0%octNb3)) then
                    if(p0%octNb3%iFLG(1) >= 1) then
                       p0%octCh1%octNb3 => p0%octNb3%octCh3
                       p0%octCh2%octNb3 => p0%octNb3%octCh4
                       p0%octCh5%octNb3 => p0%octNb3%octCh7
                       p0%octCh6%octNb3 => p0%octNb3%octCh8
                    endif
                 endif
                 if(associated(p0%octNb4)) then
                    if(p0%octNb4%iFLG(1) >= 1) then
                       p0%octCh3%octNb4 => p0%octNb4%octCh1
                       p0%octCh4%octNb4 => p0%octNb4%octCh2
                       p0%octCh7%octNb4 => p0%octNb4%octCh5
                       p0%octCh8%octNb4 => p0%octNb4%octCh6
                    endif
                 endif
                 if(associated(p0%octNb5)) then
                    if(p0%octNb5%iFLG(1) >= 1) then
                       p0%octCh1%octNb5 => p0%octNb5%octCh5
                       p0%octCh2%octNb5 => p0%octNb5%octCh6
                       p0%octCh3%octNb5 => p0%octNb5%octCh7
                       p0%octCh4%octNb5 => p0%octNb5%octCh8
                    endif
                 endif
                 if(associated(p0%octNb6)) then
                    if(p0%octNb6%iFLG(1) >= 1) then
                       p0%octCh5%octNb6 => p0%octNb6%octCh1
                       p0%octCh6%octNb6 => p0%octNb6%octCh2
                       p0%octCh7%octNb6 => p0%octNb6%octCh3
                       p0%octCh8%octNb6 => p0%octNb6%octCh4
                    endif
                 endif
              endif
           endif
        enddo
        !$omp end parallel do
     endif
  enddo
  return
end subroutine ppohAMRFDM_re_Gconnect

subroutine ppohAMRFDM_refine_DDD(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::iLv,ID,index,addG,RSize,GSize
  type(st_ppohAMRFDM_octset),pointer::p0
  ! -- remake hierarchy Goct --
  do iLv=0,st_param%LvMax-1
     ! -- initial refresh FLG --
     call ppohAMRFDM_passing_iFLG(iLv,st_param,st_meshset,st_comm_info)
     ! -- iFLG(1)>NFG ==> iFLG(1)>=NFG --
     allocate(st_meshset%rfnoct3(st_meshset%MinID(3,iLv):st_meshset%MaxID(3,iLv)))
     allocate(st_meshset%rfnoct4(st_meshset%MinID(4,iLv):st_meshset%MaxID(4,iLv)))
     st_meshset%rfnoct3(:)=0; st_meshset%rfnoct4(:)=0
     addG=0
     do ID=3,4
        if(st_meshset%MinID(ID,iLv)<st_meshset%MaxID(ID,iLv)) then
           !$omp parallel do default(none) &
           !$omp& private(index,p0) &
           !$omp& shared(st_meshset,ID,iLv) &
           !$omp& reduction(+:addG)
           do index=st_meshset%MinID(ID,iLv),st_meshset%MaxID(ID,iLv)
              p0 => st_meshset%GMesh(index)
              if(p0%iFLG(1)>0) then
                 p0%iFLG(2)=2
                 if(ID==3) then
                    st_meshset%rfnoct3(index)=8
                    addG=addG+8
                 else if(ID==4) then
                    st_meshset%rfnoct4(index)=8
                    addG=addG+8
                 endif
              else
                 p0%iFLG(2)=0
              endif
           enddo
           !$omp end parallel do
        endif
     enddo
     RSize=size(st_meshset% Mesh)
     GSize=size(st_meshset%GMesh)+addG
     call ppohAMRFDM_resize_Mesh(RSize,GSize,st_meshset,st_comm_info)
     call ppohAMRFDM_addGoct(iLv,st_param,st_meshset,st_comm_info)
     call ppohAMRFDM_connect_oct(iLv,st_param,st_meshset)
  enddo
  return
end subroutine ppohAMRFDM_refine_DDD
