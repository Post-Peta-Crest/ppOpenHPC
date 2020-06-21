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

subroutine ppohAMRFDM_pre(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint)::nrmesh
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  ! -- check nprocs --
  if(st_param%pxmax*st_param%pymax*st_param%pzmax/=st_comm_info%nprocs) then
     call ppohAMRFDM_barrier(st_comm_info)
     if(st_comm_info%rank==0) print *,'error, setup process numbers'
     call ppohAMRFDM_abort
  endif
  ! -- set thread numbers for openMP --
  !$ call omp_set_num_threads(st_param%iomp0)
  ! -- set parameters --
  st_param%nlv=st_param%nbf
  st_param%nfg=st_param%nlv+1
  st_param%nint2=2**st_param%LvMax
  st_meshset%rfncnt=0
  ! -- allocate memory --
  allocate(st_param%crtr_r(0:st_param%LvMax))
  allocate(st_param%crtr_d(0:st_param%LvMax))
  st_param%crtr_r(:)=st_param%crtr_r0
  st_param%crtr_d(:)=st_param%crtr_d0
  allocate(st_param%itorder(0:st_param%LvMax+1))
  allocate(st_param%lvorder(1:2**(st_param%LvMax+2)))
  allocate(st_param%nID(0:st_param%LvMax))
  allocate(st_comm_info%Mn2CPU(1:st_comm_info%nprocs))
  allocate(st_comm_info%nrproc(1:st_comm_info%nprocs))
  allocate(st_comm_info%ngproc(1:st_comm_info%nprocs))
  allocate(st_comm_info%snum(0:st_param%LvMax,1:st_comm_info%nprocs))
  allocate(st_comm_info%rnum(0:st_param%LvMax,1:st_comm_info%nprocs))
  allocate(st_meshset%sBuf(1:st_comm_info%nprocs))
  allocate(st_meshset%rBuf(1:st_comm_info%nprocs))
  allocate(st_meshset%getID(0:st_param%LvMax))
  allocate(st_meshset%MinID(1:4,0:st_param%LvMax))
  allocate(st_meshset%MaxID(1:4,0:st_param%LvMax))
  ! -- set initial Mesh arr. --
  nrmesh=st_param%ixmax*st_param%iymax*st_param%izmax
  allocate(st_meshset%Mesh(1:nrmesh))
  call ppohAMRFDM_setID(st_param,st_comm_info,st_meshset)
  call ppohAMRFDM_setev(st_param)
  ! -- make initial mesh data --
  call ppohAMRFDM_make_RMesh(st_param,st_meshset,st_comm_info)
  call ppohAMRFDM_make_GMesh(st_param,st_meshset,st_comm_info)
  ! -- initial mesh refinement to initLv --
  call ppohAMRFDM_refine_base(st_param,st_meshset,st_comm_info)
  return
end subroutine ppohAMRFDM_pre

subroutine ppohAMRFDM_setID(st_param,st_comm_info,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint)::index
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  type(st_ppohAMRFDM_meshset)::st_meshset
  ! - set MnDisp -
  st_comm_info%MnDisp=st_comm_info%rank*(st_param%ixmax*st_param%iymax*st_param%izmax)
  ! - set Mn2CPU -
  do index=1,st_comm_info%nprocs
     st_comm_info%Mn2CPU(index)=(st_param%ixmax*st_param%iymax*st_param%izmax)*index
  enddo
  ! - Specify the Min and Max Morton on each process -
  if(st_comm_info%rank==0) then
     st_comm_info%MinMn=1
  else
     st_comm_info%MinMn=st_comm_info%Mn2CPU(st_comm_info%rank)+1
  endif
  st_comm_info%MaxMn=st_comm_info%Mn2CPU(st_comm_info%rank+1)
  ! - for Level 0 -
  st_meshset%MinID(1,0)=1
  st_meshset%MaxID(1,0)=st_param%ixmax*st_param%iymax*st_param%izmax
  st_meshset%MinID(2,0)=st_param%ixmax*st_param%iymax*st_param%izmax
  st_meshset%MaxID(2,0)=st_param%ixmax*st_param%iymax*st_param%izmax
  ! - for Level > 1 - 
  if(st_param%LvMax>0) then 
     do index=1,st_param%LvMax
        st_meshset%MinID(1,index)=st_param%ixmax*st_param%iymax*st_param%izmax
        st_meshset%MaxID(1,index)=st_param%ixmax*st_param%iymax*st_param%izmax
        st_meshset%MinID(2,index)=st_param%ixmax*st_param%iymax*st_param%izmax
        st_meshset%MaxID(2,index)=st_param%ixmax*st_param%iymax*st_param%izmax
     enddo
  endif
  return
end subroutine ppohAMRFDM_setID

subroutine ppohAMRFDM_setev(st_param)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint)::index,itemp
  type(st_ppohAMRFDM_param)::st_param
  st_param%itorder(0)=0; st_param%itorder(1)=1
  do index=2,st_param%LvMax+1
     st_param%itorder(index)=st_param%itorder(index-1)+2**(index-1)
  enddo
  do index=1,st_param%LvMax+1
     st_param%itorder(index)=st_param%itorder(index)*2
  enddo
  itemp=st_param%itorder(st_param%LvMax+1)
  call ppohAMRFDM_makestep(0,itemp,st_param)
  st_param%itorder(:)=st_param%itorder(:)/2
  return
end subroutine ppohAMRFDM_setev

recursive subroutine ppohAMRFDM_makestep(iLv,ID,st_param)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::iLv
  integer(kind=ppohAMRFDM_kint)::ID
  type(st_ppohAMRFDM_param)::st_param
  st_param%lvorder(ID)=iLv
  if(iLv/=st_param%LvMax) call ppohAMRFDM_makestep(iLv+1,ID-1,st_param)
  ID=ID-st_param%itorder(st_param%LvMax-iLv)-1
  st_param%lvorder(ID)=iLv
  if(iLv/=st_param%LvMax) call ppohAMRFDM_makestep(iLv+1,ID-1,st_param)
  return
end subroutine ppohAMRFDM_makestep
