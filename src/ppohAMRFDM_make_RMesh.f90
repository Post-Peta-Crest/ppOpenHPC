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

subroutine ppohAMRFDM_make_RMesh(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  type(st_ppohAMRFDM_octset)::st_octset
  integer(kind=ppohAMRFDM_kint)::ix,iy,iz
  integer(kind=ppohAMRFDM_kint)::octN,octLv,Csort,MrtN,bndry,load
  integer(kind=ppohAMRFDM_kint)::iFLG(1:3),iPOS(1:3)
  real(kind=ppohAMRFDM_kdbl)::F(1:st_param%npy),C(1:st_param%npy)
  ! -- initialize --
  nullify(st_octset%octPrt)
  nullify(st_octset%octNb1); nullify(st_octset%octNb2)
  nullify(st_octset%octNb3); nullify(st_octset%octNb4)
  nullify(st_octset%octNb5); nullify(st_octset%octNb6)
  nullify(st_octset%octCh1); nullify(st_octset%octCh2)
  nullify(st_octset%octCh3); nullify(st_octset%octCh4)
  nullify(st_octset%octCh5); nullify(st_octset%octCh6)
  nullify(st_octset%octCh7); nullify(st_octset%octCh8)
  nullify(st_octset%Psort)
  octLv=0; Csort=0; iFLG=0; bndry=1; load=1
  F=0.0d0; C=0.0d0
  !$omp parallel do default(none) &
  !$omp& private(MrtN,iPOS,ix,iy,iz,octN) &
  !$omp& shared(Csort,octLv,iFLG,bndry,F,C,load,&
  !$omp& st_param,st_meshset,st_comm_info,st_octset)
  do MrtN=st_comm_info%MinMn,st_comm_info%MaxMn
     ! -- from MrtN to normal index --
     call ppohAMRFDM_MrtN2Idxn(MrtN,iPOS)
     ix=iPOS(1); iy=iPOS(2); iz=iPOS(3)
     ! -- from normal index to hierarchy index --
     iPOS(1)=2*ix*st_param%Nint2-st_param%Nint2
     iPOS(2)=2*iy*st_param%Nint2-st_param%Nint2
     iPOS(3)=2*iz*st_param%Nint2-st_param%Nint2
     ! -- index for Mesh array --
     octN=MrtN-st_comm_info%MnDisp
     st_meshset%Mesh(octN)=&
          st_ppohAMRFDM_octset(octN,octLv,Csort,iFLG,&
          iPOS,MrtN,bndry,F,C,load,&
          st_octset%octPrt,&
          st_octset%octNb1,st_octset%octNb2,&
          st_octset%octNb3,st_octset%octNb4,&
          st_octset%octNb5,st_octset%octNb6,&
          st_octset%octCh1,st_octset%octCh2,&
          st_octset%octCh3,st_octset%octCh4,&
          st_octset%octCh5,st_octset%octCh6,&
          st_octset%octCh7,st_octset%octCh8,&
          st_octset%Psort)
  enddo
  !$omp end parallel do
  call ppohAMRFDM_connect(st_param,st_meshset,st_comm_info)
  return
end subroutine ppohAMRFDM_make_RMesh

subroutine ppohAMRFDM_connect(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  type(st_ppohAMRFDM_octset),pointer::p0
  integer(kind=ppohAMRFDM_kint)::index
  integer(kind=ppohAMRFDM_kint)::nbindx(1:6),Mnarr(1:27)
  integer(kind=ppohAMRFDM_kint)::iPOS(1:3),iPOS2(1:3)
  !$omp parallel do default(none) &
  !$omp& private(index,iPOS,iPOS2,nbindx,Mnarr,p0) &
  !$omp& shared(st_meshset,st_param,st_comm_info)
  do index=st_meshset%MinID(1,0),st_meshset%MaxID(1,0)
     p0 => st_meshset%Mesh(index)
     iPOS(:)=p0%iPOS(:)
     call ppohAMRFDM_MrtN_nghbrs(iPOS,Mnarr,st_param)
     iPOS2=(iPOS+st_param%Nint2)/(2*st_param%Nint2)
     nbindx(1:6)=Mnarr(1:6)-st_comm_info%MnDisp
     !  -- for -x neighbor --
     if(nbindx(1)>0.and.nbindx(1)<=st_meshset%MaxID(1,0)) then
        p0%octNb1 => st_meshset%Mesh(nbindx(1))
     endif
     !  -- for +x neighbor --
     if(nbindx(4)>0.and.nbindx(4)<=st_meshset%MaxID(1,0)) then
        p0%octNb2 => st_meshset%Mesh(nbindx(4))
     endif
     !  -- for -y neighbor --
     if(nbindx(2)>0.and.nbindx(2)<=st_meshset%MaxID(1,0)) then
        p0%octNb3 => st_meshset%Mesh(nbindx(2))
     endif
     !  -- for +y neighbor --
     if(nbindx(5)>0.and.nbindx(5)<=st_meshset%MaxID(1,0)) then
        p0%octNb4 => st_meshset%Mesh(nbindx(5))
     endif
     !  -- for -z neighbor --
     if(nbindx(3)>0.and.nbindx(3)<=st_meshset%MaxID(1,0)) then
        p0%octNb5 => st_meshset%Mesh(nbindx(3))
     endif
     !  -- for +z neighbor --
     if(nbindx(6)>0.and.nbindx(6)<=st_meshset%MaxID(1,0)) then
        p0%octNb6 => st_meshset%Mesh(nbindx(6))
     endif
     p0%Psort => p0
  enddo
  !$omp end parallel do
  return
end subroutine ppohAMRFDM_connect
