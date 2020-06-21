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

subroutine ppohAMRFDM_copy(iLv,iF,iK,iN,st_param,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::iLv,iF,iK,iN
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  integer(kind=ppohAMRFDM_kint)::index,ID,is,ie
  type(st_ppohAMRFDM_octset),pointer::p0
  if(iLv==st_param%LvMax) return
  if(st_meshset%MaxID(iN,iLv)<=st_meshset%MinID(iN,iLv)) return
  is=1; ie=st_param%npy
  !$omp parallel do default(none) &
  !$omp& private(index,p0,ID) &
  !$omp& shared(st_param,st_meshset,iLv,iF,iK,iN,is,ie)
  do index=st_meshset%MinID(iN,iLv),st_meshset%MaxID(iN,iLv)
     if(IN==1.or.IN==2) then
        p0 => st_meshset%Mesh(index)
     else if(IN==3.or.IN==4) then
        p0 => st_meshset%GMesh(index)
     endif
     if(p0%iFLG(iK)==iF) then
        do ID=is,ie
           p0%octCh1%F(ID)=p0%F(ID)
           p0%octCh2%F(ID)=p0%F(ID)
           p0%octCh3%F(ID)=p0%F(ID)
           p0%octCh4%F(ID)=p0%F(ID)
           p0%octCh5%F(ID)=p0%F(ID)
           p0%octCh6%F(ID)=p0%F(ID)
           p0%octCh7%F(ID)=p0%F(ID)
           p0%octCh8%F(ID)=p0%F(ID)
        enddo
     endif
  enddo
  !$omp end parallel do
  return
end subroutine ppohAMRFDM_copy
